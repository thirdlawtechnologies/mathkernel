# `defkernel` DSL Manual

This document describes the `defkernel` macro and the surrounding machinery:
automatic differentiation, expression simplification, optimization, and code
generation to C.

The goal of the DSL is to let you write clean mathematical kernels (bond
stretch, angle, dihedral, nonbonded, etc.), and let the system derive
gradients and Hessians, factor common structure, and emit efficient C code.


## 1. Big picture

The pipeline from a `defkernel` form to compiled C looks like:

1. **User DSL**  
   You write a kernel using `defkernel` with:
   - parameter list
   - layout info
   - coordinate load
   - a `stmt-block` for the mathematical body
   - an optional `:derivatives` geometry spec
   - optional per‑kernel rewrite rules

2. **Kernel IR construction**  
   `defkernel` expands into a call to `make-kernel-from-block`, which:
   - normalizes the `:derivatives` spec
   - lowers explicit derivative requests `(D! target base)`
   - verifies/interpolates geometry derivatives (optional)
   - builds gradient and Hessian assignments from the body and derivative spec
   - runs an optimization pipeline (CSE, factoring, copy‑propagation, etc.)

3. **Energy/gradient/Hessian lowering**  
   `transform-eg-h-block` walks the optimized block and:
   - keeps the scalar assignments (`ENERGY`, `G_X1`, `H_X1_Y1`, ...)
   - injects energy accumulation and force/Hessian macro calls
   - respects explicit `(accumulate-here)` markers and control flow

4. **Code generation**  
   The final block is wrapped in a C function with:
   - scalar locals inferred from the statements
   - pointer parameters for positions, forces, Hessians, etc.
   - calls to macros like `KernelForceAcc`, `DiagHessAcc`, `KernelOffDiagHessAccc`


---

## 2. The `defkernel` macro

### 2.1 Syntax

```lisp
(defkernel *kernel-name*
  (:c-function-name some_c_identifier)
  (:compute (energy grad hess))      ; which pieces to generate
  (:pipeline *pipeline*)             ; optimization pipeline object
  (:extra-equivalence-rules rules1)  ; optional, per‑kernel rewrite rules
  (:extra-optimization-rules rules2) ; optional, per‑kernel rewrite rules
  (:params ((double A) (double B) ...))
  (:layout  ((1 . I3X1) (2 . I3X2))
            ((X . 0) (Y . 1) (Z . 2)))
  (:coord-vars (x1 y1 z1 x2 y2 z2))
  (:coord-load
   (coords-from-position
     ((x1 y1 z1 i3x1)
      (x2 y2 z2 i3x2))))
  (:body
   (stmt-block
     ... statements ...))
  (:derivatives
   (:mode :manual
    ... geometry spec ...)))
```

### 2.2 Expansion (what `defkernel` really does)

The macro is implemented in terms of simple `assoc`‑based clause lookup:

```lisp
(defmacro defkernel (name &body clauses)
  (labels ((clause-value (key)
             (cadr (assoc key clauses))))
    (let* ((c-function-name (clause-value :c-function-name))
           (compute-list    (clause-value :compute))
           (pipeline        (clause-value :pipeline))
           (params          (clause-value :params))
           (layout-cl       (assoc :layout clauses))
           (coord-vars      (mapcar #'expr-ir:ev (clause-value :coord-vars)))
           (coord-load      (clause-value :coord-load))
           (body-form       (clause-value :body))
           (deriv-cl        (assoc :derivatives clauses))
           (deriv-spec      (and deriv-cl (second deriv-cl)))
           (extra-eq-rules  (clause-value :extra-equivalence-rules))
           (extra-opt-rules (clause-value :extra-optimization-rules))
           (atom->ibase     (second layout-cl))
           (axis->offset    (third layout-cl))
           (coord-names     (mapcar #'symbol-name coord-vars))
           (want-grad       (and compute-list
                                 (member 'grad compute-list)))
           (want-hess       (and compute-list
                                 (member 'hess compute-list))))
      `(defparameter ,name
         (let* ((layout (make-kernel-layout
                         :atom->ibase ',atom->ibase
                         :axis->offset ',axis->offset))
                (coord-vars (vars ,@coord-names))
                (coord-load-stmts ,coord-load)
                (base-block ,body-form))
           (make-kernel-from-block
            :name ',c-function-name
            :pipeline ,pipeline
            :layout layout
            :coord-vars coord-vars
            :coord-load-stmts coord-load-stmts
            :base-block base-block
            :params ',params
            :compute-energy t
            :compute-grad ,want-grad
            :compute-hess ,want-hess
            :derivatives ',deriv-spec
            :extra-equivalence-rules ,extra-eq-rules
            :extra-optimization-rules ,extra-opt-rules))))))
```

So defining a kernel binds a `*kernel-name*` parameter to an internal
“kernel object” produced by `make-kernel-from-block`.


---

## 3. Kernel layout and coordinates

### 3.1 Layout

The `:layout` clause is split into two maps:

```lisp
(:layout  ((1 . I3X1) (2 . I3X2))
          ((X . 0) (Y . 1) (Z . 2)))
```

- **atom→ibase**  
  `(1 . I3X1)` means “atom 1’s 3‑vector index is `i3x1` in the C parameter list”.
  That is used when emitting `position[I3X1 + axis]`, `KernelForceAcc(i3x1, axis, ...)`,
  etc.

- **axis→offset**  
  `(X . 0)` tells the generator that X is component `+0`, Y is `+1`, Z is `+2`.

These maps are carried in a `kernel-layout` object and used by
`transform-eg-h-block` to translate scalar gradient/Hessian names into
`KernelForceAcc` / `DiagHessAcc` / `KernelOffDiagHessAccc` calls with the right indices.


### 3.2 Coord-vars and coord-load

```lisp
(:coord-vars (x1 y1 z1 x2 y2 z2))

(:coord-load
 (coords-from-position
   ((x1 y1 z1 i3x1)
    (x2 y2 z2 i3x2))))
```

- `:coord-vars` gives the symbolic names used in the body for coordinates.
- `:coord-load` is a small DSL that expands into assignments like:

  ```c
  x1 = position[i3x1 + 0];
  y1 = position[i3x1 + 1];
  ...
  ```

The coord‑load block is prepended to the optimized core block during
codegen; you write the physics as if `x1,y1,z1,...` were ordinary locals.


---

## 4. Statements and expressions

### 4.1 Statement IR (high level)

The `:body` is written as a `stmt-block` using helper macros like:

- `=.` – assignment from an infix string:

  ```lisp
  (=. dx "x1 - x2")
  (=. r2 "dx*dx + dy*dy + dz*dz")
  (=. r  "sqrt(r2)")
  ```

  Each form expands to a `stmt-ir:assignment-statement` whose RHS is an
  `expr-ir` expression parsed from the string.

- `stmt-block` – grouping:

  ```lisp
  (stmt-block
    (=. dx "x1 - x2")
    (=. dy "y1 - y2")
    ...)
  ```

- If‑statements via constructors:

  ```lisp
  (stmt-ir:make-if-stmt
    (expr-ir:parse-expr "r < r_cut")
    (stmt-block ...then...)
    (stmt-block ...else...))
  ```

These are all converted into a tree of `assignment-statement`,
`block-statement`, and `if-statement` objects.

### 4.2 Expression IR

RHS expressions are represented in a typed IR:

- constants, variables
- n‑ary `+` and `*`
- unary `-`
- `expt`, `sqrt`, `sin`, `cos`, `exp`, `log`, `acos`, etc.
- generic function call nodes (`function-call-expression`)

Converting between IR and sexpr is done with:

- `expr-ir:expr->sexpr`
- `expr-ir:sexpr->expr-ir`

Expression simplification (constant folding, trivial algebra) is handled
by `expr-ir:simplify-expr`.


---

## 5. Automatic differentiation

There are two intertwined mechanisms:

1. **Expression‑level AD** for building derivative expressions.
2. **Statement‑level AD** for walking a block and computing d(var)/d(base).


### 5.1 Expression‑level AD

Core driver:

```lisp
(expr-ir:differentiate-expr expr base-var env)
```

- `expr` is an expression IR tree.
- `base-var` is the symbol we differentiate with respect to.
- `env` is a derivative environment mapping intermediate variables to
  their derivatives w.r.t. `base-var` (see below).

It knows calculus rules for:

- `+`, `-`, `*`, `/`
- `expt`
- `sqrt`, `exp`, `log`
- `sin`, `cos`, `acos`

and treats unknown functions as black boxes with derivative 0 (unless
explicitly extended).


### 5.2 Derivative environment for a block

To differentiate with respect to a base variable inside a statement block,
we build a derivative environment that tracks d(var)/d(base) for each
assigned variable in order:

```lisp
(defun build-derivative-env-for-block (statements base-var)
  (let ((env (expr-ir:make-deriv-env)))
    ;; Seed env[base-var] = 1
    (setf (gethash (expr-ir:var-key base-var) env)
          (expr-ir:make-expr-const 1))
    (dolist (st statements env)
      (when (typep st 'stmt-ir:assignment-statement)
        (let* ((name (stmt-ir:stmt-target-name st))
               (rhs  (stmt-ir:stmt-expression st))
               (drhs (expr-ir:differentiate-expr rhs base-var env))
               (key  (expr-ir:var-key name)))
          (setf (gethash key env) drhs))))))
```

Given this `env`, we can retrieve the derivative of any intermediate
variable with respect to `base-var`.


### 5.3 Statement‑level AD: `D!` macro and requests

Inside a kernel body, you can request derivatives “on the fly”:

```lisp
(=. e_base "e_lj + e_coul")

(D! e_base r)
(D! de_base_dr r)
```

The macro expands into a special IR statement:

```lisp
(stmt-ir:make-derivative-request-stmt
  (expr-ir:ev 'e_base)
  (expr-ir:ev 'r))
```

Before any geometry checks or gradient/Hessian synthesis, the kernel
builder runs:

```lisp
(base-block* (expand-derivative-requests base-block))
```

`expand-derivative-requests`:

1. Walks the block in linear order.
2. When it finds a `DERIVATIVE-REQUEST-STATEMENT` for `(target base)`:
   - uses the current list of assignments in scope
   - builds a derivative environment w.r.t. `base`
   - looks up the derivative of `target`
   - emits a new assignment statement

      ```lisp
      DE_BASE_DR := (d e_base / d r)
      ```

   - inserts that assignment in place of the request.

By the time we reach optimization and codegen, there are only ordinary
assignments; no derivative request statements remain.


---

## 6. Geometry specification (`:derivatives`)

The `:derivatives` clause connects **intermediate “channel” variables**
like `r` or `theta` to **coordinate variables**, and connects overall
energy to those channels.

### 6.1 Manual mode layout

Example (stretch term):

```lisp
(:derivatives
 (:mode :manual
  :intermediates (r)

  :intermediate->coord
  ((r ((x1 "dx / r")
       (y1 "dy / r")
       (z1 "dz / r")
       (x2 "-dx / r")
       (y2 "-dy / r")
       (z2 "-dz / r"))))

  :intermediate->coord2
  ((r (((x1 x1) "(r*r - dx*dx)/(r^3)")
       ((x1 y1) "(-dx*dy)/(r^3)")
       ...
       ((z2 z2) "(r*r - dz*dz)/(r^3)"))))

  :energy->intermediate
  (:gradient ((r "dE_dr"))
   :hessian  (((r r) "d2E_dr2")))

  :hessian-modes ((r :full))
  :geometry-check :warn))
```

Conceptually:

- **`intermediates`**  
  Channel variables like `r` or `theta`.

- **`:intermediate->coord`**  
  First derivatives: ∂u/∂q where `u` is an intermediate and `q` is a
  coordinate variable (`x1`, `y1`, ...).

- **`:intermediate->coord2`**  
  Second derivatives: ∂²u / (∂qi ∂qj). Only upper triangular pairs are
  required; symmetry is assumed.

- **`:energy->intermediate`**  
  How scalar channel(s) enter the energy:
  - `:gradient` gives ∂E/∂u
  - `:hessian` gives ∂²E/(∂u ∂v)

- **`:hessian-modes`**  
  Allows per‑channel control of which Hessian contributions are kept
  (e.g. full vs. “Gauss–Newton” type approximations).

- **`:geometry-check`**  
  Controls how strictly to compare manual geometry derivatives against
  auto‑differentiated ones (`:off`, `:warn`, `:error`).


### 6.2 Geometry AD and consistency checking

During `make-kernel-from-block` (after `D!` expansion), we optionally:

1. **Check geometry**

   ```lisp
   (when manual-deriv-spec
     (check-intermediate-geometry! base-block* coord-vars manual-deriv-spec))
   ```

   For each `(u, q)` and `(u, qi, qj)` specified in
   `:intermediate->coord` / `:intermediate->coord2`:

   - auto‑differentiate `u` with respect to coordinates
   - canonicalize both manual and automatic expressions
   - compare for equivalence
   - if mismatched: report either a warning or an error, printing
     expanded sexprs so you can debug the difference

2. **Auto‑fill missing geometry**

   ```lisp
   (auto-fill-intermediate-geometry-from-ad
     base-block* coord-vars manual-deriv-spec
     :compute-second-derivs t
     :reuse-assignments    t)
   ```

   For any missing entries in `:intermediate->coord` or
   `:intermediate->coord2`, we compute them via AD and insert them into
   the internal manual‑deriv struct. This lets you specify only the
   “interesting” rows and rely on AD for the rest.


### 6.3 Energy gradient and Hessian via channels

Given:

- ∂E/∂u, ∂²E/(∂u ∂v)
- ∂u/∂q, ∂²u/(∂qi ∂qj)

the system builds:

- **Gradient wrt coordinates**

  ```text
  dE/dq = Σ_u (dE/du) (du/dq)
  ```

- **Hessian wrt coordinates**

  ```text
  d²E/(dqi dqj) =
      Σ_{u,v} (d²E/(du dv)) (du/dqi) (dv/dqj)
    + Σ_u (dE/du) (d²u/(dqi dqj))      [optional, per hessian-mode]
  ```

The builders `make-gradient-assignments-from-block` and
`make-hessian-assignments-from-block` assemble these into scalar
assignments like:

```lisp
G_X1 := ...    ; gradient on x1 component
H_X1_X1 := ... ; Hessian entry (x1,x1)
H_X1_Y1 := ... ; etc.
```


---

## 7. Optimization and expression rewriting

There are two related notions:

1. A **global optimization pipeline** (`*pipeline*`) that operates on
   statement blocks.
2. An **expression rewriting system** that can be used both for
   optimization and for canonicalization / equivalence checking.


### 7.1 Optimization pipeline

A typical pipeline looks like:

```lisp
(defparameter *pipeline*
  (stmt-ir:make-optimization-pipeline
   :name :kernel-full
   :optimizations
   (list
    ;; 1. Linear canonicalization (optional)
    (stmt-ir:make-optimization
     :name :linear-canonicalization
     :function #'stmt-ir:linear-canonicalization-optimization)

    ;; 2. Factor sums
    (stmt-ir:make-optimization
     :name :factor-sums
     :function #'stmt-ir:factor-sums-optimization
     :keyword-args (list :min-uses 2
                         :min-factors 1
                         :min-size 4))

    ;; 3. CSE over full block
    (stmt-ir:make-optimization
     :name :cse-full
     :function #'stmt-ir:cse-block-multi-optimization
     :keyword-args (list :max-passes 50
                         :min-uses 2
                         :min-size 1))

    ;; 4. Temp+param factoring
    (stmt-ir:make-optimization
     :name :factor-temp-param
     :function #'stmt-ir:factor-temp-param-products-optimization
     :keyword-args (list :min-uses    2
                         :min-factors 2
                         :max-factors 3))

    ;; 5. Copy propagation (branch‑aware)
    (stmt-ir:make-optimization
     :name :copy-propagate
     :function #'stmt-ir:copy-propagate-optimization)

    ;; 6. Sign normalization
    (stmt-ir:make-optimization
     :name :normalize-signs
     :function #'stmt-ir:normalize-signs-optimization))))
```

The pipeline is applied twice in `make-kernel-from-block`:

1. Once after adding gradient assignments (energy+grad block).
2. Again after adding Hessian assignments (energy+grad+H block).

There is also room for a **post‑EGH pipeline** (running after
`transform-eg-h-block`) to clean up C‑level expressions, including
substituting `pow(r,-1) → invr` etc.


### 7.2 Copy‑propagation and control flow

`copy-propagate-optimization` is careful around branches:

- It maintains a map `env` of aliases `dst → src` for trivial copies.
- When it encounters an `if` statement:
  - it rewrites the condition through the current `env`
  - it recurses on the then/else blocks with **clones** of `env`
  - it does **not** attempt to merge alias environments from different
    branches

That guarantees we don’t “leak” information across control‑flow paths
that may or may not execute, which is crucial for kernels where the
physics is piecewise (e.g. cutoff and switching functions).


### 7.3 Expression rewrite rules

Expressions are rewritten using pattern rules on s‑expressions.

#### 7.3.1 Template rules

A template rule is created with `make-template-rule`:

```lisp
(make-template-rule
  :expt-absorb-base-1
  '(* ?prefix* (expt ?x ?a) ?mid* ?x ?rest*)
  '(* ?prefix* (expt ?x (+ ?a 1)) ?mid* ?rest*))
```

Here:

- `?x` / `?a` are single‑subtree wildcards.
- `?prefix*`, `?mid*`, `?rest*` are sequence wildcards (lists of arguments,
  possibly empty).

The matcher binds wildcards into an environment, and the template RHS is
instantiated using those bindings.

#### 7.3.2 Function rules

Function rules allow arbitrary Lisp code to transform a match:

```lisp
(make-function-rule
  :expt-of-factors-factors-of-expts
  '(expt (* ?factors*) ?p)
  #'expand-expt-of-factors)
```

where the transformer has signature:

```lisp
(defun expand-expt-of-factors (env sexpr)
  (declare (ignore sexpr))
  (let* ((factors (lookup-wild '?factors* env))
         (power   (lookup-wild '?p env))
         (expanded (mapcar (lambda (f) `(expt ,f ,power)) factors)))
    (cons '* expanded)))
```

#### 7.3.3 Canonicalization vs optimization rule sets

We maintain separate rule sets:

- `*rewrite-rules-basic*` – algebraic identities like
  `(+ x 0) → x`, `(* x 1) → x`, basic exponent laws, etc.
- `*equivalence-canonicalization-rules*` – richer rules used when
  comparing expressions (e.g. to check geometry derivatives). These may
  expand things (e.g. distribute multiplication over addition) or rewrite
  radial geometry using known identities (`r2 = r^2`, etc.).
- `*optimization-rules*` – rewrite rules oriented around reducing flops
  or exploiting available temporaries, e.g. combining reciprocals when
  that is cheaper.

`canonicalize-expr-for-equivalence` applies the equivalence rules until
a fixed point. Optimization passes can call
`rewrite-expr-with-rules` using `*optimization-rules*` or per‑kernel
rules.


### 7.4 Per‑kernel rewrite rules

`defkernel` supports optional clauses:

```lisp
(:extra-equivalence-rules *angle-eq-rules*)
(:extra-optimization-rules *angle-opt-rules*)
```

These are simply lists of rewrite rules that get concatenated with the
global sets for this kernel only. This lets you:

- express kernel‑specific identities (e.g. dihedral geometry shortcuts)
- experiment with aggressive transformations without affecting other kernels


---

## 8. Accumulation and `accumulate-here`

For some kernels (e.g. nonbonded with cutoffs), you want to avoid
accumulating energy/forces/Hessians outside a particular control‑flow
region. For example:

```lisp
(stmt-block
  (=. dx "x1 - x2")
  (=. dy "y1 - y2")
  (=. dz "z1 - z2")
  (=. r2 "dx*dx + dy*dy + dz*dz")
  (=. r  "sqrt(r2)")
  (stmt-ir:make-if-stmt
    (expr-ir:parse-expr "r < r_cut")
    (stmt-block
      ... compute e_base, de_dr, d2e_dr2 ...
      (accumulate-here))
    (stmt-block
      ;; r >= r_cut: do nothing
      )))
```

### 8.1 The `accumulate-here` directive

`(accumulate-here)` expands into a small marker statement type
(e.g. `ACCUMULATE-HERE-STATEMENT`). It is **not** a call or expression;
it is a placeholder in the statement tree.

`transform-eg-h-block` is then responsible for:

- Traversing the block with a current “accumulation context”.
- When it encounters `accumulate-here`:
  - it emits the energy accumulation and the `KernelForceAcc` /
    `DiagHessAcc` / `KernelOffDiagHessAccc` macros for whatever gradient/Hessian
    scalar variables exist at that point.
  - it does **not** add any accumulation outside of guarded regions.

This ensures that:

- For `r >= r_cut`, nothing is accumulated (we don’t even compute the
  radial derivatives in that branch).
- For `r < r_cut`, energy, forces, and Hessians are all computed and
  accumulated inside the `if` branch.


---

## 9. Code generation to C

The final step is lowering the transformed IR into a C function.

### 9.1 Kernel IR and C function

`make-kernel-from-block` returns a `kernel-ir` instance (implemented as
a class/struct), which includes at least:

- `:name` – C function name
- `:layout`
- `:coord-vars`
- `:coord-load-stmts`
- `:params` – C parameter list
- `:core-block` – the optimized, AD‑expanded block **before** the EG/H
  transformation
- `:compute-energy-p`, `:compute-grad-p`, `:compute-hess-p`
- `:manual-deriv-spec`
- `:pipeline`

For the C backend we then:

1. Wrap coordinate loads + transformed block:

   ```lisp
   (let* ((core-block        ...)
          (block-with-coords (if coord-load-stmts
                                 (stmt-ir:make-block-stmt
                                  (append coord-load-stmts
                                          (list core-block)))
                                 core-block))
          (transformed
            (transform-eg-h-block
              block-with-coords
              layout coord-vars
              #'general-grad-name
              #'general-hess-name))
          (locals (infer-kernel-locals transformed params coord-vars)))
     (stmt-ir:make-c-function
       name
       transformed
       :return-type "void"
       :parameters  params
       :locals      locals))
   ```

2. Emit C declarations for all locals (including temporaries like
   `cse_p1_t1`).
3. Emit C code for each statement:
   - assignments `x := expr` → `x = <expr>;`
   - if‑statements → `if (...) { ... } else { ... }`
   - EG/H macros:
     - `ENERGY := ...` + accumulation → `*energy_accumulate += energy;`
     - `G_X1 := ...` → `KernelForceAcc(i3x1, 0, g_x1);`
     - `H_X1_X1 := ...` → `DiagHessAcc(i3x1, 0, i3x1, 0, h_x1_x1);`
     - `H_X1_Y1 := ...` → `KernelOffDiagHessAccc(...)`, etc.


### 9.2 Pow/exponent optimizations and named temporaries

To help the optimizer pick up named temporaries (e.g. `invr` vs.
`pow(r,-1)`), you can:

- Introduce explicit assignments in the kernel body:

  ```lisp
  (=. invr  "r^-1")
  (=. invr2 "invr*invr")
  (=. invr3 "invr*invr2")
  ```

- Add rewrite rules (global or per‑kernel) that match pow‑patterns in
  expressions *after* AD & EG/H and turn them into references to these
  named temporaries whenever it’s safe.

A typical pattern is to use a **post‑EGH optimization pass** that:
- scans for expressions structurally equivalent to `pow(r,-1)`, `pow(r,-2)`,
  `pow(r,-3)`, etc.
- replaces them with `invr`, `invr2`, `invr3` when those variables are
  in scope and represent those formulas.


---

## 10. Putting it all together: typical workflow

1. **Write the kernel in physics terms**

   - Declare parameters and coordinates.
   - Load coordinates.
   - Compute the geometric intermediates (`dx,dy,dz,r2,r`).
   - Compute the base energy and channels (`e_base`, `r`, `theta`, ...).
   - Use `D!` to obtain radial/channel derivatives when convenient.
   - Use `if` statements for piecewise regions (cutoffs, switch regions).
   - Place `(accumulate-here)` where you want energy/force/Hessian
     accumulation to occur (for conditionally active interactions).

2. **Specify geometry derivatives in `:derivatives`**

   - Add manual formulas for ∂u/∂q and ∂²u/(∂qi ∂qj) when you want tight,
     human‑optimized geometry.
   - Add `:energy->intermediate` mapping to connect channels to energy.
   - Set `:geometry-check` to `:warn` or `:error` while developing.

3. **Compile kernel**

   - The `defkernel` macro expands to a `kernel-ir` value.
   - AD requests are expanded, geometry is checked/filled, gradients and
     Hessians are generated, the optimization pipeline runs.

4. **Inspect and tune**

   - Use debugging helpers (like `debug-block`) to inspect intermediate
     IR.
   - Adjust rewrite rules or per‑kernel optimization rules to enforce
     particular factorizations or canonical forms.
   - Tighten Hessian modes as needed for your optimizer (e.g. full vs
     approximate).

5. **Emit and test C**

   - Generate the C function.
   - Verify numerical correctness against reference implementations.
   - Profile and tweak rewrite rules / pipeline ordering if needed.


---

## 11. Extensibility ideas

- Add more elementary functions and corresponding derivative rules
  (e.g. `atan2` as a true 2‑argument primitive) to expression‑level AD.
- Extend the rewrite system with more geometric identities (especially
  for angle/dihedral terms).
- Modularize pipelines per kernel type (bond, angle, dihedral, nonbond)
  using `:extra-optimization-rules`.
- Add CUDA / other backends by lowering `kernel-ir` to different target
  languages while reusing all the AD and simplification machinery.