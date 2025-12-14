
(asdf:defsystem #:mathkernel
  :description "Optimize mathematical expressions using a compiler implemented in common lisp "
  :version "0.0.1"
  :author "Christian Schafmeister <chris.schaf@verizon.net>"
  :licence "LGPL-3.0"
  :depends-on ()
  :serial t
  :components ((:module "src"
                :components ((:module "mathkernel"
                              :components ((:file "packages")
                                           (:file "math")
                                           (:file "walker")
                                           (:file "expression-ir")
                                           (:file "expression-parse")
                                           (:file "expression-rewrite")
                                           (:file "numeric-diff")
                                           (:file "expression-tests")
                                           (:file "statement-ir")
                                           (:file "copy-propagate-optimization")
                                           (:file "cse-block-optimization")
                                           (:file "cse-factor-products-optimization")
                                           (:file "expression-diff")
                                           (:file "statement-diff")
                                           (:file "energy-blocks")
                                           (:file "kernel-dsl")
                                           (:file "statement-tests")
                                           (:file "kernel-compare")
                                           ))))))
