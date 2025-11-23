
(asdf:defsystem #:symkernel
  :description "Optimize mathematical expressions using a compiler implemented in common lisp "
  :version "0.0.1"
  :author "Christian Schafmeister <chris.schaf@verizon.net>"
  :licence "LGPL-3.0"
  :depends-on ()
  :serial t
  :components ((:module "src"
                :components ((:module "symkernel"
                              :components ((:file "packages")
                                           (:file "optimize-expressions")
                                           (:file "derivatives")
                                           (:file "math")
                                           (:file "expression-ir")
                                           (:file "expression-parse")
                                           (:file "expression-print")
                                           (:file "expression-tests")
                                           (:file "statement-ir")
                                           (:file "expression-diff")
                                           (:file "statement-diff")
                                           (:file "statement-tests")
                                           (:file "energy-blocks")
                                           ))))))

