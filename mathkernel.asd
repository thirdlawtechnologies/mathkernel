
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
                                           (:file "expression-ir")
                                           (:file "expression-parse")
                                           (:file "expression-rewrite")
                                           (:file "expression-tests")
                                           (:file "statement-ir")
                                           (:file "expression-diff")
                                           (:file "statement-diff")
                                           (:file "statement-tests")
                                           (:file "kernel-dsl")
                                           ))))))

