
(asdf:defsystem :abstractnode.compiler
  :author      "hikettei <ichndm@gmail.com>"
  :depends-on ("abstractnode.graph")
  :serial t
  :components
  ((:file "package")
   (:file "conditions")
   (:file "ir")
   (:file "lazy-index-compiler")
   (:file "compile-requirements")
   (:file "compile-aref")
   (:file "compile-iteration")
   (:file "compile-dtype")
   (:file "backend-template")))

