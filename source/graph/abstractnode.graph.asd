
(asdf:defsystem :abstractnode.graph
  :author      "hikettei <ichndm@gmail.com>"
  :depends-on (:alexandria)
  :serial t
  :components
  ((:file "package")
   (:file "utils")
   (:file "lazyaxis")
   (:file "range")
   (:file "abstract-tensor")
   (:file "abstract-node")))

