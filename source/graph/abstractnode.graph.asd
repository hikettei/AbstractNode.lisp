
(asdf:defsystem :abstractnode.graph
  :author      "hikettei <ichndm@gmail.com>"
  :depends-on (:trivia
	       :alexandria)
  :serial t
  :components
  ((:file "package")
   (:file "abstract-tensor")
   (:file "abstract-node")))

