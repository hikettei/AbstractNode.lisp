
(asdf:defsystem :abstractnode.apis
  :author      "hikettei <ichndm@gmail.com>"
  :depends-on ("abstractnode.graph")
  :serial t
  :components
  ((:file "package")
   (:file "utils")
   (:file "broadcast")
   (:file "arithmetic")
   (:file "reduce")))


