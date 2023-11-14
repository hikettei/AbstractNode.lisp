
#+quicklisp(cl:push (cl:pathname "./") ql:*local-project-directories*)
(asdf:defsystem :AbstractNode
  :description "A Tiny and Portable DAG Compiler intended to be applied into Deep Learning Training/Inference"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on
  ("abstractnode.graph")
  ("abstractnode.compiler"))

