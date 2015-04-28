(in-package #:asdf)

(defsystem #:cl-plus
  :name "cl-plus"
  :version "1.0"
  :author "Adam Warner <adam@macrology.co.nz>"
  :licence "Public Domain. Refer /usr/share/doc/cl-plus/copyright for details."
  :description "Core ANSI Common Lisp extensions. Includes CL+:DEFCONST."
  :long-description "Core ANSI Common Lisp extension utilities to assist in
compiling cross-platform packages. Currently includes CL+:DEFCONST, which only
evaluates a constant's initial value when the constant name is unbound."
  :components ((:file "cl-plus")))
