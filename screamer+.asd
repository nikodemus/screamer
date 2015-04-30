(defpackage #:screamer-system (:use #:asdf #:cl))
(in-package #:screamer-system)

(defsystem screamer+
    :depends-on (:screamer)
    :components
    ((:file "screamer+")))
