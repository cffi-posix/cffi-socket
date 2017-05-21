
(in-package :common-lisp-user)

(defpackage :cffi-sockets.system
  (:use :common-lisp :asdf))

(in-package :cffi-sockets.system)

(defsystem "cffi-sockets"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("cffi" "cffi-errno" "cffi-unistd")
  :components
  ((:file "package")
   (:cffi-grovel-file "grovel-sockets" :depends-on ("package"))
   (:file "cffi-sockets" :depends-on ("grovel-sockets"))))
