
(in-package :common-lisp-user)

(defpackage :cffi-socket.system
  (:use :common-lisp :asdf))

(in-package :cffi-socket.system)

(defsystem "cffi-socket"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("cffi" "cffi-errno" "cffi-unistd")
  :components
  ((:file "package")
   (:cffi-grovel-file "grovel-socket" :depends-on ("package"))
   (:file "cffi-socket" :depends-on ("grovel-socket"))))
