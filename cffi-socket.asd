
(in-package :common-lisp-user)

(defpackage :cffi-socket.system
  (:use :common-lisp :asdf))

(in-package :cffi-socket.system)

(defsystem :cffi-socket
  :name "cffi-socket"
  :author "Thomas de Grivel <thoxdg@gmail.com>"
  :version "0.1"
  :description "Common Lisp wrapper for BSD sockets"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("cffi" "cffi-errno" "cffi-unistd")
  :components
  ((:file "package")
   (:cffi-grovel-file "grovel-socket" :depends-on ("package"))
   (:file "cffi-socket" :depends-on ("grovel-socket"))))
