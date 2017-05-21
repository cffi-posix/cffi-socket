
(in-package :common-lisp)

(defpackage :cffi-socket
  (:use
   :cffi
   :common-lisp
   :errno)
  (:shadow
   #:listen)
  (:export
   #:+af-unix+
   #:+af-local+
   #:+af-inet+
   #:+af-inet6+
   #:+af-ipx+
   #:+af-packet+
   #:+sock-stream+
   #:+sock-dgram+
   #:+sock-raw+
   #:+sock-nonblock+
   #:+sock-cloexec+
   #:+shut-rd+
   #:+shut-wr+
   #:+shut-rdwr+
   #:file-descriptor
   #:socket
   #:with-socket
   #:bind-inet
   #:listen
   #:accept
   #:with-accept
   #:recv
   #:recv-sequence
   #:send
   #:send-sequence
   #:shutdown))
