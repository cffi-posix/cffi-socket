
(in-package :common-lisp)

(defpackage :cffi-sockets
  (:use
   :cffi
   :cffi-errno
   :common-lisp)
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
   #:file-descriptor
   #:socket
   #:close-sock
   #:bind-inet
   #:listen-sock
   #:accept
   #:recv
   #:recv-sequence
   #:send
   #:send-sequence))
