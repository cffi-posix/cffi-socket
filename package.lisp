;;
;;  cffi-socket  -  Common Lisp wrapper for BSD sockets
;;
;;  Copyright 2017 Thomas de Grivel <thoxdg@gmail.com>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package :common-lisp)

(defpackage :cffi-socket
  (:nicknames :socket)
  (:use
   :cffi
   :common-lisp
   :errno)
  (:shadow
   #:listen)
  (:export
   #:+af-inet+
   #:+af-inet6+
   #:+af-ipx+
   #:+af-local+
   #:+af-packet+
   #:+af-unix+
   #:+shut-rd+
   #:+shut-rdwr+
   #:+shut-wr+
   #:+sock-cloexec+
   #:+sock-dgram+
   #:+sock-nonblock+
   #:+sock-raw+
   #:+sock-stream+
   #:accept
   #:bind
   #:bind-inet
   #:c-accept
   #:c-bind
   #:c-connect
   #:c-htons
   #:c-listen
   #:c-ntohs
   #:c-recv
   #:c-send
   #:c-shutdown
   #:c-socket
   #:connect
   #:connect-inet
   #:htons
   #:inet-addr
   #:inet-addr-from-string
   #:listen
   #:ntohs
   #:recv
   #:recv-sequence
   #:send
   #:send-sequence
   #:shutdown
   #:sockaddr-to-string
   #:socket
   #:with-accept
   #:with-sockaddr-in
   #:with-socket
   ))
