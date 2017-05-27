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

(in-package :cffi-socket)

;;  Sockets

(include "sys/types.h")
(include "sys/socket.h")

(constant (+af-unix+   "AF_UNIX"))
(constant (+af-local+  "AF_LOCAL"))
(constant (+af-inet+   "AF_INET"))
(constant (+af-inet6+  "AF_INET6"))
(constant (+af-ipx+    "AF_IPX"))
(constant (+af-packet+ "AF_PACKET"))

(constant (+sock-stream+ "SOCK_STREAM"))
(constant (+sock-dgram+  "SOCK_DGRAM"))
(constant (+sock-raw+    "SOCK_RAW"))

(constant (+sock-nonblock+ "SOCK_NONBLOCK"))
(constant (+sock-cloexec+  "SOCK_CLOEXEC"))

(constant (+shut-rd+   "SHUT_RD"))
(constant (+shut-wr+   "SHUT_WR"))
(constant (+shut-rdwr+ "SHUT_RDWR"))

(ctype socklen-t "socklen_t")
(ctype size-t "size_t")
(ctype ssize-t "ssize_t")

;;  INET

(include "arpa/inet.h")

(ctype uint16-t "uint16_t")
(ctype uint32-t "uint32_t")

;;  IP

(include "netinet/in.h")

(constant (+inaddr-any+ "INADDR_ANY"))

(ctype sa-family-t "sa_family_t")
(ctype in-port-t "in_port_t")
