
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
