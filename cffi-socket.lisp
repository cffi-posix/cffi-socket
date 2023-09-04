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

(defcfun ("socket" c-socket) :int
  (domain :int)
  (type :int)
  (protocol :int))

(defun socket (domain type protocol)
  (let ((s (c-socket domain type protocol)))
    (when (< s 0)
      (error-errno "socket"))
    s))

(defun socket-close (fd)
  (unless (< (the fixnum (unistd:c-dup2 fd fd)) 0)
    (shutdown fd)
    (unistd:close fd)))

(defmacro with-socket ((var domain type protocol) &body body)
  `(let ((,var (socket ,domain ,type ,protocol)))
     (unwind-protect (progn ,@body)
       (socket-close ,var))))

(defcfun ("connect" c-connect) :int
  (sockfd :int)
  (addr (:pointer (:struct sockaddr)))
  (addrlen socklen-t))

(defun connect (sockfd addr addrlen)
  (let ((r (c-connect sockfd addr addrlen)))
    (when (< r 0)
      (error-errno "connect"))
    r))

(defcfun ("bind" c-bind) :int
  (sockfd :int)
  (addr (:pointer (:struct sockaddr)))
  (addrlen socklen-t))

(defun bind (sockfd addr addrlen)
  (let ((r (c-bind sockfd addr addrlen)))
    (when (< r 0)
      (error-errno "bind"))
    r))

(defcfun ("htons" c-htons) uint16-t
  (hostshort uint16-t))

(defun htons (hostshort)
  (c-htons hostshort))

(defcfun ("ntohs" c-ntohs) uint16-t
  (netshort uint16-t))

(defun ntohs (netshort)
  (c-ntohs netshort))

(defcfun ("htonl" c-htonl) uint32-t
  (host32 uint32-t))

(defun htonl (host32)
  (c-htonl host32))

(defcfun ("ntohl" c-ntohl) uint32-t
  (net32 uint32-t))

(defun ntohl (net32)
  (c-ntohl net32))

;;  IP

(defun inet-addr-from-string (x &key (start 0) (end (length x)))
  (block nil
    (let ((addr 0))
      (flet ((eat-byte ()
               (let ((b 0))
                 (loop
                    (unless (< start end)
                      (return))
                    (let ((c (char x start)))
                      (unless (char<= #\0 c #\9)
                        (return))
                      (setf b (+ (* 10 b) (- (char-code c) (char-code #\0))))
                      (incf start)))
                 (setf addr (logior (ash addr 8) b)))))
        (dotimes (_ 3)
          (eat-byte)
          (unless (char= #\. (char x start))
            (return nil))
          (incf start))
        (eat-byte)
        (unless (= end start)
          (return nil)))
      addr)))

(defun inet-addr (x)
  (etypecase x
    ((unsigned-byte 32) x)
    (string (or (inet-addr-from-string x)))))

(defun sockaddr-to-string (sockaddr)
  (with-foreign-slots ((sin-port sin-addr)
                       sockaddr (:struct sockaddr-in))
    (format nil "~D.~D.~D.~D:~D"
            (mod      sin-addr      256)
            (mod (ash sin-addr -8 ) 256)
            (mod (ash sin-addr -16) 256)
            (mod (ash sin-addr -24) 256)
            (ntohs sin-port))))

(defmacro with-sockaddr-in ((sockaddr sockaddrlen host port) &body body)
  (declare (type symbol sockaddr sockaddrlen))
  `(with-foreign-object (,sockaddr '(:struct sockaddr))
     (with-foreign-slots ((sin-family sin-port sin-addr)
                          ,sockaddr (:struct sockaddr-in))
       (setf sin-family +af-inet+
             sin-port (htons ,port)
             sin-addr (htonl (inet-addr ,host))))
     (let ((,sockaddrlen (foreign-type-size '(:struct sockaddr-in))))
       ,@body)))

(defun connect-inet (sockfd host port)
  (with-sockaddr-in (addr addrlen host port)
    (connect sockfd addr addrlen)))

(defun bind-inet (sockfd host port)
  (with-sockaddr-in (addr addr-len host port)
    (bind sockfd addr addr-len)))

(defcfun ("listen" c-listen) :int
  (sockfd :int)
  (backlog :int))

(defun listen (sockfd backlog)
  (let ((r (c-listen sockfd backlog)))
    (when (< r 0)
      (error-errno "listen"))
    r))

(defcfun ("accept" c-accept) :int
  (sockfd :int)
  (addr :pointer)
  (addrlen (:pointer :int)))

(defun accept (sockfd)
  (with-foreign-object (addr '(:struct sockaddr-in))
    (with-foreign-object (addrlen :int)
      (setf (mem-ref addrlen :int) (foreign-type-size '(:struct sockaddr-in)))
      (let ((s (the integer (c-accept sockfd addr addrlen))))
        (cond ((<= 0 s)
               (values s addr))
              ((or (= errno +eagain+)
                   (= errno +ewouldblock+))
               :non-blocking)
              (t
               (error-errno "accept")))))))

(defmacro with-accept ((fd-var &optional addr-var) listening-fd
                       &body body)
  (declare (type symbol fd-var addr-var))
  (unless addr-var
    (setq addr-var (gensym)))
  `(multiple-value-bind (,fd-var ,addr-var) (accept ,listening-fd)
     (unless (eq :non-blocking ,fd-var)
       (unwind-protect (let ((,fd-var ,fd-var)
                             (,addr-var ,addr-var))
                         (declare (ignorable ,addr-var))
                         ,@body)
         (socket-close ,fd-var)))))

(defcfun ("recv" c-recv) ssize-t
  (sockfd :int)
  (buf :pointer)
  (len size-t)
  (flags :int))

(defun recv (sockfd buffer flags &key (start 0) (end (length buffer)))
  (declare (type (array (unsigned-byte 8)) buffer))
  (let ((len (- end start)))
    (with-foreign-pointer (buf len)
      (let ((r (c-recv sockfd buf len flags)))
        (dotimes (i len)
          (setf (aref buffer start) (mem-aref buf :unsigned-char i))
          (incf start))
        (when (< r 0)
          (error-errno "recv"))
        r))))

(defcfun ("recvfrom" c-recvfrom) ssize-t
  (sockfd :int)
  (buf :pointer)
  (len size-t)
  (flags :int)
  (src-addr (:pointer (:struct sockaddr)))
  (addrlen (:pointer socklen-t)))

(defun recv-from (sockfd buffer flags &key (srcaddr (null-pointer)) (addrlen (null-pointer)) (start 0) (end (length buffer)))
  (declare (type (array (unsigned-byte 8)) buffer))
  (let ((len (- end start)))
    (with-foreign-pointer (buf len)
      (let ((r (c-recvfrom sockfd buf len flags srcaddr addrlen)))
        (dotimes (i len)
          (setf (aref buffer start) (mem-aref buf :unsigned-char i))
          (incf start))
        (when (< r 0)
          (error-errno "recv-from"))
        r))))

(defun recv-sequence (sockfd buffer flags &key (start 0) (end (length buffer)))
  (loop
     (unless (< start end)
       (return))
     (let ((r (recv sockfd buffer flags :start start :end end)))
       (when (= r 0)
         (error "end of file"))
       (incf start r))))

(defcfun ("send" c-send) ssize-t
  (sockfd :int)
  (buf :pointer)
  (len size-t)
  (flags :int))

(defun send (sockfd buffer flags &key (start 0) (end (length buffer)))
  (declare (type (array (unsigned-byte 8)) buffer))
  (let ((len (- end start)))
    (with-foreign-pointer (buf len)
      (dotimes (i len)
        (setf (mem-aref buf :unsigned-char i) (aref buffer start))
        (incf start))
      (let ((r (c-send sockfd buf len flags)))
        (when (< r 0)
          (error-errno "send"))
        r))))

(defun send-sequence (sockfd buffer flags &key (start 0) (end (length buffer)))
  (loop
     (unless (< start end)
       (return))
     (let ((r (send sockfd buffer flags :start start :end end)))
       (when (= r 0)
         (error "end of file"))
       (incf start r))))

(defcfun ("sendto" c-sendto) ssize-t
  (sockfd :int)
  (buf :pointer)
  (len size-t)
  (flags :int)
  (dest-addr (:pointer (:struct sockaddr)))
  (addrlen socklen-t))

(defun send-to (sockfd buffer flags dstaddr addrlen &key (start 0) (end (length buffer)))
  (declare (type (array (unsigned-byte 8)) buffer))
  (let ((len (- end start)))
    (with-foreign-pointer (buf len)
      (let ((r (c-sendto sockfd buf len flags dstaddr addrlen)))
        (dotimes (i len)
          (setf (aref buffer start) (mem-aref buf :unsigned-char i))
          (incf start))
        (when (< r 0)
          (error-errno "send-to"))
        r))))

(defcfun ("shutdown" c-shutdown) :int
  (sockfd :int)
  (how :int))

(defun shutdown (sockfd &optional read write)
  (when (or read write)
    (c-shutdown sockfd (cond ((and read write) +shut-rdwr+)
                             (read +shut-rd+)
                             (write +shut-wr+)))))
