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

#+clisp
(ignore-errors
  (ext:without-package-lock (:socket)
    (ext:without-package-lock (:ext)
      (handler-bind ((error (invoke-restart 'continue)))
        (delete-package :socket)))))
