;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; strings.lisp --- Operations on foreign strings.
;;;
;;; Copyright (C) 2005-2006, James Bielman  <jamesjb@jamesjb.com>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;

(in-package #:cffi)

;;; FIXME: we used to support ub8 arrays here. Look into that. [2007-02 LO]

;;; ENDIANNESS features.  Should probably moved somewhere else.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew (with-foreign-object (p :uint16)
             (setf (mem-ref p :uint16) #x00ff)
             (ecase (mem-ref p :uint8)
               (0 'cffi-features:big-endian)
               (#xff 'cffi-features:little-endian)))
           *features*))

#+(or (and cffi-features:big-endian babel::le)
      (and cffi-features:little-endian babel::be)
      (not (or cffi-features:big-endian cffi-features:little-endian)))
(error "Bug.  Babel and CFFI don't agree on endianness.")

;;;# Foreign String Conversion
;;;
;;; Functions for converting NULL-terminated C-strings to Lisp strings
;;; and vice versa.  The string functions accept an ENCODING keyword
;;; argument which is used to specify the encoding to use when
;;; converting to/from foreign strings.

;;; FIXME: is this a good default? [2007-04-13 LO]
;;;
;;; :LATIN-1 would probably work better for 8-bit lisps and be
;;; backwards compatible with previous encoding unaware CFFI
;;; behaviour.  OTOH, most uses are probably counting on
;;; ASCII-compatible behaviour?  [2007-06-07 LO]
(defvar *default-foreign-encoding* :utf-8)

;;; TODO: refactor, sigh.  Also, this should probably be a function.
;;; Would have to change Babel's API for that to be possible, I think.
(defmacro bref (ptr off &optional (bytes 1) (endianness :ne))
  (let ((big-endian (member endianness '(:be #+cffi-features:big-endian :ne))))
    (once-only (ptr off)
      (ecase bytes
        (1 `(mem-ref ,ptr :uint8 ,off))
        (2 (if big-endian
               #+cffi-features:big-endian
               `(mem-ref ,ptr :uint16 ,off)
               #-cffi-features:big-endian
               `(dpb (mem-ref ,ptr :uint8 ,off) (byte 8 8)
                     (mem-ref ,ptr :uint8 (1+ ,off)))
               #+cffi-features:little-endian
               `(mem-ref ,ptr :uint16 ,off)
               #-cffi-features:little-endian
               `(dpb (mem-ref ,ptr :uint8 (1+ ,off)) (byte 8 8)
                     (mem-ref ,ptr :uint8 ,off))))
        (4 (if big-endian
               #+cffi-features:big-endian
               `(mem-ref ,ptr :uint32 ,off)
               #-cffi-features:big-endian
               `(dpb (mem-ref ,ptr :uint8 ,off) (byte 8 24)
                     (dpb (mem-ref ,ptr :uint8 (1+ ,off)) (byte 8 16)
                          (dpb (mem-ref ,ptr :uint8 (+ ,off 2)) (byte 8 8)
                               (mem-ref ,ptr :uint8 (+ ,off 3)))))
               #+cffi-features:little-endian
               `(mem-ref ,ptr :uint32 ,off)
               #-cffi-features:little-endian
               `(dpb (mem-ref ,ptr :uint8 (+ ,off 3)) (byte 8 24)
                     (dpb (mem-ref ,ptr :uint8 (+ ,off 2)) (byte 8 16)
                          (dpb (mem-ref ,ptr :uint8 (1+ ,off)) (byte 8 8)
                               (mem-ref ,ptr :uint8 ,off))))))))))

(defsetf bref (ptr off &optional (bytes 1) (endianness :ne)) (val)
  (let ((big-endian (member endianness '(:be #+cffi-features:big-endian :ne))))
    (ecase bytes
      (1 `(setf (mem-ref ,ptr :uint8 ,off) ,val))
      (2 (if big-endian
             #+cffi-features:big-endian
             `(setf (mem-ref ,ptr :uint16 ,off) ,val)
             #-cffi-features:big-endian
             `(setf (mem-ref ,ptr :uint8 (1+ ,off)) (ldb (byte 8 0) val)
                    (mem-ref ,ptr :uint8 ,off) (ldb (byte 8 8) val))
             #+cffi-features:little-endian
             `(setf (mem-ref ,ptr :uint16 ,off) ,val)
             #-cffi-features:little-endian
             `(setf (mem-ref ,ptr :uint8 ,off) (ldb (byte 8 0) val)
                    (mem-ref ,ptr :uint8 (1+ ,off)) (ldb (byte 8 8) val))))
      (4 (if big-endian
             #+cffi-features:big-endian
             `(setf (mem-ref ,ptr :uint32 ,off) ,val)
             #-cffi-features:big-endian
             `(setf (mem-ref ,ptr :uint8 (+ 3 ,off)) (ldb (byte 8 0) val)
                    (mem-ref ,ptr :uint8 (+ 2 ,off)) (ldb (byte 8 8) val)
                    (mem-ref ,ptr :uint8 (1+ ,off)) (ldb (byte 8 16) val)
                    (mem-ref ,ptr :uint8 ,off) (ldb (byte 8 24) val))
             #+cffi-features:little-endian
             `(setf (mem-ref ,ptr :uint32 ,off) ,val)
             #-cffi-features:little-endian
             `(setf (mem-ref ,ptr :uint8 ,off) (ldb (byte 8 0) val)
                    (mem-ref ,ptr :uint8 (1+ ,off)) (ldb (byte 8 8) val)
                    (mem-ref ,ptr :uint8 (+ ,off 2)) (ldb (byte 8 16) val)
                    (mem-ref ,ptr :uint8 (+ ,off 3)) (ldb (byte 8 24) val)))))))

;;; TODO: tackle optimization notes.
(defparameter *foreign-string-mappings*
  (instantiate-concrete-mappings
   :optimize ((speed 3) (debug 0) (compilation-speed 0) (safety 0))
   :octet-seq-accessor bref
   :octet-seq-type foreign-pointer
   :code-point-seq-accessor babel::string-accessor
   :code-point-seq-type simple-string))

(defun null-terminator-len (encoding)
  (length (enc-nul-encoding (get-character-encoding encoding))))

(defun lisp-string-to-foreign (string buffer bufsize
                               &key (encoding *default-foreign-encoding*))
  (check-type string string)
  (with-checked-simple-vector ((string string) (start 0) (end nil))
    (declare (type simple-string string))
    (let* ((mapping (lookup-mapping *foreign-string-mappings* encoding))
           (size (funcall (octet-counter mapping) string start end)))
      (when (<= bufsize size)
        (error "This string is too large to fit target buffer."))
      (funcall (encoder mapping) string start end buffer 0)
      (dotimes (i (null-terminator-len encoding))
        (setf (mem-ref buffer :char (+ size i)) 0)))
    buffer))

;;; Expands into a loop that calculates the length of the foreign
;;; string at PTR plus OFFSET, using ACCESSOR and looking for a null
;;; terminator of LENGTH bytes.
(defmacro %foreign-string-length (ptr offset type length)
  (once-only (ptr offset)
    `(do ((i 0 (+ i ,length)))
         ((zerop (mem-ref ,ptr ,type (+ ,offset i))) i)
       (declare (fixnum i)))))

;;; Return the length in octets of the null terminated foreign string
;;; at POINTER plus OFFSET octets, assumed to be encoded in ENCODING,
;;; a CFFI encoding.  This should be smart enough to look for 8-bit vs
;;; 16-bit null terminators, as appropriate for the encoding.
(defun foreign-string-length (pointer &key (encoding *default-foreign-encoding*)
                              (offset 0))
  (ecase (null-terminator-len encoding)
    (1 (%foreign-string-length pointer offset :uint8 1))
    (2 (%foreign-string-length pointer offset :uint16 2))
    (4 (%foreign-string-length pointer offset :uint32 4))))

;;; what to do with COUNT here? ahrg... make it default to STRLEN? but
;;; what about the "at most" part?
(defun foreign-string-to-lisp (pointer &key (offset 0) count
                               (encoding *default-foreign-encoding*))
  "Copy at most COUNT bytes from POINTER plus OFFSET encoded in
ENCODING into a Lisp string and return it.  If POINTER is a null
pointer, NIL is returned."
  (unless (null-pointer-p pointer)
    (let ((count (or count
                     (foreign-string-length
                      pointer :encoding encoding :offset offset)))
          (mapping (lookup-mapping *foreign-string-mappings* encoding)))
      (multiple-value-bind (size new-end)
          (funcall (code-point-counter mapping) pointer offset (+ offset count))
        (let ((string (make-string size)))
          (funcall (decoder mapping) pointer offset new-end string 0)
          string)))))

;;;# Using Foreign Strings

(defun foreign-string-alloc (string &key (encoding *default-foreign-encoding*)
                             (null-terminated-p t) (start 0) end)
  "Allocate a foreign string containing Lisp string STRING.
The string must be freed with FOREIGN-STRING-FREE."
  (check-type string string)
  (with-checked-simple-vector ((string string) (start start) (end end))
    (declare (type simple-string string))
    (let* ((mapping (lookup-mapping *foreign-string-mappings* encoding))
           (count (funcall (octet-counter mapping) string start end))
           (length (if null-terminated-p
                       (+ count (null-terminator-len encoding))
                       count))
           (ptr (foreign-alloc :char :count length)))
      (funcall (encoder mapping) string start end ptr 0)
      (when null-terminated-p
        (dotimes (i (null-terminator-len encoding))
          (setf (mem-ref ptr :char (+ count i)) 0)))
      (values ptr length))))

(defun foreign-string-free (ptr)
  "Free a foreign string allocated by FOREIGN-STRING-ALLOC."
  (foreign-free ptr))

(defmacro with-foreign-string ((var-or-vars lisp-string &rest args) &body body)
  "VAR-OR-VARS is not evaluated ans should a list of the form
\(VAR &OPTIONAL BYTE-SIZE-VAR) or just a VAR symbol.  VAR is
bound to a foreign string containing LISP-STRING in BODY.  When
BYTE-SIZE-VAR is specified then bind the C buffer size
\(including the possible null terminator\(s)) to this variable."
  (destructuring-bind (var &optional size-var)
      (ensure-list var-or-vars)
    `(multiple-value-bind (,var ,@(when size-var (list size-var)))
         (foreign-string-alloc ,lisp-string ,@args)
       (unwind-protect
            (progn ,@body)
         (foreign-string-free ,var)))))

(defmacro with-foreign-strings (bindings &body body)
  "See WITH-FOREIGN-STRING's documentation."
  (if bindings
      `(with-foreign-string ,(first bindings)
         (with-foreign-strings ,(rest bindings)
           ,@body))
      `(progn ,@body)))

(defmacro with-foreign-pointer-as-string
    ((var-or-vars size &rest args) &body body)
  "VAR-OR-VARS is not evaluated and should be a list of the form
\(VAR &OPTIONAL SIZE-VAR) or just a VAR symbol.  VAR is bound to
a foreign buffer of size SIZE within BODY.  The return value is
constructed by calling FOREIGN-STRING-TO-LISP on the foreign
buffer along with ARGS." ; fix wording, sigh
  (destructuring-bind (var &optional size-var)
      (ensure-list var-or-vars)
    `(with-foreign-pointer (,var ,size ,size-var)
       (progn
         ,@body
         (foreign-string-to-lisp ,var ,@args)))))

;;;# Automatic Conversion of Foreign Strings

(define-foreign-type foreign-string-type ()
  (;; CFFI encoding of this string.
   (encoding :initarg :encoding :reader encoding))
  (:actual-type :pointer))

;;; describe me
(defun fst-encoding (type)
  (or (encoding type) *default-foreign-encoding*))

;;; TODO: check if encoding is valid now.
(define-parse-method :string (&key encoding)
  (make-instance 'foreign-string-type :encoding encoding))

;;; Display the encoding when printing a FOREIGN-STRING-TYPE instance.
(defmethod print-object ((type foreign-string-type) stream)
  (print-unreadable-object (type stream :type t)
    (format stream "~S" (fst-encoding type))))

(defmethod translate-to-foreign ((s string) (type foreign-string-type))
  (values (foreign-string-alloc s :encoding (fst-encoding type)) t))

(defmethod translate-to-foreign (obj (type foreign-string-type))
  (cond
    ((pointerp obj)
     (values obj nil))
    ;; ((typep obj '(array (unsigned-byte 8)))
    ;;  (values (foreign-string-alloc obj) t))
    (t (error "~A is not a Lisp string or pointer." obj))))

(defmethod translate-from-foreign (ptr (type foreign-string-type))
  (foreign-string-to-lisp ptr :encoding (fst-encoding type)))

(defmethod free-translated-object (ptr (type foreign-string-type) free-p)
  (when free-p
    (foreign-string-free ptr)))

;;;# STRING+PTR

(define-foreign-type foreign-string+ptr-type (foreign-string-type)
  ())

(define-parse-method :string+ptr (&key encoding)
  (make-instance 'foreign-string+ptr-type :encoding encoding))

(defmethod translate-from-foreign (value (type foreign-string+ptr-type))
  (list (call-next-method) value))
