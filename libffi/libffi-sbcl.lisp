(in-package #:cffi)

;;;# Foreign Callback

;;; Used to generate c function to be passed into
;;; `ffi_prep_closure_loc` as (fun) parameter
;;; TODO: Read page 12 need to expected args then parse in and stuff into the values
;;; below:
;;;
;;; void puts_binding(ffi_cif *cif, void *ret, void* args[], void *stream)
;;; {
;;;    *(ffi_arg *)ret = fputs(*(char **)args[0], (FILE *)stream);
;;; }
;;;
;;;Create a wrapper
(defmacro with-gensyms (names &body forms)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@forms))

(declaim (optimize (speed 0) (space 0) (debug 3)))
(defmacro %defcallback-function-binder (name rettype arg-names arg-types body
                                        &key convention)
  ;; (check-type convention (member :stdcall :cdecl))
  (break)
  (with-gensyms (grettype garg-names garg-types gbody)
    (let ((grettype rettype)
          (garg-names arg-names)
          (garg-types arg-types)
          (gbody body))
  `(sb-alien:alien-sap
          (sb-alien::alien-lambda
            (:stdcall sb-alien:void)
            ((cif sb-alien:system-area-pointer) (ret-ptr sb-alien:system-area-pointer) (args-ptr sb-alien:system-area-pointer)
             (user-data sb-alien:system-area-pointer))
             (setf ret-ptr (%defcallback-function ,grettype ret-ptr ,garg-names ,garg-types args-ptr ,gbody)))))))

;; TODO: Need to map arg-values to arg-names after converting them to expected type (cl-objc type).
(declaim (optimize (speed 0) (space 0) (debug 3)))
(defun %defcallback-function (rettype ret-ptr arg-names arg-types args-ptr body)
  ;; We need to able to return
  ;; (break)
  (let ((computed-ptr (eval
    `(let ,(loop for sym in arg-names
                 for type in arg-types
                 for index from 0 to (length arg-names)
                 ;; Need to just pass pointer not converted object
                 ;; Defintely room for improvement here
                 ;; collect (list sym `(convert-from-foreign (mem-aref (mem-aref ,args-ptr :pointer ,index) :pointer) ',type)))
                 collect (list sym (if (listp type) `(mem-aref ,args-ptr :pointer ,index) `(mem-aref (mem-aref ,args-ptr :pointer ,index) :pointer))))
    ;; TODO: setf pointer to the return value via cffi facilities.
       ,body))))
    (unless (eq :void rettype) (setf (mem-aref ret-ptr :pointer)
  computed-ptr))))
