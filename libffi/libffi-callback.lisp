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

(defmacro %defcallback-function-binder (name rettype arg-names arg-types body)
  ;; (check-type convention (member :stdcall :cdecl))
  (let ((symbol-name (%defcallback-symbol name)))
    `(progn
       (defcallback
            ,symbol-name
               :void
               ((cif :pointer) (return-ptr :pointer) (args-ptr :pointer) (user-data :pointer))
            (%defcallback-function ,rettype return-ptr ,arg-names ,arg-types args-ptr ,body))
       (callback ,symbol-name))))

(defun %defcallback-function (rettype ret-ptr arg-names arg-types args-ptr body)
  ;; We need to able to return
  (let ((rtn-value (eval
    `(let ,(loop for sym in arg-names
                 for type in arg-types
                 for index from 0 to (length arg-names)
                 ;; Need to pass pointer non translated object
                 ;; Room for improvement here
                 collect (list sym (if (listp type) `(mem-aref ,args-ptr :pointer ,index) `(mem-aref (mem-aref ,args-ptr :pointer ,index) ,type))))
       ,body))))
    (unless (eql :void rettype) (setf (mem-aref ret-ptr rettype)
                                      (convert-from-foreign rtn-value rettype)))))

(defun %defcallback-symbol (name)
  (intern (format nil "~A-FSBV" (remove #\: (string name)))))
