(in-package #:cffi)

;;;# Foreign Callback

;;; Callback support for foreign structures by value using libffi's closure API.
;;; This generates the C-compatible function signature expected by ffi_prep_closure_loc.
;;; 
;;; Example from libffi documentation:
;;;   void callback_fn(ffi_cif *cif, void *ret, void* args[], void *user_data)
;;;   {
;;;      *(ffi_arg *)ret = process(*(type *)args[0], *(type *)args[1]);
;;;   }

(defmacro %defcallback-function-binder (name rettype arg-names arg-types body)
  "Create a callback wrapper that bridges libffi's C calling convention to Lisp.
The generated callback receives raw pointers and must extract/marshal arguments."
  (let ((symbol-name (%defcallback-symbol name)))
    `(progn
       (defcallback
            ,symbol-name
               :void
               ((cif :pointer) (return-ptr :pointer) (args-ptr :pointer) (user-data :pointer))
            (%defcallback-function ,rettype return-ptr ,arg-names ,arg-types args-ptr ,body))
       (callback ,symbol-name))))

(defun %defcallback-function (rettype ret-ptr arg-names arg-types args-ptr body)
  "Execute the callback body with marshaled arguments and handle return value.
This function extracts arguments from the args-ptr array, evaluates the callback body,
and stores the result in ret-ptr."
  (let ((rtn-value (eval
    `(let ,(loop for sym in arg-names
                 for type in arg-types
                 for index from 0 below (length arg-names)
                 ;; Extract each argument from the pointer array.
                 ;; Struct types (lists) need single dereference, others need double.
                 collect (list sym (if (listp type)
                                       `(mem-aref ,args-ptr :pointer ,index)
                                       `(mem-aref (mem-aref ,args-ptr :pointer ,index) ,type))))
       ,body))))
    (unless (eql :void rettype)
      (setf (mem-aref ret-ptr rettype)
            (convert-from-foreign rtn-value rettype)))))

(defun %defcallback-symbol (name)
  "Generate a unique symbol name for the libffi callback wrapper."
  (intern (format nil "~A-FSBV" (remove #\: (string name)))))