
(in-package :cffi)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun append-url (sym doc-type)
    (setf (documentation sym doc-type)
          (with-output-to-string (s)
            (princ (documentation sym doc-type) :stream s)
            ;; do the same name-normalization as texinfo
            (format s "~2% Detailed documentation: ~
                        https://common-lisp.net/project/cffi/manual/html_node/~{~a~^_002d~}.html"
                    (split-sequence:split-sequence #\- (string-downcase (symbol-name sym)))))))

  (do-external-symbols (sym :cffi)
    (cond
      ((documentation sym 'variable)
       (append-url sym 'variable))
      ((documentation sym 'function)
       (append-url sym 'function))
      ((documentation sym 'type)
       (append-url sym 'type))
      ((documentation sym 'structure)
       (append-url sym 'structcture))
      ((documentation sym 'method-combination)
       (append-url sym 'method-combination)))))

