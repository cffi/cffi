

(defsystem cffi-grovel-test
  :author "guicho"
  :depends-on (:cffi-grovel :fiveam)
  :components ((:module :grovel-test
                        :components
                        ((:file :packages)
                         (:file :test))))
  :perform (load-op :after (op c) 
		    (eval (read-from-string "(fiveam:run! :cffi-grovel)"))
		    (asdf:clear-system c)))
