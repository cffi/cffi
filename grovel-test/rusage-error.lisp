
;; See Man getrlimit (2)
;; /usr/include/linux/resource.h

(include "sys/time.h" "sys/resource.h")

(in-package cffi-grovel-test.rusage)

(cstruct timeval "struct timeval"
   (sec "tv_sec" :type :long)
   (usec "tv_usec" :type :long))

;; error case
(cstruct rusage "rusage"
   (utime "ru_utime" :type (:struct timeval))
   (stime "ru_stime" :type (:struct timeval))
   (maxrss "ru_maxrss" :type :long)
   (ixrss "ru_ixrss" :type :long)
   (idrss "ru_idrss" :type :long)
   (isrss "ru_isrss" :type :long)
   (minflt "ru_minflt" :type :long)
   (majflt "ru_majflt" :type :long)
   (nswap "ru_nswap" :type :long)
   (inblock "ru_inblock" :type :long)
   (oublock "ru_oublock" :type :long)
   (msgsnd "ru_msgsnd" :type :long)
   (msgrcv "ru_msgrcv" :type :long)
   (nsignals "ru_nsignals" :type :long)
   (nvcsw "ru_nvcsw" :type :long)
   (nivcsw "ru_nivcsw" :type :long))

