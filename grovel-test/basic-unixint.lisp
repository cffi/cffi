;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

;;; picked from osicat source

;;;
;;; basic-unixint.lisp --- Grovel definitions for CL-POSIX.
;;;
;;; Copyright (C) 2005-2006, Matthew Backes  <lucca@accela.net>
;;; Copyright (C) 2005-2006, Dan Knapp  <dankna@accela.net> and
;;; Copyright (C) 2007, Stelian Ionescu  <stelian.ionescu-zeus@poste.it>
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

;;; Definitions in this file should match POSIX 1003.1 or some such.
;;; In practice, though, we place here whatever is supported by the
;;; wimpiest of the POSIX systems, Windows.

#+linux
(define "_GNU_SOURCE")

;;; largefile support on linux
;;; TODO: check if these flags are required on solaris too
#+linux
(progn
  (define "_LARGEFILE_SOURCE")
  (define "_LARGEFILE64_SOURCE")
  (define "_FILE_OFFSET_BITS" 64))

(include "sys/types.h" "sys/stat.h" "fcntl.h" "errno.h" "signal.h" "unistd.h"
         "limits.h" "time.h" "stdlib.h")

(ctype size "size_t")
(ctype ssize "ssize_t")
(ctype pid "pid_t")
#-windows (ctype uid "uid_t")
#-windows (ctype gid "gid_t")
(ctype off "off_t")
(ctype mode "mode_t")

(constantenum errno-values
  ((:eperm "EPERM"))
  ((:enoent "ENOENT"))
  ((:esrch "ESRCH"))
  ((:eintr "EINTR"))
  ((:eio "EIO"))
  ((:enxio "ENXIO"))
  ((:e2big "E2BIG"))
  ((:enoexec "ENOEXEC"))
  ((:ebadf "EBADF"))
  ((:echild "ECHILD"))
  ((:eagain "EAGAIN"))
  ((:enomem "ENOMEM"))
  ((:eacces "EACCES"))
  ((:efault "EFAULT"))
  ((:ebusy "EBUSY"))
  ((:eexist "EEXIST"))
  ((:exdev "EXDEV"))
  ((:enodev "ENODEV"))
  ((:enotdir "ENOTDIR"))
  ((:eisdir "EISDIR"))
  ((:einval "EINVAL"))
  ((:enfile "ENFILE"))
  ((:emfile "EMFILE"))
  ((:enotty "ENOTTY"))
  ((:efbig "EFBIG"))
  ((:enospc "ENOSPC"))
  ((:espipe "ESPIPE"))
  ((:erofs "EROFS"))
  ((:emlink "EMLINK"))
  ((:epipe "EPIPE"))
  ((:edom "EDOM"))
  ((:erange "ERANGE"))
  ((:edeadlk "EDEADLK"))
  ((:enametoolong "ENAMETOOLONG"))
  ((:enolck "ENOLCK"))
  ((:enosys "ENOSYS"))
  ((:enotempty "ENOTEMPTY"))
  ((:echrng "ECHRNG") :optional t)
  ((:el2nsync "EL2NSYNC") :optional t)
  ((:el3hlt "EL3HLT") :optional t)
  ((:el3rst "EL3RST") :optional t)
  ((:elnrng "ELNRNG") :optional t)
  ((:eunatch "EUNATCH") :optional t)
  ((:enocsi "ENOCSI") :optional t)
  ((:el2hlt "EL2HLT") :optional t)
  ((:ebade "EBADE") :optional t)
  ((:ebadr "EBADR") :optional t)
  ((:exfull "EXFULL") :optional t)
  ((:enoano "ENOANO") :optional t)
  ((:ebadrqc "EBADRQC") :optional t)
  ((:ebadslt "EBADSLT") :optional t)
  ((:edeadlock "EDEADLOCK") :optional t)
  ((:ebfont "EBFONT") :optional t)
  ((:enostr "ENOSTR") :optional t)
  ((:enodata "ENODATA") :optional t)
  ((:etime "ETIME") :optional t)
  ((:enosr "ENOSR") :optional t)
  ((:enopkg "ENOPKG") :optional t)
  ((:eadv "EADV") :optional t)
  ((:esrmnt "ESRMNT") :optional t)
  ((:ecomm "ECOMM") :optional t)
  ((:edotdot "EDOTDOT") :optional t)
  ((:enotuniq "ENOTUNIQ") :optional t)
  ((:ebadfd "EBADFD") :optional t)
  ((:eremchg "EREMCHG") :optional t)
  ((:elibacc "ELIBACC") :optional t)
  ((:elibbad "ELIBBAD") :optional t)
  ((:elibscn "ELIBSCN") :optional t)
  ((:elibmax "ELIBMAX") :optional t)
  ((:elibexec "ELIBEXEC") :optional t)
  ((:eilseq "EILSEQ"))
  ((:erestart "ERESTART") :optional t)
  ((:estrpipe "ESTRPIPE") :optional t)
  ((:euclean "EUCLEAN") :optional t)
  ((:enotnam "ENOTNAM") :optional t)
  ((:enavail "ENAVAIL") :optional t)
  ((:eremoteio "EREMOTEIO") :optional t)
  ((:enomedium "ENOMEDIUM") :optional t)
  ((:emediumtype "EMEDIUMTYPE") :optional t)
  .
  #+windows nil
  #-windows
  #.'(((:estale "ESTALE"))
      ((:enotblk "ENOTBLK"))
      ((:etxtbsy "ETXTBSY"))
      ((:eusers "EUSERS"))
      ((:eloop "ELOOP"))
      ((:ewouldblock "EWOULDBLOCK"))
      ((:enomsg "ENOMSG"))
      ((:eidrm "EIDRM"))
      ((:eproto "EPROTO"))
      ((:emultihop "EMULTIHOP"))
      ((:ebadmsg "EBADMSG"))
      ((:eoverflow "EOVERFLOW"))
      ((:edquot "EDQUOT"))
      ((:einprogress "EINPROGRESS"))
      ((:ealready "EALREADY"))

      ;; TODO: These errors are related to sockets.  However they
      ;; might not be unique to them.  Remove those that are unique
      ;; and keep those that might be set elsewhere.
      ((:eprotonosupport "EPROTONOSUPPORT"))
      ((:esocktnosupport "ESOCKTNOSUPPORT"))
      ((:enotsock "ENOTSOCK"))
      ((:edestaddrreq "EDESTADDRREQ"))
      ((:emsgsize "EMSGSIZE"))
      ((:eprototype "EPROTOTYPE"))
      ((:enoprotoopt "ENOPROTOOPT"))
      ((:eremote "EREMOTE"))
      ((:enolink "ENOLINK"))
      ((:epfnosupport "EPFNOSUPPORT"))
      ((:eafnosupport "EAFNOSUPPORT"))
      ((:eaddrinuse "EADDRINUSE"))
      ((:eaddrnotavail "EADDRNOTAVAIL"))
      ((:enetdown "ENETDOWN"))
      ((:enetunreach "ENETUNREACH"))
      ((:enetreset "ENETRESET"))
      ((:econnaborted "ECONNABORTED"))
      ((:econnreset "ECONNRESET"))
      ((:eisconn "EISCONN"))
      ((:enotconn "ENOTCONN"))
      ((:eshutdown "ESHUTDOWN"))
      ((:etoomanyrefs "ETOOMANYREFS"))
      ((:etimedout "ETIMEDOUT"))
      ((:econnrefused "ECONNREFUSED"))
      ((:ehostdown "EHOSTDOWN"))
      ((:ehostunreach "EHOSTUNREACH"))
      ((:enonet "ENONET") :optional t)
      ((:enobufs "ENOBUFS"))
      ((:eopnotsupp "EOPNOTSUPP"))))

;;; more signals in unixint.lisp
(constant (sigint "SIGINT") :documentation "interrupt program.")
(constant (sigill "SIGILL") :documentation "illegal instruction.")
(constant (sigabrt "SIGABRT") :documentation "abort program (formerly SIGIOT).")
(constant (sigfpe "SIGFPE") :documentation "floating-point exception.")
(constant (sigsegv "SIGSEGV") :documentation "segmentation violation.")
(constant (sigterm "SIGTERM") :documentation "softwareermination signal.")

;;; open()
(constant (o-rdonly "O_RDONLY"))
(constant (o-wronly "O_WRONLY"))
(constant (o-rdwr "O_RDWR"))
(constant (o-creat "O_CREAT"))
(constant (o-excl "O_EXCL"))
(constant (o-trunc "O_TRUNC"))
(constant (o-append "O_APPEND"))

#+windows
(constant (o-binary "O_BINARY"))

#-windows
(progn
  (constant (o-noctty "O_NOCTTY"))
  (constant (o-nonblock "O_NONBLOCK"))
  (constant (o-ndelay "O_NDELAY"))
  (constant (o-sync "O_SYNC"))
  (constant (o-nofollow "O_NOFOLLOW"))
  (constant (o-async "O_ASYNC")))

#-(or windows darwin)
(constant (o-direct "O_DIRECT"))

#+linux
(progn
  (constant (o-directory "O_DIRECTORY"))
  (constant (o-largefile "O_LARGEFILE"))
  (constant (o-dsync "O_DSYNC"))
  (constant (o-rsync "O_RSYNC")))

;;; lseek()
(constant (seek-set "SEEK_SET"))
(constant (seek-cur "SEEK_CUR"))
(constant (seek-end "SEEK_END"))

;;; access()
(constant (r-ok "R_OK"))
(constant (w-ok "W_OK"))
(constant (x-ok "X_OK"))
(constant (f-ok "F_OK"))

;;;; stat()

(constant (s-irwxu "S_IRWXU")
	  :documentation "read, write, execute/search by owner")
(constant (s-irusr "S_IRUSR") :documentation "read permission, owner")
(constant (s-iwusr "S_IWUSR") :documentation "write permission, owner")
(constant (s-ixusr "S_IXUSR") :documentation "execute/search permission, owner")
(constant (s-ifmt "S_IFMT")   :documentation "bitmask for type of entry")
(constant (s-ififo "S_IFIFO") :documentation "named pipe, aka fifo")
(constant (s-ifchr "S_IFCHR") :documentation "special character-device")
(constant (s-ifdir "S_IFDIR") :documentation "directory")
(constant (s-ifblk "S_IFBLK") :documentation "special block-device")
(constant (s-ifreg "S_IFREG") :documentation "regular file")
(constant (s-ifwht "S_IFWHT") :documentation "whiteout" :optional t)
(constant (s-iread "S_IREAD"))
(constant (s-iwrite "S_IWRITE"))
(constant (s-iexec "S_IEXEC"))

#-windows
(progn
  (constant (s-irwxg "S_IRWXG")
            :documentation "read, write, execute/search by group")
  (constant (s-irgrp "S_IRGRP") :documentation "read permission, group")
  (constant (s-iwgrp "S_IWGRP") :documentation "write permission, group")
  (constant (s-ixgrp "S_IXGRP")
            :documentation "execute/search permission, group")
  (constant (s-irwxo "S_IRWXO")
            :documentation "read, write, execute/search by others")
  (constant (s-iroth "S_IROTH") :documentation "read permission, others")
  (constant (s-iwoth "S_IWOTH") :documentation "write permission, others")
  (constant (s-ixoth "S_IXOTH")
            :documentation "execute/search permission, others")
  (constant (s-isuid "S_ISUID") :documentation "set-user-ID on execution")
  (constant (s-isgid "S_ISGID") :documentation "set-group-ID on execution")
  (constant (s-isvtx "S_ISVTX")
            :documentation "'sticky' bit, many meanings, nonportable")
  (constant (s-iflnk "S_IFLNK") :documentation "symbolic link")
  (constant (s-ifsock "S_IFSOCK") :documentation "socket"))

(constant (path-max "PATH_MAX" "MAXPATHLEN"))

;;;; from time.h

(ctype time "time_t")

;;;; from sys/stat.h

(ctype dev "dev_t")
(ctype ino "ino_t")

#-(or windows openbsd)
(progn
  (ctype nlink "nlink_t")
  (ctype blksize "blksize_t")
  (ctype blkcnt "blkcnt_t"))

#+openbsd
(progn
  (ctype nlink "nlink_t")
  (ctype blksize "long")
  (ctype blkcnt "long"))

(cstruct stat "struct stat"
  (dev     "st_dev"     :type #-mips dev #+mips :unsigned-long)
  (ino     "st_ino"     :type ino)
  (mode    "st_mode"    :type mode)
  #-windows (nlink "st_nlink" :type nlink)
  #-windows (uid   "st_uid"   :type uid)
  #-windows (gid   "st_gid"   :type gid)
  (rdev    "st_rdev"    :type #-mips dev #+mips :unsigned-long)
  (size    "st_size"    :type off)
  #-windows (blksize "st_blksize" :type blkcnt)
  #-windows (blocks  "st_blocks"  :type blksize)
  (atime   "st_atime"   :type time)
  (mtime   "st_mtime"   :type time)
  (ctime   "st_ctime"   :type time))
