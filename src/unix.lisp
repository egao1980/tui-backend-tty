(in-package #:tui-backend-tty)

;;; Unix: CFFI straight into libc (the symbols are already in the Lisp
;;; runtime, so no library form is needed). The termios struct is an opaque
;;; buffer: it is filled by tcgetattr, copied, passed to cfmakeraw and
;;; tcsetattr, and never read field by field.

(defconstant +termios-size+ 256
  "Opaque termios buffer size; larger than glibc (60) and macOS (72).")
(defconstant +tcsanow+ 0)
(defconstant +tiocgwinsz+ #+darwin #x40087468 #-darwin #x5413)
(defconstant +eintr+ 4 "Same on Linux and macOS.")
(defconstant +eio+ 5 "Same on Linux and macOS.")
(defconstant +read-size+ 4096)

(cffi:defcfun ("isatty" %isatty) :int (fd :int))
(cffi:defcfun ("tcgetattr" %tcgetattr) :int (fd :int) (termios :pointer))
(cffi:defcfun ("tcsetattr" %tcsetattr) :int (fd :int) (action :int) (termios :pointer))
(cffi:defcfun ("cfmakeraw" %cfmakeraw) :void (termios :pointer))
(cffi:defcfun ("read" %read) :long (fd :int) (buf :pointer) (count :unsigned-long))
(cffi:defcfun ("write" %write) :long (fd :int) (buf :pointer) (count :unsigned-long))

;;; ioctl is variadic. On Apple arm64 variadic arguments are passed on the
;;; stack, while CFFI on SBCL makes a fixed-argument call. Six padding words
;;; fill x2..x7, so ARG lands in the first stack slot, where ioctl reads its
;;; variadic argument. SysV amd64 and Linux aarch64 read variadic integer and
;;; pointer arguments from the same registers as fixed ones.
#+(and darwin arm64)
(cffi:defcfun ("ioctl" %ioctl-ptr) :int
  (fd :int) (request :unsigned-long)
  (pad0 :unsigned-long) (pad1 :unsigned-long) (pad2 :unsigned-long)
  (pad3 :unsigned-long) (pad4 :unsigned-long) (pad5 :unsigned-long)
  (arg :pointer))
#-(and darwin arm64)
(cffi:defcfun ("ioctl" %ioctl-ptr) :int (fd :int) (request :unsigned-long) (arg :pointer))

(declaim (inline %ioctl))
(defun %ioctl (fd request ptr)
  #+(and darwin arm64) (%ioctl-ptr fd request 0 0 0 0 0 0 ptr)
  #-(and darwin arm64) (%ioctl-ptr fd request ptr))

;;; errno is a thread-local lvalue behind a libc function: __errno_location
;;; (glibc, musl), __error (macOS, FreeBSD), __errno (OpenBSD, NetBSD, Bionic).
(cffi:defcfun (#+(or darwin freebsd dragonfly) "__error"
               #+(or openbsd netbsd android) "__errno"
               #-(or darwin freebsd dragonfly openbsd netbsd android) "__errno_location"
               %errno-location)
    :pointer)

(defun %cffi-errno ()
  "errno read through CFFI. Call it directly after the failing call."
  (cffi:mem-ref (%errno-location) :int))

(defmacro %errno ()
  "errno of the foreign call just made. Expand it directly after the call.
SBCL saves errno right after the alien call; elsewhere it is read through
CFFI."
  #+sbcl '(sb-alien:get-errno)
  #-sbcl '(%cffi-errno))

(defun %sys-error (operation code)
  (error 'tty-system-error :operation operation :code code
                           :cause (format nil "~(~A~) failed, errno ~A" operation code)))

(defun %empty ()
  (make-array 0 :element-type '(unsigned-byte 8)))

;;; The %OS-* interface (the same names are defined in windows.lisp).

(defun %os-open (spec direction)
  "→ (values fd owned-p). SPEC NIL means standard input / output."
  (values (or spec (ecase direction (:input 0) (:output 1))) nil))

(defun %os-close (fd)
  (declare (ignore fd))
  nil)

(defun %os-terminal-p (fd)
  (= 1 (%isatty fd)))

(defun %os-save (in out)
  "→ the termios of IN as a fresh octet vector of +TERMIOS-SIZE+."
  (declare (ignore out))
  (cffi:with-foreign-pointer (p +termios-size+)
    (dotimes (i +termios-size+) (setf (cffi:mem-aref p :uint8 i) 0))
    (when (minusp (%tcgetattr in p))
      (%sys-error :tcgetattr (%errno)))
    (let ((v (make-array +termios-size+ :element-type '(unsigned-byte 8))))
      (dotimes (i +termios-size+ v)
        (setf (aref v i) (cffi:mem-aref p :uint8 i))))))

(defun %call-with-termios (saved fn)
  (cffi:with-foreign-pointer (p +termios-size+)
    (dotimes (i +termios-size+) (setf (cffi:mem-aref p :uint8 i) (aref saved i)))
    (funcall fn p)))

(defun %os-enter-raw (in out saved)
  (declare (ignore out))
  (%call-with-termios saved
                      (lambda (p)
                        (%cfmakeraw p)
                        (when (minusp (%tcsetattr in +tcsanow+ p))
                          (%sys-error :tcsetattr (%errno))))))

(defun %os-restore (in out saved)
  "Put SAVED back on IN. → NIL on success, else (values errno operation).
Never signals, so the exit hook can use it.

xnu sets PENDIN whenever ICANON goes from off to on and keeps it until the
pending input is retyped, which happens from select/FIONREAD. A zero-timeout
wait on the input fd clears it, so `stty -g` equals the saved state. Linux
never sets PENDIN; there the wait is a no-op."
  (declare (ignore out))
  (let* ((errno nil)
         (rc (%call-with-termios saved
                                 (lambda (p)
                                   (let ((rc (%tcsetattr in +tcsanow+ p)))
                                     (when (minusp rc) (setf errno (%errno)))
                                     rc)))))
    (cond ((minusp rc) (values errno :tcsetattr))
          (t #+sbcl (sb-sys:wait-until-fd-usable in :input 0 nil)
             nil))))

(defun %os-size (in out)
  "→ (values cols rows) from TIOCGWINSZ on OUT."
  (declare (ignore in))
  (cffi:with-foreign-object (ws :unsigned-short 4)
    (dotimes (i 4) (setf (cffi:mem-aref ws :unsigned-short i) 0))
    (when (minusp (%ioctl out +tiocgwinsz+ ws))
      (%sys-error :ioctl (%errno)))
    (values (cffi:mem-aref ws :unsigned-short 1)    ; ws_col
            (cffi:mem-aref ws :unsigned-short 0)))) ; ws_row

(defun %os-wait-input (in seconds)
  "True when IN is readable within SECONDS."
  #+sbcl (sb-sys:wait-until-fd-usable in :input seconds nil)
  #-sbcl (progn (sleep seconds) t))

(defun %os-read (in state)
  "One read(2) of up to +READ-SIZE+ octets → octets, empty (EINTR) or NIL (EOF).
EIO is EOF too: a Linux pty whose master has closed reports it."
  (declare (ignore state))
  (cffi:with-foreign-pointer (p +read-size+)
    (let ((n (%read in p +read-size+)))
      (cond ((plusp n)
             (let ((v (make-array n :element-type '(unsigned-byte 8))))
               (dotimes (i n v) (setf (aref v i) (cffi:mem-aref p :uint8 i)))))
            ((zerop n) nil)
            (t (let ((e (%errno)))
                 (cond ((eql e +eintr+) (%empty))
                       ((eql e +eio+) nil)
                       (t (%sys-error :read e)))))))))

(defun %os-write (out octets)
  "write(2) every octet of OCTETS to OUT, retrying on EINTR. Repeated 0-octet
writes signal TTY-SYSTEM-ERROR (see %WRITE-FULLY)."
  (let ((len (length octets)) (off 0))
    (cffi:with-foreign-pointer (p +read-size+)
      (loop while (< off len)
            do (let ((chunk (min +read-size+ (- len off))))
                 (dotimes (i chunk) (setf (cffi:mem-aref p :uint8 i) (aref octets (+ off i))))
                 (%write-fully chunk
                               (lambda (done)
                                 (let ((n (%write out (cffi:inc-pointer p done) (- chunk done))))
                                   (cond ((not (minusp n)) n)
                                         (t (let ((e (%errno)))
                                              (if (eql e +eintr+)
                                                  nil
                                                  (%sys-error :write e)))))))
                               :write)
                 (incf off chunk)))))
  nil)
