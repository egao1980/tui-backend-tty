(in-package #:tui-backend-tty)

;;; Windows 10 1809+: CFFI straight into kernel32. Console modes and code
;;; pages are saved and restored; input comes from ReadConsoleInputW (not
;;; ReadFile, which blocks while only non-key records are queued).

(cffi:define-foreign-library kernel32
  (:windows "kernel32.dll"))
(cffi:use-foreign-library kernel32)

(defconstant +std-input-handle+ -10)
(defconstant +std-output-handle+ -11)
(defconstant +generic-read-write+ #xC0000000)
(defconstant +file-share-read-write+ 3)
(defconstant +open-existing+ 3)
(defconstant +wait-object-0+ 0)
(defconstant +utf-8-code-page+ 65001)
(defconstant +input-mode-clear+ #x7 "ENABLE_PROCESSED_INPUT|LINE_INPUT|ECHO_INPUT")
(defconstant +input-mode-set+ #x208 "ENABLE_VIRTUAL_TERMINAL_INPUT|ENABLE_WINDOW_INPUT")
(defconstant +output-mode-set+ #xC "ENABLE_VIRTUAL_TERMINAL_PROCESSING|DISABLE_NEWLINE_AUTO_RETURN")
(defconstant +csbi-size+ 22 "CONSOLE_SCREEN_BUFFER_INFO")
(defconstant +input-record-size+ 20 "INPUT_RECORD")
(defconstant +input-records+ 64)
(defconstant +key-event+ 1)
(defconstant +window-buffer-size-event+ 4)
(defconstant +write-chunk+ 4096)

(cffi:defcfun ("GetStdHandle" %get-std-handle) :pointer (which :int32))
(cffi:defcfun ("CreateFileW" %create-file) :pointer
  (name (:string :encoding :utf-16le)) (access :uint32) (share :uint32)
  (security :pointer) (disposition :uint32) (flags :uint32) (template :pointer))
(cffi:defcfun ("CloseHandle" %close-handle) :int32 (handle :pointer))
(cffi:defcfun ("GetLastError" %get-last-error) :uint32)
(cffi:defcfun ("GetConsoleMode" %get-console-mode) :int32 (handle :pointer) (mode :pointer))
(cffi:defcfun ("SetConsoleMode" %set-console-mode) :int32 (handle :pointer) (mode :uint32))
(cffi:defcfun ("GetConsoleCP" %get-console-cp) :uint32)
(cffi:defcfun ("SetConsoleCP" %set-console-cp) :int32 (cp :uint32))
(cffi:defcfun ("GetConsoleOutputCP" %get-console-output-cp) :uint32)
(cffi:defcfun ("SetConsoleOutputCP" %set-console-output-cp) :int32 (cp :uint32))
(cffi:defcfun ("GetConsoleScreenBufferInfo" %get-console-screen-buffer-info) :int32
  (handle :pointer) (info :pointer))
(cffi:defcfun ("WaitForSingleObject" %wait-for-single-object) :uint32
  (handle :pointer) (milliseconds :uint32))
(cffi:defcfun ("ReadConsoleInputW" %read-console-input) :int32
  (handle :pointer) (buffer :pointer) (length :uint32) (read :pointer))
(cffi:defcfun ("WriteFile" %write-file) :int32
  (handle :pointer) (buffer :pointer) (count :uint32) (written :pointer) (overlapped :pointer))

(defun %sys-error (operation code)
  (error 'tty-system-error :operation operation :code code
                           :cause (format nil "~(~A~) failed, GetLastError ~A" operation code)))

(defun %empty ()
  (make-array 0 :element-type '(unsigned-byte 8)))

(defun %invalid-handle-p (h)
  (or (cffi:null-pointer-p h)
      (= (cffi:pointer-address h)
         (ldb (byte (* 8 (cffi:foreign-type-size :pointer)) 0) -1))))

;;; The %OS-* interface (the same names are defined in unix.lisp).

(defun %os-open (spec direction)
  "→ (values handle owned-p). SPEC NIL: the std handle, or CONIN$ / CONOUT$
when that is NULL or INVALID_HANDLE_VALUE (ConPTY with STARTF_USESTDHANDLES).
OWNED-P is true when the handle was opened here and must be closed."
  (if spec
      (values spec nil)
      (let ((h (%get-std-handle (ecase direction
                                  (:input +std-input-handle+)
                                  (:output +std-output-handle+)))))
        (if (%invalid-handle-p h)
            (let ((c (%create-file (ecase direction (:input "CONIN$") (:output "CONOUT$"))
                                   +generic-read-write+ +file-share-read-write+
                                   (cffi:null-pointer) +open-existing+ 0 (cffi:null-pointer))))
              (if (%invalid-handle-p c)
                  (values h nil)        ; GetConsoleMode fails → not a terminal
                  (values c t)))
            (values h nil)))))

(defun %os-close (h)
  (%close-handle h)
  nil)

(defun %console-mode (h)
  "→ the console mode of H, or NIL when H is not a console."
  (cffi:with-foreign-object (mode :uint32)
    (and (/= 0 (%get-console-mode h mode))
         (cffi:mem-ref mode :uint32))))

(defun %os-terminal-p (h)
  (and (not (%invalid-handle-p h)) (%console-mode h) t))

(defun %os-save (in out)
  "→ (in-mode out-mode input-cp output-cp)."
  (list (or (%console-mode in) (%sys-error :get-console-mode (%get-last-error)))
        (or (%console-mode out) (%sys-error :get-console-mode (%get-last-error)))
        (%get-console-cp)
        (%get-console-output-cp)))

(defun %os-enter-raw (in out saved)
  (destructuring-bind (in-mode out-mode in-cp out-cp) saved
    (declare (ignore in-cp out-cp))
    (when (zerop (%set-console-mode in (logior (logandc2 in-mode +input-mode-clear+)
                                               +input-mode-set+)))
      (%sys-error :set-console-mode (%get-last-error)))
    ;; Refused here on consoles before 1809 (no VT processing).
    (when (zerop (%set-console-mode out (logior out-mode +output-mode-set+)))
      (%sys-error :set-console-mode (%get-last-error)))
    (when (zerop (%set-console-cp +utf-8-code-page+))
      (%sys-error :set-console-cp (%get-last-error)))
    (when (zerop (%set-console-output-cp +utf-8-code-page+))
      (%sys-error :set-console-output-cp (%get-last-error)))))

(defun %os-restore (in out saved)
  "Put the saved modes and code pages back. → NIL, or (values code operation)
for the first failure. Every step is tried. Never signals."
  (destructuring-bind (in-mode out-mode in-cp out-cp) saved
    (let ((code nil) (operation nil))
      (flet ((try (op ok)
               (when (and (zerop ok) (null code))
                 (setf code (%get-last-error) operation op))))
        (try :set-console-mode (%set-console-mode in in-mode))
        (try :set-console-mode (%set-console-mode out out-mode))
        (try :set-console-cp (%set-console-cp in-cp))
        (try :set-console-output-cp (%set-console-output-cp out-cp)))
      (and code (values code operation)))))

(defun %os-size (in out)
  "→ (values cols rows) of the visible window (srWindow, not dwSize)."
  (declare (ignore in))
  (cffi:with-foreign-pointer (info +csbi-size+)
    (when (zerop (%get-console-screen-buffer-info out info))
      (%sys-error :get-console-screen-buffer-info (%get-last-error)))
    ;; srWindow: SHORT Left, Top, Right, Bottom at offset 10.
    (let ((left (cffi:mem-ref info :int16 10))
          (top (cffi:mem-ref info :int16 12))
          (right (cffi:mem-ref info :int16 14))
          (bottom (cffi:mem-ref info :int16 16)))
      (values (1+ (- right left)) (1+ (- bottom top))))))

(defun %os-wait-input (in seconds)
  (= +wait-object-0+ (%wait-for-single-object in (max 0 (round (* seconds 1000))))))

(defun %utf8-push (code out)
  (flet ((put (b) (vector-push-extend b out)))
    (cond ((< code #x80) (put code))
          ((< code #x800)
           (put (logior #xC0 (ash code -6)))
           (put (logior #x80 (logand code #x3F))))
          ((< code #x10000)
           (put (logior #xE0 (ash code -12)))
           (put (logior #x80 (logand (ash code -6) #x3F)))
           (put (logior #x80 (logand code #x3F))))
          (t
           (put (logior #xF0 (ash code -18)))
           (put (logior #x80 (logand (ash code -12) #x3F)))
           (put (logior #x80 (logand (ash code -6) #x3F)))
           (put (logior #x80 (logand code #x3F)))))))

(defun %os-read (in state)
  "ReadConsoleInputW → UTF-8 octets of the key-down characters; empty when
only resize, key-up or other records arrived. A console has no EOF.
STATE is a cons whose car holds a high surrogate left from the last read."
  (let ((out (make-array 16 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (cffi:with-foreign-pointer (buf (* +input-records+ +input-record-size+))
      (cffi:with-foreign-object (n :uint32)
        (when (zerop (%read-console-input in buf +input-records+ n))
          (%sys-error :read-console-input (%get-last-error)))
        (dotimes (r (cffi:mem-ref n :uint32))
          (let ((rec (cffi:inc-pointer buf (* r +input-record-size+))))
            (when (and (= (cffi:mem-ref rec :uint16 0) +key-event+)
                       (/= 0 (cffi:mem-ref rec :int32 4)))   ; bKeyDown
              (let ((ch (cffi:mem-ref rec :uint16 14))        ; UnicodeChar
                    (repeat (max 1 (cffi:mem-ref rec :uint16 8))))
                (dotimes (i repeat)
                  (let ((high (car state)))
                    (cond ((zerop ch))
                          ((<= #xD800 ch #xDBFF) (setf (car state) ch))
                          ((and high (<= #xDC00 ch #xDFFF))
                           (setf (car state) nil)
                           (%utf8-push (+ #x10000 (ash (- high #xD800) 10) (- ch #xDC00)) out))
                          ((<= #xDC00 ch #xDFFF))            ; lone low surrogate: drop
                          (t (setf (car state) nil)
                             (%utf8-push ch out)))))))))))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun %os-write (out octets)
  "WriteFile every octet of OCTETS to OUT."
  (let ((len (length octets)) (off 0))
    (cffi:with-foreign-pointer (p +write-chunk+)
      (cffi:with-foreign-object (written :uint32)
        (loop while (< off len)
              do (let ((chunk (min +write-chunk+ (- len off))) (done 0))
                   (dotimes (i chunk) (setf (cffi:mem-aref p :uint8 i) (aref octets (+ off i))))
                   (loop while (< done chunk)
                         do (when (zerop (%write-file out (cffi:inc-pointer p done) (- chunk done)
                                                      written (cffi:null-pointer)))
                              (%sys-error :write-file (%get-last-error)))
                            (incf done (cffi:mem-ref written :uint32)))
                   (incf off chunk))))))
  nil)
