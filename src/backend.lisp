(in-package #:tui-backend-tty)

;;; The backend proper. Everything OS-specific goes through the %OS-*
;;; functions of unix.lisp or windows.lisp (one of them is loaded).
;;;
;;; Invariant: (%ACTIVE B) ⇔ (%SAVED B) holds the state saved at start, the
;;; terminal is in the raw state derived from it, B's exit hook is on
;;; SB-EXT:*EXIT-HOOKS* and B counts once in *WATCHERS*. Every way out of a
;;; failed start, every BACKEND-STOP of an active B and the exit hook leave
;;; the terminal equal to the saved state and undo the hook and watcher.

(defclass tty-backend (tui:tui-backend)
  ((input :initarg :input :initform nil :reader tty-backend-input
          :documentation "Unix: input fd (NIL = 0). Windows: console input handle
(NIL = GetStdHandle(STD_INPUT_HANDLE), CONIN$ when that is NULL/invalid).")
   (output :initarg :output :initform nil :reader tty-backend-output
           :documentation "Unix: output fd (NIL = 1). Windows: console output handle
(NIL = GetStdHandle(STD_OUTPUT_HANDLE), CONOUT$ when that is NULL/invalid).")
   (active :initform nil :accessor %active
           :documentation "True between a successful BACKEND-START and BACKEND-STOP.")
   (in :initform nil :accessor %in :documentation "Input fd / handle resolved at start.")
   (out :initform nil :accessor %out :documentation "Output fd / handle resolved at start.")
   (owned :initform '() :accessor %owned
          :documentation "Handles opened by start (Windows CONIN$ / CONOUT$), closed by stop.")
   (saved :initform nil :accessor %saved
          :documentation "Terminal state saved at start: termios octets or console modes + code pages.")
   (exit-hook :initform nil :accessor %exit-hook
              :documentation "The closure this backend pushed onto the exit hooks, or NIL.")
   (watching :initform nil :accessor %watching
             :documentation "True while this backend counts in *WATCHERS*.")
   (resize-seen :initform 0 :accessor %resize-seen :type fixnum
                :documentation "*RESIZE-GENERATION* as last seen by BACKEND-READ.")
   (read-state :initform (list nil) :reader %read-state
               :documentation "Per-OS read state (Windows: a pending high surrogate)."))
  (:documentation "The real controlling terminal: raw mode + SIGWINCH on Unix,
console VT mode on Windows. Restores the saved terminal state on BACKEND-STOP,
on any unwind out of RUN-TUI, and from an exit hook."))

(defun make-tty-backend (&key input output)
  "→ a TTY-BACKEND on INPUT / OUTPUT (default: standard input and output).
Nothing touches the terminal until BACKEND-START."
  (make-instance 'tty-backend :input input :output output))

(defun tty-backend-active-p (backend)
  "True while BACKEND holds the terminal (started and not yet stopped)."
  (and (%active backend) t))

(defun tty-available-p (&key input output)
  "True when INPUT and OUTPUT (default: standard input and output) are both
terminals, i.e. BACKEND-START would not signal TTY-NOT-A-TERMINAL. Never
signals for a non-terminal."
  (multiple-value-bind (in in-owned) (%os-open input :input)
    (unwind-protect
         (multiple-value-bind (out out-owned) (%os-open output :output)
           (unwind-protect
                (and (%os-terminal-p in) (%os-terminal-p out) t)
             (when out-owned (%os-close out))))
      (when in-owned (%os-close in)))))

;;; ------------------------------------------------------------------
;;; Resize watcher: one SIGWINCH handler shared by all active backends,
;;; installed by the first start and put back by the last stop. The handler
;;; only bumps a generation counter; BACKEND-READ waits in short slices and
;;; returns empty when the counter moved.

(declaim (type fixnum *resize-generation* *watchers*))
(defvar *resize-generation* 0)
(defvar *watchers* 0)
(defvar *previous-sigwinch* nil
  "What SB-SYS:ENABLE-INTERRUPT returned when the watcher was installed.")
(defvar *watch-lock* (bt2:make-lock :name "tui-backend-tty sigwinch"))

(defconstant +read-slice+ 0.05d0
  "Longest single wait in BACKEND-READ, in seconds (the brief allows 0.1).")

#+(and sbcl (not (or windows win32)))
(defun %sigwinch-handler (signal info context)
  (declare (ignore signal info context))
  (setf *resize-generation* (logand (1+ *resize-generation*) most-positive-fixnum)))

(defun %install-watcher ()
  (bt2:with-lock-held (*watch-lock*)
    (when (= 1 (incf *watchers*))
      #+(and sbcl (not (or windows win32)))
      (setf *previous-sigwinch*
            (sb-sys:enable-interrupt sb-unix:sigwinch #'%sigwinch-handler))))
  nil)

(defun %remove-watcher ()
  (bt2:with-lock-held (*watch-lock*)
    (when (and (plusp *watchers*) (zerop (decf *watchers*)))
      #+(and sbcl (not (or windows win32)))
      (sb-sys:enable-interrupt sb-unix:sigwinch (or *previous-sigwinch* :default))
      (setf *previous-sigwinch* nil)))
  nil)

;;; ------------------------------------------------------------------
;;; Exit hook

(defun %install-exit-hook (b)
  (let ((hook (lambda () (when (%active b) (%teardown b)))))
    (setf (%exit-hook b) hook)
    #+sbcl (push hook sb-ext:*exit-hooks*)
    nil))

(defun %remove-exit-hook (b)
  (let ((hook (shiftf (%exit-hook b) nil)))
    (when hook
      #+sbcl (setf sb-ext:*exit-hooks* (remove hook sb-ext:*exit-hooks*))))
  nil)

;;; ------------------------------------------------------------------
;;; Lifecycle

(defun %teardown (b)
  "Undo whatever BACKEND-START did, in reverse order. Never signals.
→ NIL, or (values code operation) when restoring the terminal failed while
it is still a terminal (a hung-up terminal has nothing left to restore)."
  (setf (%active b) nil)
  (%remove-exit-hook b)
  (when (%watching b)
    (setf (%watching b) nil)
    (%remove-watcher))
  (let ((saved (shiftf (%saved b) nil))
        (in (%in b)) (out (%out b)))
    (multiple-value-prog1
        (when saved
          (multiple-value-bind (code operation) (%os-restore in out saved)
            (and code (%os-terminal-p in) (values code operation))))
      (dolist (h (shiftf (%owned b) '()))
        (%os-close h)))))

(defun %resolve-handles (b)
  "Open the input and output (Windows may open CONIN$ / CONOUT$)."
  (multiple-value-bind (in in-owned) (%os-open (tty-backend-input b) :input)
    (when in-owned (push in (%owned b)))
    (multiple-value-bind (out out-owned) (%os-open (tty-backend-output b) :output)
      (when out-owned (push out (%owned b)))
      (setf (%in b) in (%out b) out))))

(defmethod tui:backend-start ((b tty-backend) &key alt-screen mouse)
  (declare (ignore alt-screen mouse))   ; RUN-TUI writes those sequences itself
  (when (%active b)
    (error 'tui:tui-backend-error :cause "tui-backend-tty: backend already started"))
  (let ((done nil))
    (unwind-protect
         (progn
           (%resolve-handles b)
           ;; 1. Checks, input first; nothing has been changed yet.
           (unless (%os-terminal-p (%in b))
             (error 'tty-not-a-terminal :stream :input :cause "input is not a terminal"))
           (unless (%os-terminal-p (%out b))
             (error 'tty-not-a-terminal :stream :output :cause "output is not a terminal"))
           ;; 2. Save. 3. Raw. 4. Resize watcher. 5. Exit hook. 6. Active.
           (let ((saved (%os-save (%in b) (%out b))))
             (setf (%saved b) saved)
             (%os-enter-raw (%in b) (%out b) saved))
           (%install-watcher)
           (setf (%watching b) t
                 (%resize-seen b) *resize-generation*)
           (%install-exit-hook b)
           (multiple-value-bind (cols rows) (%os-size (%in b) (%out b))
             (setf (%active b) t done t)
             (values cols rows)))
      ;; Rollback: RUN-TUI's BACKEND-STOP is a no-op while not active, so a
      ;; partial start undoes itself here and the original condition goes on.
      (unless done (%teardown b)))))

(defmethod tui:backend-stop ((b tty-backend))
  ;; A never-started (or already stopped) backend has nothing to restore.
  ;; RUN-TUI calls this from its cleanup even when BACKEND-START failed.
  (when (%active b)
    (multiple-value-bind (code operation) (%teardown b)
      ;; Everything else is undone before the failure is reported.
      (when code (%sys-error operation code))))
  nil)

(defun %require-active (b operation)
  (unless (%active b)
    (error 'tui:tui-backend-error
           :cause (format nil "tui-backend-tty: ~(~A~) on a backend that is not started"
                          operation))))

(defmethod tui:backend-read ((b tty-backend) &key timeout)
  "→ octets, an empty vector on timeout or when a resize arrived, NIL at EOF."
  (%require-active b :read)
  (let ((deadline (and timeout
                       (+ (get-internal-real-time)
                          (round (* timeout internal-time-units-per-second))))))
    (loop
      (when (/= (%resize-seen b) *resize-generation*)
        (setf (%resize-seen b) *resize-generation*)
        (return (%empty)))
      (let ((slice (if deadline
                       (min +read-slice+
                            (max 0 (/ (- deadline (get-internal-real-time))
                                      internal-time-units-per-second)))
                       +read-slice+)))
        (when (%os-wait-input (%in b) slice)
          (return (%os-read (%in b) (%read-state b))))
        (when (and deadline (>= (get-internal-real-time) deadline))
          (return (%empty)))))))

(defmethod tui:backend-write ((b tty-backend) octets)
  (%require-active b :write)
  (%os-write (%out b) octets)
  nil)

(defmethod tui:backend-size ((b tty-backend))
  "→ (values cols rows), queried from the terminal on every call."
  (%require-active b :size)
  (%os-size (%in b) (%out b)))
