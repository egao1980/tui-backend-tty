(in-package #:tui-backend-tty)

;;; CONTRACT STUB. The locked design is in cl-stack docs/capabilities/tui.md
;;; ("tui-backend-tty"). Natives: CFFI straight into libc (Unix) and kernel32
;;; (Windows); no C shim, no overlays, no grovel. Every GF below that touches
;;; the terminal signals NOT-IMPLEMENTED until the backend lands.

(defclass tty-backend (tui:tui-backend)
  ((input :initarg :input :initform nil :reader tty-backend-input
          :documentation "Unix: input fd (NIL = 0). Windows: console input handle
(NIL = GetStdHandle(STD_INPUT_HANDLE), CONIN$ when that is NULL/invalid).")
   (output :initarg :output :initform nil :reader tty-backend-output
           :documentation "Unix: output fd (NIL = 1). Windows: console output handle
(NIL = GetStdHandle(STD_OUTPUT_HANDLE), CONOUT$ when that is NULL/invalid).")
   (active :initform nil :accessor %active
           :documentation "True between a successful BACKEND-START and BACKEND-STOP."))
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

(defun %not-implemented (operation)
  (error 'tui:tui-backend-error
         :cause (format nil "tui-backend-tty: ~A is not implemented yet" operation)))

(defun tty-available-p (&key input output)
  "True when INPUT and OUTPUT (default: standard input and output) are both
terminals, i.e. BACKEND-START would not signal TTY-NOT-A-TERMINAL."
  (declare (ignore input output))
  (%not-implemented "TTY-AVAILABLE-P"))

(defmethod tui:backend-start ((b tty-backend) &key alt-screen mouse)
  (declare (ignore alt-screen mouse))
  (%not-implemented "BACKEND-START"))

(defmethod tui:backend-stop ((b tty-backend))
  ;; A never-started (or already stopped) backend has nothing to restore.
  ;; RUN-TUI calls this from its cleanup even when BACKEND-START failed.
  (when (%active b)
    (%not-implemented "BACKEND-STOP"))
  nil)

(defmethod tui:backend-read ((b tty-backend) &key timeout)
  (declare (ignore timeout))
  (%not-implemented "BACKEND-READ"))

(defmethod tui:backend-write ((b tty-backend) octets)
  (declare (ignore octets))
  (%not-implemented "BACKEND-WRITE"))

(defmethod tui:backend-size ((b tty-backend))
  (%not-implemented "BACKEND-SIZE"))
