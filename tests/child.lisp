;;; Loaded by the child Lisp the tests start (on a pty, or with stdin not a
;;; terminal). Not part of the test system's compiled components.
;;;
;;; The child runs a probe program on TUI-BACKEND-TTY and appends one
;;; readable form per line to a report file: (KEY . PLIST). The terminal
;;; state is read with an oracle independent of the backend: `stty -g` /
;;; `stty -a` on Unix, GetConsoleMode on CONIN$ / CONOUT$ on Windows.

(defpackage #:tui-backend-tty/child
  (:use #:cl)
  (:local-nicknames (#:tui #:tui-protocol) (#:tty #:tui-backend-tty))
  (:export #:main #:tty-state))
(in-package #:tui-backend-tty/child)

(defvar *report* nil "Report file namestring.")
(defvar *last-view* nil)

(defun report (key &rest plist)
  (let ((line (with-standard-io-syntax
                (let ((*package* (find-package :keyword))
                      (*print-readably* nil))
                  (prin1-to-string (cons key plist))))))
    (with-open-file (s *report* :direction :output :if-exists :append
                                :if-does-not-exist :create :external-format :utf-8)
      (write-line line s))))

;;; ------------------------------------------------------------------
;;; Terminal state oracle

#-windows
(defun stty (arg)
  ;; :INPUT :INTERACTIVE = stty inherits our stdin, the terminal under test.
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (uiop:run-program (list "stty" arg) :input :interactive :output :string
                                                   :error-output nil :ignore-error-status t)))

#-windows
(defun tty-state ()
  "→ (:G <stty -g> :A <stty -a>); empty strings when stdin is not a terminal."
  (list :g (stty "-g") :a (stty "-a")))

#+windows
(progn
  (cffi:load-foreign-library "kernel32.dll")
  (cffi:defcfun ("CreateFileW" %create-file) :pointer
    (name (:string :encoding :utf-16le)) (access :uint32) (share :uint32)
    (security :pointer) (disposition :uint32) (flags :uint32) (template :pointer))
  (cffi:defcfun ("GetConsoleMode" %get-console-mode) :int32 (handle :pointer) (mode :pointer))
  (cffi:defcfun ("CloseHandle" %close-handle) :int32 (handle :pointer))

  (defun console-mode (name)
    "Mode of the console buffer NAME (\"CONIN$\" / \"CONOUT$\"), or NIL."
    (let ((h (%create-file name #xC0000000 3 (cffi:null-pointer) 3 0 (cffi:null-pointer))))
      ;; GENERIC_READ|GENERIC_WRITE, FILE_SHARE_READ|FILE_SHARE_WRITE, OPEN_EXISTING
      (if (= (cffi:pointer-address h) (ldb (byte 64 0) -1)) ; INVALID_HANDLE_VALUE
          nil
          (unwind-protect
               (cffi:with-foreign-object (mode :uint32)
                 (and (/= 0 (%get-console-mode h mode)) (cffi:mem-ref mode :uint32)))
            (%close-handle h)))))

  (defun tty-state ()
    "→ (:IN <input mode> :OUT <output mode>), NIL where there is no console."
    (list :in (console-mode "CONIN$") :out (console-mode "CONOUT$"))))

;;; ------------------------------------------------------------------
;;; Probe program

(defclass probe () ())

(defmethod tui:tui-init ((p probe))
  (report :started :state (tty-state))
  (values 0 nil))

(defmethod tui:tui-update ((p probe) model (m tui:key-message))
  (report :key :key (tui:key-message-key m) :text (tui:key-message-text m))
  (case (tui:key-message-key m)
    (:q (values model :quit))
    (:x (error "probe crash"))
    (t (values (1+ model) nil))))

(defmethod tui:tui-update ((p probe) model (m tui:resize-message))
  (report :resize :cols (tui:resize-message-cols m) :rows (tui:resize-message-rows m))
  (values model nil))

(defmethod tui:tui-update ((p probe) model m)
  (declare (ignore m))
  (values model nil))

(defmethod tui:tui-view ((p probe) model canvas)
  (let ((cols (tui:canvas-cols canvas)) (rows (tui:canvas-rows canvas)))
    (unless (equal *last-view* (list cols rows))
      (setf *last-view* (list cols rows))
      (report :view :cols cols :rows rows))
    (tui:draw-text canvas 0 0 (format nil "READY ~Dx~D keys ~D" cols rows model))))

(defun available ()
  (handler-case (tty:tty-available-p)
    (error (c) (list :error (princ-to-string c)))))

(defun run-probe ()
  (tui:run-tui (make-instance 'probe) :backend (tty:make-tty-backend))
  (report :stopped :state (tty-state)))

(defun main (mode report)
  "MODE: :STATE (report the terminal state only), :RUN (run the probe and
report how it ended), :UNCAUGHT (run the probe; a crash kills the process)."
  (setf *report* report)
  (ecase mode
    (:state
     (report :before :state (tty-state)))
    (:run
     (report :before :state (tty-state) :available (available))
     (handler-case (run-probe)
       (tty:tty-not-a-terminal (c)
         (report :not-a-terminal :stream (tty:tty-not-a-terminal-stream c)
                                 :backend-error-p (typep c 'tui:tui-backend-error)))
       (error (c)
         (report :error :type (string (type-of c)) :message (princ-to-string c)
                        :state (tty-state)))))
    (:uncaught
     (report :before :state (tty-state) :available (available))
     (run-probe)))
  (report :exit))
