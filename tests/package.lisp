(defpackage #:tui-backend-tty/tests
  (:use #:cl #:rove)
  (:local-nicknames (#:tui #:tui-protocol) (#:tty #:tui-backend-tty) (#:pp #:pty-protocol)))
(in-package #:tui-backend-tty/tests)

;;; Harness: the backend needs a real terminal, so each test starts a child
;;; Lisp (same runtime and core as this image, as `cl-repo test` does) on a
;;; pty from pty-protocol, or with stdin redirected. The child loads
;;; tui-backend-tty plus tests/child.lisp and reports to a file.

(defparameter *startup-timeout* 300
  "Seconds to wait for the first report. The first child may compile.")
(defparameter *step-timeout* 15)

(defun %asdf-load-form ()
  "Load ASDF in the child from this SBCL's contrib, as cl-repo's test image does."
  #+sbcl
  (let* ((home-fn (find-symbol "SBCL-HOMEDIR-PATHNAME" "SB-INT"))
         (home (and home-fn (ignore-errors (funcall home-fn))))
         (fasl (and home (probe-file (merge-pathnames "contrib/asdf.fasl" home)))))
    (if fasl
        (format nil "(load ~S)" (uiop:native-namestring fasl))
        "(require :asdf)"))
  #-sbcl "(require :asdf)")

(defun %registry-form ()
  "A source registry naming the directory of every system this image loaded."
  (let ((dirs '()))
    (dolist (name (asdf:already-loaded-systems))
      (let ((dir (ignore-errors (asdf:system-source-directory name))))
        (when dir (pushnew (uiop:native-namestring dir) dirs :test #'string=))))
    `(:source-registry ,@(mapcar (lambda (d) (list :directory d)) dirs)
                       :ignore-inherited-configuration)))

(defun child-argv (mode report)
  "→ argv that starts a child image running TUI-BACKEND-TTY/CHILD:MAIN MODE."
  #-sbcl (declare (ignore mode report))
  #-sbcl (error "the tui-backend-tty tests start children with SBCL only")
  #+sbcl
  (let ((child (uiop:native-namestring
                (asdf:system-relative-pathname "tui-backend-tty/tests" "tests/child.lisp"))))
    (append
     (list (uiop:native-namestring sb-ext:*runtime-pathname*)
           "--core" (uiop:native-namestring sb-ext:*core-pathname*) "--noinform"
           "--non-interactive" "--no-sysinit" "--no-userinit")
     (loop for form in (list (%asdf-load-form)
                             (format nil "(uiop:symbol-call :asdf :initialize-source-registry '~S)"
                                     (%registry-form))
                             "(uiop:symbol-call :asdf :load-system \"tui-backend-tty\")"
                             (format nil "(load ~S)" child)
                             (format nil "(uiop:symbol-call :tui-backend-tty/child :main ~S ~S)"
                                     mode report))
           append (list "--eval" form)))))

(defvar *report-counter* 0)

(defun fresh-report-path ()
  (uiop:native-namestring
   (merge-pathnames (format nil "tui-tty-~D-~D-~D.sexp" (get-universal-time) (random 1000000)
                            (incf *report-counter*))
                    (uiop:temporary-directory))))

(defun read-report (path)
  "→ the forms written to PATH so far; a half-written last line is skipped."
  (when (probe-file path)
    (with-open-file (s path :external-format :utf-8)
      (let ((*read-eval* nil) (*package* (find-package :keyword)))
        (loop for form = (handler-case (read s nil s) (error () s))
              until (eq form s) collect form)))))

(defun report-file (path suffix)
  "→ the trimmed contents of PATH<SUFFIX>, or NIL."
  (let ((file (concatenate 'string path suffix)))
    (when (probe-file file)
      (string-trim '(#\Space #\Tab #\Newline #\Return) (uiop:read-file-string file)))))

(defun delete-report (path)
  (dolist (suffix '("" ".before" ".after" ".code"))
    (uiop:delete-file-if-exists (concatenate 'string path suffix))))

;;; ------------------------------------------------------------------
;;; A child on a pty

(defstruct child pty report (output "") (eof nil))

(defun drain (child &optional (timeout 0.05))
  "Read what the child wrote to the terminal (it must not block on a full pty)."
  (unless (child-eof child)
    (let ((chunk (pp:pty-read (child-pty child) :timeout timeout)))
      (cond ((null chunk) (setf (child-eof child) t))
            ((plusp (length chunk))
             (setf (child-output child)
                   (concatenate 'string (child-output child) (map 'string #'code-char chunk))))))))

(defun child-finished-p (child)
  (or (child-eof child) (not (pp:pty-alive-p (child-pty child)))))

(defun wait-report (child key &key (timeout *step-timeout*) (test (constantly t)))
  "→ the first report form (KEY . PLIST) whose PLIST satisfies TEST, or NIL
after TIMEOUT seconds or once the child is gone."
  (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (loop
      (drain child)
      (let ((form (find-if (lambda (f) (and (eq (first f) key) (funcall test (rest f))))
                           (read-report (child-report child)))))
        (when form (return form)))
      (when (or (> (get-internal-real-time) deadline)
                (and (child-finished-p child)
                     ;; one last look: the report may have landed just before exit
                     (not (find key (read-report (child-report child)) :key #'first))))
        (return nil)))))

(defun wait-output (child needle &key (timeout *step-timeout*))
  "True once NEEDLE shows up in what the child wrote to the terminal."
  (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (loop
      (when (search needle (child-output child)) (return t))
      (when (or (> (get-internal-real-time) deadline) (child-finished-p child)) (return nil))
      (drain child))))

(defun finish-child (child &key (timeout *step-timeout*))
  "Drain until the child exits. → its exit code, or NIL on timeout / signal."
  (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (loop
      (drain child)
      (multiple-value-bind (code signal) (pp:pty-wait (child-pty child) :timeout 0.05)
        (when (or code signal) (return code)))
      (when (> (get-internal-real-time) deadline) (return nil)))))

(defun type-keys (child string)
  "Write STRING to the child's terminal. A child that already exited (the
backend failed) drops the write, so the test reports its assertions."
  (handler-bind ((pp:pty-io-error #'continue))
    (pp:pty-write (child-pty child) (map '(vector (unsigned-byte 8)) #'char-code string))))

(defun spawn-child (mode &key (cols 80) (rows 24) wrap)
  "Start the child on a COLS × ROWS pty. WRAP (Unix): run it under sh, which
saves `stty -g` to <report>.before / .after and the exit code to .code."
  (let* ((report (fresh-report-path))
         (argv (child-argv mode report))
         (command (if wrap
                      (list* "sh" "-c"
                             "stty -g > \"$0.before\"; \"$@\"; echo $? > \"$0.code\"; stty -g > \"$0.after\""
                             report argv)
                      argv)))
    (make-child :pty (pp:spawn-pty command :cols cols :rows rows) :report report)))

(defmacro with-child ((var mode &rest keys) &body body)
  `(let ((,var (spawn-child ,mode ,@keys)))
     (unwind-protect (progn ,@body)
       (pp:pty-close (child-pty ,var))
       (delete-report (child-report ,var)))))

;;; ------------------------------------------------------------------
;;; Reading the oracle's terminal state (see tests/child.lisp)

(defun state-of (form) (getf (rest form) :state))

#-windows
(defun stty-tokens (state)
  (uiop:split-string (substitute #\Space #\; (substitute-if #\Space
                                                            (lambda (c) (member c '(#\Tab #\Newline #\Return)))
                                                            (getf state :a)))
                     :separator " "))

(defun raw-state-p (state)
  "True when STATE is raw: Unix `stty -a` shows -icanon -echo -isig; Windows
input has VT input and no line/echo input, output has VT processing."
  #-windows (let ((tokens (stty-tokens state)))
              (every (lambda (flag) (member flag tokens :test #'string=))
                     '("-icanon" "-echo" "-isig")))
  #+windows (let ((in (getf state :in)) (out (getf state :out)))
              (and in out
                   (logtest in #x200)          ; ENABLE_VIRTUAL_TERMINAL_INPUT
                   (not (logtest in #x6))      ; ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT
                   (logtest out #x4))))        ; ENABLE_VIRTUAL_TERMINAL_PROCESSING

(defun cooked-state-p (state)
  "True when STATE is an ordinary line-mode terminal."
  #-windows (let ((tokens (stty-tokens state)))
              (every (lambda (flag) (member flag tokens :test #'string=)) '("icanon" "echo")))
  #+windows (let ((in (getf state :in)))
              (and in (logtest in #x2) (logtest in #x4))))

(defun state-key (state)
  "What must be identical before start and after stop."
  #-windows (getf state :g)
  #+windows (list (getf state :in) (getf state :out)))
