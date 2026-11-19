(in-package #:tui-backend-tty)

(define-condition tty-not-a-terminal (tui:tui-backend-error)
  ((stream :initarg :stream :reader tty-not-a-terminal-stream
           :type (member :input :output)
           :documentation ":INPUT or :OUTPUT: the side that is not a terminal."))
  (:report (lambda (c s)
             (format s "Standard ~(~A~) is not a terminal; use a headless backend (tui-backend-terminal) or run in a terminal"
                     (tty-not-a-terminal-stream c)))))

(define-condition tty-system-error (tui:tui-backend-error)
  ((operation :initarg :operation :reader tty-system-error-operation
              :documentation "Keyword naming the call that failed, e.g. :TCSETATTR, :SET-CONSOLE-MODE.")
   (code :initarg :code :initform nil :reader tty-system-error-code
         :documentation "errno (Unix) or GetLastError (Windows), if known."))
  (:report (lambda (c s)
             (format s "Terminal call ~(~A~) failed~@[ (code ~A)~]"
                     (tty-system-error-operation c) (tty-system-error-code c)))))
