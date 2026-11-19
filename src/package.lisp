(defpackage #:tui-backend-tty
  (:use #:cl)
  (:local-nicknames (#:tui #:tui-protocol))
  (:export
   ;; conditions (both are TUI-PROTOCOL:TUI-BACKEND-ERRORs)
   #:tty-not-a-terminal #:tty-not-a-terminal-stream
   #:tty-system-error #:tty-system-error-operation #:tty-system-error-code
   ;; backend
   #:tty-backend #:make-tty-backend
   #:tty-backend-input #:tty-backend-output #:tty-backend-active-p
   #:tty-available-p))
