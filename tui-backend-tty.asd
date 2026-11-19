(defsystem "tui-backend-tty"
  :version "0.1.1"
  :description "tui-protocol backend on the real terminal: termios raw mode and SIGWINCH (Unix), console VT mode (Windows), CFFI into the system libraries"
  :author "egao1980"
  :license "MIT"
  :depends-on ("tui-protocol" "cffi" "bordeaux-threads")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "write")
               (:file "unix" :if-feature (:not (:or :windows :win32)))
               (:file "windows" :if-feature (:or :windows :win32))
               (:file "backend"))
  :in-order-to ((test-op (test-op "tui-backend-tty/tests"))))

(defsystem "tui-backend-tty/tests"
  :depends-on ("tui-backend-tty" "pty-backend-native" "uiop" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               ;; Loaded only by the child Lisp that the tests start on a pty.
               (:static-file "child.lisp")
               (:file "tty-test")
               (:file "write-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
