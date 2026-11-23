(in-package #:tui-backend-tty/tests)

;;; Harness checks. These pass before the backend exists: they prove the
;;; child starts on a pty and the state oracle reads a cooked terminal.

(deftest harness-child-reads-a-cooked-pty
  (with-child (c :state)
    (let ((before (wait-report c :before :timeout *startup-timeout*)))
      (ok before "child reported")
      (ok (cooked-state-p (state-of before)) (format nil "cooked: ~S" (state-of before)))
      (ng (raw-state-p (state-of before)))
      #-windows (ok (plusp (length (state-key (state-of before)))) "stty -g read"))
    (ok (eql 0 (finish-child c)) "child exits 0")))

(deftest backend-shape
  (let ((b (tty:make-tty-backend)))
    (ok (typep b 'tui:tui-backend))
    (ok (typep b 'tty:tty-backend))
    (ng (tty:tty-backend-active-p b) "not active before start")
    (ok (null (tui:backend-stop b)) "stop on a never-started backend is a no-op")
    (ok (null (tui:backend-stop b)) "and idempotent"))
  (ok (subtypep 'tty:tty-not-a-terminal 'tui:tui-backend-error))
  (ok (subtypep 'tty:tty-system-error 'tui:tui-backend-error)))

;;; Contract. These fail until the backend is implemented.

(deftest stdin-not-a-tty-signals-the-condition
  ;; Child with stdin from the null device and stdout a pipe.
  (let ((report (fresh-report-path)))
    (unwind-protect
         (multiple-value-bind (out err code)
             (uiop:run-program (child-argv :run report) :input nil :output :string
                                                        :error-output :string :ignore-error-status t)
           (declare (ignore out))
           (ok (eql 0 code) (format nil "child exit ~A~%~A" code err))
           (let ((forms (read-report report)))
             (ok (equal (getf (rest (assoc :before forms)) :available) nil)
                 (format nil "tty-available-p is false: ~S" (assoc :before forms)))
             (ok (equal (assoc :not-a-terminal forms)
                        '(:not-a-terminal :stream :input :backend-error-p t))
                 (format nil "report: ~S" forms))
             (ng (assoc :started forms) "the program never started")))
      (delete-report report))))

(deftest raw-mode-set-and-restored
  (with-child (c :run)
    (let ((before (wait-report c :before :timeout *startup-timeout*))
          (started (wait-report c :started :timeout *startup-timeout*)))
      (ok (cooked-state-p (state-of before)) "cooked before start")
      (ok (eq t (getf (rest before) :available)) "tty-available-p on a pty")
      (ok started "program started")
      (ok (and started (raw-state-p (state-of started)))
          (format nil "raw while running: ~S" (and started (state-of started))))
      (ok (wait-output c "READY 80x24") "the frame reached the terminal")
      (type-keys c "a")
      (ok (wait-report c :key :test (lambda (p) (eq (getf p :key) :a))) "a key arrives as a key-message")
      (type-keys c "q")
      (let ((stopped (wait-report c :stopped)))
        (ok stopped "run-tui returned on :quit")
        (ok (and stopped (equal (state-key (state-of stopped)) (state-key (state-of before))))
            (format nil "restored exactly: ~S vs ~S"
                    (and stopped (state-key (state-of stopped))) (state-key (state-of before))))))
    (ok (eql 0 (finish-child c)) "child exits 0")))

(deftest resize-arrives-as-a-message
  (with-child (c :run :cols 80 :rows 24)
    (ok (wait-report c :view :timeout *startup-timeout*
                             :test (lambda (p) (equal p '(:cols 80 :rows 24))))
        "first frame is the pty size")
    (pp:pty-resize (child-pty c) 100 30)
    (ok (wait-report c :resize :test (lambda (p) (equal p '(:cols 100 :rows 30))))
        "resize-message with the new size")
    (ok (wait-report c :view :test (lambda (p) (equal p '(:cols 100 :rows 30))))
        "redrawn at the new size")
    (type-keys c "q")
    (ok (wait-report c :stopped))
    (ok (eql 0 (finish-child c)))))

(defun last-view (child)
  "The size of the newest :VIEW report, as (:COLS C :ROWS R), or NIL."
  (rest (car (last (remove :view (read-report (child-report child))
                           :key #'first :test-not #'eq)))))

(defun settle (child seconds)
  "Drain the child's output for SECONDS."
  (let ((deadline (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
    (loop while (< (get-internal-real-time) deadline) do (drain child))))

(deftest resize-burst-ends-at-the-last-size
  ;; cl-stack-code#49: resizes in quick succession (a font size change twice
  ;; within half a second) end with the program laid out for the last size.
  (with-child (c :run :cols 80 :rows 24)
    (ok (wait-report c :view :timeout *startup-timeout*
                             :test (lambda (p) (equal p '(:cols 80 :rows 24)))))
    (testing "two resizes back to back"
      (pp:pty-resize (child-pty c) 100 30)
      (pp:pty-resize (child-pty c) 90 20)
      (ok (wait-report c :view :test (lambda (p) (equal p '(:cols 90 :rows 20)))))
      (settle c 0.5)
      (ok (equal '(:cols 90 :rows 20) (last-view c)) "nothing drawn at an older size after it"))
    (testing "a burst while frames are being drawn"
      (loop for i from 0 below 20
            do (pp:pty-resize (child-pty c) (+ 60 i) (+ 15 i))
               (drain c 0.005))
      (pp:pty-resize (child-pty c) 123 41)
      (ok (wait-report c :view :test (lambda (p) (equal p '(:cols 123 :rows 41)))))
      (settle c 0.5)
      (ok (equal '(:cols 123 :rows 41) (last-view c)) "the last size wins"))
    (type-keys c "q")
    (ok (wait-report c :stopped))
    (ok (eql 0 (finish-child c)))))

(deftest condition-in-update-restores-the-terminal
  (with-child (c :run)
    (let ((before (wait-report c :before :timeout *startup-timeout*)))
      (ok (wait-report c :started :timeout *startup-timeout*) "program started")
      (type-keys c "x")                 ; the probe signals an error
      (let ((err (wait-report c :error :test (lambda (p) (equal (getf p :message) "probe crash")))))
        (ok (and err (equal (getf (rest err) :message) "probe crash"))
            (format nil "the program's error propagates out of run-tui: ~S" err))
        (ok (and err (equal (state-key (state-of err)) (state-key (state-of before))))
            "restored before the handler runs")))
    (ok (eql 0 (finish-child c)))))

#-windows
(deftest crash-exit-restores-the-terminal
  ;; Uncaught error under --non-interactive: the process dies with status 1.
  ;; sh records `stty -g` before the child starts and after it is gone.
  (with-child (c :uncaught :wrap t)
    (let ((started (wait-report c :started :timeout *startup-timeout*)))
      (ok (and started (raw-state-p (state-of started))) "raw while running")
      (type-keys c "x")
      (ok (eql 0 (finish-child c :timeout 60)) "sh exits 0")
      (let ((code (report-file (child-report c) ".code"))
            (before (report-file (child-report c) ".before"))
            (after (report-file (child-report c) ".after")))
        (ok (equal code "1") (format nil "child died with status 1, got ~S" code))
        (ok (and before (plusp (length before))) "state saved before")
        (ok (and started (equal before after))
            (format nil "restored after the crash: ~S vs ~S" before after))))))
