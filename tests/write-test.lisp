(in-package #:tui-backend-tty/tests)

;;; The write loop behind %OS-WRITE, driven through a stub writer: no
;;; terminal needed. A writer gets the octets done so far and returns how
;;; many more it wrote, or NIL for an interrupted call (EINTR).

(defun stub-writer (results)
  "→ (values writer calls): WRITER returns the next of RESULTS (the last one
repeats); CALLS returns the DONE arguments it saw, oldest first."
  (let ((seen '()))
    (values (lambda (done)
              (push done seen)
              (if (rest results) (pop results) (first results)))
            (lambda () (reverse seen)))))

(deftest write-fully-writes-everything
  (multiple-value-bind (writer calls) (stub-writer '(3 0 nil 3 4))
    (ok (null (tty::%write-fully 10 writer :write)))
    (ok (equal '(0 3 3 3 6) (funcall calls)) "short, zero and interrupted writes resume")))

(deftest write-fully-bounds-zero-writes
  (multiple-value-bind (writer calls) (stub-writer '(4 0))
    (let ((c (handler-case (progn (tty::%write-fully 10 writer :write-file) nil)
               (tty:tty-system-error (c) c))))
      (ok c "a writer stuck at 0 octets signals TTY-SYSTEM-ERROR")
      (ok (typep c 'tui:tui-backend-error))
      (ok (and c (eq :write-file (tty:tty-system-error-operation c))))
      (ok (and c (search "0 octets" (princ-to-string (tui:tui-backend-error-cause c))))
          (format nil "cause: ~A" (and c (tui:tui-backend-error-cause c))))
      (ok (= (1+ tty::+zero-write-limit+) (length (funcall calls)))
          "one write, then +ZERO-WRITE-LIMIT+ zero writes"))))

(deftest write-fully-resets-the-zero-count-on-progress
  (let ((results (append (loop repeat 5
                               append (append (make-list (1- tty::+zero-write-limit+)
                                                         :initial-element 0)
                                              (list 1)))
                         (list 0))))
    (multiple-value-bind (writer calls) (stub-writer results)
      (ok (null (tty::%write-fully 5 writer :write)))
      (ok (equal '(0 1 2 3 4) (remove-duplicates (funcall calls)))
          "five single-octet writes, each after LIMIT - 1 zero writes")
      (ok (= (* 5 tty::+zero-write-limit+) (length (funcall calls))))))
  (ok (null (tty::%write-fully 0 (lambda (done) (error "called with ~A" done)) :write))
      "nothing to write: the writer is not called"))

#-(or windows win32)
(deftest errno-is-read-through-cffi
  (ok (zerop (tty::%isatty -1)))
  (ok (eql 9 (tty::%cffi-errno)) "EBADF from isatty(-1) through the CFFI accessor")
  (ok (zerop (tty::%isatty -1)))
  (ok (eql 9 (tty::%errno)) "and through %ERRNO"))
