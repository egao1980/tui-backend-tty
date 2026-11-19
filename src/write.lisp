(in-package #:tui-backend-tty)

;;; The retry loop shared by the Unix and Windows %OS-WRITE. Short writes
;;; resume where they stopped; a write that reports 0 octets without an error
;;; makes no progress, so +ZERO-WRITE-LIMIT+ of them in a row is a
;;; TTY-SYSTEM-ERROR instead of a busy loop (issue #2).

(defconstant +zero-write-limit+ 16
  "Consecutive 0-octet writes after which %WRITE-FULLY gives up.")

(defun %write-fully (count writer operation)
  "Write COUNT octets through WRITER. WRITER is called with the octets done so
far and returns how many more it wrote (>= 0), or NIL when the call was
interrupted (EINTR) and should be repeated; it signals on a real failure.
+ZERO-WRITE-LIMIT+ consecutive 0 results signal TTY-SYSTEM-ERROR for
OPERATION (a keyword such as :WRITE). → NIL."
  (declare (type (integer 0) count) (type function writer) (type keyword operation))
  (let ((done 0) (zeros 0))
    (loop while (< done count)
          do (let ((n (funcall writer done)))
               (cond ((null n))
                     ((plusp n) (incf done n) (setf zeros 0))
                     ((>= (incf zeros) +zero-write-limit+)
                      (error 'tty-system-error
                             :operation operation
                             :cause (format nil "~(~A~) wrote 0 octets ~D times in a row with ~D of ~D left"
                                            operation zeros (- count done) count))))))
    nil))
