;;; This example is based on the examples from Ch. 19 of Peter
;;; Seibel's _Practical Common Lisp_. It models a parser for a
;;; log comprising a sequence of entries.
(import (rnrs base)
        (rnrs exceptions)
        (rnrs io ports)
        (only (srfi :152 strings) string-split)
        (srfi :249 restarts))

(define-condition-type &malformed-log-entry
                       &condition
  make-malformed-log-entry
  malformed-log-entry?
  (input malformed-log-entry-input))

(define (malformed-log-entry s)
  (raise-continuable
   (make-malformed-log-entry s)))

(define (parse-log-entry s)
  (if (well-formed-log-entry? s)
      ; (make-log-entry ...)  ; Construct representation of entry
      (malformed-log-entry s)))

(define skip-log-entry
  (make-restarter 'skip-log-entry
    '("Skip the current log entry.")
    (lambda () #f)))

;; Return a list of parsed entries read from *path*. Establishes
;; a new restarter, skip-log-entry, which can be used to skip an
;; invalid entry. [This should be put in parse-log-file's docs;
;; otherwise, users won't be able to invoke this new restarter unless
;; it's described for them by, say, restart-interactively.]
(define (parse-log-file path)
  (call-with-input-file
   path
   (lambda (port)
     (with-restarters
      skip-log-entry
      (lambda () (get-entries port))))))

(define (get-entries port)
  (let build ((s (get-line port)))
    (cond ((eof-object? s) '())
          ((parse-log-entry s) =>
           (lambda (e) (cons e (build (get-line port)))))
          (else (build (get-line port))))))

;;; If reckless Ben Bitdiddle doesn't care about parsing errors, he
;;; might run the parser as follows:
(guard (con
        ((malformed-log-entry-condition? con)
         (cond ((find-restarter 'skip-log-entry '()) =>
                restart)
               (else (raise-continuable con)))))
  (parse-log-file "couldnt-possibly-be-corrupted.log"))
