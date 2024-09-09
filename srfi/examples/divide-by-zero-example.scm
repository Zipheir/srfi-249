(import (rnrs base)
        (rnrs exceptions)
        (restarts))

(define-condition-type &restartable &assertion
  make-restartable-assertion-violation
  restartable-assertion-violation?
  (restarters condition-restarters))

(define (with-interactive-restart thunk)
  (guard (con
          ((restartable-assertion-violation? con)
           (restart-interactively (condition-restarters con))))
    (call-with-current-continuation
     (lambda (abort)
       (let ((aborter (make-restarter 'abort
                                      '("Abort the current computation")
                                      abort)))
         (with-restarters (list aborter) thunk))))))

(define (assertion-violation/restarters)
  (raise-continuable
   (make-restartable-assertion-violation (ambient-restarters))))

(define return-zero-restarter
  (make-restarter 'return-zero
   '("Return zero.")
   (lambda () 0)))

(define return-value-restarter
  (make-restarter 'return-value
                  '("Return a specified value" "The value to return")
                  (lambda (x) x)))

(define (divide num denom)
  (define return-numerator-restarter
    (make-restarter 'return-numerator
                    '("Return the numerator.")
                    (lambda () num)))

  (with-interactive-restart
   (lambda ()
     (with-restarters
      (list return-zero-restarter
            return-numerator-restarter
            return-value-restarter)
      (lambda ()
        (if (zero? denom)
            (assertion-violation/restarters)
            (/ num denom)))))))
