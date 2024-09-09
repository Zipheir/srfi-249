(import (rnrs base)
        (rnrs exceptions)
        (restarts))

(define-condition-type &restartable &assertion
  make-restartable-assertion-violation
  restartable-assertion-violation?
  (restarters condition-restarters)) ; list of restarters

;; Evaluates *thunk*, restarting interactively if a restartable
;; assertion violation is raised. The *restarters* list is extended
;; with a new restarter that aborts *thunk*'s computation.
(define (with-interactive-restart-handler restarters thunk)
  (guard (con
          ((restartable-assertion-violation? con)
           (restart-interactively (condition-restarters con))))
    (call-with-current-continuation
     (lambda (abort)
       (let ((aborter (make-restarter 'abort
                                      '("Abort the current computation")
                                      abort)))
         (with-restarters (cons aborter restarters) thunk))))))

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

;; Divide *num* by *denom*. Raises a restartable assertion violation
;; condition if *denom* is zero.
(define (divide num denom)
  (define return-numerator-restarter
    (make-restarter 'return-numerator
                    '("Return the numerator.")
                    (lambda () num)))

  (with-interactive-restart-handler
   (list return-zero-restarter
         return-numerator-restarter
         return-value-restarter)
   (lambda ()
     (if (zero? denom)
         (assertion-violation/restarters)
         (/ num denom)))))
