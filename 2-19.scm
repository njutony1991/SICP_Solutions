(define (cc amount coin-values)
  (cond ((= 0 amount) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
        	(+ (cc amount (except-first-denomination coin-values))
        	   (cc (- amount (first-denomination coin-values)) coin-values)
        	)
        )
  )
)

(define no-more? null?)
(define except-first-denomination cdr)
(define first-denomination car)