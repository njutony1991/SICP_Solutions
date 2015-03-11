(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
        	    balance)
        "Insufficient funds")
  )
  (define (deposit amount)
    (set! balance (+ balance amount))
	balance)
  (define (dispatch p op)
    (cond ((not (eq? p password)) (lambda (m) "Wrong password"))
    	  ((eq? op 'withdraw) withdraw)
          ((eq? op 'deposit) deposit)
      	  (else (error "Unknow-request -- MAKE-ACCOUNT"))))
  dispatch
)