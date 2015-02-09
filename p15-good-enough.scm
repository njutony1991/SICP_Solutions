(define (goodenough? oldguess newguess)
	(< (/ (abs (- newguess oldguess))
		oldguess) 0.01)
)