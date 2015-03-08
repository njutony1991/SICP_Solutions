(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      #f
      (let ((cur-key (key (entry set-of-records))))
      	(cond ((= given-key cur-key) (data (entry set-of-records)))
      	      ((> given-key cur-key)
      	      		(lookup given-key (right-branch set-of-records)))
      	      ((< given-key cur-key)
      	      		(lookup given-key (left-branch set-of-records)))
      	)
      )
  )
)

(define (make-record key data)
  (cons key data)
)

(define (key record)
  (car record))

(define (data record)
  (cdr record))