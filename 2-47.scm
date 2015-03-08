(load "2-46.scm")
(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2)
)

(define (origin-frame1 frame)
  (car frame)
)

(define (edge1-frame1 frame)
  (car (cdr frame))
)

(define (edge2-frame1 frame)
  (car (cdr (cdr frame)))
)

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2))
)

(define (origin-frame2 frame)
  (car frame)
)

(define (edge1-frame2 frame)
  (car (cdr frame))
)

(define (edge2-frame2 frame)
  (cdr (cdr frame))
)

(define (frame-coord-map frame)
  (lambda (v) 
  	(add-vect 
  		(origin-frame1 frame)
  		(add-vect (scale-vect (edge1-frame1) (xcor-vect v))
  				  (scale-vect (edge2-frame1) (ycor-vect v)))
  	)
  )
)