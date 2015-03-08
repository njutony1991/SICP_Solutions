(load "2-23.scm")
(load "2-47.scm")
(define (make-segment start-seg end-seg)
   (cons start-seg end-seg)
)

(define (start-segment seg)
   (car seg)
)

(define (end-segment seg)
   (cdr seg)
)

(define (segment-painter segment-list)
   (lambda (frame) 
   	(for-each
   		(lambda (segment) 
   			(draw-line
   				((frame-coord-map frame) (start-segment segment))
   				((frame-coord-map frame) (end-segment segment))
   			))
   		segment-list
   	)
   )
)