(load "2-40.scm")

(define (seq-sum n s)
  		  (flat_map (lambda (i) (flat_map (lambda (j) 
  										(map (lambda (k) (list i j k)) 
  											(enumerate-interval 1 (- j 1)))) 
  								(enumerate-interval 1 (- i 1)))) 
  		  (enumerate-interval 1 n))
)