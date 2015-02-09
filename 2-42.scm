(load "2-40.scm")

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter 
        	(lambda (positions) (safe? k positions))
        	(flatmap
        		(lambda (rest-of-queens)
        			(map (lambda (new-row)
        					(adjoin-position new-row k rest-of-queens))
        				 (enumerate-interval 1 board-size))
        		)
        		(queen-cols (- k 1))
        	)
        )
    )
  )
  (queen-cols board-size)
)

(define (safe? parameters)
  body)

(define (adjoin-position parameters)
  body)

(define empty-board )