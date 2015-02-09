(define (tree-map proc tree)
   (map (lambda (subtree)
   			(cond ((null? subtree) '())
   			      ((not (pair? subtree)) (proc subtree))
   			      (else 
   			         	(tree-map proc subtree)
   			      )
   		    ) 
   		)
    tree
   )
)
