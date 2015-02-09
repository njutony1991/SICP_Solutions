(define (reverse-flr seq)
   (fold-right (lambda (first reversed) (append reversed (list first))) '() seq)
)

(define (reverse-fll seq)
   (fold-left (lambda (reversed first) (cons first reversed)) '() seq)
)