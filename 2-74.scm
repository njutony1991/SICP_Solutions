(define (get-record division employee-name)
   ((get division 'record) employee-name)
)

(define (get-salary division record)
   ((get division 'get-salary) record))

(define (find-employee-record name division-list)
  (if (null? division-list)
      #f
      (or (get-record (car division-list) name)
      	  (find-employee-record name (cdr division-list)))
  )
)