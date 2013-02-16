;;content
(define (new-withdraw)
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))))
(define withdraw (new-withdraw))
(newline)
(display (withdraw 30))
(newline)
(display (withdraw 80))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))
(newline)
(display (W1 50))
(newline)
(display (W2 70))
(newline)
(display (W2 40))
(newline)
(display (W1 40))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request -- MAKE-ACCOUNT")
		m)))
  dispatch)

(define acc (make-account 100))
(newline)
(display ((acc 'withdraw) 50))
(newline)
(display ((acc 'withdraw) 60))
(newline)
(display ((acc 'deposit) 40))
(newline)
(display ((acc 'withdraw) 60))

;exercise 3.1
(define (make-accumulator sum)
  (lambda (value)
    (begin (set! sum (+ sum value))
	   sum)))

(define A (make-accumulator 5))
(newline)
(display (A 10))
(newline)
(display (A 10))

;;exercise 3.2
(define (make-monitored func)
  (define calls 0)
  (define (how-many-calls?)
    calls)
  (define (reset-count)
    (begin (set! calls 0)
	   calls))
  (define (others x)
    (begin (set! calls (+ calls 1))
	   (sqrt x)))
  (define (dispatch mf)
    (cond ((eq? mf 'how-many-calls?) (how-many-calls?))
	  ((eq? mf 'reset-count) (reset-count))
	  (else (others mf))))
  dispatch)

(define s (make-monitored sqrt))
(newline)
(display (s 100))
(newline)
(display (s 'how-many-calls?))
(newline)
(display (s 225))
(newline)
(display (s 'how-many-calls?))
(newline)
(display (s 'reset-count))

;;exercise 3.3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (begin (set! balance (+ balance amount))
	   balance))
  (define (incorrect-password amount)
    "Incorrect password")
  (define (dispatch secret-password m)
    (if (eq? secret-password password)
	(cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'deposit) deposit)
	      (else (error "Unknown request -- MAKE-ACCOUNT!")
		    m))
	incorrect-password))
  dispatch)

(define acc (make-account 100 'secret-password))
(newline)
(display ((acc 'secret-password 'withdraw) 40))
(newline)
(display ((acc 'secret-password2 'deposit) 50))

;;exercise 3.4
(define (make-account balance password)
  (define (call-the-cops) "Call the cpos")
  (let ((count 0)
	(limit 7))
    (define (withdraw amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Not enough money"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch pass m)
      (if (not (eq? pass password))
	  (lambda (amount)
	    (if (>= count limit)
		(call-the-cops)
		(begin (set! count (+ count 1))
		       "Incorrect password")))
	  (begin (set! count 0)
		 (cond ((eq? m 'withdraw) withdraw)
		       ((eq? m 'deposit) deposit)
		       (else (error "Uknown call --MAKE-ACCOUNT"
			     m))))))
  dispatch))

(define acc (make-account 100 '123))
(newline)
(display ((acc '1 'withdraw) 40))
(newline)
(display ((acc '2 'withdraw) 40))
(newline)
(display ((acc '3 'withdraw) 40))
(newline)
(display ((acc '4 'withdraw) 40))
(newline)
(display ((acc '5 'withdraw) 40))
(newline)
(display ((acc '6 'withdraw) 40))
(newline)
(display ((acc '7 'withdraw) 40))
(newline)
(display ((acc '8 'withdraw) 40))

;;exercise 3.5
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (circle center-x center-y r)
  (lambda (px py) (<= (+ (square (- px center-x))
			 (square (- py center-y)))
		      (square r))))

(define (in-the-region-test px py pred) (pred px py))

(define (test px py)
  (in-the-region-test px py (circle 5 7 3)))

(define (estimate-integral p x1 x2 y1 y2 trial)
  (define (iter remain passed)
    (cond ((= remain 0) (/ passed trial))
	  ((p (random-in-range x1 x2)
	      (random-in-range y1 y2))
	   (iter (- remain 1) (+ passed 1)))
	  (else (iter (- remain 1) passed))))
  (* (* (- x2 x1)
	(- y2 y1))
     (* (iter trial 0)
	1.0)))

(newline)
(display (estimate-integral test 0 10 0 10 10000))

;;exercise 3.6
(define random-init (random (- (expt 2 31)
				 1)))

(define (random-update x) (remainder (+ (* 13 x)
					5)
				     24))

(define rand
  (let ((x random-init))
    (define (generate)
      (set! x (random-update x)) x)
    (define (reset new-value)
      (set! x new-value) x)
    (define (dispatch op)
      (cond ((eq? op 'generate) (generate))
	    ((eq? op 'reset) reset)
	    (else (error "No method error! -- " op))))
    dispatch))

(newline)
(display (rand 'generate))
(newline)
(display (rand 'generate))
(newline)
(display ((rand 'reset) 20))
(newline)
(display (rand 'generate))

;;exercise 3.7
(define (make-joint acc password new-password)
  (define (withdraw amount)
    ((acc password 'withdraw) amount))
  (define (deposit account)
    ((acc password 'deposit) amount))
  (define(dispatch secret-password m)
    (if (eq? secret-password new-password)
	(cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'deposit) deposit)
	      (else (error "Unknow request -- MAKE-JOINT!")
		    m))
	(lambda (amount)
	  "Incorrect password")))
  dispatch)

(define acc (make-account 100 '123))
(define joi (make-joint acc '123 '456))
(newline)
(display ((acc '123 'withdraw) 30))
(newline)
(display ((joi '456 'withdraw) 30))

;;exercise 3.8
(define y 1)
(define (f x)
  (begin (set! y (* y x))
	 y))
(newline)
(display (+ (f 0)
	    (f 1)))
(newline)
(display (+ (f 1)
	    (f 0)))

;;exercise 3.9

;;exercise 3.10

;;exercise 3.11

;;exercise 3.12
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
(newline)
(display (cdr x))
(define w (append! x y))
(newline)
(display (cdr x))

;;exercise 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define z (make-cycle (list 'a 'b 'c)))
;(newline)
;(display z)
;infinite loop

;;exercise 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
	y
	(let ((temp (cdr x)))
	  (set-cdr! x y)
	  (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))
(newline)
(display w)
(newline)
(display v)

;;exercise 3.15

;;exercise 3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x))
	 1)))
(define l31 (list 'a 'b 'c))

(define l41 (list 'b 'c))
(define l42 (list 'a))
(set-car! l41 l42)
(set-car! (cdr l41) l42)

(define l71 (list 'a))
(define l72 (list 'b))
(define l73 (list 'c))
(set-car! l72 l73)
(set-cdr! l72 l73)
(set-car! l71 l72)
(set-cdr! l71 l72)

(newline)
(display (count-pairs l31))
(newline)
(display (count-pairs l41))
(newline)
(display (count-pairs l71))

;exercise 3.17
(define (count-pairs x)
  (let ((aux '()))
    (define (count z)
      (cond ((not (pair? z)) 0)
	    ((memq z aux) 0)
	    (else
	     (if (null? aux)
		 (set! aux (list z))
		 (set-cdr! (last-pair aux) (list z)))
	     (+ (count (car z))
		(count (cdr z))
		1))))
    (count x)))

(newline)
(display (count-pairs l71))

;;exercise 3.18
(define (cycle? x)
  (let ((aux '()))
    (define (check-cycle y)
      (cond ((null? y) false)
	    ((memq (car y) aux) true)
	    (else
	     (begin
	       (if (null? aux)
		   (set! aux (list (car y)))
		   (set-cdr! (last-pair aux) (list (car y))))
	       (check-cycle (cdr y))))))
    (check-cycle x)))

(newline)
(display (cycle? l71))
(define test-cycle (list 'a 'b 'c))
(set-cdr! (last-pair test-cycle) test-cycle)
(newline)
(display (cycle? test-cycle))

;;exercise 3.19
(define (cycle? y)
  (define (seen-last-pair? x)
    (or (null? x) (null? (cdr x))))
  (define (chase p1 p2)
    (cond ((or (null? p1) (null? p2)) #f)
	  ((eq? (car p1) (car p2)) #t)
	  ((seen-last-pair? p2) #f)
	  (else
	   (chase (cdr p1) (cddr p2)))))
  (if (seen-last-pair? y)
      #f
      (chase y (cdr y))))

(newline)
(display (cycle? test-cycle))

;;content
(define (my-cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
	  ((eq? m 'cdr) y)
	  ((eq? m 'set-car!) set-x!)
	  ((eq? m 'set-cdr!) set-y!)
	  (else (error "Undefined operator -- CONS" m))))
  dispatch)

(define (my-car z) (z 'car))
(define (my-cdr z) (z 'cdr))
(define (my-set-car! z new-value)
  ((z 'set-car!) new-value)
  z)
(define (my-set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

;;exercise 3.20
(define x (my-cons 1 2))
(define z (my-cons x x))
(my-set-car! (my-cdr z) 17)
(newline)
(display (my-car x))


;;content
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
	   (set-front-ptr! queue new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue)
	  (else (set-cdr! (rear-ptr queue) new-pair)
		(set-rear-ptr! queue new-pair)
		queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
	 (error "DELETE! called with an empty queue" queue))
	(else
	 (set-front-ptr! queue (cdr (front-ptr queue)))
	 queue)))

(define (print-queue queue)
  (display (front-ptr queue)))

(define q (make-queue))
(newline)
(display q)
(newline)
(display (insert-queue! q 'a))
(newline)
(display (insert-queue! q 'b))
(newline)
(display (delete-queue! q))
(newline)
(display (insert-queue! q 'c))
(newline)
(display (insert-queue! q 'd))
(newline)
(display (delete-queue! q))

;;exercise 3.21
(define q1 (make-queue))
(insert-queue! q1 'a)
(newline)
(print-queue q1)

(insert-queue! q1 'b)
(newline)
(print-queue q1)

(delete-queue! q1)
(newline)
(print-queue q1)

(delete-queue! q1)
(newline)
(print-queue q1)

;;exercise 3.22
(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
   
    (define (front-deque)
      (if (empty-queue?)
	  (error "FRONT call withed an empty queue" queue)
	  (car front-ptr)))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
	(cond ((empty-queue?) 
	       (set-front-ptr! new-pair)
	       (set-rear-ptr! new-pair)
	       front-ptr)
	      (else
	       (set-cdr! rear-ptr new-pair)
	       (set-rear-ptr! new-pair)
	       front-ptr))))
    (define (delete-queue!)
      (cond ((empty-queue?) 
	     (error "DELETE! called with en empty queue" queue))
	    (else
	     (set-front-ptr! (cdr front-ptr))
	     front-ptr)))
    (define (print-queue)
      (display front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'insert-queue!) insert-queue!)
	    ((eq? m 'delete-queue!) delete-queue!)
	    ((eq? m 'print-queue) print-queue)
	    (else
	     (error "ERROR! Uknown request -- MAKE-QUEUE!" front-ptr))))
    dispatch))
      

(define q2 (make-queue))
(newline)
((q2 'print-queue))

((q2 'insert-queue!) 'a)
(newline)
((q2 'print-queue))

((q2 'insert-queue!) 'b)
(newline)
((q2 'print-queue))

((q2 'delete-queue!))
(newline)
((q2 'print-queue))

((q2 'delete-queue!))
(newline)
((q2 'print-queue))

;;exercise 3.23
(define (make-deque)
  (define front '())
  (define rear '())
  
  (define (set-front! item)
    (set! front item))
  (define (set-rear! item)
    (set! rear item))
  (define (empty-deque?) (null? front))
  (define (insert-front! item)
    (let ((new-front (cons (cons item '()) front)))
      (cond ((empty-deque?)
	     (set-front! new-front)
	     (set-rear! new-front)
	     dispatch)
	    (else
	     (set-cdr! (car front)  new-front)
	     (set-front! new-front)
	     dispatch))))

  (define (insert-rear! item)
    (let ((new-rear (cons (cons item rear) '())))
      (cond ((empty-deque?)
	     (set-front! new-rear)
	     (set-rear! new-rear)
	     dispatch)
	    (else
	     (set-cdr! rear new-rear)
	     (set-rear! new-rear)
	     dispatch))))

  (define (delete-front!)
    (cond ((empty-deque?)
	   (error "DELETE-FRONT! called on empty queue!" front))
	   (else
	    (set-front! (cdr front))
	    dispatch)))

  (define (delete-rear!)
    (cond ((empty-deque?)
	   (error "DELETE-REAR! called on empty queue!" rear))
	   (else
	    (set-rear! (cdar rear))
	    (if (null? rear)
		(set-front! '())
		(set-cdr! rear '()))
	    dispatch)))
  
  (define (print-deque)
    (define (print-end) 
      (display ")")
      (newline))
    (display "(")
    (define (iter next)
      (cond ((null? next) (print-end))
	    (else
	     (display (caar next))
		      (display " ")
		      (iter (cdr next)))))
    (iter front))

  (define (dispatch m)
    (cond ((eq? m 'insert-front!) insert-front!)
	  ((eq? m 'insert-rear!) insert-rear!)
	  ((eq? m 'delete-front!) delete-front!)
	  ((eq? m 'delete-rear!) delete-rear!)
	  ((eq? m 'front) front)
	  ((eq? m 'rear) rear)
	  ((eq? m 'print) (print-deque))
	  (else (error "DEQUE -- Unknow request" m))))
  dispatch)

(define dq (make-deque))
(newline)
(((dq 'insert-rear!) 'a) 'print)

(((dq 'insert-front!) 'z) 'print)

(((dq 'insert-rear!) 'b) 'print)

(((dq 'insert-front!) 'y) 'print)

(((dq 'insert-rear!) 'c) 'print)

(((dq 'insert-front!) 'x) 'print)

(((dq 'delete-rear!)) 'print)

(((dq 'delete-front!)) 'print)

(((dq 'delete-rear!)) 'print)

(((dq 'delete-front!)) 'print)

(((dq 'delete-rear!)) 'print)

(((dq 'delete-front!)) 'print)

;;content
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
	(cdr record)
	false)))

(define (assoc key records)
  (cond ((null? records) false)
	((equal? key (caar records))
	 (car records))
	(else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
	(set-cdr! record value)
	(set-cdr! table
		  (cons (cons key value) 
			(cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define tb (make-table))

(insert! 'a 1 tb)
(insert! 'b 2 tb)
(newline)
(display (lookup 'a tb))
(newline)
(display (lookup 'c tb))


(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
	(let ((record (assoc key-2 (cdr subtable))))
	  (if record
	      (cdr record)
	      false))
	false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
	(let ((record (assoc key-2 (cdr subtable))))
	  (if record
	      (set-cdr! record value)
	      (set-cdr! subtable
			(cons (cons key-2 value)
			      (cdr subtable)))))
	(set-cdr! table
		  (cons (list key-1
			      (cons key-2 value))
			(cdr table)))))
  'ok)

(define tb-2 (make-table))
(insert! 'math '+ 43 tb-2)
(insert! 'letters 'a 97 tb-2)
(newline)
(display (lookup 'math '+ tb-2))
(newline)
(display (lookup 'math 'a tb-2))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (cdr record)
		  false))
	    false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value)
				  (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key-1
				  (cons key-2 value))
			    (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else
	     (error "Unknow operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 'math '+ 43)
(put 'letter '+ 97)
(newline)
(display (get 'math '+))
(newline)
(display (get 'letter 'a))

;;exercise 3.24
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
	    ((same-key? key (caar records))
	     (car records))
	    (else
	     (assoc key (cdr records)))))

    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (cdr record)
		  false))
	    false)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value)
				  (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key-1
				  (cons key-2 value))
			    (cdr local-table)))))
      'ok)
    
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
	    ((eq? m 'insert!) insert!)
	    (else (error "Unknow operator -- TABLE" m))))
    dispatch))

(define tb-3 (make-table equal?))
((tb-3 'insert!) 'math '+ 47)
((tb-3 'insert!) 'letters '+ 97)
(newline)
(display ((tb-3 'lookup) 'math '+))
(newline)
(display ((tb-3 'lookup) 'letters 'a))

;;exercise 3.25
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
	    ((equal? key (caar records)) (car records))
	    (else (assoc key (cdr records)))))
    (define (lookup key-list)
      (define (lookup-1 keys table)
	(let ((subtable (assoc (car keys) (cdr table))))
	  (if subtable
	      (if (null? (cdr keys))
		  (cdr subtable)
		  (lookup-1 (cdr keys) subtable))
	      false)))
      (lookup-1 key-list local-table))
    
    (define (insert! key-list value)
      (define (make-entry keys)
	(if (null? (cdr keys))
	    (cons (car keys) value)
	    (list (car keys)
		  (make-entry (cdr keys)))))
      (define (insert-1! keys table)
	(let ((subtable (assoc (car keys) (cdr table))))
	  (if subtable
	      (if (null? (cdr keys))
		  (set-cdr! subtable value)
		  (insert-1! (cdr keys) subtable))
	      (set-cdr! table
			(cons (make-entry keys)
			      (cdr table))))))
      (insert-1! key-list local-table)
      'ok)
    (define (print-table)
      (newline)
      (display (cdr local-table)))

    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
	    ((eq? m 'insert!) insert!)
	    ((eq? m 'print-table) (print-table))
	    (else
	     (error "Unknown operator -- TABLE" m))))
    dispatch))

(define tb-4 (make-table))
((tb-4 'insert!) '(e f g h) '2)
((tb-4 'insert!) '(a b c) '1)
(newline)
(display ((tb-4 'lookup) '(a b c)))
(newline)
(display ((tb-4 'lookup) '(e f g h)))

;;exercise 3.26
(define (make-table)
  (define local-table '())
  (define make-record cons)
  (define key-record car)
  (define value-record cdr)

  (define (make-tree entry left right)
    (list entry left right))
  (define entry car)
  (define left-branch cadr)
  (define right-branch caddr)
  
  (define key=? equal?)
  
  (define (key<? key1 key2)
    (cond ((and (string? key1)
		(string? key2)) (string<? key1 key2))
	  ((and (number? key1)
		(number? key2)) (< key1 key2))
	  ((and (char? key1)
		(char? key2)) (char<? key1 key2))
	  (else (error "Unsupported key types --KEY<?" key1 key2))))

  (define (element-of-set? x set)
    (cond ((null? set) false)
	  ((key=? (key-record x) (key-record (entry set)))
	   true)
	  ((key<? (key-record x) (key-record (entry set)))
	   (element-of-set? x (left-branch set)))
	  (else
	   (element-of-set? x (right-branch set)))))

  (define (adjoin-set x set)
    (cond ((null? set) (make-tree x '() '()))
	  ((key=? (key-record x) (key-record (entry set))) set)
	  ((key<? (key-record x) (key-record (entry set)))
	   (make-tree (entry set)
		      (adjoin-set x (left-branch set))
		      (right-branch set)))
	  (else
	   (make-tree (entry set)
		      (left-branch set)
		      (adjoin-set x (right-branch set))))))
  
  (define (lookup-1 key records)
    (if (null? records) 
	false
	(let ((record (entry records)))
	  (cond ((key=? key (key-record record)) (value-record record))
		((key<? key (key-record record))
		 (lookup-1 key (left-branch records)))
		(else 
		 (lookup-1 key (right-branch records)))))))
  
  (define (insert! key value)
    (set! local-table
	  (adjoin-set (make-record key value)
		      local-table)))

  (define (lookup key)
    (lookup-1 key local-table))

  (define (print)
    (newline)
    (display local-table))

  (define (dispatch m)
    (cond ((eq? m 'lookup) lookup)
	  ((eq? m 'insert!) insert!)
	  ((eq? m 'print) (print))
	  (else 
	   (error "Unknow operation -- TABLE" m))))
  dispatch)

(define tb-5 (make-table))
((tb-5 'insert!) 2 'a)
(tb-5 'print)
((tb-5 'insert!) 1 'b)
(tb-5 'print)
(newline)
(display ((tb-5 'lookup) 2))

(define tb-6 (make-table))
((tb-6 'insert!) "hello" 222)
((tb-6 'insert!) "world" 333)
(newline)
(display ((tb-6 'lookup) "hello"))
(newline)
(display ((tb-6 'lookup) "hell"))

;;exercise 3.27
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result ((table 'lookup) x)))
	(or previously-computed-result
	    (let ((result (f x)))
	      ((table 'insert!) x result)
	      result))))))
(define memo-fib 
  (memoize (lambda (n)
	     (cond ((= n 0) 0)
		   ((= n 1) 1)
		   (else 
		    (+ (memo-fib (- n 1))
		       (memo-fib (- n 2))))))))

(newline)
(display (memo-fib 3))
(newline)
(display (memo-fib 1000))

;;content
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
		   (lambda()
		     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
	((= s 1) 0)
	(else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
	   (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
		   (lambda()
		     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
	   (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
		   (lambda()
		     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;;exercise 3.29
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((not-a1 (make-wire))
	  (not-a2 (make-wire))
	  (b (make-wire)))
      (inverter a1 not-a1)
      (inverter a2 not-a2)
      (and-gate not-a1 not-a2 b)
      (inverter b output)))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;or-gate-delay-time + 2 * inverter-delay-time

;;exercise 3.30
(define (ripple-carry-adder as bs ss c-out)
  (define (last-bit? x)
    (null? (cdr x)))
  
  (define (ripple-iter as bs ss ic-in ic-out)
    (if (null? as)
	'ok
	((full-adder (car as)
		    (car bs)
		    c-in
		    (car ss)
		    (if (last-bit? as) c-out ic-out))
	(ripple-iter (cdr as) 
		     (cdr bs)
		     (cdr ss)
		     ic-out
		     (make-wire)))))

  (if (and (= (length as) (length bs))
	   (= (length bs) (length ss)))
      (let ((c-in (make-wire))
	    (sum (make-wire)))
	(ripple-iter as bs ss c-in sum))
      (error "Input must be the same length -- RIPPLE-CARRY-ADDER!" 
	     (length as) (length bs) (length cs))))

;;content
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
	  (begin (set! signal-value new-value)
		 (call-each action-procedures))
	  'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures! (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
	    ((eq? m 'set-signal!) set-my-signal!)
	    ((eq? m 'add-action!) accept-action-procedure!)
	    (else (error "Unknown operator -- MAKE-WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
	((car procedures))
	(call-each (cdr procedures)))))

(define (get-signal wire) 
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedures)
  ((wire 'add-action!) action-procedure))


(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
		  action
		  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
	(first-item)
	(remove-first-agenda-item! the-agenda)
	(propagate))))

;;exercise 3.31
;the procedure should be executed immediately, or the system would never start

;;content
(define (make-time-segmeng time queue)
  (cons time queue))

(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
	(< time (segment-time (car segments)))))
  
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))

  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
	(insert-queue! (segment-queue (car segments))
		       action)
	(let ((rest (cdr segments)))
	  (if (belongs-before? rest)
	      (set-cdr!
	       segments
	       (cons (make-new-time-segment time action)
		     (cdr segments)))
	      (add-to-segments! rest)))))
  
  (let ((segments (segments agenda)))
    (if (belongs-before? segents)
	(set-segments!
	 agenda
	 (cons (make-new-time-segment time action)
	       segments))
	 (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
	(set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? aganda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
	(set-current-time! agenda (segment-time first-seg))
	(front-queue (segment-queue first-seg)))))

;;exercise 3.32
;operations must be done in the order created, LIFO would reverse the order

;;content
(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
	  ((eq? (car items) exception) (loop (cdr items)))
	  (else (procedure (car items))
		(loop (cdr items)))))
  (loop list))

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
	     (set! value newval)
	     (set! informant setter)
	     (for-each-except setter
			      inform-about-value
			      constraints))
	    ((not (= value newval))
	     (error "Contradiction" (list value newval)))
	    (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
	  (begin (set! informant false)
		 (for-each-except retractor
				  inform-about-no-value
				  constraints))
	  'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
	       (set! constraints
		     (cons new-constraint constraints)))
      (if (has-value? me)
	  (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
	     (if informant true false))
	    ((eq? request 'value) value)
	    ((eq? request 'set-value!) set-my-value)
	    ((eq? request 'forget) forget-my-value)
	    ((eq? request 'connect) connect)
	    (else (error "Unknown operation -- CONNECTOR"
			 request))))
    me))

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))
(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
	   (process-new-value))
	  ((eq? request 'I-lost-my-value)
	   (process-forget-value))
	  (else
	   (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)


(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
	   (set-value! sum
		       (+ (get-value a1) (get-value a2))
		       me))
	  ((and (has-value? a1) (has-value? sum))
	   (set-value! a2
		       (- (get-value sum) (get-value a1))
		       me))
	  ((and (has-value? a2) (has-value? sum))
	   (set-value! a1
		       (- (get-value sum) (get-value a2))
		       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
	   (process-new-value))
	  ((eq? request 'I-lost-my-value)
	   (process-forget-value))
	  (else
	   (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
	       (and (has-value? m2) (= (get-value m2) 0)))
	   (set-value! product 0 me))
	  ((and (has-value? m1) (has-value? m2))
	   (set-value! product
		       (* (get-value m1) (get-value m2))
		       me))
	  ((and (has-value? product) (has-value? m1))
	   (set-value! m2
		       (/ (get-value product) (get-value m1))
		       me))
	  ((and (has-value? product) (has-value? m2))
	   (set-value! m1
		       (/ (get-value product) (get-value m2))
		       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
	   (process-new-value))
	  ((eq? request 'I-lost-my-value)
	   (process-forget-value))
	  (else
	   (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  (process-new-value)
  me)

(define C (make-connector))
(define F (make-connector))

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
	(v (make-connector))
	(w (make-connector))
	(x (make-connector))
	(y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(celsius-fahrenheit-converter C F)
(newline)
(probe "Celsius temp" C)
(newline)
(probe "Fahrenheit temp" F)
(set-value! C 25 'user)
(forget-value! C 'user)
(set-value! F 212 'user)

;;exercise 3.33

(define (averager a b c)
  (let ((u (make-connector))
	(v (make-connector)))
    (multiplier c v u)
    (adder a b u)
    (constant 2 v)
    'ok))

(define A (make-connector))
(define B (make-connector))
(define C (make-connector))
(averager A B C)
(probe "A-temp" A)
(probe "B-temp" B)
(probe "C-temp" C)
(set-value! A 100 'user)
(set-value! C 200 'user)

;;exercise 3.34
;the value of a would not be set be knowing the value of b

;;exercise 3.35
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
	(if (< (get-value b) 0)
	    (error "square less than 0 -- SQUARE" (get-value b))
	    (set-value! a 
			(sqrt (get-value b)) 
			me))
	(if (has-value? a)
	    (set-value! b
			(square (get-value a))
			me))))
	
	
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
	   (process-new-value))
	  ((eq? request 'I-lost-my-value)
	   (process-forget-value))
	  (else 
	   (error "Unknown request -- SQUARER" request))))

  (connect a me)
  (connect b me)
  me)

(define A (make-connector))
(define B (make-connector))
(squarer A B)
(probe "A-temp" A)
(probe "B-temp" B)
(set-value! B 225 'user)

;;exercise 3.36

;;exercise 3.37
(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
	      x)
	  (cv 32)))

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (add z y x)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(probe "C-temp" C)
(probe "F-temp" F)

(set-value! F 212 'user)

;;exercise 3.38
;a: 45/35/40/50
;b: 110/90/80/60/55/30

;;exercise 3.39
;121/101/100

;;exercise 3.40
;1000000/100000/10000/1000/100
;1000000

;;exercise 3.41
;unnecessary

;;exercise 3.42
;In my opinion, it's safe, there is no difference between these two versions

;;exercise 3.43

;;exercise 3.44
;Louis is not right. The two problems are different.The exchange problem
;involves the two accounts and makes the two accounts dependent on each other
;while in the transfer problem, the acount being deposited does not care
;where the amount being deposited is comming from.

;;exercise 3.45
;in exchanging two accounts, the same serializer would be used twice
;One in the serialized-exchange function and the other in the dispatch
;function. The same mutex be acquared twice would produce a busy 
;waiting that would never halt.

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
	(mutex 'acquire)
	(let ((val (apply p args)))
	  (mutex 'release)
	  val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
	     (if (test-and-set! cell)
		 (the-mutex 'acquire)))
	    ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
	     false)))

;;exercise 3.46

;;exercise 3.47
;a
(define (make-sempahore n)
  (let ((count n)
	(the-mutex (make-mutex)))
    (define (the-sempahore m)
      (cond ((eq? m 'acquire)
	     (the-mutex 'acquire)
	     (if (zero? count)
		 (begin 
		   (the-mutex 'release)
		   (the-sempahore 'acquire))
		 (begin 
		   (set! count (- count 1))
		   (the-mutex 'release))))
	    ((eq? m 'release)
	     (the-mutex 'acquire)
	     (if (= count n)
		 (the-mutex 'release)
		 (begin
		   (set! count (+ count 1))
		   (the-mutex 'release))))))
    the-sempahore))
;b
(define (make-sempahore n)
  (let ((count n)
	(cell (list false)))
    (define (the-sempahore m)
      (cond ((eq? m 'acquire)
	     (if (test-and-set! cell)
		 (the-sempahore 'acquire)
		 (if (zero? count)
		     (clear! cell)
		     (begin (set! count (-count 1))
			    (clear! cell)))))
	    ((eq? m 'release)
	     (if (tst-and-set! cell)
		 (the-sempahore 'release)
		 (if (= count n)
		     (clear! cell)
		     (begin (set! count (+ count 1))
			    (clear! cell)))))))
    the-sempahore))

;;exercise 3.48
(define (make-account-and-serializer balance id)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
	    ((eq? m 'deposit) deposit)
	    ((eq? m 'balance) balance)
	    ((eq? m 'id) id)
	    ((eq? m 'serializer) balance-serializer)
	    (else (error "Unknown request -- MAKE-ACCOUNT"
			 m))))
    dispatch))

(define (serialize-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
	(serializer2 (account2 'serializer))
	(id1 (account1 'id))
	(id2 (account2 'id)))
    (if (< id1 id2)
	((serializer2 (serializer1 exchange))
	 account1
	 account2)
	((serializer1 (serializer2 exchange))
	 account1
	 account2))))

;;exercise 3.49
;a process must get access to some shared resources before it can know
;which additional shared resources it will require

;;content
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
		   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
	     (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

;(define-syntax cons-stream
;  (syntax-rules ()
;    ((_ A B) (cons A (delay B)))))
;(define (cons-stream a b)
;  (display "cons-stream")
;  (newline)
;  (cons a (delay b)))


(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (lambda ()
       expr))))

(define-syntax con-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))))

;(define (stream-car stream) (car stream))

;(define (stream-cdr stream) (force (cdr stream)))

;(define (delay exp)
;  (lambda () exp))

(define the-empty-stream  (stream))
(newline)
(display "the-empty-stream: ")
(display the-empty-stream)
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
	((pred (stream-car stream))
	 (cons-stream (stream-car stream)
		      (stream-filter pred
				     (stream-cdr stream))))
	 (else (stream-filter pred (stream-cdr stream)))))

(define (force delayed-object)
  (delayed-object))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda()
      (if (not already-run?)
	  (begin (set! result (proc))
		 (set! already-run? true)
		 result)
	  result))))

;;exercise 3.50
(define (stream-map proc . argstreams)
  (newline)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
	      (cons proc (map stream-cdr argstreams))))))

;exercise 3.51
(define (show x)
  (display-line x)
  x)

(define ss (cons-stream 1 (show (+ 2 1))))
(newline)
(display (stream-cdr ss))
(define x (stream-map show (stream-enumerate-interval 0 10)))
(newline)
(display (stream-ref x 5))
(newline)
(display (stream-ref x 7))

;;exercise 3.52
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
		   (stream-map proc (stream-cdr s)))))
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
	  (begin (set! result (proc))
		 (set! already-run? true)
		 result)
	  result))))

;(define (delay exp)
;  (memo-proc (lambda () exp)))

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
			 seq))
(newline)
(display "exercise 3.52")
(newline)
(display "interval 1 20")
(newline)
(display-stream (stream-enumerate-interval 1 20))
(newline)
(display "seq:")
(newline)
(display-stream seq)
(display "y:")
(newline)
(display-stream y)
(display (stream-ref y 7))
(display "z:")
(newline)
(display-stream z)

;;content
(newline)
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
		 integers))

(display (stream-ref no-sevens 100))

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
	   (lambda (x)
	     (not (divisible? x (stream-car stream))))
	   (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(newline)
(display (stream-ref primes 50))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

;;exercise 3.53
(define s (cons-stream 1 (add-streams s s)))
;2^n

;;exercise 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1
	       (mul-streams factorials integers)))

;;exercise 3.55
(define (partial-sums args)
  (cons-stream (stream-car args)
	       (add-streams (stream-cdr args)
			   (partial-sums args))))

;;exercise 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (cond ((< s1car s2car)
		  (cons-stream s1car (merge (stream-cdr s1) s2)))
		 ((> s1car s2car)
		  (cons-stream s2car (merge s1 (stream-cdr s2))))
		 (else
		  (cons-stream s1car 
			       (merge (stream-cdr s1)
				      (stream-cdr s2)))))))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define S (cons-stream 1 (merge (scale-stream S 2) 
				(merge (scale-stream S 3)
				       (scale-stream S 5)))))

;;exercise 3.57
;O(n - 1) O(x^n) withour mem-proc

;;exercise 3.58
;give the  result of num * (radix ^ x) (base den)

;;exercise 3.59
;a
(define (integrate-series coeffs)
  (stream-map / coeffs integers))

;b
(define cosine-series
  (cons-stream 1 (integrate-series sine-series)))

(define sine-series
  (cons-stream 0 (scale-stream (integrate-series cosine-series) -1)))

;;exercise 3.60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
		  (stream-car s2))
	       (add-streams (scale-stream (stream-cdr s1)
					  (stream-car s2))
			    (mul-series s1
					(stream-cdr s2)))))

;(define cos-square+sin-square
;  (add-streams (mul-series cosine-series
;			   cosine-series)
;	       (mul-series sine-series
;			   sine-series)))

;(newline)
;(display (stream-ref 0 cos-square+sin-square))

;;exercise 3.61
(define (invert-unit-series S)
  (stream-cons 1
	       (scale-stream (mul-series (stream-cdr S)
					 (invert-unit-series S))
			     -1)))

;;exercise 3.62
(define (div-series S1 S2)
  (let ((denom-const (stream-car S2)))
    (if (zero? denom-const)
	(error ("Denominator constant term must be non-zero -- DIV-SERIES"))
	(mul-series S1
		    (scale-stream
		     (invert-unit-series
		      (scale-stream S2 (/ 1 denom-const)))
		     denom-const)))))

;(define tan-series
;  (div-series sine-series
;	      cosine-series))

;(newline)
;(display (stream-ref factorials 1))

;;content
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
		 (stream-map 
		  (lambda (guess)
		    (sqrt-improve guess x))
		  guesses)))
  guesses)

;(display-stream (sqrt-stream 2))

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
	       (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
	(s1 (stream-ref s 1))
	(s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
			  (+ s0 (* -2 s1) s2)))
		 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stram s
	      (make-tableau transform
			    (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
	      (make-tableau transform s)))

;;exercise 3.63
;This version is of the same efficiency as the version of delay without memo-proc

;;exercise 3.64
(define (stream-limit s tolerance)
  (cond ((stream-null? s) '())
	((stream-null? (stream-cdr s)) (stream-car s))
	(else
	 (let ((s0 (stream-car s))
	       (s1 (stream-car (stream-cdr s))))
	   (if (< (abs (- s0 s1))
		  tolerance)
	       s1
	       (stream-limit (stream-cdr s)
			     tolerance))))))

(define (my-sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(newline)
(display (my-sqrt 2 0.1))
(newline)
(display (my-sqrt 2 0.0001))

;;exercise 3.65
(define (ln-summands n)
  (cons-stream (/ 1.0 n)
	       (stream-map - (ln-summands (+ n 1)))))

(define ln-stream
  (partial-sums (ln-summands 1)))

;;exercise 3.66
;197 2^99+2^98-2 2^100-2

;;exercise 3.67
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
		   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) 
		  (list (stream-car s) x))
		(stream-cdr t))
    (interleave
     (stream-map (lambda (x)
		   (list x (stream-car t)))
		   (stream-cdr s))
     (pairs (stream-cdr s) (stream-cdr t))))))

;;exercise 3.68
;infinite loop

;;exercise 3.69
(define (triples s t u)
  (cons-stream
   (list (stream-car s)
	 (stream-car t)
	 (stream-car u))
   (interleave
    (stream-map
     (lambda(x) (append (list (stream-car s)) x))
     (stream-cdr (pairs t u)))
    (triples (stream-cdr s)
	     (stream-cdr t)
	     (stream-cdr u)))))

;(define pythagorean-triples
;  (stream-filter (lambda (t)
;		   (= (+ (square (car t))
;			 (square (cadr t)))
;		      (square (caddr t))))
;		 (triples integers integers integers)))

;;exercise 3.70
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((car1 (stream-car s1))
	       (car2 (stream-car s2)))
	   (cond ((< (weight car1) (weight car2))
		  (cons-stream car1
			       (merge-weighted (stream-cdr s1)
					       s2 weight)))
		 ((> (weight car1) (weight car2))
		  (cons-stream car2 
			       (merge-weighted s1
					       (stream-cdr s2) weight)))
		 (else
		  (cons-stream car1
			       (merge-weighted (stream-cdr s1)
					       (stream-cdr s2)
					       weight))))))))
(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (weighted-pairs (stream-cdr s)
		    (stream-cdr t) weight)
    weight)))

(weighted-pairs integers integers
		(lambda (x)
		  (apply + x)))

(define (weight-2 i j)
  (+ (* 2 i)
     (* 3 j)
     (* 5 i j)))

(define (unfactored? x)
  (not (or (even? x)
	   (zero? (remainder x 3))
	   (zero? (remainder x 5)))))

(define unfactored-integers 
  (stream-filter unfactored? integers))

(weighted-pairs unfactored-integers
		unfactored-integers
		weight-2)

