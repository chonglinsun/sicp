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

;;exercise 3.24
