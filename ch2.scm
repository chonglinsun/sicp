(define (gcd a b)
  (if (= b 0)
      a
      (gcd b 
	   (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g)
	  (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (numer y) (denom x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;;Test the above code

(define one-half (make-rat 1 2))

(print-rat one-half)

(define one-third (make-rat 1 3))

(print-rat (add-rat one-half one-third))

(print-rat (mul-rat one-half one-third))

(print-rat (add-rat one-third one-third))


;;interfaces for segment

(define (average x y) (/ (+ x y) 2))
(define (make-point x y) (cons x y))
(define (make-segment x y) (make-point x y))
(define (start-segment x) (car x))
(define (end-segment x) (cdr x))
(define (x-point x) (car x))
(define (y-point x) (cdr x))
(define (midpoint-segment sg)
  (let ((px (start-segment sg))
	(py (end-segment sg)))
    (make-rat (average (x-point px) (x-point py))
	      (average(y-point px) (y-point py)))))
    
;;test the above line

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (print-segment sg)
  (newline)
  (print-point (start-segment sg))
  (display "--->")
  (print-point (end-segment sg)))

(define px (make-point 1 2))
(define py (make-point 3 4))
(define sg (make-segment px py))
(print-segment sg)
(display (average 1 2))
(print-point (midpoint-segment sg))

;;exercise 2.4
(define (myCons x y)
  (lambda (m) (m x y)))

(define (myCar z)
  (z (lambda (p q) p)))

(define (myCdr z)
  (z (lambda (p q) q)))

(define ab (myCons 1 2))
(newline)
(display "ab: ")
(display "myCar: ");
(display (myCar ab))
(newline)
(display "myCdr: ");
(display (myCdr ab))
(newline)


;;exercise 2.5
(define (expt a n)
  (if (= n 0)
      1
      (* a (expt a (- n 1)))))

(define (powCons a b)
  (* (expt 2 a) (expt 3 b)))

(newline)
(display "expt: ")
(display (expt 2 3))
(newline)

(define (powCar x)
  (discrete-log x 2))

(define (powCdr x)
  (discrete-log x 3))

(define (discrete-log n base)
  (if (not 
       (= 0 (remainder n base)))
      0
      (+ 1 (discrete-log (/ n base) base))))

(define powConsTest (powCons 2 3))
(newline)
(display "powCons: ")
(display powConsTest)
(newline)
(display "powCar: ")
(display (powCar powConsTest))
(newline)
(display "powCdr: ")
(display (powCdr powConsTest))
(newline)

;;exercise 2.6

(define zero (lambda (f) (lambda(x) x)))

(define one (lambda (f) (lambda(x) (f x))))

(define two (lambda (f) (lambda(x) (f (f x)))))


;;exercise 2.7
(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))

(define (lower-bound x) (car x))

(define (display-interval x)
  (newline)
  (display "[")
  (display (lower-bound x))
  (display ", ")
  (display (upper-bound x))
  (display "]")
  (newline))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

(define a (make-interval 5 10))
(define b (make-interval 10 20))
(display-interval (add-interval a b))
(display-interval (mul-interval a b))
(display-interval (div-interval a b))

;;exercise 2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))

(display-interval (sub-interval a b))

;;exercise 2.9

;;width([x, y]) = (/ (+ x y))

;;exercise 2.10

(define (spans-zero? y)
  (and (<= (lower-bound y) 0)
       (>= (upper-bound y) 0)))

(define (div-interval x y)
  (if (spans-zero? y)
      (error "Error: The denominator should not span 0.")
      (mul-interval x
		    (make-interval (1.0 / (upper-bound x))
				   (1.0 / (lower-bound y))))))
(define aa (make-interval 2 5))
(define bb (make-interval -2 2))
;;(display-interval (div-interval aa bb))

;;exercise 2.11X
(define (mul-interval x y)
  (let ((lx (lower-bound x))
	(rx (upper-bound x))
	(ly (lower-bound y))
	(ry (upper-bound y)))
    (cond ((and (>= lx 0)
		(>= rx 0)
		(>= ly 0)
		(>= ry 0))
	   (make-interval (* lx ly) (* rx ry)))
	  ((and (< lx 0)
		(>= rx 0)
		(>= ly 0)
		(>= ry 0))
	   (make-interval (* lx ry) (* rx ry)))
	  ((and (< lx 0)
		(< rx 0)
		(>= ly 0)
		(>= ry 0))
	   (make-interval (* lx ry) (* rx ry)))
	  ((and (>= lx 0)
		(>= rx 0)
		(< ly 0)
		(>= ry 0))
	   (make-interval (* rx ly) (* rx ry)))
	  ((and (< lx 0)
		(>= rx 0)
		(< ly 0)
		(>= ry 0))
	   (make-interval (min (* rx ly) (* lx ry))
			  (max (* lx ly) (* rx ry))))
	  ((and (< lx 0)
		(< rx 0)
		(< ly 0)
		(>= ry 0))
	   (make-interval (* lx ry) (* lx ly)))
	  ((and (>= lx 0)
		(>= rx 0)
		(< ly 0)
		(< ry 0))
	   (make-interval (* ly rx) (* rx ly)))
	  ((and (< lx 0)
		(>= rx 0)
		(< ly 0)
		(< ry 0))
	   (make-interval (*rx ly) (* lx ly)))
	  ((and (< lx 0)
		(< rx 0)
		(< ly 0)
		(< ry 0))
	   (make-interval (* rx ry) (* lx ly)))))) 

(display-interval (mul-interval aa bb))

;;exercise 2.11

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center x)
  (/ (+ (lower-bound x) (upper-bound x)) 2.0))

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))

;;exercise 2.12
(define (make-center-percent c p)
  (make-interval (- c (* c p)) (+ c (* c p))))

(define (percent x)
  (/ (/ (- (upper-bound x) (lower-bound x)) 2.0) (center x)))

(define cc (make-center-percent 5 0.1))
(display-interval cc)
(display (percent cc))
(newline)


;;exercise 2.13
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))

;;exercise 2.14

;;exercise 2.15

;;exercise 2.16

;;list operation
(define one-through-four (list 1 2 3 4))
(define odds (list 1 3 5 7))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items)
		(- n 1))))

(display (list-ref odds 2))         

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(display (length one-through-four))

(define (length items)
  (define (length-iter a count)
    (if (null? a)
	count
	(length-iter (cdr a)
		     (+ count 1))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
	    (append (cdr list1)
		    list2))))

(append one-through-four odds)
(append odds one-through-four)
 
;;exercise 2.17


(define (last-pair x)
  (if (null? (cdr x))
      (car x)
      (last-pair (cdr x))))

(newline)
(display (last-pair (list 23 72 149 34)))

;;exercise 2.18
(define (my-reverse items)
  (define (my-reverse-iter x y)
    (if (null? x)
	y
	(my-reverse-iter (cdr x) 
			 (cons (car x) y))))
  (my-reverse-iter items (list)))
(newline)
(display (my-reverse (list 1 4 9 16 25)))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else
	 (+ (cc amount 
		(except-first-denomination coin-values))
	    (cc (- amount
		   (first-denomination coin-values))
		coin-values)))))
(define (first-denomination coin-values)
  (car coin-values))
(define (except-first-denomination coin-values)
  (cdr coin-values))
(define (no-more? coin-values)
  (null? coin-values))

(newline)
(display (cc 100 us-coins))

;;exercise 2.20
(define (same-parity x . y)
  (define (filter rest)
    (cond ((null? rest) '())
	  ((= (remainder x 2) (remainder (car rest) 2))
	   (cons (car rest) (filter (cdr rest))))
	  (else (filter (cdr rest)))))
  (filter (cons x y)))

(newline)
(display (same-parity 2 3 4 5 6 7))
(newline)
(display (same-parity 1 2 3 4 5 6 7))

;;content
(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
	    (scale-list (cdr items) factor))))

(newline)
(display (scale-list (list 1 2 3 4 5) 10))

(define (my-map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
	    (my-map proc (cdr items)))))

(newline)
(display (my-map abs (list -10 2.5 -11.6 17)))

(my-map (lambda (x) (* x x)) (list 1 2 3 4 5))

(define (scale-list items factor)
  (my-map (lambda(x) (* x factor)) 
	  items))

;;exercise 2.21
(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items) (car items))
	    (square-list (cdr items)))))

(newline)
(display (square-list (list 1 2 3 4 5)))

(define (my-square-list items)
  (map (lambda (x) (* x x)) items))

(newline)
(display (my-square-list (list 1 2 3 4 5)))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons (square (car things))
		    answer))))
  (iter items '()))
(newline)
(display (square-list (list 1 2 3 4 5)))

;;exercise 2.23
(define (my-for-each proc items)
  (cond ((not (null? items))
	 (proc (car items))
	 (my-for-each proc (cdr items)))))

(my-for-each (lambda (x) (newline) (display x))
	     (list 57 321 88))

;;content
(define x (cons (list 1 2) (list 3 4)))
(newline)
(display (length x))

(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))

(newline)
(display (count-leaves x))

;;exercise 2.24

;;exercise 2.25
(define l1 (list 1 3 (list 5 7) 9))
(define l2 (list (list 7)))
(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(newline)
(display (car (cdr (car (cdr (cdr l1))))))
(newline)
(display (car (car l2)))
(newline)
(display (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3)))))))))))))

;;exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(newline)
(display (append x y))
(newline)
(display (cons x y))
(newline)
(display (list x y))

;;exercise 2.27
(define x (list (list 1 2) (list 3 4)))
(newline)
(display (reverse x))

(define nil '())

(define (my-reverse items)
  (if (null? items)
      nil
      (append (my-reverse (cdr items)) (cons (car items) nil))))

(define (deep-reverse items)
  (cond ((null? items) nil)
	((not (pair? items)) items)
	(else (append (deep-reverse (cdr items))
		      (list (deep-reverse (car items)))))))
(newline)
(display (my-reverse x))
(newline)
(display (deep-reverse x))

;;exercise 2.28
(define x (list (list 1 2) (list 3 4)))
(define (fringe items)
  (cond ((null? items) nil)
	((not (pair? items)) (list items))
	(else (append (fringe (car items))
		      (fringe (cdr items))))))

(newline)
(display (fringe x))
(newline)
(display (fringe (list x x)))

;;exercise 2.29 pending
(newline)
(display "exercise 2.29")
(newline)

(define (make-mobile left right)
  (list left right))

(define (left-branch mobile)
  (if (null? mobile)
      '()
      (car mobile)))

(define (right-branch mobile)
  (if (null? mobile)
      '()
      (car (cdr mobile))))


(define (branch-length branch)
  (if (null? branch)
      0
      (car branch)))

(define (branch-structure branch)
  (if (null? branch)
      0
      (car (cdr branch))))

(define (make-branch length structure)
  (list length structure))

(define m (make-mobile (make-branch 1 2) (make-branch 3 4)))

(define (total-weight mobile)
  (define (weight? x)
    (not (pair? x)))
  (define (weight-of-branch branch)
    (let ((struct (branch-structure branch)))
      (cond ((null? struct) 0)
	    ((weight? struct) struct)
	    (else (+ (weight-of-branch (left-branch struct))
		     (weight-of-branch (right-branch struct)))))))
  (if (null? mobile) 
      0
      (+ (weight-of-branch (left-branch mobile))
	 (weight-of-branch (right-branch mobile)))))
(display (total-weight m))

(newline)

(define (torque-balance? mobile)
  (define (weight? x)
    (not (pair? x)))
  (define (torque branch)
    (let ((struct (branch-structure branch)))
      (* (branch-length branch)
	 (cond ((null? struct) 0)
	       ((weight? struct) struct)
	       (else (+ (torque (left-branch struct))
			(torque (right-branch struct))))))))
  (if (null? mobile)
      #t
      (= (torque (left-branch mobile))
	 (torque (right-branch mobile)))))

(define m (make-mobile (make-branch 1 2) (make-branch 2 1)))
(define n (make-mobile (make-branch 1 3) (make-branch 2 4)))

(display (torque-balance? m))
(newline)
(display (torque-balance? n))
;;content
(define (scale-tree tree factor)
  (cond ((null? tree) nil)
	((not (pair? tree)) (* tree factor))
	(else (cons (scale-tree (car tree) factor)
		    (scale-tree (cdr tree) factor)))))

(define (scale-tree2 tree factor)
  (map (lambda(sub-tree) 
	 (if (pair? sub-tree)
	     (scale-tree2 sub-tree factor)
	     (* sub-tree factor))) tree))
(newline)
(display (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10))
(newline)
(display (scale-tree2 (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10))

;;exercise 2.30
(define (square-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (* tree tree))
	(else (cons (square-tree (car tree))
		    (square-tree (cdr tree))))))

(define (square-tree2 tree)
  (map (lambda(sub-tree)
	 (if (pair? sub-tree)
	     (square-tree sub-tree)
	     (* sub-tree sub-tree)))
       tree))

(newline)
(display
	(square-tree
	 (list 1
	       (list 2 (list 3 4) 5)
	       (list 6 7))))
(newline)
(display
 (square-tree2
  (list 1
	(list 2 (list 3 4) 5)
	(list 6 7))))

;;exercise 2.31
(define (tree-map proc tree)
  (map
   (lambda (sub-tree)
     (if (pair? sub-tree)
	 (tree-map proc sub-tree)
	 (proc sub-tree))) 
   tree))
(define (square-tree3 tree)
  (tree-map (lambda (x) (* x x)) tree))

(newline)
(display
(square-tree3 
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7))))

;;exercise 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
	(append rest (map 
		      (lambda (x) 
			(cons (car s)
			      x))
		      rest)))))

(newline)
(display (subsets (list 1 2 3)))

;;contents
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	 (else (filter predicate (cdr sequence)))))

(newline)
(display (filter odd? (list 1 2 3 4 5)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))
(newline)
(display (accumulate + 0 (list 1 2 3 4 5)))
(newline)
(display (accumulate * 1 (list 1 2 3 4 5)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low 
	    (enumerate-interval (+ low 1) high))))
(newline)
(display (enumerate-interval 2 7))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))

(newline)
(display (enumerate-tree (list 1 (list 2 (list 3 4)) 5)))

(define (sum-odd-sequares tree)
  (accumulate +
	      0
	      (map square
		   (filter odd?
			   (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
	      nil
	      (filter even?
		      (map fib 
			   (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons
	      nil
	      (map square
		   (filter fib 
			   (enumerate-interval 0 n)))))

(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
	      1
	      (map square
		   (filter odd? sequence))))
(newline)
(display (product-of-squares-of-odd-elements (list 1 2 3 4 5)))


(define (salary-of-highes-paid-programmer records)
  (accumulate max
	      0
	      (map salary
		   (filter programmer? records))))

;;exercise 2.33
(define (my-map p sequence)
  (accumulate (lambda (x y) 
		(cons (p x)
		      y))
	      nil
	      sequence))
(newline)
(display (my-map square (list 1 2 3 4 5)))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (my-length sequence)
  (accumulate (lambda (x y)
		(+ y 1))
	      0
	      sequence))
(newline)
(display (my-length (list 1 2 3 4 5)))

;;exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ (* higher-terms x)
		   this-coeff))
	      0
	      coefficient-sequence))
(newline)
(display (horner-eval 2 (list 1 3 0 5 0 1)))

;;exercise 2.35
(define (my-count-leaves t)
  (accumulate +
	      0
	      (map (lambda (node)
		     (if (pair? node)
			 (my-count-leaves node)
			 1))
		   t)))
(newline)
(display (my-count-leaves (list 1 (list 2 (list 3 (list 4 (list 5)))))))

;;exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init
			(map car
			     seqs))
	    (accumulate-n op init 
			  (map cdr
			       seqs)))))
(newline)
(display (accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12))))

;;exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(newline)
(display (dot-product (list 1 2 3) (list 4 5 6)))

(define (matrix-*-vector m v)
  (map (lambda (x)
	 (dot-product x v))
       m))
(define (transpose mat)
  (accumulate-n cons
		nil
		mat))
(newline)
(display (transpose (list (list 1 2 3) (list 4 5 6) (list 7 8 9))))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x)
	   (matrix-*-vector cols x))
	 m)))
(newline)
(display (matrix-*-matrix 
	  (list (list 1 2) (list 1 2))
	  (list (list 1 2) (list 1 2))))

;;exercise 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))
(define fold-right accumulate)
(newline)
(display (fold-left / 1 (list 1 2 3)))
(newline)
(display (fold-right / 1 (list 1 2 3)))
(newline)
(display (fold-left list nil (list 1 2 3)))
(newline)
(display (fold-right list nil (list 1 2 3)))

;;exercise 2.39
(define (my-reverse sequence)
  (fold-left (lambda (x y)
		(cons y x))
	      nil
	      sequence))
(display (my-reverse (list 1 2 3)))

(define (my-reverse2 sequence)
  (fold-right (lambda (x y)
		(display y)
		(append y (list x)))
	      nil
	      sequence))
(newline)
(display (my-reverse2 (list 1 2 3)))
(enumerate-interval 1 10)


;;contents
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
	  sequence))
(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
		 (map (lambda(p) (cons x p))
		      (permutations (remove x s))))
	       s)))
(newline)
(display (permutations (list 1 2 3)))

;;exercise 2.40
(define (unique-pairs n)
  (flatmap (lambda(i)
	 (map (lambda (j)
		(cons j i))
	      (enumerate-interval 1 (- i 1))))
       (enumerate-interval 2 n)))
(newline)
(display (unique-pairs 10))

;;exercise 2.41
(define (triple-sum s n)
  (define (sum-to-s? triple)
    (= s (+ (car triple) (cadr triple) (caddr triple))))
  (define (unique-triples n)
    (flatmap (lambda(i)
	       (flatmap (lambda(j)
			  (map (lambda (k) 
				 (list i j k))
			       (enumerate-interval 1 (- j 1))))
			  (enumerate-interval 1 (- i 1))))
	       (enumerate-interval 1 n)))
  (filter sum-to-s? (unique-triples n)))

(newline)
(display (triple-sum 10 6))

;;exercise 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions)
	   (safe? k  positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board nil)
(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))
(define (safe? k positions)
  (if (< (length positions) 2)
      #t
      (let ((new (list-ref positions (- k 1)))
	    (first (car positions)))
	(cond ((= new first) #f)
	      ((= new (+ first (- (length positions) 1)))
	       #f)
	      ((= new (- first (- (length positions) 1)))
	       #f)
	      (else (safe? (- k 1) (cdr positions)))))))

(newline)
(display (length (queens 8)))

(define testList (list (list 1 2) (list 3 4) (list 5 6)))
(map (lambda (x) 
       (map (lambda (y)
	      (append x (list y)))
	    (enumerate-interval 1 5)))
     testList)
(flatmap (lambda (x)
	   (map (lambda (y)
		  (append x (list y)))
		(enumerate-interval 1 5)))
	 testList)

;;exercise 2.43
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions)
	   (safe? k  positions))
	 (flatmap
	  (lambda (new-row)
	    (map (lambda (rest-of-queens)
		   (adjoin-position new-row k rest-of-queens))
		 (queen-cols (- k 1))))
	  (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

;;content
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(newline)
(display (fact 10))

(define (memq item x)
  (cond ((null? x) false)
	((eq? item (car x)) x)
	(else (memq item (cdr x)))))

(newline)
(display (memq 'apple '(pear banana prune)))
(newline)
(display (memq 'apple '(x (apple sauce) y apple pear)))

;;exercise 2.53
(newline)
(display (list 'a 'b 'c))
(newline)
(display (list (list 'george)))
(newline)
(display (cdr '((x1 x2) (y1 y2))))
(newline)
(display (pair? (car '(a short list))))
(newline)
(display (memq 'red '((red shoes) (blue socks))))
(newline)
(display (memq 'red '(red shoes blue socks)))

;;exercise 2.54
(define (equal? a b)
  (cond 
   ((and (pair? a)
	 (pair? b))
    (and (equal? (car a) (car b))
	 (equal? (cdr a) (cdr b))))
   ((eq? a b) #t)
   (else #f)))

(newline)
(display (equal? '(this is a list) '(this is a list)))
(newline)
(display (equal? '(this is a list) '(this (is a) list)))

;;exercise 2.55
(newline)
(display (car ''abcdefg))

;;content
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (list '+ a1 a2))

(define (make-product m1 m2)
  (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	(else 
	 (error "unknown expression type -- DERIV" exp))))

(newline)
(display (deriv '(+ x 3) 'x))
(newline)
(display (deriv '(* x y) 'x))
(newline)
(display (deriv '(* (* x y) (+ x 3)) 'x))

;;exercise 2.56
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x) (cadr x))

(define (exponent x) (caddr x))

;;(load "~/Learn/scheme/prac/test.scm")

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
	((=number? e 1) b)
	((=number? b 1) 1)
	((and (number? b) (number? e)) (fast-expt b e))
	(else (list '** b e))))
(newline)
(display (exponentiation? (list '** 1 2)))
(newline)
(display (make-exponentiation 'b 'e))

(newline)
(display (deriv '(+ x 3) 'x))
;;sicp exercise 2.57
(define (binary-expression? e)
  (null? (cdddr e)))

(define (second-term e)
  (caddr e))

(define (all-but-first-term e)
  (cddr e))

(define (augend s)
  (if (binary-expression? s)
      (second-term s)
      (cons '+ (all-but-first-term s))))

(define (multiplicand p)
  (if (binary-expression? p)
      (second-term p)
      (cons '* (all-but-first-term p))))

(define (reduce-expression e op)
  (if (binary-expression? e)
      (second-term e)
      (cons op (all-but-first-term e))))

(define (augend s) 
  (reduce-expression s '+))

(define (multiplicand p)
  (reduce-expression p '*))

(newline)
(display (multiplicand '(* a b c d)))
(newline)
(display (augend '(+ a b c d)))
(newline)
(display (deriv '(+ x 3) 'x))
(newline)
(display (deriv '(+ x 3 4 (* 2 x 3)) 'x))

;;exercise 2.58
(define (make-sum a1 a2)
  (list a1 '+ a2))

(define (make-product a1 a2)
  (list a1 '* a2))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (addend s)
  (car s))

(define (augend s)
  (caddr s))

(define (multiplier p)
  (car p))

(define (multiplicand p)
  (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list m1 '* m2))))

(newline)
(display (deriv '(x * (3 * (x + (y + 2)))) 'x))

(define (simplify exp)
  (if (null? (cdr exp))
      (car exp)
      exp))

(define (augend s)
  (simplify (cddr s)))

(define (multiplicand p) 
  (simplify (cddr p)))

(newline)
(display (deriv '(x + 3 * (x + y + 2 + x)) 'x))
(newline)
(display (deriv '(x + 3 * (x + y + 2 + x)) 'y))

;;content
(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))
(newline)
(display (element-of-set? 3 '(1 2 3)))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
(newline)
(display (adjoin-set 4 '(1 2 3)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))
(newline)
(display (intersection-set '(1 2 3) '(1 3 4)))

;;exercise 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((element-of-set? (car set1) set2)
	 (union-set (cdr set1) set2))
	(else (cons (car set1)
		    (union-set (cdr set1) set2)))))
(newline)
(display (union-set '(1 2 3) '(1 3 4)))

;;exercise 2.60
(define (union-set set1 set2)
  (append set1 set2))

(newline)
(display (union-set '(1 2 3) '(2 3 4)))
(newline)
(display (intersection-set '(1 1 2 3) '(1 1 2 4 5 6)))

;;content
(define (element-of-set? x set)
  (cond ((null? set) false)
	((= x (car set)) true)
	((> x (car set)) false)
	(else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1
		     (intersection-set (cdr set1)
				       (cdr set2))))
	      ((< x1 x2)
	       (intersection-set (cdr set1) set2))
	      ((> x2 x1)
	       (intersection-set set1 (cdr set2)))))))
(newline)
(display (intersection-set '(1 2 3) '(1 3 4)))

;;exercise 2.61
(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((> x (car set))
	 (cons (car set)
	       (adjoin-set x (cdr set))))
	((= x (car set))
	 set)
	(else (cons x set))))
(newline)
(display (adjoin-set 3 '(1 2 4)))
(newline)
(display (adjoin-set 5 '(1 2 4)))

;;exercise 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else
	 (let ((x1 (car set1)) (x2 (car set2)))
	   (cond ((< x1 x2) (cons x1
				  (union-set (cdr set1)
					     set2)))
		 ((= x2 x2) (cons x1
				  (union-set (cdr set1)
					     (cdr set2))))
		 (else (cons x2 
			     (union-set (cdr set1)
					(cdr set2)))))))))

(newline)
(display (union-set '(1 2 3 4) '(1 3 4 6)))

;;content
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
	((= x (entry set)) true)
	((< x (entry set))
	 (element-of-set? x (left-branch set)))
	((> x (entry set))
	 (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set x (right-branch set))))))

(newline)
(define test-tree (make-tree 2 (make-tree 1 '() '()) (make-tree 5 '() '())))
(display test-tree)
(newline)
(display (element-of-set? 5 test-tree))
(newline)
(display (adjoin-set 4 test-tree))

;;exercise 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))

(define tree1 (list 7 (list 3 (list 1 '() '()) (list 5 '() '())) (list 9 '() (list 11 '() '()))))
(define tree2 (list 3 (list 1 '() '()) (list 7 (list 5 '() '()) (list 9 '() (list 11 '() '())))))
(define tree3 (make-tree 5
			 (make-tree 3
				    (make-tree 1 '() '())
				    '())
			 (make-tree 9
				    (make-tree 7 '() '())
				    (make-tree 11 '() '()))))
(newline)
(display tree1) 
(newline)
(display tree2)
 (newline)
(display tree3) 
(newline)
(display (tree->list-1 tree1))
(newline)
(display (tree->list-2 tree1))
(newline)
(display (tree->list-1 tree2))
(newline)
(display (tree->list-2 tree2))
(newline)
(display (tree->list-1 tree3))
(newline)
(display (tree->list-2 tree3))

;;exercise 2.64
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree (cdr non-left-elts)
					      right-size)))
	      (let ((right-tree (car right-result))
		    (remain-elts (cdr right-result)))
		(cons (make-tree this-entry left-tree right-tree)
		      remain-elts))))))))

(newline)
(display (length (list 1 3 5 7 9 11)))
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(newline)
(display (list->tree (list 1 3 5 7 9 11)))

;;exercise 2.65
(define (union-set-tree set1 set2)
  (let ((list1 (tree->list-2 set1))
	(list2 (tree->list-2 set2)))
    (list->tree (union-set list1 list2))))

(define (intersection-set-tree set1 set2)
  (let ((list1 (tree->list-2 set1))
	 (list2 (tree->list-2 set2)))
    (list->tree (union-set list1 list2))))

;;exercise 2.66
(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      #f
      (let ((entry-key (key (entry set-of-records))))
	(cond ((= given-key entry-key) (entry set-of-records))
	      ((< given-key entry-key)
	       (lookup given-key (left-branch set-of-records)))
	      ((> given-key entry-key)
	       (lookup given-key (right-branch set-of-records)))))))

;;content
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch
	       (choose-branch (car bits)
			      current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)
			       (cadr pair))
		    (make-leaf-set (cdr pairs))))))

;;exercise 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(newline)
(display (decode sample-message sample-tree))

;;exercise 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((not (memq symbol (symbols tree)))
	 (error "bad symbol -- ENCODE-SYMBOL" symbol))
	 ((leaf? tree) '())
	 ((memq symbol (symbols (left-branch tree)))
	  (cons 0 (encode-symbol symbol (left-branch tree))))
	 ((memq symbol (symbols (right-branch tree)))
	  (cons 1 (encode-symbol symbol (right-branch tree))))))
(newline)
(display (encode '(A D A B B C A) sample-tree))

;;exercise 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-sets)
  (if (null? (cdr leaf-sets))
	     (car leaf-sets)
	     (successive-merge 
	      (adjoin-set (make-code-tree 
			    (car leaf-sets)
			    (cadr leaf-sets))
			   (cddr leaf-sets)))))

(newline)
(display sample-tree)
(newline)
(display (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))

;;exercise 2.70
(define song-tree
  (generate-huffman-tree
   '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1))))

(newline)
(display (length
	  (encode '(get a job
			sha na na na na na na na na
			get a job
			sha na na na na na na na na
			wah yip yip yip yip yip yip yip yip yip
			sha boom) song-tree)))
;;exercise 2.71
;;1, n - 1

;;exercise 2.72
;;(1), O(n^2)

;;content
(define (real-part z) (car z))
(define (imag-part z) (cdr z))

(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag x y)
  (cons x y))

(define (make-from-mag-ang r a)
  (cons (* r (cons a)) (* r (sin a))))

(define (add-complex z1 z2)
  (make-from-real-imgg (+ (real-part z1) (real-part z2))
		       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-read-imag (- (real-part z1) (real-part z2))
		       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		     (- (angle z1) (angle z2))))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag contents)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagger datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
	   (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
	(real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular (cons (* r (cos a))) (cons (* r (sin a)))))

(define (real-part-polar z)
  (* (magitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z) (car z))

(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
	      (cons (sqrt (+ (square x) (square y)))
		    (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar
	      (cons r a)))

(define (real-part z)
  (cond ((rectangular? z)
	 (real-part-rectangular (contents z)))
	((polar? z)
	 (real-part-polar (contents z)))
	(else (error "Unknown type -- REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
	 (imag-part-rectangular (contents z)))
	((polar? z)
	 (imag-part-polar (contents z)))
	(else (error "Unknown type -- IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
	 (magnitude-rectangular (contents z)))
	((polar? z)
	 (magnitude-polar (contents z)))
	(else (error "Unknown type -- MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
	 (angle-rectangular (contents z)))
	((polar? z)
	 (angle-polar (contents z)))
	(else (error "Unknown type -- ANGLE" z))))

(define (add-complex z1 z2)
  (make-from-real-img (+ (real-part z1) (real-part z2))
		      (+ (imag-part z1) (imag-part z2))))


(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-rectangular r a))

(define (install-rectangular-package)
  ;;internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
	     (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;;interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-img x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
'done)

(define (install-polar-package)
  ;;internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-read-imag x y)
    (cons (sqrt (+ (square x) (square y)))
	  (atan y x)))
  
  ;;interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(ploar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error 
	   "No method for these types -- APPLY-GENERIC"
	   (list op type tags))))))

;;exercise 2.73
;;a
;;number?, same-variable? are predicates. there's nothing to dispatch.
;;b
(define (deriv-sum exp var)
  (make-sum (deriv (addend exp) var)
	    (deriv (augend exp) var)))

(define (deriv-product exp var)
  (make-sum (deriv (multiplicand exp) var)
	    (deriv (multiplier exp) var)))

(define (install-deriv)
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  'done)
;;c
(define (deriv-exponentiation exp var)
  (let ((b (base exp))
	(e (exponent exp)))
    (make-product e
		  (make-product (make-exponentiation b (make-sum e -1))
				(deriv b var)))))
(define (install-exponentiation-extension)
  (put 'deriv '** deriv-exponentiation)
  'done)

;;d
(define (install-deriv-changes)
  (put '** 'deriv deriv-exponentiation)
  (put '+ 'deriv deriv-sum)
  (put '* 'deriv deriv-product)
  'done)

;;exercise 2.74
(define (make-hq-file division file)
  (cons division file))

(define (file-division hq-file)
  (car hq-file))

(define (original-file hq-file)
  (cdr hq-file))

(define (get-record employee hq-file)
  ((get 'get-record (file-division hq-file))
   employee (original-file hq-file)))

(define (has-record? employee division)
  ((get 'has-record? division) employee))

;;b
(define (make-hq-record division record)
  (cons division record))

(define (record-division record)
  (car record))

(define (original-record record)
  (cdr record))

(define (get-salary record)
  ((get 'get-salary (record-division record))
   (original-record record)))

;;c
(define (find-employee-record employee files)
  (cond ((null? file) (error "FIND-EMPLOYEE-RECORD : No sunch employee." employee))
	((has-record? employee (file-division (car files)))
	 (get-record employee (car files)))
	(else (find-employee-record employee
				    (cdr files)))))


  