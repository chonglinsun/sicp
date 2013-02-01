(define (add x y)
  (+ x y))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))
(define (sqrt x)
  (define (average x y)
    (/ (+ x y) 2))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (square x)
    (* x x))
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.0001))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
	guess
	(sqrt-iter (improve guess x)
		   x)))
  (sqrt-iter 1.0 x))

(define (exp x)
  (define (square x)
    (* x x))
  (define (div3 x y)
    (/ (+ x y) 3))
  (define (improve guess x)
    (div3 (/ x (square guess)) (* 2 guess)))
  (define (good-enough? guess x)
    (< (abs (- (* guess guess guess) x)) 0.0001))
  (define (exp-iter guess x)
    (if (good-enough? guess x)
	guess
	(exp-iter (improve guess x) x)))
  (exp-iter 1.0 x))

(define (fac n)
  (if (= n 1)
      1
      (* n (fac (- n 1)))))

(define (fac2 n)
  (define (fac-iter product count max-count)
    (if (> count max-count)
	product
	(fac-iter (* product count) 
		  (+ count 1) 
		  max-count)))
  (fac-iter 1 1 n))

(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 2))
		 (fib (- n 1))))))


(define (fib n)
  (define (fib-iter a b n)
    (if (= n 0)
	a
	(fib-iter (+ a b) 
		  a
		  (- n 1))))
  (fib-iter 1 0 n))

(define (pascal n m)
  (cond ((= m 0) 1)
	((= n m) 1)
	(else (+ (pascal (- n 1)
			 (- m 1))
		 (pascal (- n 1)
			 m)))))

(define (sine angle)
  (define (cube x) (* x x x))
  (define (p x) (- (* 3 x) (* 4 (cube x))))
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(define (expt b n)
  (if (= n 0)
      1
      (* b
	 (expt b (- n 1)))))

(define (expt b n)
  (define (expt-iter ans b n)
    (if (= n 0)
	ans
	(expt-iter (* ans b)
		   b
		   (- n 1))))
  (expt-iter 1 b n))

(define (fast-expt b n)
  (define (square x) (* x x))
  (define (even? x) 
    (= (remainder x 2) 0))
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))


(define (fast-expt b n)
  (define (even? n)
    (= (remainder n 2) 0))
  (define (square x)
    (* x x))
  (cond ((= n 0) 1)
	((even? n) (fast-expt (square b)
			      (/ n 2)))
	(else (* b
		 (fast-expt (square b)
			    (/ (- n 1) 2))))))

(define (multi a b)
  (if (= b 0)
      0
      (+ a (multi a (- b 1)))))

(define (multi a b)
  (define (even? n) 
    (= (remainder n 2) 0))
  (define (double n)
    (+ n n))
  (cond ((= b 0) 0)
	((even? b) (double (multi a (/ b 2))))
	(else (+ a (multi a (- b 1))))))

(define (multi a b)
  (define (even? n)
    (= (remainder n 2) 0))
  (define (double n)
    (* n 2))
  (define (multi-iter ans a b)
    (cond ((= b 0) ans)
	  ((even? b) (multi-iter ans (+ a a) (/ b 2)))
	  (else (multi-iter (+ ans a) a (- b 1)))))
  (multi-iter 0 a b))

(define (gcd a b)
  (if(= b 0)
     a
     (gcd b (remainder a b))))

(define (fast-prime? n times)
  (define (even? n) (= (remainder n 2) 0))
  (define (square n) (* n n))
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
	  ((even? exp) (remainder (square (expmod base (/ exp 2) m))
				  m))
	  (else (remainder (* base
			      (expmod base (- exp 1) m))
			   m))))
  (define (fermat-test n)
    (define (try-it a)
      (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))


(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (define (cube a) (* a a a))
  (sum cube a inc b))

(define (sum-integers a b)
  (define (identity x) x)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (cube x) (* x x x))
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define plus4 (lambda (x) (+ x 4)))


(define (f x y)
  ((lambda (a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (f x y)
  (let ((a (+ 1 (* x y)))
	(b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

(define (close-enough? x y) 
  (< (abs (- x y)) 0.001))

(define (average x y) (/ (+ x y) 2.0))

(define (search f neg-point pos-point)
  (let ((mid-point (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	mid-point
	(let ((test-value (f mid-point)))
	  (cond ((positive? test-value) 
		 (search f neg-point mid-point))
		((negative? test-value)
		 (search f mid-point pos-point))
		(else mid-point))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
	   (search f a b))
	  ((and (negative? b-value) (positive? a-value))
	   (search f b a))
	  (else 
	   (error "values are not of opposite sign" a b)))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
	  next
	  (try next))))
  (try first-guess))

(define (cal-sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))
 
(define (cont-frace n d k)
  (if (= k 1)
      (/ (n 1) (d 1))
      (/ (n k) (+ (d k) (cont-frace n d (- k 1))))))

(define (d i)
  (cond ((= i 1) 1)
	((= i 2) 2)
	((= (remainder (- i 2) 3) 0) (/ (* 2 (+ i 1)) 3))
	(else 1)))

(define (tan-cf x k)
  (cont-frace (lambda (i) (if (= i 1)
			     x
			     (- (* x x))))
	     (lambda (i) (- (* i 2) 1))
	     k))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
	       1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
	       1.0))

(define dx 0.00001)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x))
		 dx)))


(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y)) 
			    average-damp
			    1.0))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
			    newton-transform
			    1.0))

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))


(define (double f)
  (lambda (x) (f (f x))))

(((double (double double)) inc) 5)

(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)

(define (repeated f n)
  (lambda (x)
    (if (= n 0)
	x
	(f ((repeated f (- n 1)) x)))))

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3.0)))

(define (n-fold f n)
  (lambda (x) ((repeated f n) x)))

(define (repeat-times n)
  (floor (/ (log n) (log 2))))

(define (pow x n)
  (if(= n 0)
     1
     (* x (pow x (- n 1)))))

(define (n-root x n)
  (fixed-point ((repeated average-damp (repeat-times n))
		(lambda (y) (/ x (pow y (- n 1)))))
	       1.0))

(n-root 128 7)

(define (iterative-improve good-enough? improve-guess)
  (lambda (x)
    (let ((y (improve-guess x)))
      (if (good-enough? x y)
	  y
	  ((iterative-improve good-enough? improve-guess) y)))))

(define (sqrt x)
  (define (good-enough? x y) (< (abs (- x y)) 0.00001))
  (define (improve-guess y) (average y (/ x y)))
  ((iterative-improve good-enough? improve-guess) 1.0))

(sqrt 2.0)

(define (fixed-point f first-guess)
  ((iterative-improve good-enough? f) first-guess))


