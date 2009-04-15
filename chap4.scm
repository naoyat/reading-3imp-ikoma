
(define (set-member? x s)
  (cond [(null? s) '()]
		[(eq? x (car s)) 't]
		[else (set-member? x (cdr s))]))

(define (set-cons x s)
  (if (set-member? x s)
	  s
	  (cons x s)))

(define (set-union s1 s2)
  (if (null? s1)
	  s2
	  (set-union (cdr s1) (set-cons (car s1) s2))))

(define (set-minus s1 s2)
  (if (null? s1)
	  '()
	  (if (set-member? (car s1) s2)
		  (set-minus (cdr s1) s2)
		  (cons (car s1) (set-minus (cdr s1) s2)))))

(define (set-intersect s1 s2)
  (if (null? s1)
	  '()
	  (if (set-member? (car s1) s2)
		  (cons (car s1) (set-intersect (cdr s1) s2))
		  (set-intersect (cdr s1) s2))))
