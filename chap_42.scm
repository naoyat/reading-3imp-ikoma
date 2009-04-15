;;
;; Chapter 4.2 - Stack Allocating the Dynamic Chain
;;
(require "./3imp")

(require "./chap_35") ;closure
(require "./chap_41")

; p.83
(define (continuation s)
  (closure
   (list 'refer 0 0 (list 'nuate (save-stack s) '(return)))
   '()))

(define (save-stack s)
  (let ([v (make-vector s)])
	(recur copy ([i 0])
		   (unless (= i s)
			 (vector-set! v i (vector-ref stack i))
			 (copy (+ i 1))))
	v))

(define (restore-stack v)
  (let ([s (vector-length v)])
	(recur copy ([i 0])
		   (unless (= i s)
			 (vector-set! stack i (vector-ref v i))
			 (copy (+ i 1))))
	s))

(define lookup compile-lookup)
(define (VM a x e r s)
  (record-case x
			   [halt () a]
			   [refer (n m x)
					  (VM (car (lookup n m e)) x e r s)]
			   [constant (obj x)
						 (VM obj x e r s)]
			   [close (body x)
					  (VM (closure body e) x e r s)]
			   [test (then else)
					 (VM a (if a then else) e r s)]
			   [assign (n m x)
					   (set-car! (lookup n m e) a)
					   (VM a x e r s)]
			   [conti (x)
					  (VM (continuation s) x e r s)]
			   [nuate (stack x)
					  (VM a x e r (restore-stack stack))]
			   [frame (ret x)
					  (VM a x e '() (push ret (push e (push r s))))]
			   [argument (x)
						 (VM a x e (cons a r) s)]
			   [apply ()
					  (record (body e) a
							  (VM a body (extend* e r) '() s))]
			   [return ()
					   (VM a (index s 0) (index s 1) (index s 2) (- s 3))]))
