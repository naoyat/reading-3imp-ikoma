(use gauche.test)
(test-start "4.6")

(require "./chap_46")

(define (evaluate x)
  (VM '() (compile x '() '() '(halt)) 0 '() 0))

(test-section "evaluate some expressions")
;(test* "symbol?" 2 (evaluate 'b)); '(((a b c) . (1 2 3)))))

;(test* "quote" 'hello (evaluate '(quote hello)))
;(test* "lambda" 3 (evaluate '((lambda (x) x) 3)))
;(test* "if" 5 (evaluate '(if #t 5 0)))
(test* "set!" (if #f #f) (evaluate '(set! t 7)))
;(test* "set!" 7 (evaluate '((lambda (t) ((lambda (x) t) (set! t 7))) 0)))
;(test* "call/cc" 11 (evaluate '(((call/cc (lambda (c) c)) (lambda (x) x)) 11)))
;(test* "pair:else?" 13 (evaluate '((lambda (f x) (f x)) (lambda (x) x) 13)))
;(test* "else?" 17 (evaluate 17))

(test-end)
