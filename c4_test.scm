(use gauche.test)
(test-start "Chapter 4")

(require "./c4")

(define-syntax test-eval*
  (syntax-rules ()
	[(_ title-str sexp => expected)
	 (begin (display #\.) (flush)
			(test* title-str expected (evaluate sexp)))]
	[(_ title-str sexp env => expected)
	 (begin (display #\.) (flush)
			(test* title-str expected (evaluate sexp env)))]
	[(_ . rest)
	 (begin (display #\?) (flush))]
	))

;(debug :evaluate)
;(debug :compile)
;(debug :vm)
;(debug :trace)

(test-section "evaluate some expressions")
;(test-eval* "symbol?"     'b '(((a b c) . (1 2 3))) => 2)
(test-eval* "quote"       '(quote hello) => 'hello)
(test-eval* "lambda"      '((lambda (x) x) 3) => 3)
(test-eval* "if"          '(if #t 5 55) => 5)
(test-eval* "if"          '(if #f 5 55) => 55)
;(test-eval* "set!"        '(set! t 7) '((t) . (0)) => (if #f #f))
(test-eval* "set!"        '((lambda (t) ((lambda (x) t) (set! t 7))) 0) => 7)
(test-eval* "call/cc"     '(((call/cc (lambda (c) c)) (lambda (x) x)) 11) => 11)
(test-eval* "pair:else?"  '((lambda (f x) (f x)) (lambda (x) x) 13) => 13)
(test-eval* "else?"       '17 => 17)

(test-end)
