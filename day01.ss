(import (rnrs (6)))

(define (read-input)
  (with-input-from-file "./input/day01.txt"
    (lambda ()
      (let loop ([lefts '()] [rights '()])
	(let* ([left (read)] [right (read)])
	  (if (eof-object? left)
	      (list (reverse lefts) (reverse rights))
	      (loop (cons left lefts) (cons right rights))))))))

(define (part1 lefts rights)
  (let ([lefts (list-sort < lefts)]
	[rights (list-sort < rights)])
    (fold-left (lambda (acc left right)
		 (+ acc (abs (- left right))))
	       0
	       lefts rights)))

(define (count x ls)
  (fold-left (lambda (acc y)
	       (if (= x y) (+ acc 1) acc))
	     0
	     ls))

(define (part2 lefts rights)
  (fold-left (lambda (acc x)
	       (+ acc
		  (* x (count x rights))))
	     0
	     lefts))

(define (solve)
  (let ([input (read-input)])
    (display (apply part1 input)) (newline)
    (display (apply part2 input)) (newline)))
