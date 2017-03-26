(use csv csv-string)
(load "genetic.scm")

(define math-operators
  (lambda ()
    (random-elt '(* + - /))))

(define *variables* '(x))

(define (leaves)
  (random 10))

(define (test)
  (let ((f1 (random-form math-operators *variables*))
	(f2  (random-form math-operators *variables*)))
    (print (cross-over f1 f2))))
  ;; (run-form (random-form (*operators*) (*variables*)) (*variables*) '(5)))

;; (fitness (car (initial-population (*operators*) (*variables*) 2)) (*variables*) (lambda (xl) (* (car xl) 10)) '((1) (2) (3) (4)))


(define (test-evaluate pop)
  (evaluate-population pop *variables*
		       (lambda (xl) (* (car xl) 10))
		      '((1) (2) (3))))

(define (test-advance pop)
  (advance-generation pop math-operators *variables*
		      (lambda (xl) (* (car xl) 10))
		      '((1) (2) (3))))

(define (test n)
  (run  (initial-population math-operators *variables* 100)
	math-operators *variables*
		      (lambda (xl) (* (+  (* 3 (car xl)) (car xl) (car xl))))
		      '((1) (2) (3)) n 10))

(define (test-form)
   (random-form math-operators *variables*))

(define (test-crossover n)
  (let recur ((n n)
	      (t1 (test-form))
	      (t2 (test-form)))
    (if (> n 0)
	(let ((crosses (cross-over t1 t2)))
	  (map print crosses) (newline)
	  (recur (- n 1) (car crosses) (cdr crosses)))
	'done)))

(define population (initial-population (*operators*) (*variables*) 100))
