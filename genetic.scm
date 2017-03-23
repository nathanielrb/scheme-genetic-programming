(use srfi-1)

(define *operators* (make-parameter '(* + - /)))

(define *variables* (make-parameter '(x)))

(define (random-elt seq)
  (when (not (null? seq))
	(list-ref seq (random (length seq)))))

;; TODO: write random-cond

(define-syntax random-if
  (syntax-rules ()
    ((random-if index conseq alt)
     (let ((r (random 10000)))
       (if (< (/ r 10000) index)
	   conseq
	   alt)))))

(define-syntax random-when
  (syntax-rules ()
    ((random-when index conseq)
     (let ((r (random 10000)))
       (when (< (/ r 10000) index)
	   conseq)))))

(define (random-form operators variables #!optional (max-depth 4))
  (let ((operand (lambda ()
		   (random-if (if (> max-depth 0) 1/2 0)
			      (random-form operators variables (- max-depth 1))
			      (random-if 1/2 (random 10)
					 (random-elt variables))))))
       (list (random-elt operators) (operand) (operand))))


;; do this with syntax rules?

(define (run-form form variables input)
  (handle-exceptions
   exn #f
   (eval `((lambda ,variables ,form) ,@input))))

(define (initial-population operators variables #!optional (size 100))
  (list-tabulate size (lambda (x) (random-form operators variables))))

(define (fitness form variables fitness-fn test-inputs)
  (reduce * 1
	  (map (lambda (input)
		 (let ((result (run-form form variables input)))
		   (if result
		       (/ 1 (+ 1 (abs (- result (fitness-fn input)))))
		       0)))
	       test-inputs)))

(define (random-node form)
  (if (list? form)
      (random-if 1/3  form
		 (random-if 1/2
			    (cadr form)
			    (caddr form)))
      form))

(define (replace-random-node form new-node)
  (if (list? form)
      (random-if 1/3  new-node
		 (random-if 1/2
			    (list (car form)
				  (replace-random-node (cadr form) new-node)
				  (caddr form))
			    (list (car form)
				  (cadr form)
				  (replace-random-node (caddr form) new-node))))
      (random-if 1/2 form new-node)))


(define (random-node-split form #!optional (cont (lambda (node) node)))
  (if (list? form)
      (random-if 1/3 (cons form cont)
		 (random-if 1/2
			    (random-node-split (cadr form) (lambda (node)
							    (cont
							     (list (car form)
								   node
								   (caddr form)))))

			    (random-node-split (caddr form) (lambda (node)
							     (cont
							      (list (car form)
								    (cadr form)
								    node))))))
      (cons form cont)))

(define (cross-over form1 form2)
  (let ((split1 (random-node-split form1))
	(split2 (random-node-split form2)))
    (list ((cdr split1) (car split2))
	  ((cdr split2) (car split1)))))

(define (mutate form operators variables)
  (let ((split (random-node-split form))
	(nform (random-form operators variables)))
    ((cdr split) nform)))

(define (evaluate-population population variables fitness-fn test-inputs)
  (sort
   (map (lambda (f) (cons (fitness f variables fitness-fn test-inputs)
			  f))
	population)
   (lambda (a b) (> (car a) (car b)))))

(define (advance-generation population operators variables fitness-fn test-inputs #!optional (max-population 10))
  (let ((epop (evaluate-population population variables fitness-fn test-inputs)))
    (fold (lambda (f accum)
	    (let ((fitness (car f))
		  (form (cdr f)))
	      (append
	       accum
	       (filter
		list?
		(list
		 form
		 (random-when fitness
			      (random-if 0.9
					 (cross-over form (cdr (random-elt epop)))
					 (mutate form operators variables)))
		 (random-when 0.02 (random-form operators variables)))))))
	  '()
	      
	 (take epop max-population))))


(define (test)
  (let ((f1 (random-form (*operators*) (*variables*)))
	(f2  (random-form (*operators*) (*variables*))))
    (print (cross-over f1 f2))))
  ;; (run-form (random-form (*operators*) (*variables*)) (*variables*) '(5)))

;; (fitness (car (initial-population (*operators*) (*variables*) 2)) (*variables*) (lambda (xl) (* (car xl) 10)) '((1) (2) (3) (4)))

(define (run population operators variables fitness-fn test-inputs n #!optional (max-population 10))
  (let recur ((n n)
	      (pop population))
    (if (> n 0)
	(recur (- n 1)
	       (advance-generation pop operators variables fitness-fn test-inputs))
	(evaluate-population pop variables fitness-fn test-inputs))))

(define population (initial-population (*operators*) (*variables*) 20))

(define (test-evaluate pop)
  (evaluate-population pop (*variables*)
		       (lambda (xl) (* (car xl) 10))
		      '((1) (2) (3))))

(define (test-advance pop)
  (advance-generation pop (*operators*) (*variables*)
		      (lambda (xl) (* (car xl) 10))
		      '((1) (2) (3))))

(define (test n)
 (run population (*operators*) (*variables*)
		      (lambda (xl) (* (car xl) 10))
		      '((1) (2) (3)) n 10))
