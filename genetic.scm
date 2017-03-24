(use srfi-1)

(define *operators* (make-parameter
		     (lambda ()
		       (random-elt '(* + - /)))))

(define math-operators
  (lambda ()
    (random-elt '(* + - /))))

(define *variables* (make-parameter '(x)))

(define *leaves* (make-parameter
		  (lambda ()
		    (random 10))))

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

(define (random-form operators variables #!optional (leaves (*leaves*)) (max-depth 10))
  (let ((operand (lambda ()
		   (random-if (if (> max-depth 0) 1/2 0)
			      (random-form operators variables leaves (- max-depth 1))
			      (random-if (if (> (length variables) 0)
					     1/2
					     1)
					 (leaves)
					 (random-elt variables))))))
       (list (operators) (operand) (operand))))


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

(define (random-node-split form #!optional (cont (lambda (node) node)))
  (handle-exceptions
   exn (begin (print "AN ERROR")  (format #t "ERROR RANDOM-NODE-SPLIT: ~%~A~%~A~%" exn form) exn)

  (if (list? form)
      (random-if 1/3 (cons form cont)
		 (random-if 1/2
			    (random-node-split
			     (cadr form)
			     (lambda (node)
			       (cont
				(list (car form)
				      node
				      (caddr form)))))

			    (random-node-split
			     (caddr form)
			     (lambda (node)
			       (cont
				(list (car form)
				      (cadr form)
				      node))))))
      (cons form cont))))

(define (cross-over form1 form2)
    (handle-exceptions
     exn (begin
	   (format #t "ERROR CROSS-OVER: ~%~A~%~A" form1 form2)
	   (display
	    ((condition-property-accessor 'exn 'message) exn))
	   exn)

  (let ((split1 (random-node-split form1))
	(split2 (random-node-split form2)))
    (cons ((cdr split1) (car split2))
	  ((cdr split2) (car split1))))))

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

(define (advance-generation population operators variables fitness-fn test-inputs
			    #!optional (max-population 100))
  (let ((epop (evaluate-population population variables fitness-fn test-inputs)))
    (format #t "BEST: ~A~%" (car (car epop)))
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

(define population (initial-population (*operators*) (*variables*) 100))

(define (test-evaluate pop)
  (evaluate-population pop (*variables*)
		       (lambda (xl) (* (car xl) 10))
		      '((1) (2) (3))))

(define (test-advance pop)
  (advance-generation pop (*operators*) (*variables*)
		      (lambda (xl) (* (car xl) 10))
		      '((1) (2) (3))))

(define (test n)
  (run  (initial-population (*operators*) (*variables*) 100)
	(*operators*) (*variables*)
		      (lambda (xl) (* (+  (* 3 (car xl)) (car xl) (car xl))))
		      '((1) (2) (3)) n 10))

(define (test-form)
   (random-form (*operators*) (*variables*)))

(define (test-crossover n)
  (let recur ((n n)
	      (t1 (test-form))
	      (t2 (test-form)))
    (if (> n 0)
	(let ((crosses (cross-over t1 t2)))
	  (map print crosses) (newline)
	  (recur (- n 1) (car crosses) (cdr crosses)))
	'done)))
