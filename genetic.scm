(use srfi-1)

(define (random-elt seq)
  (when (not (null? seq))
	(list-ref seq (random (length seq)))))

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

(define (random-form operators leaves #!optional (max-depth 20))
  (let ((operand (lambda ()
		   (random-if (if (> max-depth 0) 1/2 0)
			      (random-form operators leaves (- max-depth 1))
			      (leaves)))))
       (list (operators) (operand) (operand))))


;; do this with syntax rules?


(define-syntax build-form
  (syntax-rules ()
    ((build-form (var ...) body)
     (lambda (var ...) body))))

(define-syntax run-form
  (syntax-rules ()
    ((run-form form vars input)
     (handle-exceptions
      exn (begin (print ((condition-property-accessor 'exn 'message) exn)) #f)
     ((eval `(build-form ,vars ,form)) input)))))

(define (old-run-form form variables input)
  (handle-exceptions
   exn (begin (print ((condition-property-accessor 'exn 'message) exn)) #f)
   (eval `((lambda ,variables ,form) ,@input))))

(define (initial-population operators leaves #!optional (size 100))
  (list-tabulate size (lambda (x) (random-form operators leaves))))

(define (fitness form variables fitness-fn test-inputs compare)
  (reduce * 1
	  (map (lambda (input)
		 (let ((result (run-form form variables input)))
		   (if result
		       (compare result (fitness-fn input))
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

(define (mutate form operators variables leaves)
  (let ((split (random-node-split form))
	(nform (random-form operators leaves)))
    ((cdr split) nform)))

(define (evaluate-population population variables fitness-fn test-inputs
			     #!optional (compare (lambda (a b) (/ 1 (+ 1 (abs (- a b)))))))
  (sort
   (map (lambda (f) (cons (fitness f variables fitness-fn test-inputs compare)
			  f))
	population)
   (lambda (a b) (> (car a) (car b)))))

(define (advance-generation population operators variables leaves fitness-fn test-inputs
			    #!optional (max-population 100) compare)

  (let ((epop (evaluate-population population variables fitness-fn test-inputs compare)))
    (format #t "BEST of ~A: ~A~%" (length epop) (car (car epop)))
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
					 (mutate form operators variables leaves)))
		 (random-when 0.02 (random-form operators leaves)))))))
	  '()
	      
	 (take epop max-population))))

(define (run population operators variables leaves fitness-fn test-inputs n
	     #!optional (max-population 100) compare)
  (let recur ((n n)
	      (pop population))
    (if (> n 0)
	(recur (- n 1)
	       (advance-generation pop operators variables leaves fitness-fn test-inputs max-population compare))
	(evaluate-population pop variables fitness-fn test-inputs compare))))
