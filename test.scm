(use csv csv-string)
(load "genetic.scm")


(define datafile (with-input-from-file "data/TFdata20170315.csv" read-string))

(define parse (csv-parser #\,))

(define data
  (cdr (map csv-record->list (parse datafile))))

(define ECOICOP6s (map second data))

(define retailer-descriptions (map seventh data))

(define retailer-descriptions-words
  (map string-split retailer-descriptions))

(define word-bank
  (delete-duplicates
   (join retailer-descriptions-words)))

(define feature-lists
  (map (lambda (desc)
	 (map (lambda (word)
		(and (memq word desc) #t))
	      word-bank))
       retailer-descriptions-words))

(define (if-test test)
  (if test
      (lambda (x y) x)
      (lambda (x y) y)))

(define (equal-test var x)
  `(if-test (equal? ,var ,x)))

(define (random-equal-test elts var)
  (equal-test var (random-elt elts)))

(*operators* (lambda ()
	       (equal-test 'x "abc")))
