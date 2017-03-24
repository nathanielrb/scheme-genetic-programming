(use csv)
(load "genetic.scm")


(define datafile (with-input-from-file "data/TFdata20170315.csv" read-string))

(define parse (csv-parser #\,))

(define data (map csv-record->list (parse datafile)))
