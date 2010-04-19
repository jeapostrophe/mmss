(module date mzscheme
  (require (lib "time.ss" "srfi" "19")
           (prefix srfi:19: (lib "19.ss" "srfi"))
           (lib "etc.ss")           
           (prefix srfi:1: (lib "1.ss" "srfi")))
  (require "list.ss")
  (provide (all-defined))
  
  ;; date operations
  (define (date->midnight-date d)
    (srfi:19:make-date 0 0 0 0
                       (srfi:19:date-day d) (srfi:19:date-month d)
                       (srfi:19:date-year d) (srfi:19:date-zone-offset d)))
  (define (date->day-since-epoch d)
    (round (/ (srfi:19:time-second (srfi:19:date->time-utc d))
              (* 60 60 24))))
  (define (day-since-epoch->date dn)
    (srfi:19:time-utc->date (srfi:19:make-time srfi:19:time-utc 0 (* dn 60 60 24))))
  
  (define (srfi:19:op op d0 d1)
    (op (srfi:19:date->time-utc (date->midnight-date d0))
        (srfi:19:date->time-utc (date->midnight-date d1))))
  (define (srfi:19:today? d)
    (srfi:19:op srfi:19:time=? d (srfi:19:current-date)))
  (define (srfi:19:after-today? d)
    (srfi:19:op srfi:19:time>? d (srfi:19:current-date)))
  
  (define (srfi:19:date->seconds d)
    (srfi:19:time-second (srfi:19:date->time-utc d)))
  (define (srfi:19:seconds->date s)
    (srfi:19:time-utc->date (srfi:19:make-time srfi:19:time-utc 0 s)))
  (define (time-minus-seconds t s)
    (srfi:19:make-time (srfi:19:time-type t)
                       0
                       (- (srfi:19:time-second t) s)))
  (define (compute-prev-days x)
    (let* ([current-date/midnight (date->midnight-date (srfi:19:current-date))])
      (build-list x
                  (lambda (i) (srfi:19:time-utc->date 
                               (time-minus-seconds (srfi:19:date->time-utc current-date/midnight)
                                                   (* +1 60 60 24
                                                      (- x (add1 i)))))))))
  (define (compute-next-days x)
    (let* ([current-date/midnight (date->midnight-date (srfi:19:current-date))])
      (reverse
       (build-list x
                   (lambda (i) (srfi:19:time-utc->date 
                                (time-minus-seconds (srfi:19:date->time-utc current-date/midnight)
                                                    (* -1 60 60 24 
                                                       (- x i)))))))))
  
  (define (month-name/number m)
    (date->string (srfi:make-date 0 0 0 0 0 m 0 0) "~B"))
  
  (define (generate-calendar year month)
    (let ([days-in-month (list-ref
                          (if (or (zero? (remainder year 400))
                                  (and (zero? (remainder year 4))
                                       (not (zero? (remainder year 100)))))
                              '(0 31 29 31 30 31 30 31 31 30 31 30 31)
                              '(0 31 28 31 30 31 30 31 31 30 31 30 31))
                          month)]
          [offset (date-week-day (make-date 0 0 0 0 0 1 month year 0 0))])
      (slices (append (build-list offset (lambda (x) #f)) (srfi:1:iota days-in-month 1)) 7 #t #f))))