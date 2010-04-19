(module string mzscheme
  (provide (all-defined))
  
  (define (write/string a)
    (let ([p (open-output-string)])
      (write a p)
      (get-output-string p)))
  (define (read/string s)
    (read (open-input-string s))))