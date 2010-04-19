(module xexpr mzscheme
  (require (lib "plt-match.ss"))
  (provide (all-defined))
  
  (define (string->xexpr/replace-entities s)
    (cond 
      [(regexp-match "^(.*?)&([^&;]+);(.*?)$" s)
       => (match-lambda
            [(list _ left entity right)
             (append (string->xexpr/replace-entities left)
                     (list (string->symbol entity))
                     (string->xexpr/replace-entities right))])]
      [else
       (list s)])))