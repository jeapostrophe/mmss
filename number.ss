(module number mzscheme
  (require (lib "contract.ss"))
  (provide/contract
   [string->number* (string? . -> . number?)])
  
  (define string->number* string->number))