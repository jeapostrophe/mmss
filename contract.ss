(module contract mzscheme
  (require (lib "contract.ss"))
  (provide (all-defined))
  
  ; pair/c : s flat-contract -> flat-contract
  ; returns a contract that recognizes a pair containing the symbol and an element matching the contract
  (define (pair/c s fc)
    (cons/c (symbols s) fc)))