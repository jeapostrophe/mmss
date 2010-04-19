(module path mzscheme
  (require (lib "file.ss")
           (lib "etc.ss")
           (lib "list.ss"))
  (require "list.ss")
  (provide (all-defined))
  
  (define (directory-from-path p)
    (apply build-path
           (reverse
            (rest
             (reverse
              (explode-path p))))))
  
  (define (path->subpaths p)
    (all-prefixes (explode-path p)))
  
  (define (chop-prefix r p)
    (apply build-path (list-tail (explode-path (normalize-path p r))
                                 (length (explode-path (normalize-path r (build-path "/"))))))))