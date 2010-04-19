(module hash-table mzscheme
  (require (lib "list.ss"))
  (provide (all-defined))
  
  (define (hash-table-append! h k v)
    (hash-table-put! h k (list* v (hash-table-get h k (lambda () empty)))))
  (define (hash-table-get/set-default! h k default)
    (hash-table-get h k 
                    (lambda () 
                      (hash-table-put! h k default)
                      default)))
  (define (hash-table-get* h . ks)
    (foldl
     (lambda (k h)
       (hash-table-get h k (lambda () #f)))
     h
     ks))
  (define (hash-table->key-list h)
    (hash-table-map h (lambda (k v) k)))
  (define (hash-table->value-list h)
    (hash-table-map h (lambda (k v) v))))