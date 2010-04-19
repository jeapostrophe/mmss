(module sort mzscheme
  (require (prefix srfi:1: (lib "1.ss" "srfi"))
           (lib "list.ss"))
  (provide stable-quicksort
           stable-mergesort
           unstable->stable)
  
  (define (unstable->stable sort)
    (lambda (l compare)
      (map car (sort (map cons l (srfi:1:iota (length l)))
                     (lambda (pair1 pair2)
                       (let ((x1 (car pair1)) (x2 (car pair2)))
                         (or (compare x1 x2)
                             (and (not (compare x2 x1))
                                  (< (cdr pair1) (cdr pair2))))))))))
  
  (define stable-quicksort (unstable->stable quicksort))
  (define stable-mergesort (unstable->stable mergesort)))