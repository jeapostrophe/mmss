(module read mzscheme
  (require (lib "list.ss"))
  (provide (all-defined))
  
  (define (read-line* port line-mode)
    (let loop ([lines empty])
      (let ([cur (read-line port line-mode)])
        (if (eof-object? cur)
            lines
            (loop (list* cur lines)))))))