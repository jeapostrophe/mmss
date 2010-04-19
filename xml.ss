(module xml mzscheme
  (require (lib "xml.ss" "xml")
           (lib "file.ss"))
  (provide (all-defined))
  
  (define (write-xml! path Xexpr)
    (empty-tag-shorthand html-empty-tags)
    (make-directory* (path-only path))
    (with-output-to-file path
      (lambda ()
        (write-xml/content (xexpr->xml Xexpr)))
      'replace)))