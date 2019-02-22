#lang racket

(define-syntax f
  (begin
    (display "窓は閉めましたよね？ (y/n)\n")
    (lambda (x)
      (syntax-case x (y)
        ((_) (let ((c (read))) #`(f #,c)))
        ((_ y) #'(display "お疲れ様でした．\n"))
        (_
         (begin
           (display "窓を閉めてください．鳩が入ります．\n")
           #'(f)))))))
(f)
