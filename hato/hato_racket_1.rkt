#lang racket

(display "窓は閉めましたよね？ (y/n)\n")
(call/cc
 (lambda (break)
   (let f ()
     (let ((c (read-line)))
       (if (not (equal? c "y"))
           (display "窓を閉めてください．鳩が入ります．\n")
           (break))
       (f)))))
(display "お疲れ様でした．\n")