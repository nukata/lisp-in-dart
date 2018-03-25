;; quick sort for a sequence
(defun qsort (seq) (_qsort seq nil))

(defun _qsort (left-seq right-sorted)
  (if (null left-seq)
      right-sorted
    (let ((pivot (car left-seq))
          (tail (cdr left-seq)))
      (let ((pair (_split pivot tail)))
        (let ((left (car pair))
              (right (cons pivot
                           (_qsort (cadr pair) right-sorted))))
          (_qsort left right))))))

(defun _split (pivot seq)               ; => (left-seq right-seq)
  (if (null seq)
      (list nil nil)
    (let ((x (car seq))
          (tail-pair (_split pivot (cdr seq))))
      (if (< x pivot)
          (list (cons x (car tail-pair))
                (cadr tail-pair))
        (list (car tail-pair)
              (cons x (cadr tail-pair)))))))

(print (qsort '(3 1 4 1 5 9 2 6 5 3 5 8 9 7 9)))
;;  => (1 1 2 3 3 4 5 5 5 6 7 8 9 9 9)
