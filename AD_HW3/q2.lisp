;; Mergesort Implementation in Lisp

;; Function to split list into nearly equal halves
(defun partition-list (lst)
  (labels ((split (lst left right toggle)
             (cond ((null lst) (list (reverse left) (reverse right)))
                   (toggle (split (cdr lst) (cons (car lst) left) right nil))
                   (t (split (cdr lst) left (cons (car lst) right) t)))))
    (split lst '() '() t)))

;; Function to merge two sorted lists
(defun merge-lists (list1 list2)
  (cond ((null list1) list2)
        ((null list2) list1)
        ((<= (car list1) (car list2))
         (cons (car list1) (merge-lists (cdr list1) list2)))
        (t (cons (car list2) (merge-lists list1 (cdr list2))))))

;; Recursive Mergesort Function
(defun mergesort (lst)
  (if (or (null lst) (null (cdr lst)))
      lst
      (let ((halves (partition-list lst)))
        (merge-lists (mergesort (car halves)) (mergesort (cadr halves))))))

;; Example usage
(print (mergesort '(1 7 2 1 8 6 5 3 7 9 4))) ;;
