;; Bottom-up MergeSort in Lisp
;; Function to merge two sorted lists into one sorted list
(defun merge-two-lists (list1 list2)
  "Merge two sorted lists into one sorted list."
  (cond ((null list1) list2)  ;; If first list is empty, return second list
        ((null list2) list1)  ;; If second list is empty, return first list
        ((<= (car list1) (car list2)) ;; Compare heads of both lists
         (cons (car list1) (merge-two-lists (cdr list1) list2)))
        (t (cons (car list2) (merge-two-lists list1 (cdr list2)))))) ;; Otherwise take from second list

;; Function to partition the input list into sorted pairs
(defun partition-into-pairs (lst)
  "Partition the input list into sorted pairs."
  (if (null lst)
      '() ;; Return empty list if input is empty
      (let ((first (car lst))
            (rest (cdr lst)))
        (if (null rest)
            (list (list first)) ;; Single element forms a single pair
            (cons (sort (list first (car rest)) #'<) ;; Sort each pair
                  (partition-into-pairs (cdr rest))))))) ;; Recursive call for remaining elements

;; Function to perform one pass of merging adjacent lists
(defun merge-pass (lists)
  "Perform one pass of merging adjacent lists."
  (if (or (null lists) (null (cdr lists))) ;; Base case: no adjacent pairs
      lists
      (cons (merge-two-lists (car lists) (cadr lists)) ;; Merge current pair
            (merge-pass (cddr lists))))) ;; Recur for the remaining lists

;; Main function for Bottom-up MergeSort implementation
(defun bottom-up-mergesort (lst)
  "Bottom-up MergeSort implementation."
  (let ((sublists (partition-into-pairs lst))) ;; Step 1: Partition into pairs
    (loop while (> (length sublists) 1) do    ;; Step 2: Merge until single sorted list remains
          (setq sublists (merge-pass sublists)))
    (car sublists))) ;; Return the final sorted list

;; Example usage and output
(print (bottom-up-mergesort '(1 7 2 1 8 6 5 3 7 9 4))) ;; 