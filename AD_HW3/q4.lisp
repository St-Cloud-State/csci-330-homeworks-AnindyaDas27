;; Function to insert an element into a sorted list
(defun insert-sorted (item sorted-list)
  (if (or (null sorted-list) (<= item (car sorted-list)))
      (cons item sorted-list)   ; Insert at the beginning
      (cons (car sorted-list)   ; Keep first element and insert recursively
            (insert-sorted item (cdr sorted-list)))))

;; Recursive insertion sort function
(defun insertion-sort (unsorted-list)
  (if (null unsorted-list)
      '()                         ; Return empty when no items are left
      (insert-sorted (car unsorted-list) ; Insert first item in sorted list
                     (insertion-sort (cdr unsorted-list))))) ; Recur for the rest

;; Example usage
(print (insertion-sort '(5 2 9 1 5 6))) ; Output: (1 2 5 5 6 9)
