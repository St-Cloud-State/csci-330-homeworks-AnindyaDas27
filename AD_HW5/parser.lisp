;; parser.lisp
;; This file defines a recursive descent parser for the following grammar:
;; I → i E S | i E S e S
;; E → E o G | G
;; G → x | y | z | w
;; S → s | d L b
;; L → s | s L

;; Top-level function to check if input string matches the grammar (starts from rule I)
(defun parse-I (input)
  ;; Skip 'i', then parse E and S
  (multiple-value-bind (success rest1) (parse-E (cdr input)) ; parse E after 'i'
    (when success
      (multiple-value-bind (success2 rest2) (parse-S rest1) ; parse S
        (if success2
            ;; Check optional branch: 'e S'
            (if (and rest2 (equal (car rest2) 'e))
                (multiple-value-bind (success3 rest3) (parse-S (cdr rest2))
                  (if success3
                      (values t rest3) ; matched I → i E S e S
                      (values nil input))) ; failed second S
                (values t rest2)) ; matched I → i E S
            (values nil input))))))

;; Function to parse rule E → E o G | G
(defun parse-E (input)
  (multiple-value-bind (success rest1) (parse-G input) ; try E → G
    (if success
        (if (and rest1 (equal (car rest1) 'o)) ; if there's an 'o', try E → E o G
            (multiple-value-bind (success2 rest2) (parse-E (cdr rest1))
              (if success2
                  (values t rest2)
                  (values t rest1))) ; accept E → G
            (values t rest1)) ; accept E → G if no 'o'
        (values nil input))))

;; Function to parse G → x | y | z | w
(defun parse-G (input)
  (if (and input (member (car input) '(x y z w))) ; check if first symbol is G
      (values t (cdr input))
      (values nil input)))

;; Function to parse S → s | d L b
(defun parse-S (input)
  (cond
    ((and input (equal (car input) 's)) ; S → s
     (values t (cdr input)))
    ((and input (equal (car input) 'd)) ; S → d L b
     (multiple-value-bind (success rest1) (parse-L (cdr input))
       (if (and success rest1 (equal (car rest1) 'b))
           (values t (cdr rest1))
           (values nil input))))
    (t (values nil input))))

;; Function to parse L → s | s L (recursive rule)
(defun parse-L (input)
  (if (and input (equal (car input) 's))
      ;; Try to parse next L recursively
      (multiple-value-bind (success rest1) (parse-L (cdr input))
        (if success
            (values t rest1)
            (values t (cdr input)))) ; only one 's' accepted as well
      (values nil input)))

;; Main interface function: returns T if string is fully matched, NIL otherwise
(defun match-pattern (input)
  (multiple-value-bind (success rest) (parse-I input)
    (and success (null rest)))) ; make sure nothing is left unparsed
