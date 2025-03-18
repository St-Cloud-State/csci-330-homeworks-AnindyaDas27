;; test.lisp
;; This file loads the parser and tests it with both valid and invalid input strings.

(load "parser.lisp")

(format t "~%Valid Strings (5-20 characters):~%")

;; Each input is a list of symbols representing a string of 5–20 characters that should match the grammar
(defparameter *valid-strings*
  '((i x s e s)                    ; I → i E S e S
    (i x o y s e s)               ; E → E o G
    (i x o y o z s e s)           ; E → E o G o G
    (i x o y s e d s b)           ; S → d L b
    (i x o y o z s e d s s b)     ; complex L
    (i x o y o z o w s e s)       ; E → E o G o G o G
    (i x o y o z o w o x s e s))) ; longest valid string (~13 chars)

;; Run parser on all valid test strings
(dolist (s *valid-strings*)
  (format t "~A: ~A~%" s (if (match-pattern s) "VALID ✅" "INVALID ❌")))

(format t "~%Invalid Strings (5-20 characters):~%")

;; Invalid strings between 5-20 characters (due to grammar violations)
(defparameter *invalid-strings*
  '((i x x x x)                   ; Invalid Gs
    (i x o y o a s e s)          ; 'a' is not allowed in grammar
    (i x s e e)                  ; S followed by invalid S
    (i x o y o s s)              ; G not followed by valid S
    (i x o y s e d b)            ; S → d b (missing L)
    (i x o y o z o s d s b)      ; 'd' misplaced
    (x i o y s e s)))            ; Invalid starting symbol

;; Run parser on all invalid test strings
(dolist (s *invalid-strings*)
  (format t "~A: ~A~%" s (if (match-pattern s) "VALID ✅" "INVALID ❌")))
