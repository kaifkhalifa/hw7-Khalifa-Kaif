#lang racket

(require rackunit
         "hw7.rkt"  ;; Assuming the provided code is saved in hw7.rkt
         2htdp/image)

(provide TESTS)

;; Define test suite
(define TESTS
  (test-suite
   "HW7 test-suite"

   ;; Testing textbox-insert function
   (test-case
    "Testing textbox-insert"
    (check-equal? (textbox-insert (create-TextBox "ka" "if") "r")
                  (create-TextBox "kar" "if")))

   (test-case
    "Testing textbox-insert with empty post"
    (check-equal? (textbox-insert (create-TextBox "ka" "") "f")
                  (create-TextBox "kaf" "")))

   ;; Testing textbox-left function
   (test-case
    "Testing textbox-left with non-empty post"
    (check-equal? (textbox-left (create-TextBox "hell" "o world"))
                  (create-TextBox "hel" "lo world")))

   (test-case
    "Testing textbox-left with empty pre"
    (check-equal? (textbox-left (create-TextBox "" "world"))
                  (create-TextBox "" "world")))

   ;; Testing textbox-right function
   (test-case
    "Testing textbox-right with non-empty pre"
    (check-equal? (textbox-right (create-TextBox "hello" "world"))
                  (create-TextBox "hellow" "orld")))

   (test-case
    "Testing textbox-right with empty post"
    (check-equal? (textbox-right (create-TextBox "hello" ""))
                  (create-TextBox "hello" "")))

   ;; Testing remove-char function
   (test-case
    "Testing remove-char with non-empty post"
    (check-equal? (remove-char (create-TextBox "ka" "if"))
                  (create-TextBox "ka" "f")))

   (test-case
    "Testing remove-char with empty post"
    (check-equal? (remove-char (create-TextBox "ka" ""))
                  (create-TextBox "ka" "")))

   ;; Testing shift-char function
   (test-case
    "Testing shift-char"
    (check-equal? (shift-char (create-TextBox "ka" "if"))
                  (create-TextBox "kai" "f")))

   (test-case
    "Testing shift-char with non-empty pre and post"
    (check-equal? (shift-char (create-TextBox "xy" "z123")) (create-TextBox "xyz" "123")))


   ;; Testing key-handler function
   (test-case
    "Testing key-handler with backspace"
    (check-equal? (key-handler (create-TextBox "ka" "if") "backspace")
                  (textbox-backspace (create-TextBox "ka" "if"))))

   (test-case
    "Testing key-handler with delete"
    (check-equal? (key-handler (create-TextBox "ka" "if") "\u007F")
                  (textbox-delete (create-TextBox "ka" "if"))))
   
   ;; Testing render function
   (test-case
    "Testing render output"
    (check-true (image? (render (create-TextBox "hello" "world")))))

   ;; Testing main indirectly by verifying initial state
   (test-case
    "Testing main indirectly by verifying initial state"
    (check-true (image? (render (create-TextBox "" "")))))
   ))

;; Run tests
(module+ main
  (require rackunit/text-ui)
  (run-tests TESTS 'verbose))