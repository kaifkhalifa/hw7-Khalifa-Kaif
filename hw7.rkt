#lang racket

(provide (all-defined-out))

(require rackunit
         2htdp/image
         2htdp/universe)

;; Constants for rendering
(define FONT-SIZE 32)
(define FONT-COLOR "black")
(define CURSOR-WIDTH 2)
(define TEXTBOX-WIDTH 512)
(define TEXTBOX-HEIGHT 40)
(define INITIAL-SPACE 4)
(define CURSOR (rectangle CURSOR-WIDTH FONT-SIZE "solid" "black"))
(define TEXTBOX (rectangle TEXTBOX-WIDTH TEXTBOX-HEIGHT "outline" "black"))

;; A TextBox contains text split by a cursor:
;; 'pre' - characters before the cursor
;; 'post' - characters after the cursor
(struct TextBox [pre post] #:transparent)

;; create-TextBox: string? string? -> TextBox?
;; Creates a TextBox given 'pre' and 'post' strings.
(define/contract (create-TextBox pre post)
  (-> string? string? TextBox?)
  (TextBox pre post))

;; string-to-text: string -> image
;; Converts a string into an image for rendering.
(define/contract (string-to-text str)
  (-> string? image?)
  (text str FONT-SIZE FONT-COLOR))
;; Example:
(check-equal? (string-to-text "kaif") (text "kaif" FONT-SIZE FONT-COLOR))

;; create-text-img: TextBox -> image
;; Renders the text and cursor inside the textbox.
(define/contract (create-text-img tb)
  (-> TextBox? image?)
  (match-define (TextBox pre post) tb)
  (beside (string-to-text pre) CURSOR (string-to-text post)))
;; Example:
(check-equal? (create-text-img (create-TextBox "abc" "def"))
              (beside (string-to-text "abc") CURSOR (string-to-text "def")))

;; last-index: string -> integer
;; Returns the index of the last character in a string.
(define (last-index str)
  (max 0 (- (string-length str) 1)))
;; Example:
(check-equal? (last-index "abc") 2)

;; first-index: string -> integer
;; Returns 1 for non-empty strings, otherwise 0.
(define (first-index str)
  (min 1 (string-length str)))
;; Example:
(check-equal? (first-index "") 0)

;; text-center-x: image -> number
;; Computes the x-coordinate for centering an image in the textbox.
(define/contract (text-center-x img)
  (-> image? number?)
  (+ INITIAL-SPACE (/ (image-width img) 2)))
;; Example:
(check-equal? (text-center-x (create-text-img (create-TextBox "abc" "def")))
              (+ INITIAL-SPACE (/ (image-width (create-text-img (create-TextBox "abc" "def"))) 2)))

;; textbox-delete: TextBox -> TextBox
;; Deletes the character after the cursor.
(define/contract (textbox-delete tb)
  (-> TextBox? TextBox?)
  (match-define (TextBox pre post) tb)
  (create-TextBox pre (if (empty? post) "" (substring post (first-index post)))))
;; Example:
(check-equal? (textbox-delete (create-TextBox "abc" "def")) (create-TextBox "abc" "ef"))

;; textbox-backspace: TextBox -> TextBox
;; Deletes the character before the cursor.
(define/contract (textbox-backspace tb)
  (-> TextBox? TextBox?)
  (match-define (TextBox pre post) tb)
  (create-TextBox (substring pre 0 (last-index pre)) post))
;; Example:
(check-equal? (textbox-backspace (create-TextBox "abc" "def")) (create-TextBox "ab" "def"))

;; textbox-insert: TextBox string -> TextBox
;; Inserts a string at the cursor's position and shifts the cursor right.
(define/contract (textbox-insert tb str)
  (-> TextBox? string? TextBox?)
  (match-define (TextBox pre post) tb)
  (create-TextBox (string-append pre str) post))
;; Example:
(check-equal? (textbox-insert (create-TextBox "ab" "cdef") "x") (create-TextBox "abx" "cdef"))

;; textbox-left: TextBox -> TextBox
;; Moves the cursor one character left, shifting the last character of 'pre' to 'post'.
(define/contract (textbox-left tb)
  (-> TextBox? TextBox?)
  (match-define (TextBox pre post) tb)
  (create-TextBox (substring pre 0 (last-index pre))
                  (string-append (substring pre (last-index pre)) post)))
;; Example:
(check-equal? (textbox-left (create-TextBox "abc" "def")) (create-TextBox "ab" "cdef"))

;; textbox-right: TextBox -> TextBox
;; Moves the cursor one character right, shifting the first character of 'post' to 'pre'.
(define/contract (textbox-right tb)
  (-> TextBox? TextBox?)
  (match-define (TextBox pre post) tb)
  (create-TextBox (string-append pre (substring post 0 (first-index post)))
                  (substring post (first-index post))))
;; Example:
(check-equal? (textbox-right (create-TextBox "abc" "def")) (create-TextBox "abcd" "ef"))

;; remove-char: TextBox -> TextBox
;; Removes the first character in 'post' (after the cursor).
(define/contract (remove-char tb)
  (-> TextBox? TextBox?)
  (match-define (TextBox pre post) tb)
  (create-TextBox pre (substring post (first-index post))))
;; Example:
(check-equal? (remove-char (create-TextBox "abc" "def")) (create-TextBox "abc" "ef"))

;; shift-char: TextBox -> TextBox
;; Moves the first character from 'post' to 'pre'.
(define/contract (shift-char tb)
  (-> TextBox? TextBox?)
  (match-define (TextBox pre post) tb)
  (if (empty? post)
      tb
      (create-TextBox (string-append pre (substring post 0 1))
                      (substring post 1))))
;; Example:
(check-equal? (shift-char (create-TextBox "abc" "def")) (create-TextBox "abcd" "ef"))

;; key-handler: TextBox string -> TextBox
;; Modifies the TextBox based on key input.
;; - Moves the cursor left with "left" key.
;; - Moves the cursor right with "right" key.
;; - Removes characters with "backspace" and "delete".
;; - Inserts characters otherwise.
(define/contract (key-handler textbox key)
  (-> TextBox? string? TextBox?)
  (cond
    [(or (string=? key "\r") (string=? key "\t")) textbox] 
    [(or (string=? key "backspace") (string=? key "\b")) (textbox-backspace textbox)] 
    [(string=? key "\u007F") (textbox-delete textbox)]  
    [(string=? key "left") (textbox-left textbox)] 
    [(string=? key "right") (textbox-right textbox)] 
    [(= (string-length key) 1) (textbox-insert textbox key)]  
    [else textbox])) 

;; Example key-handler tests
(define textbox-example (create-TextBox "Hello" "World"))

(check-equal? (key-handler textbox-example "left")
              (textbox-left textbox-example))
(check-equal? (key-handler textbox-example "right")
              (textbox-right textbox-example))
(check-equal? (key-handler textbox-example "backspace")
              (textbox-backspace textbox-example))
(check-equal? (key-handler textbox-example "\u007F")
              (textbox-delete textbox-example))
(check-equal? (key-handler textbox-example "!")
              (textbox-insert textbox-example "!"))

;; render: TextBox -> image
;; Renders the text and cursor in the textbox.
(define/contract (render tb)
  (-> TextBox? image?)
  (place-image (create-text-img tb)
               (text-center-x (create-text-img tb))
               (/ TEXTBOX-HEIGHT 2) TEXTBOX))
;; Example:
(check-equal? (render (create-TextBox "abc" "def"))
              (place-image (create-text-img (create-TextBox "abc" "def"))
                           (text-center-x (create-text-img (create-TextBox "abc" "def")))
                           (/ TEXTBOX-HEIGHT 2) TEXTBOX))

;; main: -> void
;; Starts the interactive text editor with a blank TextBox.
(define (main)
  (big-bang (create-TextBox "" "")
    (on-key key-handler)
    (to-draw render)))

