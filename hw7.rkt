#lang racket

(provide (all-defined-out))

(require rackunit
         2htdp/image
         2htdp/universe)


(define FONT-SIZE 32)
(define FONT-COLOR "black")
(define CURSOR-WIDTH 2)
(define TEXTBOX-WIDTH 512)
(define TEXTBOX-HEIGHT 40)
(define INITIAL-SPACE 4)
(define CURSOR (rectangle CURSOR-WIDTH FONT-SIZE "solid" "black"))
(define TEXTBOX (rectangle TEXTBOX-WIDTH TEXTBOX-HEIGHT "outline" "black"))

;; A TextBox represents text input, split into two parts:
;; 'pre' contains characters before the cursor (left side).
;; 'post' contains characters after the cursor (right side).
(struct TextBox [pre post] #:transparent)

;; create-TextBox: string? string? -> TextBox?
;; Constructs a TextBox by combining the given 'pre' and 'post' strings.
(define/contract (create-TextBox pre post)
  (-> string? string? TextBox?)
  (TextBox pre post))

;; string-to-text: string -> image
;; Converts a string into an image for rendering in the textbox.
(define/contract (string-to-text str)
  (-> string? image?)
  (text str FONT-SIZE FONT-COLOR))
;; Example:
(check-equal? (string-to-text "kaif") (text "kaif" FONT-SIZE FONT-COLOR))

;; create-text-img: TextBox -> image
;; Creates an image representation of the TextBox, including the cursor and text.
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
(check-equal? (last-index "") 0)

;; first-index: string -> integer
;; Returns 1 if the string is non-empty, otherwise returns 0 (to represent the first index).
(define (first-index str)
  (min 1 (string-length str)))
;; Example:
(check-equal? (first-index "") 0)


;; text-center-x: image -> number
;; Computes the x-coordinate for centering an image horizontally inside the textbox.
(define/contract (text-center-x img)
  (-> image? number?)
  (+ INITIAL-SPACE (/ (image-width img) 2)))
;; Example:
(check-equal? (text-center-x (create-text-img (create-TextBox "abc" "def")))
              (+ INITIAL-SPACE (/ (image-width (create-text-img (create-TextBox "abc" "def"))) 2)))


;; Deletes the character immediately after the cursor.
(define/contract (textbox-delete tb)
  (-> TextBox? TextBox?)
  (match-define (TextBox pre post) tb)
  (create-TextBox pre (if (empty? post) "" (substring post (first-index post)))))
;; Example:
(check-equal? (textbox-delete (create-TextBox "abc" "def")) (create-TextBox "abc" "ef"))



;; textbox-backspace: TextBox -> TextBox
;; Deletes the character immediately before the cursor.
(define/contract (textbox-backspace tb)
  (-> TextBox? TextBox?)
  (match-define (TextBox pre post) tb)
  (create-TextBox (substring pre 0 (last-index pre)) post))
;; Example:
(check-equal? (textbox-backspace (create-TextBox "abc" "def")) (create-TextBox "ab" "def"))


;; textbox-insert: TextBox string -> TextBox
;; Inserts a given string at the cursor's position and moves the cursor to the right of the inserted string.
(define/contract (textbox-insert tb str)
  (-> TextBox? string? TextBox?)
  (match-define (TextBox pre post) tb)
  (create-TextBox (string-append pre str) post))
;; Example:
(check-equal? (textbox-insert (create-TextBox "ab" "cdef") "x") (create-TextBox "abx" "cdef"))


;; textbox-left: TextBox -> TextBox
;; Moves the cursor one character to the left by shifting the last character from 'pre' to 'post'.
(define/contract (textbox-left tb)
  (-> TextBox? TextBox?)
  (match-define (TextBox pre post) tb)
  (create-TextBox (substring pre 0 (last-index pre)) 
                  (string-append (substring pre (last-index pre)) post)))
;; Example:
(check-equal? (textbox-left (create-TextBox "abc" "def")) (create-TextBox "ab" "cdef"))


;; textbox-right: TextBox -> TextBox
;; Moves the cursor one character to the right by shifting the first character from 'post' to 'pre'.
(define/contract (textbox-right tb)
  (-> TextBox? TextBox?)
  (match-define (TextBox pre post) tb)
  (create-TextBox (string-append pre (substring post 0 (first-index post)))
                  (substring post (first-index post))))
;; Example:
(check-equal? (textbox-right (create-TextBox "abc" "def")) (create-TextBox "abcd" "ef"))


;; remove-char : TextBox -> TextBox
;; Removes the first character in the `post` (right) part of the TextBox, right after the cursor.
(define/contract (remove-char tb)
  (-> TextBox? TextBox?)
  (match-define (TextBox pre post) tb)
  (create-TextBox pre (substring post (first-index post))))
;; Example
(check-equal? (remove-char (create-TextBox "abc" "def")) (create-TextBox "abc" "ef"))


;; shift-char : TextBox -> TextBox
;; Moves the first character from `post` to `pre`.
(define/contract (shift-char tb)
  (-> TextBox? TextBox?)
  (match-define (TextBox pre post) tb)
  (if (empty? post)
      tb 
      (create-TextBox (string-append pre (substring post 0 1))
                      (substring post 1)))) 
;; Example
(check-equal? (shift-char (create-TextBox "abc" "def")) (create-TextBox "abcd" "ef"))


;; key-handler : TextBox String -> TextBox
;; Handles key inputs to modify the TextBox.
;; - "left" moves the cursor one character left.
;; - "right" moves the cursor one character right.
;; - "backspace" removes the character before the cursor.
;; - "delete" removes the character after the cursor.
;; - All other single characters insert the character at the cursor.
;; - "tab" and "return" should be ignored.
(define/contract (key-handler textbox key)
  (-> TextBox? string? TextBox?)
  (cond
    ;; Ignore the "return" and "tab" keys for Gradscope
    [(or (string=? key "\r")  
         (string=? key "\t"))  
     textbox]

    ;; Handle the "backspace" key (remove character before the cursor)
    [(or (string=? key "backspace")
         (string=? key "\b"))  ;; Backspace as "\b"
     (textbox-backspace textbox)]
    
    ;; Handle the "delete" key (remove character after the cursor)
    [(string=? key "\u007F")
     (textbox-delete textbox)]
    
    ;; Move the cursor left (pop from pre to post)
    [(string=? key "left")
     (textbox-left textbox)]
    
    ;; Move the cursor right (pop from post to pre)
    [(string=? key "right")
     (textbox-right textbox)]
    
    ;; Insert any other single character
    [(= (string-length key) 1)
     (textbox-insert textbox key)]
    
    ;; Ignore all other keys
    [else textbox]))


;; Example key-handler tests
(define textbox-example (create-TextBox "Hello" "World"))

;; Move cursor left
(check-equal? (key-handler textbox-example "left")
              (textbox-left textbox-example))

;; Move cursor right
(check-equal? (key-handler textbox-example "right")
              (textbox-right textbox-example))

;; Backspace
(check-equal? (key-handler textbox-example "backspace")
              (textbox-backspace textbox-example))

;; Delete
(check-equal? (key-handler textbox-example "\u007F")
              (textbox-delete textbox-example))

;; Insert character "!"
(check-equal? (key-handler textbox-example "!")
              (textbox-insert textbox-example "!"))


;; render: WorldState -> image
;; Renders the text and cursor in a textbox
(define/contract (render tb)
  (-> TextBox? image?)
  (place-image (create-text-img tb) (text-center-x (create-text-img tb)) (/ TEXTBOX-HEIGHT 2) TEXTBOX))
;; example:
(check-equal? (render (create-TextBox "abc" "def"))
              (place-image (create-text-img (create-TextBox "abc" "def"))
                           (text-center-x (create-text-img (create-TextBox "abc" "def")))
                           (/ TEXTBOX-HEIGHT 2) TEXTBOX))

;; main : -> void
;; Starts the interactive text editor with a blank TextBox.
(define (main)
  (big-bang (create-TextBox "" "")  
    (on-key key-handler)            
    (to-draw render)))            
