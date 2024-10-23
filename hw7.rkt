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

;; A TextBox is a (create-TextBox [right: list] [left: list])
;; where
;; pre: represents the list of characters before the cursor
;; post: represents the list of characters after the cursor
(struct TextBox [pre post] #:transparent)
(define/contract (create-TextBox pre post)
  (-> string? string? TextBox?)
  (TextBox pre post))

;; string-to-text: string -> image
;; takes a list and converts it to text to go in the textbox
(define/contract (string-to-text str)
  (-> string? image?)
  (text str FONT-SIZE FONT-COLOR))
;; example:
(check-equal? (string-to-text "hi") (text "hi" FONT-SIZE FONT-COLOR))

;; create-text-img: TextBox -> TextBox
;; creates the image that enpsulates the text and cursor which goes inside of the textbox
(define/contract (create-text-img tb)
  (-> TextBox? image?)
  (match-define (TextBox pre post) tb)
  (beside (string-to-text pre) CURSOR (string-to-text post)))
;; example:
(check-equal? (create-text-img (create-TextBox "hel" "lo"))
              (beside (string-to-text "hel") CURSOR (string-to-text "lo")))

;; text-center-x: image -> number
;; calculates the x-coordinate for the center of an image on the textbox
(define/contract (text-center-x img)
  (-> image? number?)
  (+ INITIAL-SPACE (/ (image-width img) 2)))
;; example:
(check-equal? (text-center-x (create-text-img (create-TextBox "h" "i")))
              (+ INITIAL-SPACE (/ (image-width (create-text-img (create-TextBox "h" "i"))) 2)))

;; render: WorldState -> image
;; Renders the text and cursor in a textbox
(define/contract (render tb)
  (-> TextBox? image?)
  (place-image (create-text-img tb) (text-center-x (create-text-img tb)) (/ TEXTBOX-HEIGHT 2) TEXTBOX))
;; example:
(check-equal? (render (create-TextBox "hel" "lo"))
              (place-image (create-text-img (create-TextBox "hel" "lo"))
                           (text-center-x (create-text-img (create-TextBox "hel" "lo")))
                           (/ TEXTBOX-HEIGHT 2) TEXTBOX))

;; last-index: string -> integer
;; helper function to get the last index of a string 
(define (last-index str)
  (max 0 (- (string-length str) 1)))

;; first-index: string -> string
;; helper function to get the first index of a string 
(define (first-index str)
  (min 1 (string-length str)))

;; textbox-delete
;; removes the character right after the cursor
(define/contract (textbox-delete tb)
  (-> TextBox? TextBox?)
  (match-define (TextBox pre post) tb)
  (create-TextBox pre (substring post (first-index post))))
;; example
(check-equal? (textbox-delete (create-TextBox "hel" "lo")) (create-TextBox "hel" "o"))


;; textbox-backspace: TextBox -> TextBox
;; removes the character right before the cursor
(define/contract (textbox-backspace tb)
  (-> TextBox? TextBox?)
  (match-define (TextBox pre post) tb)
  (create-TextBox (substring pre 0 (last-index pre)) post))
;; example
(check-equal? (textbox-backspace (create-TextBox "hel" "lo")) (create-TextBox "he" "lo"))


;; textbox-insert: TextBox, string -> TextBox
;; inserts string where the cursor is and moves the cursor to the right of that character
(define/contract (textbox-insert tb str)
  (-> TextBox? string? TextBox?)
  (match-define (TextBox pre post) tb)
  (create-TextBox (string-append pre str) post))
;; example
(check-equal? (textbox-insert (create-TextBox "h" "llo") "e") (create-TextBox "he" "llo"))


;; textbox-left: TextBox -> TextBox
;; moves the cursor one character to the left
(define/contract (textbox-left tb)
  (-> TextBox? TextBox?)
  (match-define (TextBox pre post) tb)
  (create-TextBox (substring pre 0 (last-index pre)) 
                      (string-append (substring pre (last-index pre)) post)))
;; example
(check-equal? (textbox-left (create-TextBox "hel" "lo")) (create-TextBox "he" "llo"))


;; textbox-right: TextBox -> TextBox
;; moves the cursor one character to the right
(define/contract (textbox-right tb)
  (-> TextBox? TextBox?)
  (match-define (TextBox pre post) tb)
  (create-TextBox (string-append pre (substring post 0 (first-index post)))
                  (substring post (first-index post))))
;; example
(check-equal? (textbox-right (create-TextBox "hel" "lo")) (create-TextBox "hell" "o"))


;; remove-char : TextBox -> TextBox
;; Removes the first character in the `post` (right) part of the TextBox, right after the cursor.
(define/contract (remove-char tb)
  (-> TextBox? TextBox?)
  (match-define (TextBox pre post) tb)
  (create-TextBox pre (substring post (first-index post))))

;; Example
(check-equal? (remove-char (create-TextBox "hel" "lo")) (create-TextBox "hel" "o"))

;; shift-char : TextBox -> TextBox
;; Moves the first character from `post` to `pre`.
(define/contract (shift-char tb)
  (-> TextBox? TextBox?)
  (match-define (TextBox pre post) tb)
  (if (empty? post)
      tb  ;; No characters to shift
      (create-TextBox (string-append pre (substring post 0 1))  ;; Move first char from post to pre
                      (substring post 1))))  ;; Remove first char from post

;; Example
(check-equal? (shift-char (create-TextBox "hel" "lo")) (create-TextBox "hell" "o"))


;; key-handler : TextBox String -> TextBox
;; Handles key inputs to modify the TextBox.
;; - "left" moves the cursor one character left.
;; - "right" moves the cursor one character right.
;; - "backspace" removes the character before the cursor.
;; - "delete" removes the character after the cursor.
;; - All other single characters insert the character at the cursor.
(define/contract (key-handler textbox key)
  (-> TextBox? string? TextBox?)
  (cond
    ;; Move the cursor left (pop from pre to post)
    [(string=? key "left")
     (textbox-left textbox)]
    
    ;; Move the cursor right (pop from post to pre)
    [(string=? key "right")
     (textbox-right textbox)]
    
    ;; Backspace (remove character before the cursor)
    [(string=? key "backspace")
     (textbox-backspace textbox)]
    
    ;; Delete (remove character after the cursor)
    [(string=? key "\b")
     (textbox-delete textbox)]
    
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
(check-equal? (key-handler textbox-example "delete")
              (textbox-delete textbox-example))

;; Insert character "!"
(check-equal? (key-handler textbox-example "!")
              (textbox-insert textbox-example "!"))

;; main : -> void
;; Starts the interactive text editor with a blank TextBox.
(define (main)
  (big-bang (create-TextBox "" "")  ;; Initial empty TextBox
    (on-key key-handler)            ;; Handle key inputs
    (to-draw render)))              ;; Render the TextBox
