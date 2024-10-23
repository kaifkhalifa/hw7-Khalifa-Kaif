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

;; A TextBox is a (TextBox [pre: list] [post: list])
;; where
;; pre: represents the list of characters to the left of the cursor
;; post: represents the list of characters to the right of the cursor
(struct TextBox [pre post] #:transparent) 


;; create-TextBox : String String -> TextBox?
;; Takes two strings (pre and post) and creates a TextBox instance.
(define/contract (create-TextBox pre post)
  (-> string? string? TextBox?)
  (TextBox (string->list pre) (string->list post)))

;; A WorldState is a TextBox
;; interp: represents the text box the user types characters into
(define (WorldState? w)
  (TextBox? w))

;; render : TextBox -> Image
;; Takes an instance of TextBox, and renders the text and cursor into a box.
(define (render textbox)
  (let* ([pre-str (list->string (TextBox-pre textbox))]  ;; Do not reverse the pre list
         [post-str (list->string (TextBox-post textbox))]
         [pre-text (text pre-str FONT-SIZE FONT-COLOR)]
         [post-text (text post-str FONT-SIZE FONT-COLOR)]
         [cursor (rectangle CURSOR-WIDTH FONT-SIZE "solid" "black")]
         [text-image (beside pre-text cursor post-text)])
    (place-image text-image 
                 (+ INITIAL-SPACE (/ (image-width text-image) 2)) 
                 (/ TEXTBOX-HEIGHT 2)
                 (rectangle TEXTBOX-WIDTH TEXTBOX-HEIGHT "outline" "black"))))



;; Example for render
(define textbox-example (create-TextBox "Hello" "World"))

(check-equal? (render textbox-example)
              (place-image
               (beside (text "Hello" FONT-SIZE FONT-COLOR) 
                       (rectangle CURSOR-WIDTH FONT-SIZE "solid" "black")
                       (text "World" FONT-SIZE FONT-COLOR))
               (+ INITIAL-SPACE (/ (+ (image-width (text "Hello" FONT-SIZE FONT-COLOR))
                                      CURSOR-WIDTH
                                      (image-width (text "World" FONT-SIZE FONT-COLOR))) 2))
               (/ TEXTBOX-HEIGHT 2)
               (rectangle TEXTBOX-WIDTH TEXTBOX-HEIGHT "outline" "black")))



;; remove-char : list -> list
;; Removes the first character from the given list.
(define (remove-char lst)
  (if (empty? lst)
      lst
      (rest lst)))

;; Example
(check-equal? 
  (remove-char '(#\a #\b #\c))
  '(#\b #\c))


;; shift-char : list * list -> list * list
;; Moves the first character from 'from-list' to the front of 'to-list'.
(define (shift-char from-list to-list)
  (if (empty? from-list)
      (values from-list to-list)
      (values (rest from-list) (cons (first from-list) to-list))))

;; Example
(define-values (new-from new-to)
  (shift-char '(#\a #\b #\c) '(#\x #\y)))
(check-equal? new-from '(#\b #\c))
(check-equal? new-to '(#\a #\x #\y))


;; textbox-left : TextBox -> TextBox
;; Moves the cursor one character to the left.
(define (textbox-left tb)
  (define-values (new-pre new-post)
    (shift-char (TextBox-pre tb) (TextBox-post tb)))
  (if (equal? (TextBox-pre tb) new-pre)
      tb
      (TextBox new-pre new-post)))


;; Example for textbox-left
(define empty-left-textbox (create-TextBox "World" ""))

(check-equal? 
  (textbox-left empty-left-textbox)
  (create-TextBox "orld" "W"))


;; textbox-right : TextBox -> TextBox
;; Moves the cursor one character to the right.
(define (textbox-right tb)
  (define-values (new-post new-pre)  ;; Reverse the lists correctly
    (shift-char (TextBox-post tb) (TextBox-pre tb)))  ;; Move from 'post' to 'pre'
  (if (equal? (TextBox-post tb) new-post)
      tb 
      (TextBox new-pre new-post)))  ;; Correctly swap the lists



;; Example for textbox-right
(define empty-right-textbox (create-TextBox "" "Hello"))

(check-equal? 
  (textbox-right empty-right-textbox)
  (create-TextBox "H" "ello"))




;; textbox-backspace : TextBox -> TextBox
;; Removes the character to the left of the cursor.
(define (textbox-backspace tb)
  (TextBox (remove-char (TextBox-pre tb)) (TextBox-post tb)))


;; Example for textbox-backspace
(define empty-left-textbox2 (create-TextBox "World" ""))

(check-equal? 
  (textbox-backspace empty-left-textbox2)
  (create-TextBox "orld" ""))


;; textbox-delete : TextBox -> TextBox
;; Removes the character to the right of the cursor.
(define (textbox-delete tb)
  (if (empty? (TextBox-post tb))
      tb
      (TextBox (TextBox-pre tb) (rest (TextBox-post tb)))))


;; Example for textbox-delete
(define example-textbox (create-TextBox "World" "Hello"))

(check-equal? 
  (textbox-delete example-textbox)
  (create-TextBox "World" "ello"))


;; textbox-insert : TextBox String -> TextBox
;; Inserts a single character at the cursor and moves the cursor right.
(define (textbox-insert tb char)
  (if (not (= (string-length char) 1))
      tb  ; Ignore if not a single character
      (TextBox (cons (string-ref char 0) (TextBox-pre tb)) (TextBox-post tb))))


;; Example for textbox-insert
(define textbox5 (create-TextBox "World" "olleH"))

(check-equal? 
  (textbox-insert textbox5 "")
  textbox5)


;; key-handler : TextBox String -> TextBox
;; Handles key inputs to modify the TextBox.
(define (key-handler textbox key)
  (cond
    [(string=? key "left")
     (textbox-left textbox)]
    
    [(string=? key "right")
     (textbox-right textbox)]
    
    [(string=? key "backspace")
     (textbox-backspace textbox)]
    
    [(string=? key "delete")
     (textbox-delete textbox)]
  
    [(= (string-length key) 1)
     (textbox-insert textbox key)]
    
    [else
     textbox]))  ; Ignore all other keys


;; main : -> TextBox
;; Runs the interactive program with a TextBox.
(define (main)
  (big-bang (create-TextBox "" "")  ; Initial empty TextBox with empty strings
    (on-key key-handler)              ; Handle key inputs
    (to-draw render)))                ; Render the TextBox

