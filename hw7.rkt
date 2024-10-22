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

;; A TextBox is a (TextBox [right: list] [left: list])
;; where
;; right: represents the list of characters to the right of the cursor
;; left: represents the list of characters to the left of the cursor
(struct TextBox [right left] #:transparent) 


;; create-TextBox : list? list? -> TextBox?
;; Takes two lists (right and left) and creates a TextBox instance.
(define/contract (create-TextBox right left)
  (-> list? list? TextBox?)
  (TextBox right left))

;; A WorldState is a TextBox
;; interp: represents the text box the user types characters into
(define (WorldState? w)
  (TextBox? w))

;; render : TextBox -> Image
;; Takes an instance of TextBox, and renders the text and cursor into a box.
(define (render textbox)
  (let* ([left-str (list->string (reverse (TextBox-left textbox)))]  ;; reverse the left list
         [right-str (list->string (TextBox-right textbox))]
         [left-text (text left-str FONT-SIZE FONT-COLOR)]
         [right-text (text right-str FONT-SIZE FONT-COLOR)]
         [cursor (rectangle CURSOR-WIDTH FONT-SIZE "solid" "black")]
         [text-image (beside left-text cursor right-text)])
    (place-image text-image (+ INITIAL-SPACE (/ (image-width text-image) 2)) (/ TEXTBOX-HEIGHT 2)
                 (rectangle TEXTBOX-WIDTH TEXTBOX-HEIGHT "outline" "black"))))


;; Example for render
(define textbox-example (create-TextBox '(#\H #\e #\l #\l #\o) '(#\d #\l #\r #\o #\W)))

(check-equal? (render textbox-example)
              (place-image
               (beside (text "World" FONT-SIZE FONT-COLOR) 
                       (rectangle CURSOR-WIDTH FONT-SIZE "solid" "black")
                       (text "Hello" FONT-SIZE FONT-COLOR))
               (+ INITIAL-SPACE (/ (+ (image-width (text "World" FONT-SIZE FONT-COLOR))
                                      CURSOR-WIDTH
                                      (image-width (text "Hello" FONT-SIZE FONT-COLOR))) 2))
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
  (define-values (new-left new-right)
    (shift-char (TextBox-left tb) (TextBox-right tb)))
  (if (equal? (TextBox-left tb) new-left)
      tb
      (create-TextBox new-right new-left)))

;; Example
(define empty-left-textbox (create-TextBox '(#\W #\o #\r #\l #\d) '()))

(check-equal? 
  (textbox-left empty-left-textbox)
  empty-left-textbox)

;; textbox-right : TextBox -> TextBox
;; Moves the cursor one character to the right.
(define (textbox-right tb)
  (define-values (new-right new-left)
    (shift-char (TextBox-right tb) (TextBox-left tb)))
  (if (equal? (TextBox-right tb) new-right)
      tb  ; No change if 'right' was empty
      (create-TextBox new-right new-left)))

;; Example
(define empty-right-textbox (create-TextBox '() '(#\o #\l #\l #\e #\H)))

(check-equal? 
  (textbox-right empty-right-textbox)
  empty-right-textbox)

;; textbox-backspace : TextBox -> TextBox
;; Removes the character to the left of the cursor.
(define (textbox-backspace tb)
  (create-TextBox (TextBox-right tb) (remove-char (TextBox-left tb))))


;; Example
(define empty-left-textbox2 (create-TextBox '(#\W #\o #\r #\l #\d) '()))

(check-equal? 
  (textbox-backspace empty-left-textbox2)
  empty-left-textbox2)

;; textbox-delete : TextBox -> TextBox
;; Removes the character to the right of the cursor.
(define (textbox-delete tb)
  (create-TextBox (remove-char (TextBox-right tb)) (TextBox-left tb)))

;; Example
(define empty-right-textbox2 (create-TextBox '() '(#\o #\l #\l #\e #\H)))

(check-equal? 
  (textbox-delete empty-right-textbox2)
  empty-right-textbox2)

;; textbox-insert : TextBox String -> TextBox
;; Inserts a single character at the cursor and moves the cursor right.
(define (textbox-insert tb char)
  (if (not (= (string-length char) 1))
      tb  ; Ignore if not a single character
      (create-TextBox 
        (TextBox-right tb) 
        (cons (string-ref char 0) (TextBox-left tb)))))



;; Example
(define textbox5 (create-TextBox '(#\W #\o #\r #\l #\d) '(#\o #\l #\l #\e #\H)))
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
  (big-bang (create-TextBox '() '())  ; Initial empty TextBox
    (on-key key-handler)              ; Handle key inputs
    (to-draw render)))                ; Render the TextBox