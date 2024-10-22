#lang racket

(provide (all-defined-out))

(require rackunit
         2htdp/image)


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
  (let* ([left-str (list->string (TextBox-left textbox))]
         [right-str (list->string (TextBox-right textbox))]
         [left-text (text left-str FONT-SIZE FONT-COLOR)]
         [right-text (text right-str FONT-SIZE FONT-COLOR)]
         [cursor (rectangle CURSOR-WIDTH FONT-SIZE "solid" "black")]
         [text-image (beside left-text cursor right-text)])
    (place-image text-image (+ INITIAL-SPACE (/ (image-width text-image) 2)) (/ TEXTBOX-HEIGHT 2)
                 (rectangle TEXTBOX-WIDTH TEXTBOX-HEIGHT "outline" "black"))))

;; Example for render
(define textbox-example (create-TextBox '(#\H #\e #\l #\l #\o) '(#\W #\o #\r #\l #\d)))

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

;; key-handler : TextBox String -> TextBox
;; Handles key inputs to modify the TextBox.
(define (key-handler textbox key)
  (cond
    ;; Move the cursor to the left (pop from 'left' and push to 'right')
    [(string=? key "left")
     (if (empty? (TextBox-left textbox))
         textbox
         (TextBox 
           (cons (first (TextBox-left textbox)) (TextBox-right textbox)) ; New 'right'
           (rest (TextBox-left textbox)) ))] ; New 'left'

    ;; Move the cursor to the right (pop from 'right' and push to 'left')
    [(string=? key "right")
     (if (empty? (TextBox-right textbox))
         textbox
         (TextBox 
           (rest (TextBox-right textbox)) ; New 'right'
           (cons (first (TextBox-right textbox)) (TextBox-left textbox)) ))] ; New 'left'

    ;; Backspace (remove the first character from 'left')
    [(string=? key "backspace")
     (if (empty? (TextBox-left textbox))
         textbox
         (TextBox 
           (TextBox-right textbox) ; 'right' remains unchanged
           (rest (TextBox-left textbox)) ))] ; New 'left'

    ;; Delete (remove the first character from 'right')
    [(string=? key "delete")
     (if (empty? (TextBox-right textbox))
         textbox
         (TextBox 
           (rest (TextBox-right textbox)) ; New 'right'
           (TextBox-left textbox) ))] ; 'left' remains unchanged

    ;; Insert any single character (into 'left', where the cursor is)
    [(= (string-length key) 1)
     (TextBox 
       (TextBox-right textbox) 
       (cons (string-ref key 0) (TextBox-left textbox)) ) ]

    ;; Ignore all other keys
    [else textbox]))


;; Example for keyhandler
(define example-textbox (create-TextBox '(#\W #\o #\r #\l #\d) '(#\H #\e #\l #\l #\o)))

;;Move cursor left
(check-equal? 
  (key-handler example-textbox "left")
  (create-TextBox '(#\H #\W #\o #\r #\l #\d) '(#\e #\l #\l #\o)))

;;Backspace
(check-equal? 
  (key-handler example-textbox "backspace")
  (create-TextBox '(#\W #\o #\r #\l #\d) '(#\e #\l #\l #\o)))

;;Insert character 'X'
(check-equal? 
  (key-handler example-textbox "X")
  (create-TextBox '(#\W #\o #\r #\l #\d) '(#\X #\H #\e #\l #\l #\o)))

;;Delete character to the right
(check-equal? 
  (key-handler example-textbox "delete")
  (create-TextBox '(#\o #\r #\l #\d) '(#\H #\e #\l #\l #\o)))

