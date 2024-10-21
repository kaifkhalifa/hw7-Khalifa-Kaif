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
(struct TextBox [right left])

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







