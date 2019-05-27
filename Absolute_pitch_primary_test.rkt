;; William Paul
;; Cosc 231 Professor Boehm

(module Absolute_pitch_primary_test racket
  ;#lang racket

  ;;        TODO
  ;;        ***   DONE --kinda---   1) Generate a menu-bar and information page for instructions 
  ;;        ***   DONE ----------   2) Install sharp/flat notes
  ;;        ***   DONE ----------   3) Create a difficulty option (splash page?)
  ;;              4) Keyboard (musical) functionality for the user
  ;;              5) SVG animations on the main canvas
  ;;              6) Allow user to determine the range of notes played
  ;;        ***   DONE ----------   7) Create the function test responses for intervals
  ;;              8) Display the letter of the pitch class 
  
  ;;  -- the program will not throw an exception if a user chooses A-B 9 or C-G 0
  ;;  -- it simply doesn't play any notes because the midi values don't exist

  (require racket/gui)
  (require rsound)
  (require rsound/piano-tones)
  (require rsound/draw)         
  (require racket/draw)
  (require net/url)
  (require mrlib/switchable-button)
  (require(file "./mybuttons.rkt"))
 
  (provide (all-defined-out))

  ; global variables -admittedly not the best approach but the easiest right now
  (define paramlist (list 60 69 1))
  (define high-note 69)
  (define low-note 60)
  (define time-interval 1)
  (define high-octave 4)
  (define low-octave 4)
  (define the-interval "Unison")
  (define highguess 69)
  (define lowguess 60)
  (define highaccguess 0)
  (define lowaccguess 0)
  (define intervalguess "Unison")
  (define lowoctaveguess 4)
  (define highoctaveguess 4)
   (define low-octave-range 0)
  (define high-octave-range 9)
  
  ; MIDI values for lowest notes in pitch classes A-G
  ; (when combined with the chosen register will return the correct
  ;  pitch for that octave)  -- for example A1 = midi 33 A4 = midi 21 + ((4-1) * 12) = midi 69
  ; ** the exceptions are Octaves 0 & 9 (0 has no C-G pitch classes and 9 has no A-B) **
  (define notes (hash "A" 33
                      "B" 35
                      "C" 24
                      "D" 26
                      "E" 28
                      "F" 29
                      "G" 31 
                      ))
  (define (get-octave note hi)
    (let ((oct 0))
      (cond ((and (< note 36) (> note 23) (set! oct 1)))
            ((and (< note 48) (> note 35)) (set! oct 2))
            ((and (< note 60) (> note 47)) (set! oct 3))
            ((and (< note 72) (> note 59)) (set! oct 4))
            ((and (< note 84) (> note 71)) (set! oct 5))
            ((and (< note 96) (> note 83)) (set! oct 6))
            ((and (< note 108)(> note 95)) (set! oct 7))
            ((and (< note 120)(> note 107))(set! oct 8))
            ((and (< note 129)(> note 119))(set! oct 9)))
      (if hi (set! high-octave oct)
          (set! low-octave oct))))
  
(define (set-hi-oct-range val)
  (set! high-octave-range val))
  (define (set-low-oct-range val)
    (set! low-octave-range val))
  
  (define generate-audio-test
    (lambda ()
      (let* ((gap 1)
             (result gap)
             (low-r (+ 24 (* 12 (- low-octave-range 1))))
             (high-r(+ 35 (* 12 (- high-octave-range 1))))
             (seed (random 0 2))
             (a (random low-r (+ high-r 1))))
        (cond ((< (+ a 13) 128) (set! low-note a) (set! high-note (random a (+ 13 a))))
              ((> (+ a 12) 128) (set! high-note a) (set! low-note (random (- a 12) (+ a 1)))))
        (get-octave high-note #t)
        (get-octave low-note #f)
        (or (> time-interval gap) (< time-interval gap))
        (set! gap time-interval)
        (set-interval))))
  
(define (set-interval)
  (let* ((freq-high (* 440 (expt 2 (/ (- high-note 69) 12))))
        (freq-low (* 440 (expt 2 (/ (- low-note 69) 12))))
        (tet (/ freq-high freq-low)))
    (cond ((and (>= tet 1.000 ) (<= tet 1.00009))(set! the-interval "Unison"))
          ((and (>= tet 1.0001) (<= tet 1.066667)) (set! the-interval "Minor Second"))
          ((and (>= tet 1.066668) (<= tet 1.125)) (set! the-interval "Major Second"))
          ((and (>= tet 1.1251) (<= tet 1.2)) (set! the-interval "Minor Third"))
          ((and (>= tet 1.21) (<= tet 1.259921)) (set! the-interval "Major Third"))
          ((and (>= tet 1.259922) (<= tet 1.33484)) (set! the-interval "Perfect Fourth"))
          ((and (>= tet 1.33485) (<= tet 1.414214)) (set! the-interval "Diminished Fifth"))
          ((and (>= tet 1.414215) (<= tet 1.5)) (set! the-interval "Perfect Fifth"))
          ((and (>= tet 1.51) (<= tet 1.6)) (set! the-interval "Minor Sixth"))
          ((and (>= tet 1.61) (<= tet 1.681793)) (set! the-interval "Major Sixth"))
          ((and (>= tet 1.681794) (<= tet 1.791797)) (set! the-interval "Minor Seventh"))
          ((and (>= tet 1.791798) (<= tet 1.88749)) (set! the-interval "Major Seventh"))
          ((>= tet 1.8875) (set! the-interval "Octave")))))
  
  (define (setlowguess choice event)
    (set! lowguess (+ lowaccguess(* (- lowoctaveguess 1) 12)(hash-ref notes (send choice get-string-selection)))))

  ; 'overloaded method to set low note without the choice% callback procedure
  ;  ** could just use 'send method I guess
  (define (setlow stringnote)
    (set! lowguess (+ lowaccguess (* (- lowoctaveguess 1) 12)(hash-ref notes stringnote))))

  (define (sethighguess choice event)
    (set! highguess (+ highaccguess (* (- highoctaveguess 1) 12)(hash-ref notes (send choice get-string-selection)))))

  (define (sethigh stringnote)
    (set! highguess (+ highaccguess (* (- highoctaveguess 1) 12)(hash-ref notes stringnote))))

  (define (set-high-accidental choice event)
    (let ((acc (send choice get-selection)))
      (if (= 2 acc) (set! highaccguess -1)
          (set! highaccguess acc)))
    (sethigh (send high-pitch-guess get-string-selection)))

  (define (set-low-accidental choice event)
    (let ((acc (send choice get-selection)))
      (if (= 2 acc) (set! lowaccguess -1)
          (set! lowaccguess acc)))
    (setlow (send low-pitch-guess get-string-selection)))

  (define (setintervalguess choice event)
    (set! intervalguess (send choice get-string-selection)))

  (define (set-high-octave-guess! choice event)
    (set! highoctaveguess (string->number(send choice get-string-selection)))
    (sethigh (send high-pitch-guess get-string-selection)))

  (define (set-low-octave-guess! choice event)
    (set! lowoctaveguess (string->number(send choice get-string-selection)))
    (setlow (send low-pitch-guess get-string-selection)))


  ; print values for testing
  (define (submit button)
    (cond ((and (= high-note highguess)(= low-note lowguess)
                (equal? the-interval intervalguess)(= low-octave lowoctaveguess)
                (= high-octave highoctaveguess))
           (printf "\n**Correct**\nLowguess: ~a (Octave ~a)\nHighguess: ~a (Octave ~a)\nInterval Guess: ~a\n" lowguess lowoctaveguess highguess highoctaveguess intervalguess) (send mydisp show #f) (set! text2 winlogo) (send mydisp show #t)(set! main-logo correct) (send main-canvas refresh-now))
          (else  (printf "\n**Incorrect**\nLowguess: ~a (Octave: ~a)\nHighguess: ~a (Octave: ~a)\nInterval Guess: ~a" lowguess lowoctaveguess highguess highoctaveguess intervalguess)
                 (printf "\nLow Note: ~a (Octave ~a)\nHigh Note: ~a (Octave ~a)\nInterval: ~a\n" low-note low-octave high-note high-octave the-interval )(send mydisp show #f) (set! text2 loselogo) (send mydisp show #t))))
     

  (define (change-gap newgap)
    (set! time-interval (string->number newgap)))

  (define (set-gap choice event)
    (change-gap(send choice get-string-selection)))

  ;; scale bitmap images (jpeg, png, etc.)
  ;; shamelessly stolen from RacketUsers group
  ; ************************************************************************ ;
  (define bitmap-blank
    (lambda [[w 0] [h #false] #:backing-scale [backing-scale 2.0]]
      (define width  (max 1 (exact-ceiling w)))
      (define height (max 1 (exact-ceiling (or h w))))
      (make-bitmap width height #:backing-scale backing-scale)))

  (define bitmap-scale
    (case-lambda
      [(bmp scale)
       (if (= scale 1.0) bmp (bitmap-scale bmp scale scale))]
      [(bmp scale-x scale-y)
       (cond [(and (= scale-x 1.0) (= scale-y 1.0)) bmp]
             [else (let ([w (max 1 (exact-ceiling (* (send bmp get-width) scale-x)))]
                         [h (max 1 (exact-ceiling (* (send bmp get-height) scale-y)))])
                     (define dc (make-object bitmap-dc% (bitmap-blank w h)))
                     (send dc set-smoothing 'aligned)
                     (send dc set-scale scale-x scale-y)
                     (send dc draw-bitmap bmp 0 0)
                     (or (send dc get-bitmap) (bitmap-blank)))])]))
  ; ************************************************************************ ;

  ; Main window
  (define test-frame (new frame% [label "Absolute Pitch Test"]
                          (style (list 'no-resize-border))))


                      
  (define panel1 (new horizontal-panel% (parent test-frame)
                      (min-width 550)
                      (min-height 250)
                      (border 10 )
                      (alignment (list 'center 'top))
                      ))


  (define panel2 (new horizontal-panel% (parent test-frame)
                      (min-width 550)
                      (min-height 90)
                      (border 10)
                      (alignment (list 'center 'center))
                      ))



  ; messages in icon format (to look a little sharper)
  (define enter (bitmap-scale mytexticon .5))
  (define winlogo (bitmap-scale winicon .5))
  (define loselogo (bitmap-scale loseicon .5))
  (define text2 enter)
  (define correct (bitmap-scale winicon2 1))
  ; canvas for the primary message
  (define mydisp(new canvas% (parent panel2)
                     (paint-callback
                      (lambda (canvas dc)
                        (send dc set-scale 3 3)
                        (send dc draw-bitmap text2 30 5)))))

  ; main canvas for artwork/logo & logo                      ********** TODO change gay logo *************
  (define logo (read-bitmap "C:/Users/PhoenixZero4/OneDrive/School_in_the_Cloud/Okanagan_Notes/_231_Scheme/Scheme_programs/bitmap_images/music8.jpg" ))
  (define logo2 (bitmap-scale logo .50))
  (define main-logo logo2)
  (define main-canvas(new canvas% (parent panel1)
                          (paint-callback
                           (lambda(canvas dc)
                             (send dc set-scale 3 3)
                             (cond ((equal? main-logo logo2)               
                                    (send dc draw-bitmap main-logo 0 0))
                                   ((equal? main-logo correct)
                                    (send dc draw-bitmap main-logo 5 25)))))))

  (define answerpanel1 (new horizontal-panel% (parent test-frame)
                            (min-width 250)
                            (min-height 100)
                            (border 10)
                            (alignment (list'center 'center))
                            ))
  (define answerpanel2 (new horizontal-panel% (parent test-frame)
                            (min-width 250)
                            (min-height 100)
                            (border 10)
                             (horiz-margin 35)
                            (alignment (list'center 'center))
                            ))
  (define answerpanel3 (new horizontal-panel% (parent test-frame)
                            (min-width 250)
                            (min-height 100)
                            (border 10)
                            (horiz-margin 10)
                            (alignment (list'center 'center))
                            ))


  
  (define (play-replay button)
    (play (piano-tone high-note))           
    (set! text2 enter) (send mydisp refresh-now)
    (set! main-logo logo2) (send main-canvas refresh-now)
    (printf "Playing high-note ~a \n" high-note)  ; remove after debugging
    (sleep time-interval)
    (printf "Playing low-note ~a\n" low-note)
    (play (piano-tone low-note)))

  (define (test-play-new button)
    (generate-audio-test)
    (set! text2 enter) (send mydisp refresh-now)
    (set! main-logo logo2) (send main-canvas refresh-now)
    (printf "Playing high-note ~a \n" high-note)  ; remove after debugging
    (play (piano-tone high-note))
    (sleep time-interval)
    (printf "Playing low-note ~a\n" low-note)
    (play (piano-tone low-note)))

  (define test-control-pane (new horizontal-pane% [parent test-frame]
                                 (alignment (list 'right 'center))
                                 [border 25]
                                 [spacing 25]))

  (define time-gap (new choice%
                        (label "Time Between Notes  ")
                        (parent test-control-pane)
                        (choices '(".25" ".5" ".75" "1" "2" "3" "4" "5"))
                        (selection 3)
                        (callback set-gap)))
  (define (test-go-home button)
    (send main-frame show #t)
    (send test-frame show #f)
    (send instruction-frame show #f))
  
  (define test-home-button(new switchable-button% (parent test-control-pane)
                               (label "Home")
                               (bitmap mybitmap4)                        
                               (min-width-includes-label? #t)
                               (callback test-go-home)))
  
  ; *** not sure about slider vs. choice yet
  ; * slider looks better but requires integers over rational numbers

  ;(define time-gap (new slider%
  ;                      (label "Time Between Notes (1/100s)")
  ;                      (parent test-control-pane)
  ;                      (style (list 'horizontal 'vertical-label))
  ;                      (min-value 25)
  ;                      (max-value 500)
  ;                      (init-value 100)
  ;                      (callback set-gap)
  ;                      ))

  ; plays the same two notes generated previously
  (define replay (new switchable-button% [parent test-control-pane]
                      [label "RePlay"]
                      (bitmap mybitmap2)
                      (min-width-includes-label? #t)
                      [callback play-replay]))
  ; generates two new notes
  (define test-play-button(new switchable-button% (parent test-control-pane)
                               (label "Play")
                               (bitmap mybitmap)                        
                               (min-width-includes-label? #t)
                               (callback test-play-new)))

  (define low-pitch-guess (new choice%
                               (label "Low Note ")
                               (style (list 'vertical-label))
                               (parent answerpanel2)
                               (vert-margin 2)
                               (horiz-margin 10)
                               (selection 2)
                               (callback setlowguess)
                               (choices (list "A" "B" "C" "D" "E" "F" "G"))))

  (define high-pitch-guess (new choice%
                                (label "High Note")
                                (style (list 'vertical-label))
                                (vert-margin 4)
                                (horiz-margin 10)
                                (parent answerpanel1)
                                (selection 0)
                                (callback sethighguess)
                                (choices (list "A" "B" "C" "D" "E" "F" "G"))))


  (define acc1(new radio-box%
                   (label " High Accidental")
                   (choices (list (string #\u266E)(string #\u266F)(string #\u266D))) 
                   (parent answerpanel1)
                   (callback set-high-accidental)
                   (horiz-margin 10)
                   (vert-margin 2)
                   (style (list 'horizontal 'vertical-label))
                   (selection 0)))
  
   (define acc2(new radio-box%
                   (label " Low Accidental")
                   (choices (list (string #\u266E)(string #\u266F)(string #\u266D))) 
                   (parent answerpanel2)
                   (callback set-low-accidental)
                   (horiz-margin 10)
                   (vert-margin 2)
                   (style (list 'horizontal 'vertical-label))
                   (selection 0)))
  
  ;; -- The radio-buttons look a little better i think -- ;;
  
;  (define acc2(new choice%
;                   (label "Accidental ")
;                   (parent answerpanel2)
;                   (horiz-margin 10)
;                   (vert-margin 2)
;                   (style (list 'vertical-label))
;                   (callback set-low-accidental) 
;                   (choices (list (string #\u266E)(string #\u266F)(string #\u266D)))))
  
  ; could make these more generic so each 'choice-widget' could
  ; be created from a class definition --
  
  (define low-octave-guess (new choice%
                                (label "Octave ")
                                (style (list 'vertical-label))
                                (vert-margin 2)
                                (horiz-margin 10)
                                (parent answerpanel2)
                                (callback set-low-octave-guess!)
                                (selection 4)
                                (choices (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))))
  (define high-octave-guess (new choice%
                                 (label "Octave")
                                 (style (list 'vertical-label))
                                 (vert-margin 2)
                                 (horiz-margin 10)
                                 (parent answerpanel1)
                                 (callback set-high-octave-guess!)  ; same with these methods (maybe generalize and use a flag to
                                 (selection 4)                      ; allow one method to set different variables
                                 (choices (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))))
  (define interval-guess (new choice%
                              (label "  Interval")
                              (style (list 'vertical-label))
                              (horiz-margin 5)
                              (parent answerpanel3)
                              (callback setintervalguess)
                              (choices (list "Unison" "Minor Second" "Major Second" "Minor Third" "Major Third" "Perfect Fourth" "Diminished Fifth" "Perfect Fifth" "Minor Sixth" "Major Sixth" "Minor Seventh" "Major Seventh" "Octave"))))

  (define enterbutton mybitmap3)
  (define enter-guess (new switchable-button% (parent answerpanel3)
                           (label "Submit Guess")
                           (bitmap enterbutton)
                           (min-width-includes-label? #t)
                           (callback submit)))                


  )                 
; Display GUI
;(send test-frame show #t)