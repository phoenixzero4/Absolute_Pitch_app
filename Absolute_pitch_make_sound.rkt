;; William Paul
;; Cosc 231 Professor Boehm

(module Absolute_pitch_make_sound racket
  ;#lang racket


  (require racket/gui)
  (require rsound)
  (require rsound/piano-tones)
  (require rsound/draw)       
  (require racket/draw)
  (require net/url)
  (require mrlib/switchable-button)
  (require(file "./mybuttons.rkt"))

  (provide (all-defined-out))
  (define octave 4)
  (define note 24)
  (define accidental 0)

  (define pitches (hash "A" 33
                        "B" 35
                        "C" 24
                        "D" 26
                        "E" 28
                        "F" 29
                        "G" 31 
                        ))
  ;(define play-notes-frame (new frame% [label "Sound Generator"]))
  (define control-pane (new horizontal-pane% [parent play-notes-frame]
                            [border 25]
                            [spacing 25]))

  ;(define (get-octave2 note)
  ;  (let ((oct 0))
  ;  (cond ((and (< note 36) (> note 23) (set! oct 1)))
  ;         ((and (< note 48) (> note 35)) (set! oct 2))
  ;         ((and (< note 60) (> note 47)) (set! oct 3))
  ;         ((and (< note 72) (> note 59)) (set! oct 4))
  ;         ((and (< note 84) (> note 71)) (set! oct 5))
  ;         ((and (< note 96) (> note 83)) (set! oct 6))
  ;         ((and (< note 108)(> note 95)) (set! oct 7))
  ;         ((and (< note 120)(> note 107))(set! oct 8))
  ;         ((and (< note 129)(> note 119))(set! oct 9)))
  ;        (set! octave oct)))

  (define (play-new button)
    (let ((pitch (+ accidental (+ note(* 12 (- octave 1))))))
      (play (piano-tone pitch)) (printf "Playing note: ~a Octave: ~a Acc: ~a Pitch: ~a" note octave accidental pitch)))

  (define (set-accidental! choice event)
    (let ((acc (send choice get-selection)))
      (if (= 2 acc) (set! accidental -1)
          (set! accidental acc))))

  (define acc(new choice%
                  (label "Accidental ")
                  (parent control-pane)
                  (horiz-margin 10)
                  (vert-margin 2)
                  (style (list 'vertical-label))
                  (callback set-accidental!) 
                  (choices (list (string #\u266E)(string #\u266F)(string #\u266D)))))

  (define play-button(new switchable-button% (parent control-pane)
                          (label "Play")
                          (bitmap mybitmap)                        
                          (min-width-includes-label? #t)
                          (callback play-new)))
  (define (go-home button)
    (send main-frame show #t)
    (send play-notes-frame show #f)
    (send instruction-frame show #f))
  
  (define home-button(new switchable-button% (parent control-pane)
                          (label "Home")
                          (bitmap mybitmap4)                        
                          (min-width-includes-label? #t)
                          (callback go-home)))
  
  (define (set-octave! choice event)
    (set! octave (string->number(send choice get-string-selection))))
  
  (define choose-octave (new choice%
                             (label "Octave")
                             (style (list 'vertical-label))
                             (vert-margin 2)
                             (horiz-margin 10)
                             (parent control-pane)
                             (selection 4)                    
                             (choices (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
                             (callback set-octave!)))
  
  (define (set-pitch! choice event)
    (set! note (hash-ref pitches(send choice get-string-selection))))
  
  (define choose-pitch (new choice%
                            (label "Pitch Class")
                            (style (list 'vertical-label))
                            (parent control-pane)
                            (vert-margin 2)
                            (horiz-margin 10)
                            (selection 2)
                            (callback set-pitch!)
                            (choices (list "A" "B" "C" "D" "E" "F" "G"))))
  )
; Display GUI
;(send play-notes-frame show #t)