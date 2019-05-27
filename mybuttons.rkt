;; William Paul
;; Cosc 231 Professor Boehm


(module mybuttons racket  
  (require pict images/icons/style images/icons/control images/icons/symbol)
  (require racket/draw)
  (require racket/gui)
  ; (require images/compile-time
  ;     (for-syntax images/icons/control))
  
  (provide (all-defined-out))

  (define mybitmap (pict->bitmap(cc-superimpose                               
                                 (bitmap (record-icon #:color "green" #:height 40
                                                      #:material glass-icon-material))
                                 (bitmap (step-icon #:color light-metal-icon-color #:height 20
                                                    #:material metal-icon-material)))))

  (define mybitmap2 (pict->bitmap(cc-superimpose                               
                                  (bitmap (record-icon #:color "blue" #:height 40
                                                       #:material glass-icon-material))
                                  (bitmap (lambda-icon #:color light-metal-icon-color #:height 20
                                                       #:material metal-icon-material)))))
  (define mybitmap3 (pict->bitmap(cc-superimpose                               
                                  (bitmap (record-icon #:color "red" #:height 40
                                                       #:material glass-icon-material))
                                  (bitmap (check-icon #:color light-metal-icon-color #:height 20
                                                      #:material metal-icon-material)))))

  (define mybitmap4 (pict->bitmap(cc-superimpose                               
                                  (bitmap (record-icon #:color "black" #:height 40
                                                       #:material glass-icon-material))
                                  (bitmap (check-icon #:color light-metal-icon-color #:height 20
                                                      #:material metal-icon-material)))))  
  (define main-frame (new frame% [label "Welcome Page"]
                          (style (list 'no-resize-border))))
  
  (define play-notes-frame (new frame% [label "Sound Generator"]
                                (style (list 'no-resize-border))))

    (define instruction-frame (new frame% [label "Instructions"]
                                 (style (list 'no-resize-border))))
  
  (define mytexticon(text-icon "Enter Your Guess: "
                               (make-font #:weight 'bold )
                               #:color "blue" #:height 20))
  (define winicon2(text-icon "Correct !! "
                             (make-font #:weight 'bold )
                             #:color "green" #:height 30))
  (define winicon(text-icon "Absolute Pitch!!"
                            (make-font #:weight 'bold)
                            #:color "purple" #:height 25))
  (define loseicon(text-icon "Incorrect. Try Again."
                             (make-font #:weight 'bold)
                             #:color "red" #:height 25)))

