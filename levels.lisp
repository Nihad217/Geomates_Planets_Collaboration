;;;
;;; Levels are defined in a global variable *levels* as list of levels. Each level consists out of objects
;;; given as s-expressions. The coordinate system is x to the right, positive y is up. Unit size is meter.
;;; Levels should be 80 x 40 in size to fit the graphical output. 
;;; Format and semantics are as follows:
;;; (:rect x y) starting position of the rect, the rect player starts with initial width according to +rect-length+ (2m)
;;; (:disc x y) starting position of the disc, the disc is of radius +disc-radius+ (1.0m)
;;; (:platform x1 y1 x2 y2) defines a rectangular platform with corners (x1 y1) and (x2 y2)
;;;                         be nice and add a ground plane and side walls to avoid your players falling off accidentally
;;; (:diamond x y) diamond to collect: each level must contain at least one diamand as a level is said to be finished if
;;;                no diamonds are left
;;; 

(defparameter *levels*
  '(;;Original test level
     ((:disc 10 23)
     (:rect 40 23)
     (:platform 0 0 80 1)
     (:platform 0 39 80 40)
     (:platform 0 1 1 40)
     (:platform 79 1 80 40)
     (:platform 1 20 30 21)
     (:platform 40 20 79 21)
     (:diamond 20 26)
     (:diamond 60 26)
     (:diamond 35 10))
    ;level 1, testing disc jumping over gap
    ((:disc 10 23)
     (:rect 40 10)
     (:platform 0 0 80 1)
     (:platform 0 39 80 40)
     (:platform 0 1 1 40)
     (:platform 79 1 80 40)
     (:platform 1 20 31.5 21)
     (:platform 38.5 20 79 21)
     (:diamond 20 26)
     (:diamond 60 26)
     (:diamond 35 10))
     ;; 4 Clash of disc and rect
     ((:disc 10 2)
     (:rect 45 5)
     (:platform 0 0 80 1)
     (:platform 0 39 80 40)
     (:platform 0 1 1 40)
     (:platform 79 1 80 40)
     (:diamond 20 10)
     (:diamond 40 10)
     (:diamond 60 10))
    ;;10 level 2
    ((:disc 35 15)
     (:rect 57 15)
     (:platform 0 0 80 1)
     (:platform 0 39 80 40)
     (:platform 0 1 1 40)
     (:platform 79 1 80 40)
     (:platform 1 10 12 11)
     (:platform 20 10 73 11)
     (:diamond 7 15)
     (:diamond 70 23)
     (:diamond 70 6)
     ) 
     ;;11 level 3
     ((:disc 40 15)
     (:rect 60 15)
     (:platform 0 0 80 1)
     (:platform 0 39 80 40)
     (:platform 0 1 1 40)
     (:platform 79 1 80 40)
     (:platform 1 10 79 11)
     (:platform 1 11 4 12)
     (:platform 70 11 79 12)
     (:diamond 4 16)
     (:diamond 35 17)
     (:diamond 74 16)
     )
     (;14 Level 3   with collobration case study
	 (:disc 10 4)
     (:rect 20 2)
     (:platform 0 0 80 1)    ; Ground plane
     (:platform 0 39 80 40)  ; Top wall
     (:platform 0 1 1 40)    ; Left wall
     (:platform 79 1 80 40)  ; Right wall
     (:platform 70 5 79 6)   ; Right middle platform
	 (:diamond 3 5)          ; diamond
     (:diamond 76 12)        ; diamond
     ) 
    ;; 21
    ((:disc 10 23) ;disc like in 1
     (:rect 40 3) ; Rect down
     (:platform 0 0 80 1) 
     (:platform 0 39 80 40) 
     (:platform 0 1 1 40)  
     (:platform 79 1 80 40) 
     (:platform 1 20 79 21) ; Plattform in der mitte (durchgehend)
     (:diamond 20 26)
     (:diamond 60 26)
     (:diamond 35 10))
    ;; 25 
    ((:disc 60 23)
     (:rect 40 23)
     (:platform 0 0 80 1)
     (:platform 0 39 80 40)
     (:platform 0 1 1 40)
     (:platform 79 1 80 40)
     (:platform 1 20 30 21)
     (:platform 40 20 79 21)
     (:diamond 20 26)
     (:diamond 60 26)
     (:diamond 35 10)
     (:diamond 35 26)
     (:diamond 60 10)
     )
    ;;level3, testing disc jumping for diamond
     ;; should be able to go for it, but doesn't.
     ;; seems to be a bug in the "searcher.lisp".
     ((:disc 10 23)
     (:rect 40 10)
     (:platform 0 0 80 1)
     (:platform 0 39 80 40)
     (:platform 0 1 1 40)
     (:platform 79 1 80 40)
     (:platform 1 20 31.5 21)
     (:diamond 38 17)
     (:diamond 20 26)
     (:diamond 60 26)
     (:diamond 35 10))
     ;;5 Testlevel for jumping on platform
     ((:disc 10 2)
     (:rect 45 23)
     (:platform 0 0 80 1)
     (:platform 0 39 80 40)
     (:platform 0 1 1 40)
     (:platform 79 1 80 40)
     (:platform 40 20 79 21)
     (:platform 1 5 30 6)
     (:diamond 20 10)
     (:diamond 60 26)
     (:diamond 55 10))
     ;;7 rect dropping and sliding
     ((:diamond 40 5)
     (:diamond 35 15)     
     (:platform 0 0 80 1)
     (:platform 0 39 80 40)
     (:platform 0 1 1 40)
     (:platform 79 1 80 40)
     (:platform 25 10 70 11)
     (:platform 10 20 40 21)
     (:disc 5 5)
     (:rect 30 25))
    ; 24 Level 5 endgegner des lernens Fahrstuhl fähigkeit notwendig
    ((:disc 10 3)  
     (:rect 15 3)
     (:platform 0 0 80 1)  
     (:platform 0 39 80 40) 
     (:platform 0 1 1 40)  
     (:platform 79 1 80 40) 
     (:platform 40 1 41 20)  
     (:platform 40 20 79 21) 
     (:diamond 70 35)) 
    )
  "level description for simple test levels")
