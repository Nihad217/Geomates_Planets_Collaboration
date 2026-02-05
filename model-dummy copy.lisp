;;;
;;; example dummy agent
;;;  

(clear-all)

;;; in the agent, arbitrary helper functions may be defined
;;; using Common Lisp, but also all add-ons of the SBCL lisp
;;; system, in particular loading shared libraries and calling
;;; functions in those libraries.
;;; For details, see SBCL manual regarding its alien function interace
;;; or have a look into geomates.lisp which connects to a C library
;;;
;;; Additionally, you can use run-program to call any external software.
;;; Note that the process will be run in a null environment by default, so
;;; all pathnames must be explicit. To handle different locations, a simple
;;; "or" may be all it takes:

(defparameter *my-ls* (or (probe-file "/bin/ls")
			  (probe-file "/usr/bin/ls")
			  (probe-file "some/path"))
  "binds to the first file that exists")

(defun count-entries ()
  "counts the number of files/directories in the root directory"
  (count #\Newline ; just count linebreaks since after printing a name, ls prints a newline
	 (with-output-to-string (result) ; temporary string output stream
	   (run-program (probe-file "/bin/ls") (list "/") :output result))))
      
;;; In case you need to differentiate different environments/OS/compilers:
;;; have a look at Common-Lisps reader macros #+/#- (like #ifdef in C),
;;; which refer to the global variable *features*
;;; examples:
;;; #+SBCL (print "I'm running the SBCL compiler")
;;; (defparameter *magic-code* #+LITTLE-ENDIAN #x0f12 #-LITTLE-ENDIAN 0x120f)


;;;
;;; Now comes the core Act-R agent
;;;


(define-model lost-agent

  ;; ACT-R parameters to choose production rules randomly if more than one is matched 
  (sgp :egs 0.2)
  (sgp :ans 0.5 :lf 0.2)
  (sgp :esc t)


  ;; [find explanation in actr7.x/examples/vision-module]
  (chunk-type (polygon-feature (:include visual-location)) regular)
  (chunk-type (polygon (:include visual-object)) sides)
  (define-chunks true false polygon)

  
  ;; [see definition in vision module]
  (chunk-type (oval (:include visual-object)) radius)

  
  ;; [might be obsolete] Do this to avoid the warnings when the chunks are created
  (define-chunks true false polygon)
  
  ;; [might be obsolete] stuff the leftmost item
  (set-visloc-default screen-x lowest)

  (chunk-type goal state intention
             oval-x oval-y
             rect-x rect-y
             diamond-locs)
  (chunk-type control intention button speakname)
  (chunk-type rectangle rect-x rect-y)
  (chunk-type disc disc-x disc-y)

  (add-dm
   (rect) (disc) (heard)
   (move-left) (move-right)
   (move-up)  (move-down)
   (w) (a) (s) (d) 
   (go-up)
   (something-should-change)
   (i-want-to-do-something) 
   (self-info-after-jump)
   (disc-x)
   (disc-y)
   (rect-x)
   (rect-y)
   (up-control    isa control intention move-up    button w speakname "move up")
   (down-control  isa control intention move-down  button s speakname "move down")
   (left-control  isa control intention move-left  button a speakname "move left")
   (right-control isa control intention move-right button d speakname "move right")
   (first-goal isa goal
               state wait-for-scene
               diamond-locs nil)

   )

  (goal-focus first-goal)
  


;; Detect und Identify sounds in the enviroment!
;; Step 1: detect new audio event and move to aural buffer
(p detected-sound
   =aural-location>
     isa      audio-event
     location =who
   ?aural>
     state    free
   ==>
   +aural>
     isa      sound
     event    =aural-location
   +imaginal>
     heard    =who)

;; Step 2: hear the sound and print it
(p hear
   =aural>
     isa     sound
     content =x
   =imaginal>
     heard   =who
   ==>
   !output! ("I heard ~a say: ~a~%" =who =x)
   )


(p want-to-move
  =goal>
    state i-want-to-do-something
    intention =intention
  ?retrieval>
    state free
==>
  =goal>
    state something-should-change

  ;; retrieve the control chunk that matches the intention
  +retrieval>
    isa control
    intention =intention
)


;; Move in direction and vocalize direction
  (p move
     =goal>
     state something-should-change
     =retrieval>
     button =button
     speakname =text
    ?manual>
     state free
 ==>
     =goal>
     state go-up
     +manual>
     cmd press-key
     key =button
     +vocal>
     isa speak
     string =text
   )
     

  (p retrieval-failure
     =goal>
     state something-should-change
     ?retrieval>
     buffer failure
==>
     =goal>
        state go-up
     )
  
  (p move-up
     =goal>
     state go-up
     ?manual>
     state free
==>
     =goal>
     state i-want-to-do-something
     intention move-up
     )

;; Warten, bis das Visicon befüllt ist
  (p wait-for-scene
     =goal>
       state wait-for-scene
       intention nil
     ?visual-location>
       state free
  ==>
     +visual-location>
       :attended nil
     =goal>
       intention scene-requested)

  (p scene-ready
     =goal>
       state wait-for-scene
       intention scene-requested
     =visual-location>
       screen-x =x
  ==>
     !eval! (clear-buffer 'visual-location)
     !eval! (clear-buffer 'visual)
     =goal>
       state scan-oval
       intention nil)

;; Alle sichtbaren Objekte nacheinander über visual-location erfassen
  ;; DISC (oval)
  (p scan-oval
     =goal>
       state scan-oval
     ?visual-location>
       state free
  ==>
     +visual-location>
       oval t
       color yellow
       :attended nil
     =goal>
       intention attend-oval)

  (p attend-oval
     =goal>
       state scan-oval
       intention attend-oval
     =visual-location>
       oval t
     ?visual>
       state free
  ==>
     +visual>
       isa visual-object
       cmd move-attention
       screen-pos =visual-location
     =goal>
       intention save-oval)

  (p save-oval
     =goal>
       state scan-oval
       intention save-oval
     =visual>
       value "disc"
       screen-x =x
       screen-y =y
  ==>
     =goal>
       state scan-rect
       intention nil
       oval-x =x
       oval-y =y)

  (p skip-oval
     =goal>
       state scan-oval
       intention attend-oval
    ?visual-location>
      buffer failure
  ==>
     =goal>
       state scan-rect
       intention nil)

  ;; Platform-Suche entfernt (soll nicht geladen werden)

  (p scan-rect
     =goal>
       state scan-rect
    ?visual-location>
      state free
  ==>
     +visual-location>
       kind polygon
       value "rect"
       color red
       :attended nil
    =goal>
      intention attend-rect)

  (p attend-rect
     =goal>
       state scan-rect
       intention attend-rect
     =visual-location>
       kind polygon
     ?visual>
       state free
  ==>
     +visual>
       isa visual-object
       cmd move-attention
       screen-pos =visual-location
     =goal>
       intention save-rect)

  (p save-rect
     =goal>
       state scan-rect
       intention save-rect
     =visual>
       value "rect"
       screen-x =x
       screen-y =y
  ==>
     =goal>
       state scan-diamonds
       intention nil
       rect-x =x
       rect-y =y)

  (p skip-rect
     =goal>
       state scan-rect
       intention attend-rect
     ?visual-location>
       buffer failure
  ==>
     =goal>
       state scan-diamonds
       intention nil)

  (p scan-diamonds
     =goal>
       state scan-diamonds
    ?visual-location>
      state free
  ==>
     +visual-location>
       kind polygon
       value "diamond"
       color orange
       :attended nil
    =goal>
      intention attend-diamonds)

  (p attend-diamonds
     =goal>
       state scan-diamonds
       intention attend-diamonds
     =visual-location>
       kind polygon
     ?visual>
       state free
  ==>
     +visual>
       isa visual-object
       cmd move-attention
       screen-pos =visual-location
     =goal>
       intention save-diamonds)

  (p save-diamonds
     =goal>
       state scan-diamonds
       intention save-diamonds
       diamond-locs =dlocs
     =visual>
       value "diamond"
       screen-x =x
       screen-y =y
  ==>
     !bind! =new-dlocs (cons (list =x =y) =dlocs)
     =goal>
       state scan-diamonds
       intention nil
       diamond-locs =new-dlocs)

  (p skip-diamonds
     =goal>
       state scan-diamonds
       intention attend-diamonds
     ?visual-location>
       buffer failure
  ==>
     =goal>
       state go-up
       intention nil)

)



  
