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

  ;; [find explanation in actr7.x/examples/vision-module]
  (chunk-type (polygon-feature (:include visual-location)) regular)
  (chunk-type (polygon (:include visual-object)) sides)
  
  ;; [see definition in vision module]
  ;;(chunk-type (oval (:include visual-object)) (oval t))
  
  ;; [might be obsolete] Do this to avoid the warnings when the chunks are created
  (define-chunks true false polygon)
  
  ;; [might be obsolete] stuff the leftmost item
  (set-visloc-default screen-x lowest)

  (chunk-type goal state intention)
  (chunk-type control intention button speakname)

  (add-dm
   (rect) (disc)
   (move-left) (move-right)
   (move-up)  (move-down)
   (w) (a) (s) (d)
   (i-dont-know-where-to-go)
   (something-should-change)
   (i-want-to-do-something)
   (up-control    isa control intention move-up    button w speakname "move up")
   (down-control  isa control intention move-down  button s speakname "move down")
   (left-control  isa control intention move-left  button a speakname "move left")
   (right-control isa control intention move-right button d speakname "move right")
   (first-goal isa goal state i-dont-know-where-to-go)
   )

  (goal-focus first-goal)
  

;; Detect und Identify sounds in the enviroment!

   ;; Step 1: detect new audio event and move to aural buffer
(p detected-sound-direct
   =aural-location>
     isa      audio-event
     location =who
   ?aural>
     state    free
   ==>
   !output! ("I heard ~a~%" =who)
   +aural>
     isa      sound
     event    =aural-location)


   ;; Step 2: hear the sound and print it
(p hear-direct
   =aural>
     isa     sound
     content =x
   ==>
   !output! ("I heard say ~a~%" =x)
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
     state i-dont-know-where-to-go
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
        state i-dont-know-where-to-go
     )
  
  (p maybe-left
     =goal>
     state i-dont-know-where-to-go
     ?manual>
     state free
==>
     =goal>
     state i-want-to-do-something
     intention move-left
     )
  
  (p maybe-right
    =goal>
     state i-dont-know-where-to-go
     ?manual>
     state free
==>
     =goal>
     state i-want-to-do-something
     intention move-right
)
  
  (p maybe-down
     =goal>
     state i-dont-know-where-to-go
     ?manual>
     state free
==>
     =goal>
        state i-want-to-do-something
     intention move-down
     )
  
  (p maybe-up
     =goal>
     state i-dont-know-where-to-go
     ?manual>
     state free
==>
    =goal>
     state i-want-to-do-something
     intention move-up
     )
  
  )
