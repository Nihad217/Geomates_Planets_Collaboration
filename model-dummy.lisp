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

#+SBCL (require :sb-bsd-sockets)

(defvar *planner-socket* nil)
(defvar *planner-stream* nil)

(defun planner-connect ()
  (unless (and *planner-stream* (open-stream-p *planner-stream*))
    (setf *planner-socket*
          (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
    (sb-bsd-sockets:socket-connect *planner-socket* #(127 0 0 1) 5005)
    (setf *planner-stream*
          (sb-bsd-sockets:socket-make-stream *planner-socket*
                                             :input t :output t :element-type :default))))

(defun planner-next-intention (x y gx gy)
  (planner-connect)
  (format *planner-stream* "Self_POS ~a ~a GOAL ~a ~a~%" x y gx gy)
  (finish-output *planner-stream*)
  (let ((resp (string-trim '(#\Return #\Newline #\Space) (read-line *planner-stream*))))
    (cond ((string= resp "LEFT")  'move-left)
          ((string= resp "RIGHT") 'move-right)
          ((string= resp "UP")    'move-up)
          ((string= resp "DOWN")  'move-down)
          (t 'move-right))))



  ;; [find explanation in actr7.x/examples/vision-module]
  ;;(chunk-type (polygon-feature (:include visual-location)) regular)
  ;;(chunk-type (polygon (:include visual-object)) sides)
  
  ;; [see definition in vision module]
  ;;(chunk-type (oval (:include visual-object)) (oval t))
  
  ;; [might be obsolete] Do this to avoid the warnings when the chunks are created
  (define-chunks true false polygon)
  
  ;; [might be obsolete] stuff the leftmost item
  (set-visloc-default screen-x lowest)

  (chunk-type goal state intention x y goal-x goal-y) 
  (chunk-type control intention button speakname)

  (add-dm
   (rect) (disc) (heard)
   (move-left) (move-right)
   (move-up)  (move-down)
   (w) (a) (s) (d) 
   (go-up)
   (something-should-change)
   (i-want-to-do-something)
   (up-control    isa control intention move-up    button w speakname "move up")
   (down-control  isa control intention move-down  button s speakname "move down")
   (left-control  isa control intention move-left  button a speakname "move left")
   (right-control isa control intention move-right button d speakname "move right")
   (first-goal isa goal 
            
            state i-want-to-do-something
             x 10
             y 23
             goal-x 20
             goal-y 26))

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
     x =x
     y =y
     goal-x =gx
     goal-y =gy
==>
   !bind! =intention
     (planner-next-intention =x =y =gx =gy)
   =goal>
     state something-should-change
     intention =intention)

(p retrieve-control
   =goal>
     state something-should-change
     intention =i
==>
   +retrieval>
     isa control
     intention =i)


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
     state go-right 
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
  
  (p move-right
     =goal>
      state go-right
     ?manual>
      state free
==> 
     =goal>
      state i-want-to-do-something
      intention move-right
     ) 



     
)
  