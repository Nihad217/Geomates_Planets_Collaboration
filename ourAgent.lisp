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
             oval2-x oval2-y
             rect2-x rect2-y
             self
             self-x self-y
             target-x target-y
             jumped
             diamond-x diamond-y
             diamond1-x diamond1-y
             diamond2-x diamond2-y
             diamond3-x diamond3-y
             diamond-count)
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
               diamond-count 0
               jumped nil)

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
       screen-x =x
       screen-y =y
    ?visual>
      state free
  ==>
     +visual>
       isa visual-object
       cmd move-attention
       screen-pos =visual-location
     =goal>
       intention save-oval
       oval-x =x
       oval-y =y)

  (p save-oval
     =goal>
       state scan-oval
       intention save-oval
    =visual>
      value "disc"
  ==>
     =goal>
       state scan-rect
       intention nil
)

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
       intention nil
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
      screen-x =x
      screen-y =y
    ?visual>
      state free
  ==>
     +visual>
       isa visual-object
       cmd move-attention
       screen-pos =visual-location
     =goal>
       intention save-rect
       rect-x =x
       rect-y =y)

  (p save-rect
     =goal>
       state scan-rect
       intention save-rect
     =visual>
       value "rect"
   ==>
     !eval! (clear-buffer 'visual-location)
     !eval! (clear-buffer 'visual)
     =goal>
       state scan-diamonds
       intention nil
)

  (p skip-rect-after-attend
     =goal>
       state scan-rect
       intention save-rect
     ?visual>
       buffer failure
  ==>
     !eval! (clear-buffer 'visual-location)
     !eval! (clear-buffer 'visual)
     =goal>
       state scan-diamonds
       intention nil)

  (p skip-rect-wrong-object
     =goal>
       state scan-rect
       intention save-rect
     =visual>
       - value "rect"
  ==>
     !eval! (clear-buffer 'visual-location)
     !eval! (clear-buffer 'visual)
     =goal>
       state scan-diamonds
       intention nil)

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
       intention nil
       diamond-count =n
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
       screen-x =x
       screen-y =y
    ?visual>
      state free
  ==>
     +visual>
       isa visual-object
       cmd move-attention
       screen-pos =visual-location
     =goal>
       intention save-diamonds
       diamond-x =x
       diamond-y =y)

  (p save-diamond-1
     =goal>
       state scan-diamonds
       intention save-diamonds
       diamond-x =x
       diamond-y =y
       diamond-count 0
    =visual>
      value "diamond"
  ==>
     !eval! (clear-buffer 'visual-location)
     !eval! (clear-buffer 'visual)
     =goal>
       state scan-diamonds
       intention nil
       diamond1-x =x
       diamond1-y =y
       diamond-count 1)

  (p save-diamond-2
     =goal>
       state scan-diamonds
       intention save-diamonds
       diamond-x =x
       diamond-y =y
       diamond-count 1
    =visual>
      value "diamond"
  ==>
     !eval! (clear-buffer 'visual-location)
     !eval! (clear-buffer 'visual)
     =goal>
       state scan-diamonds
       intention nil
       diamond2-x =x
       diamond2-y =y
       diamond-count 2)

  (p save-diamond-3
     =goal>
       state scan-diamonds
       intention save-diamonds
       diamond-x =x
       diamond-y =y
       diamond-count 2
    =visual>
      value "diamond"
  ==>
     !eval! (clear-buffer 'visual-location)
     !eval! (clear-buffer 'visual)
     =goal>
       state scan-diamonds
       intention nil
       diamond3-x =x
       diamond3-y =y
       diamond-count 3)

  (p skip-diamonds-wrong-object
     =goal>
       state scan-diamonds
       intention save-diamonds
     =visual>
       - value "diamond"
  ==>
     !eval! (clear-buffer 'visual-location)
     !eval! (clear-buffer 'visual)
     =goal>
       state scan-diamonds
       intention nil)

  (p skip-diamonds
     =goal>
       state scan-diamonds
       intention attend-diamonds
    ?visual-location>
      buffer failure
  ==>
     =goal>
       state do-move
       intention nil)

  (p done-diamonds
     =goal>
       state scan-diamonds
       intention nil
       diamond-count 3
  ==>
     =goal>
       state do-move
       intention nil)

  ;; Einmal bewegen, dann erneut scannen
  (p move-once
     =goal>
       state do-move
    ?manual>
      state free
  ==>
    +manual>
      cmd press-key
      key d
    =goal>
      state after-move)

  (p after-move
     =goal>
       state after-move
     ?manual>
       state free
  ==>
     =goal>
       state scan-oval-after
       intention nil)

  ;; OVAL nach der Bewegung
  (p scan-oval-after
     =goal>
       state scan-oval-after
       intention nil
     ?visual-location>
       state free
  ==>
     +visual-location>
       oval t
       color yellow
       :attended nil
     =goal>
       intention attend-oval-after)

  (p attend-oval-after
     =goal>
       state scan-oval-after
       intention attend-oval-after
     =visual-location>
       oval t
       screen-x =x
       screen-y =y
     ?visual>
       state free
  ==>
     +visual>
       isa visual-object
       cmd move-attention
       screen-pos =visual-location
     =goal>
       intention save-oval-after
       oval2-x =x
       oval2-y =y)

  (p save-oval-after
     =goal>
       state scan-oval-after
       intention save-oval-after
    =visual>
      value "disc"
  ==>
     !eval! (clear-buffer 'visual-location)
     !eval! (clear-buffer 'visual)
     =goal>
       state scan-rect-after
       intention nil
)

  (p skip-oval-after-wrong-object
     =goal>
       state scan-oval-after
       intention save-oval-after
     =visual>
       - value "disc"
  ==>
     !eval! (clear-buffer 'visual-location)
     !eval! (clear-buffer 'visual)
     =goal>
       state scan-rect-after
       intention nil)

  (p skip-oval-after
     =goal>
       state scan-oval-after
       intention attend-oval-after
     ?visual-location>
       buffer failure
  ==>
     =goal>
       state scan-rect-after
       intention nil)

  ;; RECT nach der Bewegung
  (p scan-rect-after
     =goal>
       state scan-rect-after
       intention nil
     ?visual-location>
       state free
  ==>
     +visual-location>
       kind polygon
       value "rect"
       color red
       :attended nil
     =goal>
       intention attend-rect-after)

  (p attend-rect-after
     =goal>
       state scan-rect-after
       intention attend-rect-after
     =visual-location>
       kind polygon
       screen-x =x
       screen-y =y
    ?visual>
      state free
  ==>
     +visual>
       isa visual-object
       cmd move-attention
       screen-pos =visual-location
     =goal>
       intention save-rect-after
       rect2-x =x
       rect2-y =y)

  (p save-rect-after
     =goal>
       state scan-rect-after
       intention save-rect-after
     =visual>
       value "rect"
   ==>
     !eval! (clear-buffer 'visual-location)
     !eval! (clear-buffer 'visual)
     =goal>
       state decide-self
       intention nil
)

  (p skip-rect-after
     =goal>
       state scan-rect-after
       intention attend-rect-after
     ?visual-location>
       buffer failure
  ==>
     =goal>
       state decide-self
       intention nil)

  ;; Wer bin ich?
  (p decide-self
     =goal>
       state decide-self
       oval-x =ox1
       oval-y =oy1
       oval2-x =ox2
       oval2-y =oy2
       rect-x =rx1
       rect-y =ry1
       rect2-x =rx2
       rect2-y =ry2
  ==>
     !bind! =who (if (or (/= =ox1 =ox2) (/= =oy1 =oy2)) 'disc 'rect)
     =goal>
       self =who
       state pick-target
     !output! ("I am the ~a" =who))

  ;; Ziel-Diamond wählen (nächstgelegener)
  (p pick-target-disc
     =goal>
       state pick-target
       self disc
       oval2-x =sx
       oval2-y =sy
       diamond1-x =d1x
       diamond1-y =d1y
       diamond2-x =d2x
       diamond2-y =d2y
       diamond3-x =d3x
       diamond3-y =d3y
  ==>
     !bind! =d1 (+ (* (- =sx =d1x) (- =sx =d1x)) (* (- =sy =d1y) (- =sy =d1y)))
     !bind! =d2 (+ (* (- =sx =d2x) (- =sx =d2x)) (* (- =sy =d2y) (- =sy =d2y)))
     !bind! =d3 (+ (* (- =sx =d3x) (- =sx =d3x)) (* (- =sy =d3y) (- =sy =d3y)))
     !bind! =tx (if (< =d1 =d2)
                    (if (< =d1 =d3) =d1x =d3x)
                    (if (< =d2 =d3) =d2x =d3x))
     !bind! =ty (if (< =d1 =d2)
                    (if (< =d1 =d3) =d1y =d3y)
                    (if (< =d2 =d3) =d2y =d3y))
     =goal>
       self-x =sx
       self-y =sy
       target-x =tx
       target-y =ty
       state scan-self
       intention nil
     !output! ("Target diamond at (~a, ~a)" =tx =ty))

  (p pick-target-rect
     =goal>
       state pick-target
       self rect
       rect2-x =sx
       rect2-y =sy
       diamond1-x =d1x
       diamond1-y =d1y
       diamond2-x =d2x
       diamond2-y =d2y
       diamond3-x =d3x
       diamond3-y =d3y
  ==>
     !bind! =d1 (+ (* (- =sx =d1x) (- =sx =d1x)) (* (- =sy =d1y) (- =sy =d1y)))
     !bind! =d2 (+ (* (- =sx =d2x) (- =sx =d2x)) (* (- =sy =d2y) (- =sy =d2y)))
     !bind! =d3 (+ (* (- =sx =d3x) (- =sx =d3x)) (* (- =sy =d3y) (- =sy =d3y)))
     !bind! =tx (if (< =d1 =d2)
                    (if (< =d1 =d3) =d1x =d3x)
                    (if (< =d2 =d3) =d2x =d3x))
     !bind! =ty (if (< =d1 =d2)
                    (if (< =d1 =d3) =d1y =d3y)
                    (if (< =d2 =d3) =d2y =d3y))
     =goal>
       self-x =sx
       self-y =sy
       target-x =tx
       target-y =ty
       state scan-self
       intention nil
     !output! ("Target diamond at (~a, ~a)" =tx =ty))

  ;; Eigene Position erneut scannen (für Bewegungsschleife)
  (p scan-self-disc
     =goal>
       state scan-self
       self disc
       intention nil
    ?visual-location>
      state free
  ==>
     +visual-location>
       oval t
       color yellow
    =goal>
       intention attend-self-disc)

  (p scan-self-rect
     =goal>
       state scan-self
       self rect
       intention nil
    ?visual-location>
      state free
  ==>
     +visual-location>
       kind polygon
       color red
    =goal>
       intention attend-self-rect)

  (p attend-self-disc
     =goal>
       state scan-self
       intention attend-self-disc
     =visual-location>
       oval t
       color yellow
       screen-x =x
       screen-y =y
     ?visual>
       state free
  ==>
     +visual>
       isa visual-object
       cmd move-attention
       screen-pos =visual-location
     =goal>
       self-x =x
       self-y =y
       intention decide-move)

  (p attend-self-rect
     =goal>
       state scan-self
       intention attend-self-rect
    =visual-location>
       kind polygon
       color red
       screen-x =x
       screen-y =y
    ?visual>
      state free
  ==>
     +visual>
       isa visual-object
       cmd move-attention
       screen-pos =visual-location
     =goal>
      self-x =x
      self-y =y
      intention decide-move)

  (p skip-self-loc-failure
     =goal>
       state scan-self
       intention attend-self-disc
    ?visual-location>
      buffer failure
  ==>
     =goal>
       state scan-self
       intention nil)

  (p skip-self-loc-failure-rect
     =goal>
       state scan-self
       intention attend-self-rect
     ?visual-location>
       buffer failure
  ==>
     =goal>
       state scan-self
       intention nil)

  (p decide-move
     =goal>
       state scan-self
       intention decide-move
       self =who
       self-x =sx
       self-y =sy
       target-x =tx
       target-y =ty
  ==>
     !bind! =dx (abs (- =sx =tx))
     !bind! =dy (abs (- =sy =ty))
     !bind! =xclose (if (<= =dx 2.0) 'true 'false)
     !bind! =yclose (if (<= =dy 0.2) 'true 'false)
     !bind! =dir (if (eq =who 'disc)
                     (if (and (< (abs (- =sx =tx)) 0.5) (< (abs (- =sy =ty)) 0.5))
                         'at-target
                         (if (< =sy =ty)
                             'move-up
                             (if (< =sx =tx) 'move-right 'move-left)))
                     (if (eq =xclose 'true)
                         (if (eq =yclose 'true) 'at-target
                             (if (< =sy =ty) 'move-up 'at-target))
                         (if (< =sx =tx) 'move-right 'move-left)))
     =goal>
       state move-to-target
       intention =dir)

  (p move-right-to-target
     =goal>
       state move-to-target
       intention move-right
     ?manual>
       state free
  ==>
     +manual>
       cmd press-key
       key d
     =goal>
       state scan-self
       intention nil)

  (p move-left-to-target
     =goal>
       state move-to-target
       intention move-left
    ?manual>
      state free
  ==>
    +manual>
      cmd press-key
      key a
    =goal>
      state scan-self
      intention nil)

  (p move-up-to-target
     =goal>
       state move-to-target
       intention move-up
       self rect
     ?manual>
       state free
  ==>
     +manual>
       cmd press-key
       key w
     =goal>
       state scan-self
       intention nil)

  (p move-up-to-target-disc
     =goal>
       state move-to-target
       intention move-up
       self disc
     ?manual>
       state free
  ==>
     +manual>
       cmd press-key
       key w
     =goal>
       state rescan-diamonds
       intention nil
       diamond-count 0
       diamond1-x nil
       diamond1-y nil
       diamond2-x nil
       diamond2-y nil
       diamond3-x nil
       diamond3-y nil)

  (p at-target
     =goal>
       state move-to-target
       intention at-target
  ==>
     =goal>
       state rescan-diamonds
       intention nil
       diamond-count 0
       diamond1-x nil
       diamond1-y nil
       diamond2-x nil
       diamond2-y nil
       diamond3-x nil
       diamond3-y nil)

  ;; Diamonds nach Einsammeln neu zählen und speichern
  (p scan-diamonds-rescan
     =goal>
       state rescan-diamonds
       intention nil
     ?visual-location>
       state free
  ==>
     +visual-location>
       kind polygon
       value "diamond"
       color orange
       :attended nil
     =goal>
       intention attend-diamonds-rescan)

  (p attend-diamonds-rescan
     =goal>
       state rescan-diamonds
       intention attend-diamonds-rescan
     =visual-location>
       kind polygon
       screen-x =x
       screen-y =y
     ?visual>
       state free
  ==>
     +visual>
       isa visual-object
       cmd move-attention
       screen-pos =visual-location
     =goal>
       intention save-diamonds-rescan
       diamond-x =x
       diamond-y =y)

  (p save-diamond-rescan-1
     =goal>
       state rescan-diamonds
       intention save-diamonds-rescan
       diamond-x =x
       diamond-y =y
       diamond-count 0
     =visual>
       value "diamond"
  ==>
     !eval! (clear-buffer 'visual-location)
     !eval! (clear-buffer 'visual)
     =goal>
       state rescan-diamonds
       intention nil
       diamond1-x =x
       diamond1-y =y
       diamond-count 1)

  (p save-diamond-rescan-2
     =goal>
       state rescan-diamonds
       intention save-diamonds-rescan
       diamond-x =x
       diamond-y =y
       diamond-count 1
     =visual>
       value "diamond"
  ==>
     !eval! (clear-buffer 'visual-location)
     !eval! (clear-buffer 'visual)
     =goal>
       state rescan-diamonds
       intention nil
       diamond2-x =x
       diamond2-y =y
       diamond-count 2)

  (p save-diamond-rescan-3
     =goal>
       state rescan-diamonds
       intention save-diamonds-rescan
       diamond-x =x
       diamond-y =y
       diamond-count 2
     =visual>
       value "diamond"
  ==>
     !eval! (clear-buffer 'visual-location)
     !eval! (clear-buffer 'visual)
     =goal>
       state rescan-diamonds
       intention nil
       diamond3-x =x
       diamond3-y =y
       diamond-count 3)

  (p skip-diamonds-rescan
     =goal>
       state rescan-diamonds
       intention attend-diamonds-rescan
    ?visual-location>
      buffer failure
  ==>
     =goal>
       state rescan-diamond-try3
       intention nil)

  ;; Falls nach Rescan nur 2 Diamonds gefunden wurden, einmal gezielt nach einem dritten suchen
  (p scan-diamond-try3
     =goal>
       state rescan-diamond-try3
       diamond-count 2
     ?visual-location>
       state free
  ==>
     +visual-location>
       kind polygon
       value "diamond"
       color orange
       screen-x highest
     =goal>
       intention attend-diamond-try3)

  (p attend-diamond-try3
     =goal>
       state rescan-diamond-try3
       intention attend-diamond-try3
     =visual-location>
       kind polygon
       screen-x =x
       screen-y =y
     ?visual>
       state free
  ==>
     +visual>
       isa visual-object
       cmd move-attention
       screen-pos =visual-location
     =goal>
       intention save-diamond-try3
       diamond-x =x
       diamond-y =y)

  (p save-diamond-try3
     =goal>
       state rescan-diamond-try3
       intention save-diamond-try3
       diamond-x =x
       diamond-y =y
       diamond-count 2
     =visual>
       value "diamond"
  ==>
     !eval! (clear-buffer 'visual-location)
     !eval! (clear-buffer 'visual)
     =goal>
       state report-remaining
       intention nil
       diamond3-x =x
       diamond3-y =y
       diamond-count 3)

  (p skip-diamond-try3
     =goal>
       state rescan-diamond-try3
       intention attend-diamond-try3
     ?visual-location>
       buffer failure
  ==>
     =goal>
       state report-remaining
       intention nil)

  (p skip-diamond-try3-not-needed
     =goal>
       state rescan-diamond-try3
       diamond-count =n
  ==>
     =goal>
       state report-remaining
       intention nil)

  (p report-remaining-diamonds
     =goal>
       state report-remaining
       diamond-count =n
  ==>
     !output! ("Diamonds remaining: ~a" =n)
     =goal>
       state scan-self-for-target
       intention nil
       jumped nil)

  (p no-diamonds-left
     =goal>
       state report-remaining
       diamond-count 0
  ==>
     !output! ("No diamonds remaining.")
     =goal>
       state done)

)



  
