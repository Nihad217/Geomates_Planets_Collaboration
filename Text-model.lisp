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

(chunk-type goal state intention)

  (add-dm
   (rect) (disc) (heard)
   (move-left) (move-right)
   (move-up)  (move-down)
   (w) (a) (s) (d) 
   (next-step)
   (something-should-change)
   (i-want-to-do-something)
   (first-goal isa goal state i-want-to-do-something)

   )

  (goal-focus first-goal)
  



(p want-to-move
  =goal>
    state i-want-to-do-something
  ?retrieval>
    state free
==>
  =goal>
    state something-should-change

  ;; retrieve the control chunk that matches the intention
  +retrieval>
    isa control
)


;; Move in direction and vocalize direction
  (p move-up
     =goal>
     state something-should-change
     intention move-up
     =retrieval>
     button =button
    ?manual>
     state free
 ==>
     =goal>
     state next-step
     +manual>
     cmd press-key
     key =button
   )
     

  (p retrieval-failure
     =goal>
     state something-should-change
     ?retrieval>
     buffer failure
==>
     =goal>
        state next-step
     )
  
  (p next-step
     =goal>
     state next-step
     ?manual>
     state free
==>
     =goal>
     state i-want-to-do-something
     intention move
     )
     
)
  