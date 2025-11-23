;;;
;;; clone of the 'geometry friends' competition
;;;
;;; This clone provides socket connections to agents and a web browser for GUI (graphics output only so far).
;;; World simulation is performed using a dynamic library that wraps calls to the box2d library (which only comes as static
;;; library, unfortunately). You need to build the wrapper library and add the path to your library at the beginning of this file.
;;; Be sure to leave existing "probe-files" untouched so this program will work in different environments. 
;;; Agent and GUI connect to the ports defined above. You may change the ports as you like. The assignment of agents
;;; to disc and rect player is performed randomly; each agent is informed which player it controls.
;;; Agents interact with the game by sending simple text data via the TCP/IP socket connection:
;;; a : move left
;;; d : move right
;;; w : jump (disc player only)
;;; w : strech horizontally (rect player only)
;;; s : compress horizonztally (rect player only)
;;; q : request to quit this level (quitting only occurs when both agents have requested to quit)
;;; m(...) : send message (...) in KIF format, i.e., a symbolic expression to other agent
;;;
;;; You can use "telnet" in the command line to connect to the game server and play an agent yourself.
;;; To this end, the game server sends some commands (according to RFC 854) to connected agent/terminal that
;;; switch a telnet client into byte mode in order to send a packet immediately after a key is pressed.
;;; In your agent implementation you should therefore ignore the first 6 bytes received in case your
;;; agent gets confused with receiving non-printable characters. Sending the telnet commands can be
;;; disabled in "await-agent-connections", but then you would need to configure telnet each time manually
;;; to switch to byte mode first in order to use it.
;;;
;;; Levels are defined in a global variable *levels* as list of levels. Each level consists out of objects
;;; given as s-expressions. The coordinate system is x to the right, positive  is up. Unit size is meter.
;;; Levels should be 80 x 40 in size to fit the graphical output. 
;;; Format and semantics are as follows:
;;; (:rect x y) starting position of the rect, the rect player starts with initial width according to +rect-length+ (2m)
;;; (:disc x y) starting position of the disc, the disc is of radius +disc-radius+ (1.0m)
;;; (:platform x1 y1 x2 y2) defines a rectangular platform with corners (x1 y1) and (x2 y2)
;;;                         be nice and add a ground plane and side walls to avoid your players falling off accidentally
;;; (:diamond x y) diamond to collect: each level must contain at least one diamand as a level is said to be finished if
;;;                no diamonds are left
;;; 

(eval-when (:execute :load-toplevel :compile-toplevel)
  (load "base64.lisp")
  (load "sha1.lisp")
  (load "levels.lisp")
  ;; (require :sb-alien) ;; not needed in modern SBCL (uncomment only if sb-alien module is unknown)
  (require :sb-bsd-sockets)  ;; for networking
  (require :sb-concurrency)) ;; for lock-free threadsafe queues


(defparameter *path-to-wrapper-library* (or (probe-file "wrapper.so")
					    (probe-file "wrapper.dll")
					    (probe-file "ADD/YOUR/PATH/HERE"))
  "path to the shared library that wraps around the static box2d library")

(defparameter *agent-port* 45678
  "ports on which agents connect to the game")

(defparameter *gui-port* 8000
  "port on which the browser GUI connects to the game")

;;;
;;; Do not change parameters below except for your own experiments
;;;

(defparameter +rect-length+ 4.0f0
  "initial width of the rect shape as single float")

(defparameter +rect-force+ 30.0f0
  "force for moving rect (needs to be adjusted for different rect sizes)")

(defparameter +disc-radius+ 1.5f0
  "radius of the disc shape as single float")

(defparameter +disc-force+ 80.0f0
  "force for moving disc (needs to be adjusted for different disc sizes)")

(defparameter +announce-who-am-i+ t
  "whether or not to inform agents which character they are controlling")

(defun initialize-box2d ()
  (sb-alien:load-shared-object *path-to-wrapper-library*))

;; foreign function interface for linking to box2d wrapper
;; functions and types from C library
(define-alien-type pose (* (struct bodyPose (x float) (y float) (r float) (w float) (h float))))
(define-alien-routine "initWorld" void (gravity-x float) (gravity-y float))
(define-alien-routine "destroyWorld" void)
(define-alien-routine "initPlayers" void (rect-x float) (rect-y float) (rect-size float) (rect-ratio float) (rect-density float) (rect-friction float) (disc-x float) (disc-y float) (disc-size float) (disc-density float) (disc-friction float))
(define-alien-routine "worldInsertPlatform" void (pos-x float) (pos-y float) (size-x float) (size-y float))
(define-alien-routine "stepWorld" void)
(define-alien-routine "getRectPlayerPose" pose)
(define-alien-routine "getDiscPlayerPose" pose)
(define-alien-routine "moveDiscPlayer" void (force float))
(define-alien-routine "jumpDiscPlayer" void (force float))
(define-alien-routine "moveRectPlayer" void (force float))
(define-alien-routine "transformRectPlayer" void (dir float))
(define-alien-routine "pointInRectPlayer" int (x float) (y float))

(defun setup-level (level)
  "sets up the box2d simulation environment for a given level description, returns list of diamonds"
  (let ((disc (assoc :disc level))
	(rect (assoc :rect level))
	(platforms (remove-if-not #'(lambda (entry) (eql (first entry) :platform)) level))
	(diamonds  (mapcar #'(lambda (d) ; cast to single-floats for box2d 
			       (list :diamond (coerce (second d) 'float) (coerce (third d) 'float)))
			   (remove-if-not #'(lambda (entry) (eql (first entry) :diamond)) level))))
    ;; sanity checking
    (unless diamonds
      (error "no diamonds in level"))
    (initworld 0.0f0 -10.0f0)
    (initplayers (coerce (second rect) 'single-float) ; starting position rectangle x/y
		 (coerce (third rect) 'single-float)
		 (coerce +rect-length+ 'single-float)
		 4.0f0 ; ratio
		 1.0f0 ; density
		 0.1f0 ; friction
		 (coerce (second disc) 'single-float) ; starting position disc x/y
		 (coerce (third disc) 'single-float)
		 +disc-radius+ ; radius
		 1.0f0 ; density
		 0.3f0); friction
    (loop for (pf x1 y1 x2 y2) in platforms do
      (worldinsertplatform (coerce x1 'single-float) (coerce y1 'single-float) (coerce x2 'single-float) (coerce y2 'single-float)))
    (values diamonds platforms)))

(defun act-r-scene-format (scene-string)
  "Transforms the scene-string into the correct ACT-R format."
  (let ((scene (read-from-string scene-string))) ;; Convert string to list
    (setf scene
          (mapcar (lambda (item)
                    (case (car item)
                      (:RECT (destructuring-bind (x y w h r d) (cdr item)
                               `(:RECT SCREEN-X ,x SCREEN-Y ,y WIDTH ,w HEIGHT ,h ROTATION ,r DIAMONDS ,d)))
                      (:DISC (destructuring-bind (x y r d) (cdr item)
                               `(:DISC SCREEN-X ,x SCREEN-Y ,y RADIUS ,r DIAMONDS ,d)))
                      (:DIAMOND (destructuring-bind (x y) (cdr item)
                                 `(:DIAMOND SCREEN-X ,x SCREEN-Y ,y)))
                      (:PLATFORM (destructuring-bind (x1 y1 x2 y2) (cdr item)
                                  `(:PLATFORM SCREEN-X ,x1 SCREEN-Y ,y1 WIDTH ,(- x2 x1) HEIGHT ,(- y2 y1))))
                      (t item))) scene)) ;; Apply transformation to each item in the scene
    
    ;; Now convert the modified scene list back into a string
    (format nil "(~{~a~^ ~})" scene))) ;; Format the list as a string
;;
;; TCP/IP connections to agents 
;;

(defun accept (socket)
  "Like socket-accept, but retry on EAGAIN."
  (loop (handler-case
            (return (sb-bsd-sockets:socket-accept socket))
          (sb-bsd-sockets:interrupted-error ()))))

(defun await-agent-connections (port &optional (telnet2character-mode t))
  "returns two streams once two agents have connected to the given TCP port"
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-tcp-nodelay socket)   t
          (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket #(0 0 0 0) port)
    (sb-bsd-sockets:socket-listen socket 3)

    (let* ((client1 (accept socket))
           (stream (sb-bsd-sockets:socket-make-stream client1 :input t :output t :element-type :default)))
      (format t "~&Agent 1 connected.~%")

      (let* ((client2 (accept socket))
             (stream2 (sb-bsd-sockets:socket-make-stream client2 :input t :output t :element-type :default)))
        (format t "~&Agent 2 connected.~%")

        (when telnet2character-mode
          (loop for x in '(255 253 34 255 251 1) do (write-byte x stream))  ; agent 1
          (loop for x in '(255 253 34 255 251 1) do (write-byte x stream2)) ; agent 2
          (finish-output stream)
          (finish-output stream2))

        (values stream stream2 socket)))))


(defun slurp-input (input-stream)
  "swallows all pending data"
  (loop while (listen input-stream) do (read-byte input-stream)))

;;
;; WebSocket for the GUI
;;
;; To ease installation, no WebSocket library is used but a bare-bones implementation instead
;; for better approaches see, e.g., https://lispcookbook.github.io/cl-cookbook/websockets.html
;;
;; This code relies on the SHA1 and base64 implementation provided at
;; https://github.com/massung/sha1/blob/master/README.md
;; the base64 and sha1 package must be loaded

(defparameter +web-socket-magick+ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  "the magic string constant for WebSocket authentification")

(defparameter *gui-thread* nil
  "the thread that serves the GUI")

(defparameter *gui-running?* t
  "flag to tell gui server to shut down")

(defparameter *gui-connected?* nil
  "boolean flag telling whether a GUI connection is active")

(defparameter *gui-view-queue* (sb-concurrency:make-queue)
  "level data to be send to the GUI")

(defparameter *gui-command-queue* (sb-concurrency:make-queue)
  "commands received from the GUI")

(defun gui-handler (port)
  "infinitely loop to handle incoming GUI connections"
  (setf *gui-thread* sb-thread:*current-thread*
	*gui-running?* t)
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-tcp-nodelay socket)   t
	  (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket #(0 0 0 0) port)
    (loop while *gui-running?* finally (sb-bsd-sockets:socket-close socket) do
      (setf *gui-connected?* nil)
      (sb-bsd-sockets:socket-listen socket 3)
      (let ((stream (sb-bsd-sockets:socket-make-stream (accept socket) :input t :output t :element-type :default))
	    (key nil))
	;; wait until connection request is received
	(format *standard-output* "~&Connection request by GUI on port ~d.~%" port)
	;; scan input for web socket key
	(loop until key do 
	  (loop while (and (listen stream)) do
	    (let ((line (read-line stream nil "" nil)))
	      (when (and (< 17 (length line))
			 (string= "Sec-WebSocket-Key:" line :end1 17 :end2 17))
		(setq key (subseq line 19 (- (length line) 1)))
		(format *standard-output* "request with key '~a'~%" key))))
	  (sleep 1))
	;; reply to client
	(let ((reply-code (sha1:sha1-base64 (format nil "~a~a" key +web-socket-magick+) #'base64:base64-encode)))
	  (format  stream "HTTP/1.1 101 Switching Protocols~a~aConnection: Upgrade~a~aUpgrade: websocket~a~aSec-WebSocket-Accept: ~a~a~a~a~a"
		   #\Return #\Newline #\Return #\Newline #\Return #\Newline
		   reply-code
		   #\Return #\Newline #\Return #\Newline))
	(finish-output stream)
	(format *standard-output* "~&send handshake~%")
	(setf *gui-connected?* t)

	(loop with eof? = nil
	      until (or eof? (not *gui-running?*))
	      finally (progn (close stream)
			     (format *standard-output* "~&GUI client hang-up~%"))
	      do
		 ;; interact with gui
		 ;; something to send?
		 (unless (sb-concurrency:queue-empty-p *gui-view-queue*)
		   (let* ((data (sb-concurrency:dequeue *gui-view-queue*))
			  (dlen (length data))
			  frame-header)
		     (cond ((< dlen 126) ; short message?
			    (setf frame-header (list 129 dlen)))
			   ((< dlen 65536)
			    (setf frame-header (list 129 126 (ash dlen -8) (logand dlen 255))))
			   (t ;; FIXME: sending frames over 64K
			    (error "sending data frames over 64KB size not implemented")))
		     ;; send frame header and data
		     ;(format *standard-output* "~&sending to GUI:~a (~d bytes)~%" data dlen)
		     (loop for byte in frame-header do
		       (write-byte byte stream))
		     (loop for char across data do
		       (write-byte (char-code char) stream))
		     (finish-output stream)))
		 ;; something to receive?
		 (when (listen stream)
		   (let ((frame-header (read-byte stream nil :eof))
			 (frame-info   (read-byte stream nil :eof)))
		     (if (or (eql frame-header :eof)
			     (eql frame-info :eof))
			 (setf eof? t)
			 (let ((len (logand frame-info 127)))
			   ;; FIXME: we don't accept larger messages, i.e., len=126/127 with length in separate field, see https://en.wikipedia.org/wiki/WebSocket#Frame-based_message
			   (let ((masking-key (loop for i from 1 to 4 collect (read-byte stream nil :eof)))
				 (payload (loop for i from 1 to len collect (read-byte stream nil :eof))))
			     (if (or (find :eof payload)
				     (find :eof masking-key))
				 (setf eof? t)
				 (let ((msg (loop for data in payload
						  for index from 1
						  collect (code-char (logxor data (nth (mod index 4) masking-key))))))
				   (format *standard-output* "received from GUI:~{~a~} raw:~a~%" msg (list frame-header frame-info payload masking-key)  )))))))))))))
	

(defun start-gui-connection ()
  "establishes WebSocket communication with the browser GUI"
  (format *standard-output* "~%Listening for GUI on port ~d.~%" *gui-port*)
  (sb-thread:make-thread (lambda ()
			   (gui-handler *gui-port*))))

;;
;; The game loop is rather straightforward. We run the GUI socket connection in a dedicated thread so we don't need to worry
;; whether a GUI is actually connected or not. In the main loop, we wait for the two agents to connect and then loop through
;; every level with a 0.1 second delay between updating the world. If all diamonds are collected, the next level is activated.
;; When we run out of levels, the game ends and connections are terminated.
;;
(defun main ()
  "main loop of game server"
  (setf *random-state* (make-random-state t))
  (initialize-box2d)
  (start-gui-connection)

  ;; wait for agents to connect
  (let ((port *agent-port*)
	(diamonds-disc 0)
	(diamonds-rect 0))
    (format t "~&Waiting for agents to connect on port ~d.~%" port)
    (let* (disc-agent-stream rect-agent-stream info-stream)
      (unwind-protect
	   (multiple-value-bind (tcp-stream-1 tcp-stream-2 agent-socket) (await-agent-connections port) ; wait until agents have connected 
	     ;; randomly assign disc and rect player
	     (when (= 1 (random 2)) 
	       (rotatef tcp-stream-1 tcp-stream-2))
	     (setq disc-agent-stream tcp-stream-1
		   rect-agent-stream tcp-stream-2
		   info-stream (make-broadcast-stream disc-agent-stream rect-agent-stream *standard-output*))

	     ;; play all levels
	     (loop for level in *levels*
		   for level-count from 1
		   finally (progn (format info-stream "end~%")
				  (finish-output info-stream)
				  (close disc-agent-stream)
				  (close rect-agent-stream)
				  (sb-bsd-sockets:socket-close agent-socket)
				  (setq *gui-running?* nil)
				  (format *standard-output* "disc: ~d diamond~:P~%rect: ~d diamond~:P~%"
					  diamonds-disc diamonds-rect))
		   do
		      (slurp-input disc-agent-stream)
		      (slurp-input rect-agent-stream)
		      (let ((rect-aborts? nil) ; whether agents have requested to abort this level
			    (disc-aborts? nil)
			    (message-from-rect nil) ; messages agents wish to exchange
			    (message-from-disc nil)
			    (rect-listens? nil) ; whether the agent has send some data so it should be ready to listen as well
			    (disc-listens? nil))
			(multiple-value-bind (diamonds platforms) (setup-level level)
			  ;; inform agents which player they control, depending on config
			  (if +announce-who-am-i+
			      (progn 
				(format disc-agent-stream "(:playing disc)~a~a" #\Return #\Newline) 
				(format rect-agent-stream "(:playing rect)~a~a" #\Return #\Newline) )
			      (progn
				(format disc-agent-stream "(:playing unknown)~a~a" #\Return #\Newline)
				(format rect-agent-stream "(:playing unknown)~a~a" #\Return #\Newline)))
			  (finish-output disc-agent-stream)
			  (finish-output rect-agent-stream)

			  ;; ply the level
			  (loop while (and diamonds (not (and rect-aborts? disc-aborts?)))
				finally (progn
					  (format info-stream "level-finished~a~a" #\Return #\Newline)
					  (finish-output info-stream)
					  (destroyworld))
				do
				   ;; attend to disc agent
				   ;(format t "~&disc agent listening: ~a" (listen disc-agent-stream))
				   ;; note: streams are accessed byte-wise, not in character mode to avoid oddities of UTF-8 decode problems
				   ;; in case a broken UTF octet sequence will be received (which will be the case in case of being connected
				   ;; to some telnet that sends telnet protocol stuff
				   (when (listen disc-agent-stream)
				     (let ((chr (ignore-errors (read-byte disc-agent-stream))))
				       (setq disc-listens? t)
				       (case chr
					 (97 (moveDiscPlayer -5.0f0)) ; a
					 (100 (moveDiscPlayer +5.0f0)) ; d
					 (119 (jumpDiscPlayer +disc-force+)) ; w
					 ;; messages get appended in order to make sure they will be delivered, not overridded by a newly received one. To this end,  message-from-disc gets reset to NIL after delivery to rect agent further below
					 (109 (setq message-from-disc (append message-from-disc (read disc-agent-stream nil nil nil)))) ; m(...)
					 (113 (setq disc-aborts? t))))) ; q
				   
				   ;; attend to rect agent
				   ;(format t "~&rect agent listening: ~a" (listen rect-agent-stream))
				   (when (listen rect-agent-stream)
				     ; we read raw bytes to avoid problems in UTF decoding in case we receive illegal codes
				     (let ((chr (ignore-errors (read-byte rect-agent-stream))))
				       (setq rect-listens? t)
				       (case chr
					 (97 (moveRectPlayer (- +rect-force+))) ; a
					 (100 (moveRectPlayer +rect-force+)) ; d
					 (115 (transformRectPlayer +0.1f0)) ; s
					 (119 (transformRectPlayer -0.1f0)) ; w
					 (109 (setq message-from-rect (append message-from-rect (read rect-agent-stream nil nil nil)))) ; m(...) see note on messages above
					 (113 (setq rect-aborts? t))))) ; q
				   ;; step simulation and post updates to agents
				   (let (disc-pos-x disc-pos-y rect-pos-x rect-pos-y rect-rotation rect-width rect-height)
				     (dotimes (i 6) ; 6*1/60 ~ 0.1s
				       (ignore-errors (stepworld)) ; tmp fix for box2d crashes
				       ;; read poses from box2d
				       (let ((pose-struct (deref (getDiscPlayerPose))))
					 (setq disc-pos-x (slot pose-struct 'x)
					       disc-pos-y (slot pose-struct 'y)))
				       (let ((pose-struct (deref (getRectPlayerPose))))
					 (setq rect-pos-x (slot pose-struct 'x)
					       rect-pos-y (slot pose-struct 'y)
					       rect-rotation (slot pose-struct 'r)
					       rect-width (slot pose-struct 'w)
					       rect-height (slot pose-struct 'h)))
				       ;; check for diamonds taken
				       (let ((d (find-if #'(lambda (d)
							     (> 2.0 (abs (- (complex (second d) (third d))
									    (complex disc-pos-x disc-pos-y)))))
							 diamonds)))
					 (when d
					   (setq diamonds (delete d diamonds))
					   (incf diamonds-disc)))
				       (let ((d (find-if #'(lambda (d)
							     (< 0 (pointInRectPlayer (second d) (third d))))
							 diamonds)))
					 (when d
					   (setq diamonds (delete d diamonds))
					   (incf diamonds-rect))))
				     
    				 (when *gui-connected?*
    				   (let* ((*print-pretty* nil)
    				          (base-scene
    				            (append
    				             (list
    				              (list :RECT rect-pos-x rect-pos-y rect-width rect-height rect-rotation diamonds-rect)
    				              (list :DISC disc-pos-x disc-pos-y +disc-radius+ diamonds-disc))
    				             diamonds
    				             platforms
    				             (list (list :LEVEL level-count))))
    				          (scene-for-gui (format nil "~a" base-scene)))
    				     (sb-concurrency:enqueue scene-for-gui *gui-view-queue*)))

				    ;; send current scene to anyone listening
					;; send current scene to agents (respecting message direction)
					(when rect-listens?
					;; Build the scene for RECT (only include :msg->rect)
					(let* ((*print-pretty* nil)
							(scene-list
							(append
							(list
								(list :RECT rect-pos-x rect-pos-y rect-width rect-height rect-rotation diamonds-rect)
								(list :DISC disc-pos-x disc-pos-y +disc-radius+ diamonds-disc))
							diamonds
							platforms
							;; Insert message for RECT only
							(if message-from-disc
								(list (list :msg->rect message-from-disc))
								nil)
							(list (list :LEVEL level-count))))
							(scene-for-rect (format nil "~a" scene-list)))
						(write-line scene-for-rect rect-agent-stream)
						(finish-output rect-agent-stream)
						(setq rect-listens? nil
							message-from-disc nil)))

					(when disc-listens?
					;; Build the scene for DISC (only include :msg->disc)
					(let* ((*print-pretty* nil)
							(scene-list
							(append
							(list
								(list :RECT rect-pos-x rect-pos-y rect-width rect-height rect-rotation diamonds-rect)
								(list :DISC disc-pos-x disc-pos-y +disc-radius+ diamonds-disc))
							diamonds
							platforms
							;; Insert message for DISC only
							(if message-from-rect
								(list (list :msg->disc message-from-rect))
								nil)
							(list (list :LEVEL level-count))))
							(scene-for-disc (format nil "~a" scene-list)))
						(write-line scene-for-disc disc-agent-stream)
						(finish-output disc-agent-stream)
						(setq disc-listens? nil
							message-from-rect nil)))

				     ;; handling requests from GUI
				     (unless (sb-concurrency:queue-empty-p *gui-command-queue*)
				       (let ((command (sb-concurrency:dequeue *gui-command-queue*)))
					 (format *standard-output* "received GUI command ~a.~%" command))))
				   (sleep 0.1))))))
	;; clean-up on exit
	(close disc-agent-stream)
	(close rect-agent-stream)
	(setq *gui-running?* nil) ; set flag of GUI server to exit
	:done))))

(eval-when (:execute :load-toplevel)
  (main))
