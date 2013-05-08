;==============================================================================;
; X | O | _ |[#]| _ | _ | _ | O | _ |[#]| _ | _ | _ | _ | X | X | X | X | O | _;
; _ | _ | _ | _ | _ | _ | X |.____________________. | _ |[#]| O | O | X | _ | O;
; _ | _ | X | X | O | X | _ |;                    ; | X | O | O | O | _ | _ |[#;
; _ | _ |[#]| O | O | O | X |;      gomoku++      ; | _ | O | X | X | O | _ | _;
; O | _ | _ | _ | _ | O | _ |;____________________; | _ | O | _ | _ | O | O | _;
; _ | _ | _ | _ | _ | _ | O | _ |[#]| O | _ | _ |[#]| _ | O | _ | _ |[#]| _ | X;
; _ |[#]| _ | _ | O | X | X | X | O | _ | _ | _ | _ | _ | X | _ | _ | _ | _ | _;
;==============================================================================;
;        Computer Science 1901 - Spring 2013 - University of Minnesota         ;
;                             Final Course Project                             ;
;==============================================================================;


;;;=====================;;;
;;;  DEFINED CONSTANTS  ;;;
;;;=====================;;;

;; Tracks version of code in case updates are released.
(define *SOURCE-REVISION* 60)

;; Size of the board. Default is (list-ref '(6 7 8 9 10) (random 5)).  
(define *BOARD-SIZE* 
  (* 2 (list-ref '(6 7 8 9 10) (random 5)))) ;; Must be even,
                                             ;; between 12 and 20

;; Number of blocked spaces. Default is (ceiling (/ *BOARD-SIZE* 3.0)).
(define *NUM-BLOCKS* (ceiling (/ *BOARD-SIZE* 3.0)))

;; How long a procedure can take to move before it is considered
;; a violation of the time limit. Default is 2 seconds.
(define *TIME-LIMIT* 2)

;; Game-Play Delay Settings
(define *SLEEP-MILLISECONDS* 100) ;; Time to pause between moves.
(define *STALL* #f)               ;; Prompt to continue?

;; Symbols used to keep track of players, win requirements,
;; time restriction violations and game results.
(define *PLAYER-1-SYMBOL*   'P1)
(define *P1-OT-SYMBOL*      'P1-ot)
(define *PLAYER-2-SYMBOL*   'P2)
(define *P2-OT-SYMBOL*      'P2-ot)
(define *BOTH-OT-SYMBOL*    'BOTH-ot)
(define *TIE-SYMBOL*        'TIE)
(define *WINNING-LENGTH*     5)

;; Change this variable if you are working in the terminal to see 
;; beautiful colors! BEWARE: colors appear to make things run rather
;; slowly.
(define *IN-TERMINAL* #f)

;; Below are the values representing the different colors. If 
;; you wish to change the colors, modify the *BOARD-SPACE* 
;; values. Replace the string before X, O, #, *X* and *O* with
;; the string listed below that represents the color you want
;; to see. So to change X from displaying in red (default) to
;; violet, one would change 
;;
;;    " \033[22;31mX\033[22;30m "
;; to
;;    " \033[22;35mX\033[22;30m "

;; Black  = "\033[22;30m\]"
;; Red    = "\033[22;31m\]"
;; Green  = "\033[22;32m\]"
;; Yellow = "\033[22;33m\]"
;; Blue   = "\033[22;34m\]"
;; Violet = "\033[22;35m\]"
;; Cyan   = "\033[22;36m\]"
;; White  = "\033[22;37m\]"

;; List of space possibilities.
;; 0 is an empty space
;; 1 is player 1's space
;; 2 is player 2's space
;; 3 is a blocked space
(define *BOARD-SPACE* 
  (if *IN-TERMINAL*
      '(" - " 
  " \033[22;31mX\033[22;30m " 
	" \033[22;34mO\033[22;30m " 
	"[\033[22;32m#\033[22;30m]" 
	"\033[22;33m*X*\033[22;30m" 
	"\033[22;33m*O*\033[22;30m"
	"\033[22;30m'X'\033[22;30m"
	"\033[22;30m'O'\033[22;30m")
      '(" - " " X " " O " "[#]" "*X*" "*O*" "'X'" "'O'")))
(define *OPEN-SPACE*         0)
(define *PLAYER-1-SPACE*     1)
(define *PLAYER-2-SPACE*     2)
(define *BLOCKED-SPACE*      3)
(define *PLAYER-1-WIN-SPACE* 4)
(define *PLAYER-2-WIN-SPACE* 5)

;; Prompt Strings
(define *ROW-PROMPT* 
  (string-append "Select Row (1-" (number->string *BOARD-SIZE*) "): "))
(define *COLUMN-PROMPT* 
  (string-append "Select Column (1-" (number->string *BOARD-SIZE*) "): "))
(define *PLAYER-1-PROMPT* "Player 1's Turn")
(define *PLAYER-2-PROMPT* "Player 2's Turn")
(define *WAIT-PROMPT*    "Press enter to continue:")

;; Error Messages
(define *INVALID-POSITION-STRING* "Invalid position specified.")
(define *CANNOT-MOVE-STRING*      "Selected position is not open.")

;; Game Over Strings
(define *GAME-OVER-INVALID-MOVE*    "Game Over!\nInvalid move by player ")
(define *GAME-OVER-TIE-STRING*      "Game Over!\nGame ends in tie.\n")
(define *GAME-OVER-PLAYER-1-STRING* "Game Over!\nPlayer 1 wins.\n")
(define *GAME-OVER-PLAYER-2-STRING* "Game Over!\nPlayer 2 wins.\n")


;;;======================;;;
;;;  COMPATIBILITY CODE  ;;;
;;;======================;;;

;; SELECT YOUR IMPLEMENTATION ;;
;;----------------------------;;
(define implementation 
  (let ((mit  0)  ;; GNU/MIT Scheme
	(plt  1)  ;; PLT Scheme Implementations -- Dr. Scheme
	(stk  2)) ;; STk -- Scheme w/ Tk

    ;;Implementation being used:
    mit
))

(define (my-sleep t)
  (define (helper t)
    (if (> (system-clock) t)
    0
    (helper t)))
  (helper (+ (system-clock) (/ t 1000))))

(define (sleep-ms ms)
  (cond ((= implementation 0) ;; MIT
	 (my-sleep ms))
	((= implementation 1) ;; PLT
	 (sleep (/ *SLEEP-MILLISECONDS* 1000.0)))
	((= implementation 2) ;; STk
	 (sleep ms))
	(else
	 #f)))


;;;=======================;;;
;;;  LOAD EXTERNAL FILES  ;;;
;;;=======================;;;
(load "util.scm")  ;; General Utility Procedures
(load "help.scm")  ;; gomoku++ Helper Procedures


;;;====================;;;
;;;  GLOBAL VARIABLES  ;;;
;;;====================;;;
(define player-procedure #f)





;;;==============;;;
;;; TESTING CODE ;;;
;;;==============;;;

;; Run play-game n times and display results.
(define (run-tests player1-file player2-file n)
  ;; Store settings temporarily.
  (let ((old-sleep *SLEEP-MILLISECONDS*)
	(old-stall *STALL*))
    
    ;; Update Settings
    (set! *SLEEP-MILLISECONDS* 0)
    (set! *STALL* #f)

    ;; Suppress normal output.
    ;; If there is a crash while this procedure is running, display and newline
    ;;   may not work.  In that case, from the prompt, make the following calls:
    ;;     (display 'reset)
    ;;     (newline 'reset)
    (set! newline  
	  (let ((old-newline newline))
	    (define (temp-newline . args) ;; Using define rather than lambda
	      (if (and (not (null? args)) ;; to let this work in Dr Scheme
		       (eq? (car args) 'reset)) 
		  (set! newline old-newline) #f))
	    temp-newline))

    (set! display 
	  (let ((old-display display))
	    (define (temp-display . args) ;; Using define rather than lambda
	      (if (and (not (null? args)) ;; to let this work in Dr Scheme
		       (eq? (car args) 'reset)) 
		  (set! display old-display) #f))
	    temp-display))

    ;; Counts the number of times the given player went
    ;; over the time limit.
    (define (count-overtimes player-symbol lst)
      (if (null? lst)
	  0
	  (if (eq? player-symbol *PLAYER-1-SYMBOL*)
	      (if (and (pair? (car lst))
		       (or (eq? *P1-OT-SYMBOL*   (cdar lst))
			   (eq? *BOTH-OT-SYMBOL* (cdar lst))))
		  (+ 1 (count-overtimes player-symbol (cdr lst)))
		  (count-overtimes player-symbol (cdr lst)))
	      (if (and (pair? (car lst))
		       (or (eq? *P2-OT-SYMBOL*   (cdar lst))
			   (eq? *BOTH-OT-SYMBOL* (cdar lst))))
		  (+ 1 (count-overtimes player-symbol (cdr lst)))
		  (count-overtimes player-symbol (cdr lst))))))

    ;; Run games and store results in a list.
    (let ((results (map
		    (lambda (n)		      
                (play-game player1-file player2-file))
		    (iota n))))

      ;; Restore previous definitions of display and newline.
      (display 'reset) (newline 'reset)

      ;; Display Results
      (newline) (newline)
      (display "TEST RESULTS: ") (newline)
      (display "============================================") (newline)
      (display "Player 1: ") (display player1-file) (newline)
      (display "Player 2: ") (display player2-file) (newline)
      (display "============================================") (newline)
      (display "Total Games Run: ") (display n) (newline)
      (newline)
      
      (display "Player 1 Wins: ") 
      (display (+ (count *PLAYER-1-SYMBOL* results)
		  (apply + 
			 (map 
			  (lambda (x) 
			    (if (and (pair? x) 
				     (eq? *PLAYER-1-SYMBOL* (car x))) 1 0)) 
			  results)))) (newline)
      (display "Player 1 Win Percentage: ") 
      (display (* 100.0 (/ (+ (count *PLAYER-1-SYMBOL* results)
			      (apply + 
				     (map 
				      (lambda (x) 
					(if (and (pair? x) 
						 (eq? *PLAYER-1-SYMBOL* (car x))) 1 0)) 
				      results))) n))) (display "%") (newline)
      (display "Player 1 Over-times: ")
      (display (count-overtimes *PLAYER-1-SYMBOL* results))
      (newline) (newline)
      
      (display "Player 2 Wins: ") 
      (display (+ (count *PLAYER-2-SYMBOL* results)
		  (apply + 
			 (map 
			  (lambda (x) 
			    (if (and (pair? x) 
				     (eq? *PLAYER-2-SYMBOL* (car x))) 1 0)) 
			  results)))) (newline)
      (display "Player 2 Win Percentage: ") 
      (display (* 100.0 (/ (+ (count *PLAYER-2-SYMBOL* results)
			      (apply + 
				     (map 
				      (lambda (x) 
					(if (and (pair? x) 
						 (eq? *PLAYER-2-SYMBOL* (car x))) 1 0)) 
				      results))) n))) (display "%") (newline)
      (display "Player 2 Over-times: ")
      (display (count-overtimes *PLAYER-2-SYMBOL* results))
      (newline) (newline)
      
      (display "Tie Games: ")
      (display (count *TIE-SYMBOL* results)) (newline)
      (display "Tie Percentage: ")
      (display (* 100.0 (/ (count *TIE-SYMBOL* results) n))) (display "%")
      (newline))

    ;; Restore Settings
    (set! *SLEEP-MILLISECONDS* old-sleep)
    (set! *STALL* old-stall)

    )
  'done)

;;;==================;;;
;;;  MAIN PROCEDURE  ;;;
;;;==================;;;
(define (play-game player1-file player2-file)


  ;;======================;;
  ;;  DISPLAY PROCEDURES  ;;
  ;;======================;;

  ;; Display current game board.
  (define (display-board board)
    (display (board->string board)))

  ;; Display string specifying who's turn it is.
  (define (display-player-prompt current-player)
    (display+ (prompt-string current-player) "\n"))


  ;; Converts a board to a string.
  (define (board->string board)

    ;; Builds a string representing a single row.
    (define (row->string r n)
      (let ((row (if (< n 10)
		     (string-append ":  " (number->string n))
		     (string-append ": " (number->string n)))))
	(define (row-builder r)
	  (cond ((null? r) (set! row (string-append row "|  :\n")))
		(else 
		 (set! row 
		       (string-append row 
				      "|"
				      (list-ref *BOARD-SPACE* (car r))))
		 (row-builder (cdr r)))))
	(row-builder r)
	row))

    (define top-border
      (string-append
       "\n" 
       ".-[ gomoku++ ]-" (make-string (- (* *BOARD-SIZE* 4) 9) #\-) "-.\n"
       ":  " (make-string (+ (* *BOARD-SIZE* 4) 3) #\space) " :\n"
       ":   ._" (apply string-append
		      (map 
		       (lambda (col) 
			 (if (not (= col *BOARD-SIZE*))
			     (if (< col 9)
				 (string-append (number->string col) "___")
				 (string-append (number->string col) "__"))
			     (string-append (number->string col) "_.  :\n")))
		       (iota 1 (1+ *BOARD-SIZE*))))))
    
    (define bottom-border
      (string-append
       ":   '" (make-string (- (* *BOARD-SIZE* 4) 1) #\-) "'  :\n"
       ":  " (make-string (+ (* *BOARD-SIZE* 4) 3) #\space) " :\n"
       "'-" (make-string (+ (* *BOARD-SIZE* 4) 4) #\-)     "-'\n"))
    
    (string-append
     top-border
     (apply string-append
	    (map 
	     (lambda (row) 
	       (row->string (contents (list-ref board row)) row))
	     (iota 1 (1+ *BOARD-SIZE*))))
     bottom-border))
  
  (define (highlight-win symbol pos line-direction diag? board)
    (let ((status (if (pair? symbol)
		      (if (eq? (car symbol) *PLAYER-1-SYMBOL*)
			  *PLAYER-1-WIN-SPACE*
			  *PLAYER-2-WIN-SPACE*)
		      (if (eq? symbol *PLAYER-1-SYMBOL*)
			  *PLAYER-1-WIN-SPACE*
			  *PLAYER-2-WIN-SPACE*)))
	  (deltaRow (if diag?
			1
			(if (eq? line-direction 'row)
			    0
			    1)))
	  (deltaCol (if diag?
			(if (eq? line-direction 'left)
			    -1
			    1)
			(if (eq? line-direction 'row)
			    1
			    0))))
      
    ;; Does the highlighting!
    (define (highlighter curpos count)
      (if (<= count 1)
	  (set-position-status! status curpos board)
	  (begin
	    (set-position-status! status curpos board)
	    (highlighter (make-position (+ (get-row curpos)    deltaRow)
					(+ (get-column curpos) deltaCol))
			 (- count 1)))))
    (highlighter pos *WINNING-LENGTH*)))

  ;;======;;
  ;; MISC ;;
  ;;======;;

  (define (safe-load filename)
    (let ((result (ignore-errors (lambda () (load filename)))))
      (if (condition? result)
	  'error 
	  result)))

  ;; Convert file string to player-procedure
  (define (file->player file)
    (cond ((equal? file 'human) 'human)
	  (else
	   (set! player-procedure #f)
	   (load file)
	   (if player-procedure
	       player-procedure
	       (error "player-procedure not properly defined.")))))

  (define (read-line)
    (let ((char (peek-char)))
      (if (eof-object? char)
	  char
	  (list->string
	   (let loop ((char char))
	     (if (or (eof-object? char)
		     (equal? #\newline char)
		     (equal? #\return char)
		     (equal? #\linefeed char))
		 (begin
		   (read-char)
		   '())
		 (begin
		   (read-char)
		   (cons char
			 (loop (peek-char))))))))))

  ;;=======;;
  ;; MOVES ;;
  ;;=======;;

  ;; Gets a move from the human and doesn't give up
  (define (get-human-move player board board-string)

    ;; Displays error if the input was invalid and prompts again.
    (define (handle-invalid invalid-string) 
      (newline) (display board-string)
      (newline) (display invalid-string)
      (newline) (display (prompt-string player))
      (newline) (get-human-move player board board-string))

    ;; Prompts for human player to input row and column.
    (define (get-position)
      (let ((row '()) (column '()))
	(display *ROW-PROMPT*) (flush-output) (set! row (read))
	(display *COLUMN-PROMPT*) (flush-output) (set! column (read))
	(make-position row column)))


    (let ((position (get-position)))
      (cond ((not (valid-position? position))
	     (handle-invalid *INVALID-POSITION-STRING*))
	    ((not (open-position? position board))
	     (handle-invalid *CANNOT-MOVE-STRING*))
	    (else position))))


  ;;=============;;
  ;; INFORMATIVE ;; 
  ;;=============;;

  ;; Finds the first index of the winning chain
  (define (index-lookup winner lst)
    (define (index-finder curlst curind winind count)
      (cond ((<= *WINNING-LENGTH* count)
	     winind)
	    ((null? curlst)
	     (error "INDEX-FINDER: winning chain not found."))
	    ((and 
	      (= (if (eq? winner *PLAYER-1-SYMBOL*) 
		     *PLAYER-1-SPACE*
		     *PLAYER-2-SPACE*) 
		  (car curlst))
	      (<= winind 0))
	     (index-finder (cdr curlst) 
			   (+ 1 curind)
			   curind 
			   (+ 1 count)))
	    ((= (if (eq? winner *PLAYER-1-SYMBOL*) 
		     *PLAYER-1-SPACE*
		     *PLAYER-2-SPACE*) 
		  (car curlst))
	     (index-finder (cdr curlst)
			   (+ 1 curind)
			   winind
			   (+ 1 count)))
	    (else (index-finder (cdr curlst) 
				(+ 1 curind)
				0 
				0))))
    (index-finder lst 1 0 0))

  ;; Returns PLAYER-1-SYMBOL, PLAYER-2-SYMBOL, or TIE-SYMBOL
  ;; and may pair them with a relevant OVERTIME symbol
  (define (winner? player-symbol board)
    (define (edge-finder pos direction)
    (let ((row (get-row pos)) (col (get-column pos)))
      (if (eq? direction 'left)
	  (cond ((< (+ col row) (1+ *BOARD-SIZE*))
		 (make-position 1 (- (+ row col) 1)))
		((> (+ col row) (1+ *BOARD-SIZE*))
		 (make-position (- row (- *BOARD-SIZE* col)) *BOARD-SIZE*))
		(else 
		 (make-position 1 *BOARD-SIZE*)))
	  (cond ((> col row)
		 (make-position 1 (1+ (- col row))))
		((< col row) 
		 (make-position (1+ (- row col)) 1))
		(else (make-position 1 1))))))
    (let* ((last-move  (get-last-move board))
	   (left-edge  (edge-finder last-move 'left))
	   (right-edge (edge-finder last-move 'right))
	   (player (if (eq? player-symbol *PLAYER-1-SYMBOL*) 1 2))
	   (row (count-chain player 
			     (row-lookup 
			      (get-row last-move) board)))
	   (column (count-chain player
				(col-lookup
				 (get-column last-move) board)))
	   (left-diag (count-chain player
				   (diagonal-lookup 
				    last-move 'left board)))
	   (right-diag (count-chain player
				    (diagonal-lookup 
				     last-move 'right board))))
      
      (if (>= (max row column left-diag right-diag) *WINNING-LENGTH*)
	  (if (eq? player *PLAYER-1-SPACE*)
	      (begin
		(cond ((>= row *WINNING-LENGTH*)
		       (highlight-win *PLAYER-1-SYMBOL*
				      (make-position (get-row last-move) 
						     (index-lookup player-symbol
								   (contents
								    (row-lookup 
								     (get-row last-move) 
								     board))))
				      'row 
				      #f 
				      board))
		      ((>= column *WINNING-LENGTH*)
		       (highlight-win *PLAYER-1-SYMBOL*
				      (make-position (index-lookup player-symbol
								   (contents
								    (col-lookup 
								     (get-column last-move) 
								     board)))
						     (get-column last-move))
				      'column 
				      #f 
				      board))
		      ((>= left-diag *WINNING-LENGTH*)
		       (let ((index (index-lookup player-symbol
						  (contents
						   (diagonal-lookup last-move 'left board)))))
			 (highlight-win *PLAYER-1-SYMBOL*
					(make-position (- (+ (get-row    left-edge) index) 1)
						       (+ (- (get-column left-edge) index) 1))
					'left 
					#t 
					board)))
		      ((>= right-diag *WINNING-LENGTH*)
		       (let ((index (index-lookup player-symbol
						  (contents
						   (diagonal-lookup last-move 'right board)))))
			 (highlight-win *PLAYER-1-SYMBOL*
					(make-position (- (+ (get-row    right-edge) index) 1)
						       (- (+ (get-column right-edge) index) 1))
					'right 
					#t 
					board)))
		      (else #f))
		*PLAYER-1-SYMBOL*)
	      (begin
		(cond ((>= row *WINNING-LENGTH*)
		       (highlight-win *PLAYER-2-SYMBOL*
				      (make-position (get-row last-move) 
						     (index-lookup player-symbol
								   (contents
								    (row-lookup 
								     (get-row last-move) 
								     board))))
				      'row 
				      #f 
				      board))
		      ((>= column *WINNING-LENGTH*)
		       (highlight-win *PLAYER-2-SYMBOL*
				      (make-position (index-lookup player-symbol
								   (contents
								    (col-lookup 
								     (get-column last-move) 
								     board)))
						     (get-column last-move))
				      'column 
				      #f 
				      board))
		      ((>= left-diag *WINNING-LENGTH*)
		       (let ((index (index-lookup player-symbol
						  (contents
						   (diagonal-lookup last-move 'left board)))))
			 (highlight-win *PLAYER-2-SYMBOL*
					(make-position (- (+ (get-row    left-edge) index) 1)
						       (+ (- (get-column left-edge) index) 1))
					'left 
					#t 
					board)))
		      ((>= right-diag *WINNING-LENGTH*)
		       (let ((index (index-lookup player-symbol
						  (contents
						   (diagonal-lookup last-move 'right board)))))
			 (highlight-win *PLAYER-2-SYMBOL*
					(make-position (- (+ (get-row    right-edge) index) 1)
						       (- (+ (get-column right-edge) index) 1))
					'right 
					#t 
					board)))
		      (else #f))
		*PLAYER-2-SYMBOL*))
	  #f)))
  

  ;;======================;;
  ;;  Begin Main Process  ;;
  ;;======================;;

  (let* ((player1-procedure (file->player player1-file))
	 (player2-procedure (file->player player2-file))
	 (current-player *PLAYER-1-SYMBOL*)
	 (board (make-board))
	 (longest-move-p1 0)
	 (longest-move-p2 0))
    

    ;; Gets a position from human or player procedure
    (define (get-a-position board board-string)
      (let ((player-procedure 
	     (if (eq? current-player *PLAYER-1-SYMBOL*) 
		 player1-procedure 
		 player2-procedure)))
	
	(if (eq? player-procedure 'human)
	    (get-human-move current-player board board-string)
	    (player-procedure current-player (tree-copy board)))))

    ;; Forces Loop To Pause
    (define (do-delay)
      (if (not (or (eq? player1-file 'human) 
		   (eq? player2-file 'human)))
	  (begin
	    (flush-output) 
	    (sleep-ms *SLEEP-MILLISECONDS*)
	    (if *STALL* 
		(begin (display *WAIT-PROMPT*) 
		       (flush-output) 
		       (read-line)
		       (newline))))))

    ;; Returns a string containing the
    ;; longest time spent making a move.
    (define (time player-symbol)
      (if (eq? player-symbol *PLAYER-1-SYMBOL*)
	  (string-append "Player 1's longest move: " 
			 (number->string longest-move-p1) "\n")
	  (string-append "Player 2's longest move: " 
			 (number->string longest-move-p2) "\n")))

    ;; Handles all the messy checking for the winner's symbol
    (define (handle-win symbol)
      (if (eq? symbol *PLAYER-1-SYMBOL*)
	  (cond ((> longest-move-p1 *TIME-LIMIT*) 
		 (cons *PLAYER-1-SYMBOL* *P1-OT-SYMBOL*))
		((> longest-move-p2 *TIME-LIMIT*)
		 (cons *PLAYER-1-SYMBOL* *P2-OT-SYMBOL*))
		((and (> longest-move-p1 *TIME-LIMIT*)
		      (> longest-move-p2 *TIME-LIMIT*)) 
		 (cons *PLAYER-1-SYMBOL* *BOTH-OT-SYMBOL*))
		(else *PLAYER-1-SYMBOL*))
	  (if (eq? symbol *PLAYER-2-SYMBOL*)
	      (cond ((> longest-move-p2 *TIME-LIMIT*)  
		     (cons *PLAYER-2-SYMBOL* *P2-OT-SYMBOL*))
		    ((> longest-move-p1 *TIME-LIMIT*) 
		     (cons *PLAYER-2-SYMBOL* *P1-OT-SYMBOL*))
		    ((and (> longest-move-p1 *TIME-LIMIT*)
			  (> longest-move-p2 *TIME-LIMIT*)) 
		     (cons *PLAYER-2-SYMBOL* *BOTH-OT-SYMBOL*))
		    (else *PLAYER-2-SYMBOL*))
	      (cond ((> longest-move-p1 *TIME-LIMIT*) 
		     (cons *TIE-SYMBOL* *P1-OT-SYMBOL*))
		    ((> longest-move-p2 *TIME-LIMIT*) 
		     (cons *TIE-SYMBOL* *P2-OT-SYMBOL*))
		    ((and (> longest-move-p1 *TIME-LIMIT*)
			  (> longest-move-p2 *TIME-LIMIT*)) 
		     (cons *TIE-SYMBOL* *BOTH-OT-SYMBOL*))
		    (else *TIE-SYMBOL*)))))
	      
    ;;===================;;
    ;; Main program loop ;;
    ;;===================;;
    (define (loop)
      (let* ((board-string (board->string board))
	     (position '()))

	;; Display Current Board State and Score
	(display board-string)

	;; Get Input From Player
	(display-player-prompt current-player) (flush-output)

	(with-timings (lambda () (set! position (get-a-position board board-string)))
		      (lambda (run-time gc-time real-time) 
			(let ((time (internal-time/ticks->seconds real-time)))
			  (if (eq? current-player *PLAYER-1-SYMBOL*)
			      (if (> time longest-move-p1)
				  (set! longest-move-p1 time))
			      (if (> time longest-move-p2)
				  (set! longest-move-p2 time))))))
      
	;; Check Input For Validity
	(cond ((and (valid-position? position)
		    (open-position? position board))
	       
	      
	       ;; Execute Move
	       (make-move! current-player position board)

	       (let ((winner (winner? current-player board)))
		 (if winner
		     
		     ;; Game Over - Winner
		     (begin
		       (display-board board)
		       (if (eq? winner *PLAYER-1-SYMBOL*)
			   (display *GAME-OVER-PLAYER-1-STRING*)
			   (display *GAME-OVER-PLAYER-2-STRING*))
		       (display (time winner))
		       (display (time (other-player winner)))
		       (handle-win winner))
		     
		     (if (board-filled? board)

			 ;; Game Over - Tie
			 (begin
			   (display (board->string board))
			   (display *GAME-OVER-TIE-STRING*)
			   (display (time current-player))
			   (display (time (other-player current-player)))
			   (handle-win *TIE-SYMBOL*))
		     
			 ;; Game Continues
			 (begin
			   (set! current-player (other-player current-player))
			   (do-delay)
			   (loop))))))

	       (else (display board-string)
		     (display *GAME-OVER-INVALID-MOVE*)(display current-player)(display " : ")(display position)(display "\n")
		     (other-player current-player)))))
    
    ;; Execute Main Program Loop
    (loop))
  
    )  ;; End Play-Game Procedure

(display "Use the play game function with 2 players as arguments to start a game")(newline)
(display "Players can be either the name of a AI file (eg. \"random\") or 'human")(newline)
(display "Examples : (play-game 'human 'human) OR (play-game 'human \"random\")")(newline)(newline)
(display "Multiple games can be run at once with the run-tests function")(newline)
(display "Example  : (run-tests \"random\" \"random\" 10)")(newline)
