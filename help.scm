;==============================================================================;
;        Computer Science 1901 - Spring 2013 - University of Minnesota         ;
;                             Final Course Project                             ;
;==============================================================================;
;  Helper Procedures For gomoku++.scm                                          ;
; ----------------------------------                                           ;
;  Notes:                                                                      ;
;   + This file is loaded by gomoku++.scm.                                     ;
;   + Your player-procedure has access to these procedures through the top     ;
;      level environment.  There is no need to copy them in to your file.      ;
;   + Do not submit this file.                                                 ;
;==============================================================================;

;;;=====================;;;
;;;  STRINGS & SYMBOLS  ;;;
;;;=====================;;;

;; Given the space of symbol for a player, 
;; gets the space or symbol for the other player.
(define (other-player player-symbol)
  (cond ((eq? player-symbol  *PLAYER-1-SPACE*) *PLAYER-2-SPACE*)
  ((eq? player-symbol *PLAYER-1-SYMBOL*) *PLAYER-2-SYMBOL*)
	((eq? player-symbol  *PLAYER-2-SPACE*) *PLAYER-1-SPACE*)
	(else *PLAYER-1-SYMBOL*)))

;; Gets the prompt string for the given player
(define (prompt-string player-symbol)
  (if (eq? player-symbol *PLAYER-1-SYMBOL*) 
      *PLAYER-1-PROMPT* 
      *PLAYER-2-PROMPT*))

;; Gets the winner string for the player-symbol or 'tie
(define (winner-string winner)
  (cond ((eq? winner *TIE-SYMBOL*) *GAME-OVER-TIE-STRING*)
	((eq? winner *PLAYER-1-SYMBOL*) *GAME-OVER-PLAYER-1-STRING*)
	(else *GAME-OVER-PLAYER-2-STRING*)))


;;;=========================;;;
;;;  POSITION ABSTRACTIONS  ;;;
;;;=========================;;;

;; Position Constructor
(define (make-position row col)
  (attach-tag 'position (list row col)))

;; Is the object a position?
(define (position? obj)
  (and (list? obj)
       (= (length obj) 3)
       (eq? (type-tag obj) 'position)))

;; Row Accessor
(define (get-row position)
  (cond ((position? position)  
	 (car (contents position)))
	(else #f)))

;; Column Accessor
(define (get-column position)
  (cond ((position? position)  
	 (cadr (contents position)))
	(else #f)))

;; Does the given position have valid coordinates?
(define (valid-position? position)
  (and (list? position)
       (position? position)  
       (in-range? 1 *BOARD-SIZE* (get-row position))
       (in-range? 1 *BOARD-SIZE* (get-column position))))

;;;=====================;;;
;;;  BOARD INFORMATION  ;;;
;;;=====================;;;

;; Returns true if the board is completely filled.
(define (board-filled? board)
  (= (count-open-positions board) 0))

;; Get a list of all possible positions on a board, open or otherwise
(define (enumerate-positions)
  (concat-map 
   (lambda (row)
     (map (lambda (column) (make-position row column)) 
	  (iota 1 (1+ *BOARD-SIZE*))))
   (iota 1 (1+ *BOARD-SIZE*))))

;; Returns the queried row
(define (row-lookup row board)
  (if (not (in-range? 1 *BOARD-SIZE* row))
      (error "ROW-LOOKUP: invalid row: " row)
      (list-ref board row)))

;; Returns the queried column
(define (col-lookup col board)
  (if (not (in-range? 1 *BOARD-SIZE* col))
      (error "COL-LOOKUP: invalid column: " col)
      (attach-tag 'column 
		  (map (lambda (row) (list-ref row col))
		       (take (contents board) *BOARD-SIZE*)))))
  
;; Returns the queried diagonal in a very incomprehensible way. 
;; Here's how to call it:
;; start-pos: the position of a point in the diagonal.
;;            e.g.: (position 5 5) will give the diagonal from  
;;            the top corner to the bottom corner.           
;; direction: 'left or 'right, judged from the top to the bottom
;;            IE: / is left, \ is right.
(define (diagonal-lookup start-pos direction board)
  (if (not (position? start-pos))
      (error "DIAGONAL-LOOKUP: invalid position: " start-pos))


  (let ((row (get-row start-pos)) (col (get-column start-pos)))

  ;; Finds an edge to work with (to make the next parts easier)
  ;; Uses a lot of lemmas about graphs. :D
    (define (edge-finder)
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
		(else (make-position 1 1)))))
    
    (if (not (or (= 1 (get-row start-pos)) 
		 (and (eq? direction 'right)
		      (= 1 (get-column start-pos)))
		 (and (eq? direction 'left)
		      (= *BOARD-SIZE* (get-column start-pos)))))
	(begin
	  (set! start-pos (edge-finder))
	  (set! row (get-row start-pos))
	  (set! col (get-column start-pos))))
  
    (if (eq? direction 'left)
	(cond ((= row 1)
	       (attach-tag 
		'diagonal 
		(map (lambda (num) 
		       (list-ref (list-ref board num) (1+ (- col num))))
		     (iota 1 (1+ col)))))
	      ((= col *BOARD-SIZE*)
	       (attach-tag 
		'diagonal
		(map (lambda (num) 
		       (list-ref (list-ref board num) (+ row (- col num))))
		     (iota row (1+ col)))))
	      (else (error "DIAGONAL-LOOKUP: incorrect row/col: " row col)))
	(cond ((= col 1)
	       (attach-tag 
		'diagonal
		(map (lambda (num) 
		       (list-ref (list-ref board num) (+ num (- col row))))
		     (iota row (1+ *BOARD-SIZE*)))))
	      ((= row 1)
	       (attach-tag 
		'diagonal
		(map (lambda (num) 
		       (list-ref (list-ref board (1+ (- num col))) num))
		     (iota col (1+ *BOARD-SIZE*)))))
	      (else (error "DIAGONAL-LOOKUP: incorrect row/col: " row col))))))

	
;; Returns a list of all open positions on the board.
(define (get-open-positions board)
  (filter
   (lambda (position) 
     (open-position? position board))
   (enumerate-positions)))

;; Counts the number of open positions on the board.
(define (count-open-positions board)
  (length 
   (filter
    (lambda (position) 
      (open-position? position board))
    (enumerate-positions))))

;; Counts the number of positions matching status-space
(define (count-owned-positions status-space board)
  (length
   (filter
    (lambda (position) 
      (owns-position? status-space position board))
    (enumerate-positions))))

;; Returns the last move made
(define (get-last-move board)
  (let ((last-move (contents (list-ref board (- (length board) 1)))))
    (if (position? last-move)
	last-move
	#f)))

;; Counts the longest chain of positions owned by 
;; player (1 or 2) in a given row, column or diagonal.
(define (count-chain player-space line)
  (if (not (or (eq? 'row      (type-tag line))
	       (eq? 'column   (type-tag line))
	       (eq? 'diagonal (type-tag line))))
      (error "COUNT-CHAIN: invalid line: " line))
  (define (counter cur-tot cur-max cur-line)
    (cond ((null? cur-line) cur-max)
	  ((= player-space (car cur-line)) 
	   (counter (+ 1 cur-tot) 
		    (if (>= cur-tot cur-max)
			(+ 1 cur-tot)
			cur-max)
		    (cdr cur-line)))
	  (else (counter 0 cur-max (cdr cur-line)))))
  (counter 0 0 (contents line)))

;; Counts the longest chain of positions owned by player
;; on the whole board. Very, very complex. Alter at your
;; own peril.
(define (count-longest-chain player-space board)
  (apply max 
	 (map 
	  (lambda (num) 
	    (max 
	     (count-chain player-space (row-lookup num board))
	     (count-chain player-space (col-lookup num board))
	     (count-chain player-space 
			  (diagonal-lookup (make-position 1 num) 'right board))
	     (count-chain player-space 
			  (diagonal-lookup (make-position num 1) 'right board))
	     (count-chain player-space 
			  (diagonal-lookup (make-position 1 num) 'left board))
	     (count-chain player-space 
			  (diagonal-lookup 
			   (make-position num *BOARD-SIZE*) 'left board))))
	  (iota 1 (1+ *BOARD-SIZE*)))))
						  

;;;========================;;;
;;;  POSITION INFORMATION  ;;;
;;;========================;;;

;; Returns the value at the given position
;; 0 for OPEN
;; 1 for PLAYER 1
;; 2 for PLAYER 2
;; 3 for BLOCKED
(define (position-lookup position board)
  (if (not (position? position))
      (error "POSITION-LOOKUP: invalid position: " position)
      (let ((row (get-row position))
	    (col (get-column position)))
	(list-ref (list-ref board row) col))))

;; Returns true if a position has not been captured.
(define (open-position? position board)
  (if (not (position? position))
      (error "OPEN-POSITION?: invalid position: " position)
      (eq? *OPEN-SPACE* (position-lookup position board))))

;; owns-position?
(define (owns-position? status-symbol position board)
  (eq? status-symbol (position-lookup position board)))

;;;================;;;
;;;  MODIFY BOARD  ;;;
;;;================;;;

;; Make a new board.
(define (make-board)
  (let ((board ())
	(mid-index (/ *BOARD-SIZE* 2)))
    (set! board
	  (append 
	   (attach-tag 'board
		       (map (lambda (row)
			      (attach-tag 'row
					  (map (lambda (col) 
						 *OPEN-SPACE*)
					       (iota *BOARD-SIZE*))))
			    (iota *BOARD-SIZE*)))
	   (list (attach-tag 'last-move '()))))
	  
    (set-position-status! *BLOCKED-SPACE* 
			  (make-position mid-index mid-index) board)
    (set-position-status! *BLOCKED-SPACE* 
			  (make-position (+ 1 mid-index) mid-index) board)
    (set-position-status! *BLOCKED-SPACE* 
			  (make-position mid-index (+ 1 mid-index)) board)
    (set-position-status! *BLOCKED-SPACE* 
			  (make-position (+ 1 mid-index) (+ 1 mid-index)) board)
    (set-position-status! *PLAYER-1-SPACE* 
			  (make-position (- mid-index 1) (- mid-index 1)) board)
    (set-position-status! *PLAYER-1-SPACE* 
			  (make-position (+ 2 mid-index) (+ 2 mid-index)) board)
    (set-position-status! *PLAYER-2-SPACE* 
			  (make-position (- mid-index 1) (+ 2 mid-index)) board)
    (set-position-status! *PLAYER-2-SPACE* 
			  (make-position (+ 2 mid-index) (- mid-index 1)) board)
    (set-random-blocks! *NUM-BLOCKS* board)
    board))

;; Sets n random spaces to blocks
(define (set-random-blocks! n board)
  (map (lambda (x) 
	 (set-position-status! *BLOCKED-SPACE* 
			       (random-element (get-open-positions board))
			       board))
	 (iota n)))
  
;; Sets the status of a position as empty, blocked or owned by player 1 or 2.
(define (set-position-status! status-space position board)

  (define (set-space! status-space row column)
    (cond ((null? row) #f)
	  ((<= column 1) (set-car! row status-space) #t)
	  (else (set-space! status-space (cdr row) (- column 1)))))

  (let ((row (get-row position)) (column (get-column position)))
    (set-space! status-space (contents (row-lookup row board)) column)))

;; Sets the last move made
(define (set-last-move! position board)
  (set-car! (leave (contents board) *BOARD-SIZE*) 
	    (attach-tag 'last-move position)))

;; Marks a move on the board.
;; Returns true if move was successful.
(define (make-move! player-symbol position board)
    (cond ((null? position) #f)
	  ((open-position? position board)
	   (set-position-status! 
	    (if (eq? player-symbol *PLAYER-1-SYMBOL*) 1 2) position board)
	   (set-last-move! position board)
	   #t)
	  (else #f)))
