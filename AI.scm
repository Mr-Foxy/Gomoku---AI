;;;; CSci 1901 Project - Spring 2013
;;;; gomoku++ Player AI

;;;======================;;;
;;;  SUBMISSION DETAILS  ;;;
;;;======================;;;

;; List both partners' information below.
;; Leave the second list blank if you worked alone.
(define (r) (load "random.scm"))
(define authors 
  '((
     "Yao Zhao"   ;; Author 1 Name
     "zhaox383"   ;; Author 1 X500
     "4660658"   ;; Author 1 ID
     "011"   ;; Author 1 Section
     )
    (
     "Mingzi Zhou"   ;; Author 2 Name
     "zhoux494"   ;; Author 2 X500
     "4641154"   ;; Author 2 ID
     "12"   ;; Author 2 Section
     )))


;;;====================;;;
;;;  Player-Procedure  ;;;
;;;====================;;;

(define player-procedure
  (let () 

    (define (my-sleep t)
      (define (helper t)
  (if (> (system-clock) t)
	    0
	    (helper t)))
      (helper (+ (system-clock) (/ t 1000))))
    
;;FIND CHAIN PACKAGE

;Input:
    ;tag: distinguish if it is denfense-evaluation or offense-evaluation
    ;posit: the open-position procedure need to find chain
    ;board: situation in the board
    ;player: Ai player tag, 'p1 or 'p2
;Output
;This procedure would scan around the open position and find chains possible to form
  (define (find-chain-value tag posit board player)
    
;Ouput player number 
    ;player1 is 1 and player2 is 2
    ;translate player tag to player number
    (define *PLAYER_NUMBER*
      (if (eq? player 'p1)
	  1
	  2))

;Input:
    ;Posit: the open position
    ;Chain: a list, the row list need to check
;Output:
    ;A list consisting of tag of type chain and chain position List in order 
    ;Sample: ('row ('position 3 4) (position 3 5))
;The procedure would check the chain in the row

    (define (check-row-chain posit chain)
      
      (define initial-num (get-column posit))

      (define (left-chain num)
	(if (< num 1)
	    ()
	    (cond ((= (list-ref chain num) *PLAYER_NUMBER*) (cons (make-position (get-row posit) num) (left-chain (- num 1))))
		  (else ()))))

      (define (right-chain num)
	(if (> num *BOARD-SIZE*) 
	     ()
	     (cond ((= (list-ref chain num) *PLAYER_NUMBER*) (cons (make-position (get-row posit) num) (right-chain (+ num 1))))
		   (else ()))))
	
      (attach-tag 'row (append (reverse (left-chain (- initial-num 1))) (cons posit (right-chain (+ initial-num 1))))))

;Input:
    ;Posit: the open position
    ;Chain: a list, the col list need to check
;Output:
    ;A list consisting of tag of type chain and chain position List in order 
    ;Sample: ('col ('position 3 4) (position 4 4))
;The procedure would check the chain in the col

    (define (check-col-chain posit chain)
      
      (define initial-num (get-row posit))

      (define (left-chain num)
	(if (< num 1)
	    ()
	    (cond ((= (list-ref chain num) *PLAYER_NUMBER*) (cons (make-position num (get-column posit)) (left-chain (- num 1))))
		  (else ()))))
      
      (define (right-chain num)
	(if (> num *BOARD-SIZE*) 
	    ()
	    (cond ((= (list-ref chain num) *PLAYER_NUMBER*) (cons (make-position num (get-column posit)) (right-chain (+ num 1))))
		  (else ()))))
      
      (attach-tag 'col  (append (reverse (left-chain (- initial-num 1))) (cons posit (right-chain (+ initial-num 1))))))

;Input:
    ;Posit: the open position
;Output:
    ;A list consisting of tag of type chain and chain position List in order 
    ;Sample: ('row ('position 2 2) (position 1 3))
;The procedure would check the chain in the left dialog   

    (define (check-left-dia posit)
      
      (define (left-chain row col)
	(let ((current-position (make-position row col)))
	  (If (valid-position? current-position)
	      (cond ((= (position-lookup current-position board) *PLAYER_NUMBER*)
		     (cons (make-position row col) (left-chain (+ row 1) (- col 1))))
		    (else ()))
	      ())))
      
      (define (right-chain row col)
	(let ((current-position (make-position row col)))
	  (If (valid-position? current-position)
	      (cond ((= (position-lookup current-position board) *PLAYER_NUMBER*)
		     (cons (make-position row col) (right-chain (- row 1) (+ col 1))))
		    (else ()))
	      ())))
     
      (attach-tag 'left-dia (append (left-chain (+ (get-row posit) 1) (- (get-column posit) 1)) (cons posit (right-chain (- (get-row posit) 1) (+ (get-column posit) 1))))))

;Input:
    ;Posit: the open position
;Output:
    ;A list consisting of tag of type chain and chain position List in order 
    ;Sample: ('right-dia ('position 4 4) (position 3 3))
;The procedure would check the chain in the right dialog     
  
    (define (check-right-dia posit)
  
      (define (left-chain row col)
	(let ((current-position (make-position row col)))
	  (if (valid-position? current-position)
	      (cond ((= (position-lookup current-position board) *PLAYER_NUMBER*)
		     (cons (make-position row col) (left-chain (- row 1) (- col 1))))
		    (else ()))
	      ())))
      
      (define (right-chain row col)
	(let ((current-position (make-position row col)))
	  (if (valid-position? current-position)
	      (cond ((= (position-lookup current-position board) *PLAYER_NUMBER*)
		     (cons (make-position row col) (right-chain (+ row 1) (+ col 1))))
		    (else ()))
	      ())))
      
      (attach-tag 'right-dia (append (reverse (left-chain (- (get-row posit) 1) (- (get-column posit) 1))) (cons posit (right-chain (+ (get-row posit) 1) (+ (get-column posit) 1))))))

;Output:
;      The list would gather all the result from check-row-chain check-col-chain check-left-dia and check-right-dia 
    (define raw-chain-lst (list (check-row-chain posit (row-lookup (get-row posit) board)) 
				(check-col-chain posit (col-lookup (get-column posit) board)) 
				(check-left-dia posit) (check-right-dia posit)))
;Output: 
;     Create a list consists of length of the all chains and block situation of all chains
    (define (create-detailed-chain-list lst)
      (if (null? lst)
	  ()
	  (cons (list (type-tag (car lst)) (length (list-tail (car lst) 1)) (block-situation (car lst))) (create-detailed-chain-list (cdr lst)))))
;Input:
;    chain:a list with all points position
;Output:
    ;;2 for non blocked chain
    ;;1 for one-sided blocked chain
    ;;0 for two-sided blocked chain
;The procedure would check the chain and find if it is blocked

    (define (block-situation chain)
      (cond ((< (length (cadr chain)) 3) ())
	    ((eq? (type-tag chain) 'row) (block-for-row (list-tail chain 1)))
	    ((eq? (type-tag chain) 'col) (block-for-col (list-tail chain 1)))
	    ((eq? (type-tag chain) 'left-dia) (block-for-l-dia (list-tail chain 1)))
	    ((eq? (type-tag chain) 'right-dia) (block-for-r-dia (list-tail chain 1)))
	    (else "wtf")))
    
      (define (block-for-row chain-lst)
	(let ((start-position (make-position (get-row (car chain-lst)) (-1+ (get-column (car chain-lst)))))
	      (end-position (make-position (get-row (car chain-lst)) (1+ (get-column (get-last-element chain-lst))))))
	 
	 (+ (check-open-and-valid start-position) (check-open-and-valid end-position))))
      
      (define (block-for-col chain-lst)
	(let ((start-position (make-position (-1+ (get-row (car chain-lst))) (get-column (car chain-lst))))
	      (end-position (make-position (1+ (get-row (get-last-element chain-lst)))  (get-column (car chain-lst)))))
	 
	 (+ (check-open-and-valid start-position) (check-open-and-valid end-position))))

      (define (block-for-l-dia chain-lst)
	(let ((start-position (make-position (1+ (get-row (car chain-lst))) (-1+ (get-column (car chain-lst)))))
	      (end-position (make-position (-1+ (get-row (get-last-element chain-lst))) (1+ (get-column (get-last-element chain-lst))))))
	  
	  (+ (check-open-and-valid start-position) (check-open-and-valid end-position))))

      (define (block-for-r-dia chain-lst)
	(let ((start-position (make-position (-1+ (get-row (car chain-lst))) (-1+ (get-column (car chain-lst)))))
	      (end-position (make-position (1+ (get-row (get-last-element chain-lst))) (1+ (get-column (get-last-element chain-lst))))))
	  
	  (+ (check-open-and-valid start-position) (check-open-and-valid end-position))))
	      
    ;;block situation:
    ;;2 for non blocked chain
    ;;1 for one-sided blocked chain
    ;;0 for two-sided blocked chain

      (define (get-chain-num datum)
	(cadr datum))
      
      (define (get-block-situation datum)
	(caddr datum))

      (define (get-chain-type datum) 
	(car datum))
      
;Input:
;    lst: detailed chain lst with chain length and block situation
;Output:
;    Value got by analyzing chain
;This procedure would analyze the situation of chain and get a value according to chain length and block situation(for opponent)

      (define (defense-evaluate lst)
	(cond ((null? lst) 0)
	      ((>= (get-chain-num (car lst)) 5) (+ 1000000000 (defense-evaluate (cdr lst))))
	      ((and (= (get-chain-num (car lst)) 4) (= (get-block-situation (car lst)) 2)) (+ 1000000 (defense-evaluate (cdr lst))))

	      ((and (= (get-chain-num (car lst)) 4) (= (get-block-situation (car lst)) 1) 
		    (or (eq? (get-chain-type lst) 'right-dia) (eq? (get-chain-type lst) 'left-dia)))
	       
	       (+ 4500 (defense-evaluate (cdr lst))))

	      ((and (= (get-chain-num (car lst)) 4) (= (get-block-situation (car lst)) 1))
	       
	       (+ 4000  (defense-evaluate (cdr lst))))

	      ((and (= (get-chain-num (car lst)) 3) (= (get-block-situation (car lst)) 2) 
		    (or (eq? (get-chain-type lst) 'right-dia) (eq? (get-chain-type lst) 'left-dia)))
	       
	       (+ 7000 (defense-evaluate (cdr lst))))
	      
	      ((and (= (get-chain-num (car lst)) 3) (= (get-block-situation (car lst)) 2)) (+ 7500 (defense-evaluate (cdr lst))))
	      
	      (else (defense-evaluate (cdr lst)))))
;Input:
;    lst: detailed chain lst with chain length and block situation
;Output:
;    Value got by analyzing chain
;This procedure would analyze the situation of chain and get a value according to chain length and block situation(for player)

      (define (offense-evaluate lst)
	(cond ((null? lst) 0)
	      ((= (get-chain-num (car lst)) 5) (+ 100000000000 (offense-evaluate (cdr lst))))
	      ((and (= (get-chain-num (car lst)) 4) (= (get-block-situation (car lst)) 2)) (+ 100000 (offense-evaluate (cdr lst))))
	      
	      ((and (= (get-chain-num (car lst)) 4) (= (get-block-situation (car lst)) 1))
	       
	       (+ 1000 (offense-evaluate (cdr lst))))

	      ((and (= (get-chain-num (car lst)) 4) (= (get-block-situation (car lst)) 1)) (+ 850 (offense-evaluate (cdr lst))))
	      
	      ((and (= (get-chain-num (car lst)) 3) (= (get-block-situation (car lst)) 2) 
		    (or (eq? (get-chain-type lst) 'right-dia) (eq? (get-chain-type lst) 'left-dia)))
	       
	       (+ 1800 (offense-evaluate (cdr lst))))

	      ((and (= (get-chain-num (car lst)) 3) (= (get-block-situation (car lst)) 2)) (+ 1500 (offense-evaluate (cdr lst))))
	      
	      (else (offense-evaluate (cdr lst)))))
      
      (define (get-last-element lst)
	(list-ref lst (- (length lst) 1)))
      
      (define (check-open-and-valid posit)
	(if (valid-position? posit)
	    (if (open-position? posit board)
		1
	      0)
	    0))
      
      (if (eq? tag 'defense)
	  (defense-evaluate (create-detailed-chain-list raw-chain-lst))
	  (offense-evaluate (create-detailed-chain-list raw-chain-lst))));end of find-chain-value
  
    ;;VALUE CALCULATING PACKAGE   
    (define (get-value player position board)

;get play number by player symbol
      (define *PLAYER_NUMBER*
	(if (eq? player 'p1)
	    1
	    2))
;Input:
;    position: an open position
;Output:
;    it would return a list of pairs:
;                                   (start/end-row/col number . value by distance from position and boundary)
;    This procedure will calculate the position's boundary value. The closer to boudnary, the bigger the value.  
      (define (check-boundary position)
	
	(define (start-row position)
	  (if (< (get-row position) 4)
	      (cons 1 (* 150 (- 3 (get-row position))))
	      (cons (- (get-row position) 3) 0)))
	
	(define (end-row position)
	  (if (> (get-row position) (- *BOARD-SIZE* 3))
	      (cons *BOARD-SIZE* (* 150 (- 3 (- *BOARD-SIZE* (get-row position)))))
	      (cons (+ (get-row position) 3) 0)))

	(define (start-col position)
	  (if (< (get-column position) 4)
	      (cons 1 (* 150 (- 3 (get-column position))))
	      (cons (- (get-column position) 3) 0)))
	
	(define (end-col position)
	  (if (> (get-column position) (- *BOARD-SIZE* 3))
	      (cons *BOARD-SIZE* (* 150 (- 3 (- *BOARD-SIZE* (get-column position)))))
	      (cons (+ (get-column position) 3) 0)))
	  

	(list (start-row position) (start-col position) (end-row position) (end-col position)))
      
      (define boundary-lst (check-boundary position))
      (define begin-row (car (list-ref boundary-lst 0)))
      (define terminate-row (car (list-ref boundary-lst 2)))
      (define begin-col (car (list-ref boundary-lst 1)))
      (define terminate-col (car (list-ref boundary-lst 3)))

;Input
;    row:row number of position
;    col:col number of position
;Output
;    a value of position(float)
;This procedure will calculate a 3*3 reagion. Player's move would increase the value, oppoent's move and block would decrease the value by distance.

      (define (value-cal row col)

	(define position-current (make-position row col))

	(define status (position-lookup position-current board))
	
	(define (square x) (* x x))

	(cond ((= status *PLAYER_NUMBER*) (* 75 (- 3 (max (abs (- row (get-row position))) (abs (- col (get-column position)))))))
	      ((= status 0) 0)
	      ((= status 3) (* -1 (* 150 (- 3 (max (abs (- row (get-row position))) (abs (- col (get-column position))))))))
	      (else (* -1 (* 50 (- 3 (max (abs (- row (get-row position))) (abs (- col (get-column position))))))))))

      (define (get-value-iter value current-col)

	(define (row-loop row col)
	  (cond ((<= row terminate-row) (+ (value-cal row col) (row-loop (+ 1 row) col)))
		(else 0)))

	(cond ((<= current-col terminate-col) (get-value-iter (+ value (row-loop begin-row current-col)) (+ current-col 1)))
	      (else value)))
;Input
;    lst: the boundary-list consisting of value by calculating distance to boundary
;Output
;    result value got by boudary calculation
;This procedure would gather the information from the boudary list and add them up

      (define (get-boudary-value lst)
	(if (null? lst)
	    0
	    (+ (cdar lst) (get-boudary-value (cdr lst)))))

      (define *OPPONENT* 
	(if (eq? player 'p1)
	    'p2
	    'p1))
;Get defense value and offense value by calculating player's and opponent's chain
      (define offense-value-from-chain (find-chain-value 'offense position board player))
      (define defense-value-from-chain (find-chain-value 'defense position board *OPPONENT*))
      
;add them up and find the position's value
      (+ defense-value-from-chain offense-value-from-chain (- (get-value-iter 0 begin-col) (get-boudary-value boundary-lst)))
      );end of get-value
;Input
;    player: the player symbol
;    position-lst- open-position lst
;    board: current board
;Output
;    the position with max value in get-value
;    The procedure would scan every open position and calculate their value. Finally find a largest value position and return it 
    (define (max-value-position player position-lst board)
      
      (define (iter-part position-list max-value-position max-value)
	(cond ((null? position-list) max-value-position)
	      ((> (get-value player  (car position-list) board) max-value)
	       (iter-part (cdr position-list) (car position-list) (get-value player (car position-list) board)))
	      (else (iter-part (cdr position-list) max-value-position max-value))))
      
      (iter-part position-lst '() -99999999))

;;End of VALUE CALCULATION PACKAGE

    (define (get-move player board)
      (max-value-position player (get-open-positions board) board)
      )
    
    
    get-move
    )) ;; End of player-procedure

;;TEST PACKAGE

(define (list-set-nth my-lst n newvalue)
  (define (recur-part n counter lst-re)
    (cond ((null? lst-re) ())
	  ((= counter n) (begin (set-car! lst-re newvalue) lst-re))
	  (else (cons (car lst-re) (recur-part n (+ 1 counter) (cdr lst-re))))))
  (recur-part n 0 my-lst))

(define (m-b position board)
  
  (list-set-nth (list-ref board (get-row position)) (get-column position) 1))

(define (m r l)
  (make-position r l))
