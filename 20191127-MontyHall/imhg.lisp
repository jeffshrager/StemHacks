;; (load (compile-file "imhg.lisp"))

;;; Iterated Monty Hall game.
;;; B is the number of total boxes
;;; N is the number taken out at each step
;;; Player always picks one, and then gets to stick or switch.

;;; So if, say, B=10 and N=2, the game goes: 10->9(player chooses
;;; one)->7 left(host opens 2) then player either sticks or switches
;;; and the game is now 7->6(player chooses)->4, until there are only
;;; two boxes left. There are some confusing termination conditions,
;;; like what happens when you can only end up with one box; we simply
;;; ignore those and throw an error if we end up with less than 2
;;; boxes in the last step, so we only cope with the case of ending up
;;; with 1 box.

;;; (Yeah, I know that closed is spelled closed, I want it to align in outputs with 'open')

(defun imhg (n-total-boxes n-to-open &key (depth-limit 1) (plays 10000) (trace nil))
  (format t "~%~%================~%Running: n-total=boxes = ~a, n-to-open = ~a, depth-limit = ~a, plays = ~a~%================~%"
	  n-total-boxes n-to-open depth-limit plays)
  ;; boxes are either :clos :open or :winn (the :winn is obviously also :clos, and all :open ones are non-:winns)
  (report-results
   (loop for i below plays
	 as boxes = (loop for i below n-total-boxes collect :clos)
	 as choice = (random n-total-boxes)
	 do (setf (nth (random n-total-boxes) boxes) :winn) ;; Hide the prize
	 (when trace (format t "~%---~%"))
	 ;; Okay, so the boxes are set, the winner has been hidden, and the player has chosen his.
	 collect (iterate-mhg boxes choice n-to-open depth-limit trace)))
  )

;;; a result is (:winn/:loss choice choice choice...)  In the 1-deep
;;; "normal" game, this counts up the win/loss stick/switch table; It
;;; makes a little less sense for more than a 2-deep game.

(defun report-results (results) 
  (let* ((sw 0) (sl 0) (xw 0) (xl 0))
    (loop for result in results
	  as switch? = (loop for i in (cddr result) as first = (cadr result) when (not (= i first)) do (return t))
	  do (case (car result)
		   (:winn (if switch? (incf xw) (incf sw)))
		   (:loss (if switch? (incf xl) (incf sl)))))
    (print `(sw ,sw sl ,sl xw ,xw xl ,xl))
    (ignore-errors
      (print `([sw]/[sw+sl] ,(float (/ sw (+ sw sl))) xw/[xw+xl] ,(float (/ xw (+ xw xl))))))))

(defun iterate-mhg (boxes choice n-to-open depth-limit trace &optional selection-list)
  (labels
   ((report-result
     ()
     (let ((r (cons (if (eq (nth choice boxes) :winn) :winn :loss) selection-list)))
       (when trace (print r))
       r)))
   (push choice selection-list)
   (when trace (pprint boxes))
   (let* ((available-choices (loop for b in boxes as i from 0 by 1 when (member b '(:clos :winn)) collect i))
	  (n-closed-boxes (loop for b in boxes when (member b '(:winn :clos)) sum 1)))
     (when trace (pprint `(,depth-limit choice= ,choice n-to-open= ,n-to-open n-closed-boxes= ,n-closed-boxes)))
     ;; If we hit the limit, or are out of options return the win/loss immediately -- depth limit of 1 is the "normal" game
     (if (or (zerop depth-limit) (= 1 (length available-choices)))
	 (report-result)
       ;; If we're down to just 2 boxes, just report win or loss
       (if (= 2 n-closed-boxes)
	   (report-result)
	 ;; Otherwise iterate
	 (progn 
	   ;; When n-to-open is greater than the number of closed boxes left, plus one, we reduce it by 1 (WWW likely Obiwon error!)
	   (if (>= (+ 1 n-to-open) n-closed-boxes) (decf n-to-open))
	   ;; Monty chooses n-to-open boxes to open from among those that are closed
	   ;; Since it doesn't matter at all which ones are which, we just open the first n we come to.
	   ;; And don't open the one the player chose!
	   (loop for b on boxes
		 for i from 0 by 1
		 with n = n-to-open
		 until (zerop n)
		 if (and (eq :clos (car b)) (not (= i choice)))
		 do (setf (car b) :open n (1- n))
		 )
	   ;; Now player decides to stick or switch -- actually, all
	   ;; player has to do is pick a box from the new avilability
	   ;; list. If it happens to be the same box, then he's stuck
	   ;; by definition.
	   (let* ((available-choices (loop for b in boxes as i from 0 by 1 when (member b '(:clos :winn)) collect i))
		  (new-choice (nth (random (length available-choices)) available-choices)))
	     (iterate-mhg boxes new-choice n-to-open (- depth-limit 1) trace selection-list))))))))

;;(print (imhg 3 1 :plays 100 :trace t))
;;(print (imhg 10 5 :plays 100 :trace t))
;;(print (imhg 3 1 :plays 10000 :trace nil))
;;(print (imhg 10 5 :plays 10000 :trace nil))
(loop for n-to-open from 1 to 8
      do (print (imhg 10 n-to-open :depth-limit 100 :plays 10000 :trace nil)))
  
