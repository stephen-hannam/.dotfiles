;;; mine-sweeper
;; $Id: mine-sweeper.el,v 1.2 2001/01/31 05:23:56 tshm Exp $
;; To autoload it, insert in .emacs
;; (autoload 'mine-sweeper "mine-sweeper" nil t)

;; variables
(defvar mine-sweeper-mode-hook nil)
(defvar mine-sweeper-mode-map nil)
(if mine-sweeper-mode-map nil
  (setq mine-sweeper-mode-map (make-sparse-keymap))
  (define-key mine-sweeper-mode-map "\C-f" 'mine-move-right)
  (define-key mine-sweeper-mode-map "\C-b" 'mine-move-left)
  (define-key mine-sweeper-mode-map "\C-n" 'mine-move-down)
  (define-key mine-sweeper-mode-map "\C-p" 'mine-move-up)
  (define-key mine-sweeper-mode-map " " 'mine-open)
  (define-key mine-sweeper-mode-map "m" 'mine-mark)
  (define-key mine-sweeper-mode-map "b" 'mine-mark)
  (define-key mine-sweeper-mode-map "q" 'mine-clean-and-quit)
  (define-key mine-sweeper-mode-map [right] 'mine-move-right)
  (define-key mine-sweeper-mode-map [left] 'mine-move-left)
  (define-key mine-sweeper-mode-map [down] 'mine-move-down)
  (define-key mine-sweeper-mode-map [up] 'mine-move-up))
(defvar mine-field-hsize 20
  "horizontal size of mine-field")
(defvar mine-field-wsize 40
  "vertical size of mine-field")
(defvar mine-field nil
  "mine-field array")
(defvar mine-amount 30
  "number of mines in the mine-field")
(defvar mine-marked 0
  "number of marked cell")
(defvar mine-opened 0
  "number of opend cell")
(defvar mine-point [0 0]
  "current pointing cell")
(defvar mine-time nil
  "started time in sec.")
(defvar mine-mark-bomb ?*
  "symbol represents the mine")
(defvar mine-mark-field ?.
  "symbol represents unsearched field")
(defvar mine-mark-0 ?0
  "symbol represents cleared field")

;;; main
(defun mine-sweeper ()
  "mine-sweeper
SPC  open
b    mark
m    mark
q    quit"
  (interactive)
  (setq max-lisp-eval-depth 3000)  ;for recursive call of open.           
  (setq max-specpdl-size 3000)     ;for recursive call of open.
  (get-buffer-create "Mine")
  (set-window-buffer (selected-window) "Mine")
  (setq overwrite-mode t)
  (setq major-mode 'mine-sweeper-mode
        mode-name  "Mines")
  (use-local-map mine-sweeper-mode-map)
  (run-hooks 'mine-sweeper-mode-hook)
  (mine-start)
  (setq buffer-read-only t))

(defun mine-start ()
  (erase-buffer)
  (setq mine-marked 0)
  (setq mine-opened 0)
  (let ((wsize (string-to-int 
	       (read-string "How large is the ground(width)? " 
			     (int-to-string mine-field-wsize))))
	(hsize (string-to-int 
		(read-string "How large is the ground(height)? " 
			     (int-to-string mine-field-hsize))))
	(amount (string-to-int 
		 (read-string "How many bombs? " 
			       (int-to-string mine-amount)))))
    (setq mine-field-wsize (if (> wsize 80) mine-field-wsize wsize))
    (setq mine-field-hsize (if (> hsize 40) mine-field-hsize hsize))
    (setq mine-amount 
	  (if (or (> amount (* mine-field-hsize mine-field-wsize))
		  (> amount 100)) mine-amount amount))
    (setq mode-name 
	  (concat "Mines " (int-to-string mine-marked) 
		  "/" (int-to-string mine-amount)))
    (force-mode-line-update)
    (mine-seeding)
    (mine-display)
    (mine-move-cursor 0 0)
    (setq mine-time (nth 1 (current-time)))))

(defun mine-field-set (row column &optional val)
  (if (and (<= 0 row) (< row mine-field-hsize)
	   (<= 0 column) (< column mine-field-wsize))
      (if val (aset mine-field (+ column (* row mine-field-wsize)) val)
	(mine-field-set row column (1+ (mine-field-ref row column))))))

(defun mine-field-ref (row column)
  (if (and (<= 0 row) (< row mine-field-hsize)
	   (<= 0 column) (< column mine-field-wsize))
      (aref mine-field (+ column (* row mine-field-wsize)))
    nil))

(defun mine-seeding ()
  (random t)
  (setq mine-field (make-vector (* mine-field-wsize mine-field-hsize) 0))
  (let ((i 0))
    (while (< i mine-amount)
      (let ((pos (mod (random) (* mine-field-hsize mine-field-wsize))))
	(if (= 0 (aref mine-field pos))
	    (progn (aset mine-field pos 9)
		   (setq i (1+ i)))))))
  (let ((i 0)
	(buffer-read-only nil))
    (while (< i mine-field-hsize)
      (let ((j 0) 
	    (buffer-read-only nil))
	(while (< j mine-field-wsize)
	  (if (> (mine-field-ref i j) 8)
	      (progn 
		(mine-field-set (1- i) (1- j))
		(mine-field-set (1- i) j)
		(mine-field-set (1- i) (1+ j))
		(mine-field-set i (1- j))
		(mine-field-set i (1+ j))
		(mine-field-set (1+ i) (1- j))
		(mine-field-set (1+ i) j)
		(mine-field-set (1+ i) (1+ j))))
	  (setq j (1+ j))))
      (insert-string "\n")
      (setq i (1+ i)))))

(defun mine-move-right ()
  (interactive)
  (let ((row (aref mine-point 0))
	(column (aref mine-point 1)))
    (mine-move-cursor row (1+ column))))

(defun mine-move-left ()
  (interactive)
  (let ((row (aref mine-point 0))
	(column (aref mine-point 1)))
    (mine-move-cursor row (1- column))))

(defun mine-move-up ()
  (interactive)
  (let ((row (aref mine-point 0))
	(column (aref mine-point 1)))
    (mine-move-cursor (1- row) column)))

(defun mine-move-down ()
  (interactive)
  (let ((row (aref mine-point 0))
	(column (aref mine-point 1)))
    (mine-move-cursor (1+ row) column)))

(defun mine-open ()
  (interactive)
  (if (= (following-char) mine-mark-field)
      (let ((i (aref mine-point 0))
	    (j (aref mine-point 1))
	    (num (mine-field-ref (aref mine-point 0) (aref mine-point 1)))
	    (buffer-read-only nil))
	(delete-char 1)
	(insert-char (cond ((= num 0) mine-mark-0)
			   (t (string-to-char (int-to-string num)))) 1)
	(backward-char 1)
	(cond 
	 ((< num 9) 
	  (setq mine-opened (1+ mine-opened))
	  (if (= (+ mine-opened mine-amount)
		 (* mine-field-wsize mine-field-hsize))
	      (mine-over t))))
	(cond 
	 ((= 0 num)
	  (mine-move-cursor (1- i) (1- j)) (mine-open)
	  (mine-move-cursor (1- i) j)      (mine-open)
	  (mine-move-cursor (1- i) (1+ j)) (mine-open)
	  (mine-move-cursor i (1- j))      (mine-open)
	  (mine-move-cursor i (1+ j))      (mine-open)
	  (mine-move-cursor (1+ i) (1- j)) (mine-open)
	  (mine-move-cursor (1+ i) j)      (mine-open)
	  (mine-move-cursor (1+ i) (1+ j)) (mine-open)
	  (mine-move-cursor i j))
	 ((<= 9 (mine-field-ref i j)) (mine-over nil))))))

(defun mine-over (&optional win-p)
  (mine-display t)
  (let ((dt (- (nth 1 (current-time)) mine-time)))
    (if (y-or-n-p 
	 (concat "You spend " (int-to-string dt) "sec.  "
		  (if win-p "You won the game!! Try another? "
		      "Bomb!! You lost.  Game Over!! Want another game?")))
	(mine-start)
      (mine-clean-and-quit))))

(defun mine-clean-and-quit () 
  (interactive)
  (kill-buffer "Mine"))

(defun mine-mark ()
  (interactive)
  (let ((buffer-read-only nil))
    (cond 
     ((= (following-char) mine-mark-field) 
      (progn
	(delete-char 1)
	(insert-char mine-mark-bomb 1)
	(setq mine-marked (1+ mine-marked))
	(setq mode-name 
	      (concat "Mines " (int-to-string mine-marked) 
		      "/" (int-to-string mine-amount)))
	(force-mode-line-update)
	(backward-char 1)))
     ((= (following-char) mine-mark-bomb)
      (progn 
	(delete-char 1)
	(insert-char mine-mark-field 1)
	(setq mine-marked (1- mine-marked))
	(setq mode-name 
	      (concat "Mines " (int-to-string mine-marked) 
		      "/" (int-to-string mine-amount)))
	(force-mode-line-update)
	(backward-char 1))))))
  
(defun mine-move-cursor (row column)
  (if (and (<= 0 row) (< row mine-field-hsize)
	   (<= 0 column) (< column mine-field-wsize))
      (progn
	(aset mine-point 0 row)
	(aset mine-point 1 column)
	(goto-line (1+ row))
	(forward-char column))))

(defun mine-display (&optional all)
  (erase-buffer)
  (let ((i 0)
	(buffer-read-only nil))
    (while (< i mine-field-hsize)
      (let ((j 0))
	(while (< j mine-field-wsize)
	  (mine-move-cursor i j)
	  (insert-char
	   (if all 
	       (let ((num (mine-field-ref i j)))
		 (cond ((> num 8) mine-mark-bomb)
		       ((= num 0) mine-mark-0)
		       (t (string-to-char (int-to-string num))))) 
	     mine-mark-field) 1)
	  (setq j (1+ j)))
	(insert "\n"))
      (setq i (1+ i)))))
