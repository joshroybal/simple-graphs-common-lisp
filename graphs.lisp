(defun letter (n)
  (intern (string (code-char n))))

(defun nodes (n)
  (labels ((recur (k lis)
	     (if (= k 0)
		 lis
		 (recur (- k 1) (cons (letter (+ 64 k)) lis)))))
    (recur n nil)))

(defun edge (a b)
  (list a b))

(defun edge-set (edges)
  (if (null edges)
      nil
      (cons (car edges) (edge-set (cdr edges)))))

(defun assign-edge ()
  (if (> (random 2) 0)
      t
      nil))

;;; no loops
(defun random-edges-aux (node nodes)
  (cond ((null (cdr nodes))
	 nil)
	((assign-edge)
	 (cons (list node (cadr nodes)) (random-edges-aux node (cdr nodes))))
	(t
	 (random-edges-aux node (cdr nodes)))))

;;; no loops for now
(defun random-edges (nodes)
  (cond ((null nodes)
	 nil)
	(t
	 (append (random-edges-aux (car nodes) nodes)
		 (random-edges (cdr nodes))))))

(defun random-graph (n)
  (let ((lis (nodes n)))
    (progn
      (setf *random-state* (make-random-state t))
      (list lis (random-edges lis)))))

(defun edge-list (node nodes)
  (cond ((null nodes)
	 nil)
	((assign-edge)
	 (cons (cadr nodes) (edge-list node (cdr nodes))))
	(t
	 (edge-list node (cdr nodes)))))

(defun neighbors (node edges)
  (cond ((null edges) nil)
	((member node (car edges))
	 (append (set-difference (car edges) (list node))
		 (neighbors node (cdr edges))))
	(t
	 (neighbors node (cdr edges)))))

;; (defun adjacency-list-aux (nodes edges res)
;;   (if (null nodes)
;;       (reverse res)
;;       (adjacency-list-aux
;;        (cdr nodes)
;;        edges
;;        (cons (neighbors (car nodes) edges) res))))

;; (defun adjacency-list (graph)
;;   (let ((nodes (first graph)) (edges (second graph)))
;;     (mapcar #'(lambda (x y) (cons x (list y)))
;; 	    nodes
;; 	    (adjacency-list-aux nodes edges nil))))

;; (defun print-adjacency-list (graph)
;;   (dolist (node (adjacency-list graph))
;;     (format t "~& ~S -> " (car node))
;;     (dolist (edge (second node))
;;       (format t "|~S|->" edge))
;;     (format t "|/|" (car (last (second node))))))

;; (defun letter-index (letter)
;;   (let ((c (coerce letter 'character)))
;;     (- (char-code c) 65)))

;; (defun adjacency-matrix (g)
;;   (let ((n (length (first g))) (edges (second g)))
;;     (let ((mat (make-array (list n n) :initial-element 0)))
;;       (do ((in edges (cdr in))
;; 	   (indices (mapcar #'letter-index (car edges))
;; 		    (mapcar #'letter-index (car in))))
;; 	  ((null in) mat)
;; 	(let ((row (first indices)) (col (second indices)))
;; 	  (progn
;; 	    (setf (aref mat row col) 1)
;; 	    (setf (aref mat col row) 1)))))))

;; (defun print-adjacency-matrix (g)
;;   (let ((n (length (first g))) (matrix (adjacency-matrix g)))
;;     (do ((i 0 (+ i 1)))
;; 	((= i n) nil)
;;       (format t "~&")
;;       (do ((j 0 (+ j 1)))
;; 	  ((= j n) nil)
;; 	(format t "~2d" (aref matrix i j))))))

;;; applicative adjacency list construction
(defun adjacency-list (g)
  (let ((v (first g)) (e (second g)))
    (mapcar #'(lambda (x) (cons x (list (neighbors x e)))) v)))

(defun print-dot-graph (g)
  (let ((edges (second g)))
    (progn
      (format t "graph {")
      (dolist (edge edges)
	(format t " ~S -- ~S " (first edge) (second edge)))
      (format t "}"))))

(defun node-color (label color)
  (format nil "~&~S [fillcolor=~A]" (string label) color))

(defun node-colors (nodes)
  (do ((in nodes (cdr in))
       (colors
	'("red" "orange" "yellow" "green" "blue" "indigo" "violet")
	(cdr colors))
       (out nil (cons (node-color (car in) (car colors)) out)))
      ((null in) (reverse out))))

(defun write-dot-file (g)
  (let ((nodes (first g)) (edges (second g)))
    (with-open-file (outfile "graph.gv"
			     :direction :output
			     :if-exists :supersede
			     :if-does-not-exist :create)
      (progn
	(format outfile "graph { layout=circo; node [shape=circle height=0 width=0 margin=0 style=filled]")
	(dolist (edge edges)
	  (format outfile " ~S -- ~S " (first edge) (second edge)))
	(dolist (line (node-colors nodes))
	  (format outfile "~&~a" (intern line)))
	(format outfile "}")))))
