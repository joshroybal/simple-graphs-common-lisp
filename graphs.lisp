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
  (mapcar #'car
	  (mapcar #'(lambda (x) (set-difference x (list node)))
		  (remove-if #'(lambda (x) (not (member node x))) edges))))

(defun print-adjacency-list (graph)
  (dolist (node (adjacency-list graph))
    (format t "~& ~S -> " (car node))
    (dolist (edge (second node))
      (format t "|~S|->" edge))
    (format t "|/|" (car (last (second node))))))

(defun letter-index (letter)
  (let ((c (coerce letter 'character)))
    (- (char-code c) 65)))

(defun adjacency-matrix (g)
  (let ((n (length (first g))) (edges (second g)))
    (let ((mat (make-array (list n n) :initial-element 0)))
      (do ((in edges (cdr in))
	   (indices (mapcar #'letter-index (car edges))
		    (mapcar #'letter-index (car in))))
	  ((null in) mat)
	(let ((row (first indices)) (col (second indices)))
	  (progn
	    (setf (aref mat row col) 1)
	    (setf (aref mat col row) 1)))))))

(defun print-adjacency-matrix (g)
  (let ((n (length (first g))) (matrix (adjacency-matrix g)))
    (do ((i 0 (+ i 1)))
	((= i n) nil)
      (format t "~&")
      (do ((j 0 (+ j 1)))
	  ((= j n) nil)
	(format t "~2d" (aref matrix i j))))))

;;; applicative adjacency list construction
(defun adjacency-list (g)
  (let ((v (first g)) (e (second g)))
    (mapcar #'(lambda (x) (cons x (list (neighbors x e)))) v)))

;;; if we ever don't want the consed node symbols
(defun adjacency-list-beta (nodes edges)
  (mapcar #'(lambda (x) (neighbors x edges)) nodes))

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
  (let ((edges (second g)))
    (with-open-file (outfile "graph.gv"
			     :direction :output
			     :if-exists :supersede
			     :if-does-not-exist :create)
      (progn
	(format outfile "graph { layout=circo; node [shape=circle height=0 width=0 margin=0 style=filled]")
	(dolist (edge edges)
	  (format outfile " ~S -- ~S " (first edge) (second edge)))
	(dolist (node (color g))
	  (format outfile "~&~a [fillcolor=~a]" (car node) (cdr node)))
	(format outfile "}")))))

(defun random-choice (lis)
  (nth (random (length lis)) lis))

(defun degree (node graph)
  (let ((edges (second graph)))
    (length (neighbors node edges))))

(defun degree-list (graph)
  (let ((nodes (first graph)))
    (mapcar #'(lambda (x) (degree x graph)) nodes)))

(defun degree-alist (graph)
  (pairlis (first graph) (degree-list graph)))

(defun ordered-degree-alist (graph)
  (sort (pairlis (first graph) (degree-list graph)) #'> :key #'cdr))

(defun non-neighbors (node v e)
  (set-difference v (neighbors node e)))

(defun paint-aux (node v e)
  (set-difference v (neighbors node e)))

;;; Welch-Powell algorithm (Seymour Lipschutz, Discrete Mathematics)
(defun paint (graph)
  (let ((v (mapcar #'car (ordered-degree-alist graph)))
	(e (second graph))
	(colors '(red orange yellow green blue indigo violet)))
    (do ((nodes v)
	 (c colors (cdr c))
	 (coloring nil))
	((null nodes) coloring)
      (let ((seq (non-neighbors (car nodes) nodes e)))
	(format t "~&~S" seq)
	(dolist (item seq)
	  (cond ((null (all-adjacents item seq graph))
		 (setf coloring (cons (cons item (car c)) coloring))
		 (setf nodes (remove item nodes)))))))))

(defun adjacent (v1 v2 g)
  (member (list v1 v2) (second g) :test 'equalp))

(defun adjacent-p (v1 v2 g)
  (if (null (adjacent v1 v2 g))
      nil
      t))

(defun all-adjacents (v seq g)
  (remove nil (mapcar #'(lambda (x) (adjacent v x g)) seq)))

(defun adjacent-nodes (v g)
  (let ((e (second g)))
     (remove-if #'null
		(mapcar #'car
			(mapcar #'(lambda (x) (set-difference x (list v)))
				(mapcar #'(lambda (x) (member v x)) e))))))

(defun non-adjacent-nodes (v g)
  (set-difference (first g) (adjacent-nodes v g)))

;;; auxiliary function for use below
(defun non-adjacent-nodes-aux (v nodes g)
  (set-difference nodes (adjacent-nodes v g)))

(defun color (g)
  (let ((v (mapcar #'car (ordered-degree-alist g)))
	(colors '(red orange yellow green blue indigo violet)))
    (do ((nodes v)
	 (c colors (cdr c))
	 (coloring nil))
	((null nodes) coloring)
      (let ((seq (non-adjacent-nodes-aux (car nodes) nodes g)))
	(format t "~&~S" seq)
	(dolist (item seq)
	  (cond ((null (all-adjacents item seq g))
		 (setf coloring (cons (cons item (car c)) coloring))
		 (setf nodes (remove item nodes)))))))))
