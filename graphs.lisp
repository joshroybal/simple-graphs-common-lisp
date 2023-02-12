(defun letter (n)
  (intern (string (code-char n))))

(defun nodes (n)
  (labels ((recur (k lis)
	     (if (= k 0)
		 lis
		 (recur (- k 1) (cons (letter (+ 64 k)) lis)))))
    (recur n nil)))

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

(defun neighbors (node edges)
  (mapcar #'car
	  (mapcar #'(lambda (x) (set-difference x (list node)))
		  (remove-if #'(lambda (x) (not (member node x))) edges))))

(defun print-adjacency-list (graph)
  (dolist (nodes (adjacency-list graph))
    (format t "~& ~S: " (car nodes))
    (do ((in (cdr nodes) (cdr in)))
	((null in) 'done)
      (format t "~S" (car in))
      (if (cdr in) (format t ",")))))

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
;;; car is node, cdr are adjacenct nodes
(defun adjacency-list (g)
  (let ((v (first g)) (e (second g)))
    (pairlis v (mapcar #'(lambda (x) (neighbors x e)) v))))

(defun node-color (label color)
  (format nil "~&~S [fillcolor=~A]" (string label) color))

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

;; (defun random-choice (lis)
;;   (nth (random (length lis)) lis))

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

;;; returns nil if node not ajacent to any node in nodes
(defun none-adjacent-p (node nodes adjlis)
  (not (remove nil (mapcar #'(lambda (x) (adjacent-p node x adjlis)) nodes))))

(defun adjacent (node1 node2 adjlis)
  (member node1 (cdr (assoc node2 adjlis))))

(defun adjacent-p (v1 v2 adjlis)
  (if (null (adjacent v1 v2 adjlis))
      nil
      t))

(defun adjacent-nodes (node adjlis)
  (cdr (assoc node adjlis)))

(defun non-adjacent-nodes (node nodes adjlis)
  (set-difference nodes (adjacent-nodes node adjlis)))

;;; Welch-Powell algorithm.
(defun color (g)
  (let ((v (mapcar #'car (ordered-degree-alist g)))
	(colors '(red orange yellow green blue indigo violet))
	(adjlis (adjacency-list g)))
    (do ((nodes v)
	 (c colors (cdr c))
	 (coloring nil))
	((null nodes) coloring)
      (let ((seq (non-adjacent-nodes (car nodes) nodes adjlis))
	    (assigned nil))
	(format t "~&~S" seq)
	(dolist (item seq)
	  (cond ((none-adjacent-p item assigned adjlis)
		 (setf coloring (cons (cons item (car c)) coloring))
		 (setf assigned (cons item assigned))
		 (setf nodes (remove item nodes)))))))))
