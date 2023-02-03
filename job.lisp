#!/usr/bin/clisp
(load "graphs.fas")
(defvar *G*)
(setf *random-state* (make-random-state t))
(setf *G* (random-graph (+ 3 (random 6 *random-state*))))
(print-adjacency-list *G*)
(write-dot-file *G*)
(run-shell-command "dot -Tsvg graph.gv > graph.svg")
(run-shell-command "geeqie graph.svg &")
