#!/bin/sh
set -v
./job.lisp
dot -Tsvg graph.gv > graph.svg
