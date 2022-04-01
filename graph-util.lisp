;; Replaces each of the non-alphanumeric characters of exp with an "_"
(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))


;; If the length of the label surpases *max-label-length*, then it takes the
;; substring corresponding to the (- length 3) and appends a hypen "..." to it
(defparameter *max-label-length* 30)

(defun dot-label (exp)
  (if exp
    (let ((s (write-to-string exp :pretty nil)))
      (if (> (length s) *max-label-length*)
        (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
        s))
    ""))

;; Takes a list of nodes and prints them one after the
;; other in the format: [label="node-name"];
(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))

;; Takes a list of edges where each item has the format:
;; (node (place1 direction1 via1) (place2 direction2 via2) ...)
;; and prints each of them in the format of: NODE_NAME->PLACE[label="(DIRECTION VIA)"];
(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
          edges))

;; Takes a list of nodes and another of edges in the following format, respectivelly:
;; (node1 ...)
;; ((node1 (place1 direction1 via1) (place2 direction2 via2) ...) ...)
;; and prints them in the format required for the Graphviz CLI
(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

;; Creates a dot file in disk of name fname.
;; If the file already exists in disk, overwrites it.
(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
                   fname
                   :direction :output
                   :if-exists :supersede)
    (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O " fname)))

;; Creates a thunk function that takes no arguments, which calls graph->dot
;; with their nodes and edges and forwards it to the dot->png function
(defun graph->png (fname nodes edges)
  (dot->png fname
            (lambda  ()
              (graph->dot nodes edges))))

(defun uedges->dot (edges)
  (maplist (lambda (lst) ; start with the whole list, then takes its cdr on each iteration
              (mapc (lambda (edge) ; take one item from (cdar lst) at a time
                      ;; if the edge is not found as a key on the cdr of the lst
                      ;; then create a new entry. In other words: "If the current edge is
                      ;; not connected to any of the other edges, prints a new entry."
                      ;; (unless (assoc (car edge) (cdr lst))
                        (fresh-line)
                        (princ (dot-name (caar lst)))
                        (princ "--")
                        (princ (dot-name (car edge)))
                        (princ "[label\"")
                        (princ (dot-label (cdr edge)))
                        (princ "\"];"))
                      (cdar lst)))
            edges))
