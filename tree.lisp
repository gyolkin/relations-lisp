(ql:quickload '(:split-sequence :uiop :cl-ppcre))

; declaration of tree node structure
(defstruct node
  name
  gender
  parents
  spouse
  children)

; create instance of node struct
(defun make-tree-node (name gender)
  (make-node :name name :gender gender :parents () :spouse nil :children ()))

; dfs search of tree
(defun find-child (root name)
  (if (equal (node-name root) name)
      (return-from find-child root)
      (dolist (child (node-children root))

        (let ((result (find-child child name)))
          (if result
              (return-from find-child result)
              )))))

; search in neighbor tree
(defun find-node (from_node goal_name &optional visited)
    (if (or (member from_node visited) (not (typep from_node 'node)))
        (return-from find-node nil)
    )
    (setf *new_visited* (nconc visited (list from_node)))
    (if (equal (node-name from_node) goal_name)
        (return-from find-node from_node)
            ; get list of all relatives for current node
            (dolist (next_node (nconc (node-children from_node) (node-parents from_node) (list (node-spouse from_node))))

            (let ((result (find-node next_node goal_name *new_visited*)))
            (if result
                (return-from find-node result))
            ))
    )
)

; add spouse relation for nodes
(defun set-spouse (node1 node2)
  (setf (node-spouse node1) node2)
  (setf (node-spouse node2) node1 ))

; add parent-child relation for nodes
(defun set-child (parent child)
  (setf (node-parents child) (nconc (list parent) (node-parents child)))
  (setf (node-children parent) (nconc (list child) (node-children parent)))
)

; print node data
(defun print-node (node)
    (if (equal node nil)
        (progn
        (format t "Node not found")
        (return-from print-node nil)
        )
    )
    (format t "~%Node properties:~%")
    (format t "Name: ~A~%" (node-name node))
    (format t "Gender: ~A~%" (node-gender node))
    (format t "Parents: ")
    (dolist (parent (node-parents node))
        (format t "~a " (node-name parent))
    )
    (terpri)
    (format t "Children: ")
    (dolist (child (node-children node))
        (format t "~a " (node-name child))
    )
    (terpri)
    (format t "Spouse: ~A~%" (if (node-spouse node)
                                (node-name (node-spouse node))
                                "None")))


; node creation
;(setf *node-a* (make-tree-node "George" "male"))
;(setf *node-b* (make-tree-node "Mary" "female"))
;(setf *node-c* (make-tree-node "Robert" "male"))
;(setf *node-d* (make-tree-node "Jane" "female"))
;(setf *node-e* (make-tree-node "Michael" "male"))
;(setf *node-f* (make-tree-node "Patricia" "female"))
;
;; adding relations
;(set-child *node-e* *node-f*)
;(set-child *node-d* *node-f*)
;(set-child *node-d* *node-a*)
;(set-spouse *node-b* *node-a*)
;(set-child *node-c* *node-b*)
;
;; node search example
;(print-node (find-node *node-e* "Mary"))
