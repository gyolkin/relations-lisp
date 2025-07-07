
(load "io.lisp") 

(defun main-loop () 
  (format t "Введите 'help' для просмотра списка команд~%~%") 
  (parse-relations "files/rels.txt") 
  (loop (format t "~%> ") (force-output) 
    (let* 
      ((input (read-line)) 
        (tokens 
          (cl-ppcre:split "\\s+" input)) 
        (command (first tokens)) 
        (args 
          (if (> (length tokens) 1) (second tokens) nil))) 
      (let 
        ( 
          (result 
            (process-command command args))) 
        (when 
          (and 
            (string-equal command "exit") result) (return)))))) 

(defun main () (main-loop)) 
(main)
