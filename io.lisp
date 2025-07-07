
(load "tree.lisp") 
(load "parser.lisp") 

(defparameter *nodes-table* 
  (make-hash-table :test 'equal)) 

(defun gender-code-to-string (code) 
  (cond 
    ( 
      (string-equal code "М") "male") 
    ( 
      (string-equal code "Ж") "female") 
    (t 
      (error "Неизвестное обозначение пола: ~A" code)))) 

(defun find-node-by-name (name) 
  (gethash name *nodes-table*)) 

(defun add-person (name gender-code) 
  (let* 
    ( 
      (gender 
        (gender-code-to-string gender-code)) 
      (node 
        (make-tree-node name gender))) 
    (setf 
      (gethash name *nodes-table*) node) node)) 

(defun set-spouse-by-names (name1 name2) 
  (let 
    ( 
      (node1 
        (find-node-by-name name1)) 
      (node2 
        (find-node-by-name name2))) 
    (if (and node1 node2) 
      (set-spouse node1 node2) 
      (error "Не найден один из людей: ~A или ~A" name1 name2)))) 

(defun set-parent-by-names 
  (parent-name child-name) 
  (let 
    ( 
      (parent 
        (find-node-by-name parent-name)) 
      (child 
        (find-node-by-name child-name))) 
    (if (and parent child) 
      (set-child parent child) 
      (error "Не найден один из людей: ~A или ~A" parent-name child-name)))) 

(defun parse-line (line) 
  (let 
    ( 
      (tokens 
        (cl-ppcre:split "\\s+" line))) 
    (cond 
      ((= (length tokens) 2) 
        (add-person (first tokens) (second tokens))) 
      ( 
        (and (= (length tokens) 3) 
          (string= (second tokens) "<->")) 
        (set-spouse-by-names (first tokens) (third tokens))) 
      ( 
        (and (= (length tokens) 3) 
          (string= (second tokens) "->")) 
        (set-parent-by-names (first tokens) (third tokens))) 
      ( 
        (or (zerop (length line)) 
          (char= (char line 0) #\;))
 nil) 
          (t 
            (format t "Неверный формат строки: ~A~%" line))))) 
    (defun load-family-data (filename) 
      (format t "Загрузка данных из файла ~A...~%" filename) 
      (with-open-file 
        (stream filename :if-does-not-exist nil) 
        (if (null stream) 
          (format t "Файл ~A не найден~%" filename) 
          (let 
            ((line-count 0) (people-count 0) (relation-count 0)) 
            (loop for line = 
              (read-line stream nil nil) while line do (incf line-count) 
              (when 
                (and (> (length line) 0) 
                  (not 
                    (char= (char line 0) #\;)))
                      (let 
                        ( 
                          (tokens 
                            (cl-ppcre:split "\\s+" line))) 
                        (cond 
                          ((= (length tokens) 2) (incf people-count)) 
                          ((= (length tokens) 3) (incf relation-count)))))) 
                  (file-position stream 0) 
                  (clrhash *nodes-table*) 
                  (loop for line = 
                    (read-line stream nil nil) while line do 
                    (when 
                      (and (> (length line) 0) 
                        (not 
                          (char= (char line 0) #\;)))
 
                            (let 
                              ( 
                                (tokens 
                                  (cl-ppcre:split "\\s+" line))) 
                              (when (= (length tokens) 2) (parse-line line))))) 
                        (file-position stream 0) 
                        (loop for line = 
                          (read-line stream nil nil) while line do 
                          (when 
                            (and (> (length line) 0) 
                              (not 
                                (char= (char line 0) #\;)))
 
                                  (let 
                                    ( 
                                      (tokens 
                                        (cl-ppcre:split "\\s+" line))) 
                                    (when (= (length tokens) 3) (parse-line line))))) 
                              (format t "Загружено ~A строк, ~A людей, ~A связей~%" line-count people-count relation-count))))) 
                      (defun show-relations (name) 
                        (let 
                          ( 
                            (node 
                              (find-node-by-name name))) 
                          (if node 
                            (progn 
                              (format t "Отношения для ~A:~%" name) 
                              (maphash 
                                (lambda (rel-name rel-list) 
                                  (let 
                                    ( 
                                      (result 
                                        (process-relation node rel-list))) 
                                    (when result 
                                      (format t "~a:~%" rel-name) 
                                      (mapc 
                                        (lambda (related-node) 
                                          (format t " ~a~%" 
                                            (node-name related-node))) result)))) *relations-hash-table*)) 
                            (format t "Человек с именем ~A не найден~%" name)))) 
                      (defun show-person-info (name) 
                        (let 
                          ( 
                            (node 
                              (find-node-by-name name))) 
                          (if node (print-node node) 
                            (format t "Человек с именем ~A не найден~%" name)))) 
                      (defun list-people () 
                        (format t "Список загруженных людей:~%") 
                        (maphash 
                          (lambda (name node) 
                            (format t " ~A ~A~%" name 
                              (if 
                                (string= (node-gender node) "male") "М" "Ж"))) *nodes-table*)) 
                      (defun show-help () 
                        (format t "~%Доступные команды:~%") 
                        (format t " load <filename> - загрузить данные из файла~%") 
                        (format t " relations <name> - показать отношения для человека~%") 
                        (format t " info <name> - показать информацию о человеке~%") 
                        (format t " list - показать список всех людей~%") 
                        (format t " help - показать эту справку~%") 
                        (format t " exit - выйти из программы~%")) 
                      (defun process-command (command args) 
                        (cond 
                          ( 
                            (string-equal command "load") 
                            (if args 
                              (load-family-data args) 
                              (format t "Укажите имя файла~%"))) 
                          ( 
                            (string-equal command "relations") 
                            (if args (show-relations args) 
                              (format t "Укажите имя человека~%"))) 
                          ( 
                            (string-equal command "info") 
                            (if args 
                              (show-person-info args) 
                              (format t "Укажите имя человека~%"))) 
                          ( 
                            (string-equal command "list") (list-people)) 
                          ( 
                            (string-equal command "help") (show-help)) 
                          ( 
                            (string-equal command "exit") t) 
                          (t 
                            (format t "Неизвестная команда. Введите 'help' для получения справки~%") nil)))
