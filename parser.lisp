
(load "tree.lisp") 
;; Вспомогательные функции
 

(defun print-nodes (nodes) 
  (mapcar 
    (lambda (node) (print-node node)) nodes)) 
;; Хеш-таблицы для отношений
 

(defparameter *relations-primitives* 
  (make-hash-table :test 'equal)) 

(defparameter *relations-hash-table* 
  (make-hash-table :test 'equal)) 

(defparameter *primitive-fun-names* '("parents" "children" "spouse" "?m" "?f")) 
;; Примитивные функции для работы с отношениями
 

(defun get-parents (nodes) 
  (let ((parents '())) 
  (dolist (node nodes) 
    (dolist 
      (parent (node-parents node)) (push parent parents))) parents)) 

(defun get-children (nodes) 
  (let ((children '())) 
  (dolist (node nodes) 
    (dolist 
      (child (node-children node)) (push child children))) children)) 

(defun get-spouse (nodes) 
  (mapcan 
    (lambda (node) 
      (if (node-spouse node) 
        (list (node-spouse node)) '())) nodes)) 

(defun apply-female-gender-filter (nodes) 
  (remove-if-not 
    (lambda (node) 
      (equal (node-gender node) "female")) nodes)) 

(defun apply-male-gender-filter (nodes) 
  (remove-if-not 
    (lambda (node) 
      (equal (node-gender node) "male")) nodes)) 
;; Регистрация примитивных функций
 

(setf 
  (gethash "parents" *relations-primitives*) #'get-parents) 

(setf 
  (gethash "children" *relations-primitives*) #'get-children) 

(setf 
  (gethash "spouse" *relations-primitives*) #'get-spouse) 

(setf 
  (gethash "?m" *relations-primitives*) #'apply-male-gender-filter) 

(setf 
  (gethash "?f" *relations-primitives*) #'apply-female-gender-filter) 
;; Функции парсинга отношений
 

(defun parse-relation (tokens) "Парсит правую часть отношения" 
  (cond 
    (
      (equal (length tokens) 1) ; parents
 (list (car tokens))) 
    (
      (and 
        (equal (length tokens) 3) 
        (equal (second tokens) "(") 
        (equal (third tokens) ")")) ; parent()
 (list (car tokens))) 
    (
      (equal (length tokens) 4) ; parent(w) | parent(parent)
 
      (let* 
        (
          (fun-name (first tokens)) (sec (second tokens)) (arg (caddr tokens)) 
          (last-el (cadddr tokens))) 
        (if 
          (and 
            (equal sec "(") (equal last-el ")") 
            (or (equal arg "m") (equal arg "f"))) 
          (append (list fun-name) 
            (list 
              (concatenate 'string "?" arg))) ; parent(f)
 
          (if 
            (and 
              (equal sec "(") (equal last-el ")")) ; parent(parent)
 
            (append (list arg) (list fun-name)) 
            (error "Syntax error: ~A" tokens))))) 
    ((> (length tokens) 4) ; parent(Mother(), f) | parent(Mother())
 
      (let* 
        (
          (fun-name (first tokens)) (sec (second tokens)) 
          (inner-tokens 
            (subseq tokens 2 (- (length tokens) 2))) ; переименовал arg -> inner-tokens
 
          (pred-pred-last (car (last tokens 3))) 
          (pred-last (car (last tokens 2))) 
          (last-el (car (last tokens)))) 
        (if 
          (and 
            (equal sec "(") (equal last-el ")") 
            (or (equal pred-last "m") (equal pred-last "f")) 
            (equal pred-pred-last ",")) 
          (append 
            (parse-relation 
              (subseq tokens 2 (- (length tokens) 3))) (list fun-name) 
            (list 
              (concatenate 'string "?" pred-last))) 
          (if (equal pred-last ")") 
            (append 
              (parse-relation 
                (subseq tokens 2 (- (length tokens) 1))) (list fun-name)) 
            (error "Syntax error: ~A" tokens))))) 
    (t 
      (error "Syntax error: ~A" tokens)))) 

(defun parse-declaration (tokens) "Парсит определение отношения" 
  (let* 
    ( 
      (fun-name (first tokens)) (sec (second tokens)) 
      (relation-expr-tokens (cddr tokens)) 
      (base-fun 
        (gethash fun-name *relations-hash-table*))) 
    (if 
      (or 
        (member fun-name '("parents" "children" "spouse" "m" "f") :test 'equal) (not (equal sec "="))) 
    (error "Invalid syntax in relation declaration: ~A" tokens) 
    (if (not base-fun) 
      (setf 
        (gethash fun-name *relations-hash-table*) 
        (parse-relation relation-expr-tokens)) 
      (error "Function with name \"~A\" already exists" fun-name))))) 
;; Регулярные выражения для различных токенов
 

(defparameter *relation-regex* "[a-zA-Zа-яА-Я_ё][a-zA-Z0-9а-яА-Я_ё]*") 

(defparameter *operator-regex* "[=,()]") 

(defparameter *whitespace-regex* "\\s+") 

(defparameter *comments-regex* ";.*")

;; Лексический анализатор
 

(defun lex (input) 
  (labels 
    ( 
      (match-regex (regex string) 
        (multiple-value-bind (start end) 
          (ignore-errors 
            (ppcre:scan regex string)) 
          (when (eq start 0) 
            (subseq string start end))))) 
    (let 
      ((tokens '()) (remaining input)) 
    (loop while 
      (not 
        (zerop (length remaining))) do 
      (let ((token nil)) 
        (cond 
          ( 
            (setq token 
              (match-regex *comments-regex* remaining)) ; Пропускаем комментарии
 
            (setf remaining 
              (subseq remaining (length token)))) 
          ( 
            (setq token 
              (match-regex *relation-regex* remaining)) ; Отношения
 (push token tokens) 
            (setf remaining 
              (subseq remaining (length token)))) 
          ( 
            (setq token 
              (match-regex *operator-regex* remaining)) ; Операторы
 (push token tokens) 
            (setf remaining 
              (subseq remaining (length token)))) 
          ( 
            (setq token 
              (match-regex *whitespace-regex* remaining)) ; Пропускаем пробелы
 
            (setf remaining 
              (subseq remaining (length token)))) 
          (t 
            (error "Invalid symbol: ~A" remaining))))) ; Некорректный токен
 (reverse tokens)))) 
;; Парсинг файла отношений
 

(defun parse-relations (filename) 
  (format t "Загрузка определений отношений из ~A...~%" filename) 
  (with-open-file 
    (stream filename :if-does-not-exist nil) 
    (if (null stream) 
      (format t "Файл ~A не найден.~%" filename) 
      (let 
        ( 
          (text 
            (make-string (file-length stream) :initial-element #\Space)) (relation-count 0)) 
        (read-sequence text stream) 
        (let 
          ( 
            (lines 
              (uiop:split-string text :separator '(#\newline)))) 
        (dolist (line lines) 
          (let ((tokens (lex line))) 
            (when 
              (and tokens 
                (not 
                  (member (first tokens) '("parents" "children" "spouse") :test 'equal)) 
              (string= (second tokens) "=")) (incf relation-count) 
            (parse-declaration tokens))))) 
    (format t "Загружено ~A определений отношений.~%" relation-count))))) 
;; Параметр для хранения посещенных узлов
 

(defparameter *visited* nil) 
;; Рекурсивная обработка отношений
 

(defun recursive-process-relation (nodes fun-list) 
  (let* ((rels nodes)) 
    (dolist (fun fun-list) 
      (let* 
        ( 
          (primitive-fun 
            (gethash fun *relations-primitives*)) 
          (inner-fun-list 
            (gethash fun *relations-hash-table*))) 
        (if primitive-fun 
          (setf rels 
            (funcall primitive-fun rels)) 
          (if inner-fun-list 
            (setf rels 
              (recursive-process-relation rels inner-fun-list)) 
            (error "Function with name \"~A\" not exist" fun))))) 
    (setf rels 
      (remove-if 
        (lambda (node) 
          (member node *visited*)) rels)) rels)) 
;; Обработка отношения для узла
 

(defun process-relation (node fun-list) 
  (let* 
    ( 
      (first-fun (car fun-list)) (res-nodes '())) (setf *visited* '()) 

(if 
  (member first-fun *primitive-fun-names* :test #'equal) 
  (let* 
    ( 
      (nodes 
        (recursive-process-relation (list node) (list first-fun)))) 
    (if 
      (not 
        (equal (length fun-list) 1)) 
      (setf *visited* (list node))) 
    (setf res-nodes 
      (recursive-process-relation nodes (cdr fun-list)))) 
  (let* 
    ( 
      (nodes 
        (recursive-process-relation (list node) (list first-fun)))) 
    (if 
      (not 
        (equal (length fun-list) 1)) 
      (setf *visited* nodes)) 
    (setf res-nodes 
      (recursive-process-relation nodes (cdr fun-list))))) 

(remove-duplicates res-nodes :test #'equal)))
