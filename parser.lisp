(load "tree.lisp")

;; Инициализация тестового графа
(setf *node-masha* (make-tree-node "Маша" "female"))

(setf *node-inna* (make-tree-node "Инна" "female"))
(setf *node-sasha* (make-tree-node "Саша" "male"))
(set-spouse *node-inna* *node-sasha*)
(set-child *node-masha* *node-inna*)

(setf *node-ivan* (make-tree-node "Иван" "male"))
(setf *node-nadya* (make-tree-node "Надя" "female"))
(set-spouse *node-ivan* *node-nadya*)

(setf *node-vova* (make-tree-node "Вова" "male"))
(setf *node-dasha* (make-tree-node "Даша" "female"))
(set-spouse *node-vova* *node-dasha*)

(setf *node-german* (make-tree-node "Герман" "male"))
(setf *node-glasha* (make-tree-node "Глаша" "female"))
(setf *node-karina* (make-tree-node "Карина" "female"))
(setf *node-semen* (make-tree-node "Семён" "male"))
(set-spouse *node-german* *node-karina*)
(set-spouse *node-glasha* *node-semen*)
(set-child *node-vova* *node-german*)
(set-child *node-dasha* *node-glasha*)
(set-child *node-vova* *node-glasha*)
(set-child *node-dasha* *node-german*)

(setf *node-sveta* (make-tree-node "Света" "female"))
(setf *node-dima* (make-tree-node "Дима" "male"))
(set-spouse *node-sveta* *node-dima*)

(set-child *node-dima* *node-vova*)
(set-child *node-sveta* *node-vova*)

(set-child *node-ivan* *node-dima*)
(set-child *node-nadya* *node-dima*)

(set-child *node-inna* *node-sveta*)
(set-child *node-sasha* *node-sveta*)


(setf *node-gosha* (make-tree-node "Гоша" "male"))
(setf *node-alisa* (make-tree-node "Алиса" "female"))
(set-child *node-dima* *node-alisa*)
(set-child *node-sveta* *node-gosha*)

(setf *node-sergey* (make-tree-node "Сергей" "male"))
(setf *node-vera* (make-tree-node "Вера" "female"))
(set-child *node-sergey* *node-dasha*)
(set-child *node-vera* *node-dasha*)
(set-spouse *node-sergey* *node-vera*)

(setf *node-anton* (make-tree-node "Антон" "male"))
(setf *node-nastya* (make-tree-node "Настя" "female"))
(set-child *node-sergey* *node-anton*)
(set-child *node-vera* *node-anton*)
(set-child *node-sergey* *node-nastya*)
(set-child *node-vera* *node-nastya*)
(setf *node-kolya* (make-tree-node "Коля" "male"))
(set-spouse *node-nastya* *node-kolya*)

(setf *node-vitya* (make-tree-node "Витя" "male"))
(setf *node-ilya* (make-tree-node "Илья" "male"))
(setf *node-tanya* (make-tree-node "Таня" "female"))
(set-child *node-inna* *node-vitya*)
(set-child *node-sasha* *node-vitya*)

(set-child *node-vitya* *node-ilya*)
(set-child *node-vitya* *node-tanya*)


(defun print-nodes (nodes)
  (mapcar (lambda (node) (print-node node)) nodes))

(defparameter *relations-primitives* (make-hash-table :test 'equal))
(defparameter *relations-hash-table* (make-hash-table :test 'equal))
(defparameter *primitive-fun-names* '("parents" "children" "spouse" "?m" "?f"))

(defun get-parents (nodes)
  (let ((parents '()))
    (dolist (node nodes)
      (dolist (parent (node-parents node))
        (push parent parents)))
    parents))

(defun get-children (nodes)
  (let ((children '()))
    (dolist (node nodes)
      (dolist (child (node-children node))
        (push child children)))
    children))

(defun get-spouse (nodes)
  (mapcan (lambda (node)
            (if (node-spouse node)
                (list (node-spouse node))
                '()))
          nodes))

(defun apply-female-gender-filter (nodes)
  (remove-if-not (lambda (node) (equal (node-gender node) "female")) nodes))

(defun apply-male-gender-filter (nodes)
  (remove-if-not (lambda (node) (equal (node-gender node) "male")) nodes))

(setf (gethash "parents" *relations-primitives*) #'get-parents)
(setf (gethash "children" *relations-primitives*) #'get-children)
(setf (gethash "spouse" *relations-primitives*) #'get-spouse)
(setf (gethash "?m" *relations-primitives*) #'apply-male-gender-filter)
(setf (gethash "?f" *relations-primitives*) #'apply-female-gender-filter)

(defun parse-relation (tokens)
  "Парсит правую часть отношения"
  (cond
    ((equal (length tokens) 1)                                              ; parents
     (list (car tokens)))
    ((and (equal (length tokens) 3) (equal (second tokens) "(") (equal (third tokens) ")"))     ; parent()
     (list (car tokens)))
    ((equal (length tokens) 4)                                              ; parent(w) | parent(parent)
     (let* ((fun-name (first tokens))
            (sec (second tokens))
            (arg (caddr tokens))
            (last-el (cadddr tokens)))
       (if (and (equal sec "(") (equal last-el ")") (or (equal arg "m") (equal arg "f")))
           (append (list fun-name) (list (concatenate 'string "?" arg)))                          ; parent(f)
           (if (and (equal sec "(") (equal last-el ")"))                                       ; parent(parent)
               (append (list arg) (list fun-name))
               (error "Syntax error: ~A" tokens)))))
    ((> (length tokens) 4)                                          ; parent(Mother(), f) | parent(Mother())
     (let* ((fun-name (first tokens))
        (sec (second tokens))
        (arg (subseq tokens 2 (- (length tokens) 2)))
        (pred-pred-last (car (last tokens 3)))
        (pred-last (car (last tokens 2)))
        (last-el (car (last tokens))))
       (if (and (equal sec "(") (equal last-el ")") (or (equal pred-last "m") (equal pred-last "f")) (equal pred-pred-last ","))
           (append (parse-relation (subseq tokens 2 (- (length tokens) 3))) (list fun-name) (list (concatenate 'string "?" pred-last)))
           (if (equal pred-last ")")
               (append (parse-relation (subseq tokens 2 (- (length tokens) 1))) (list fun-name))
               (error "Syntax error: ~A" tokens)))))
    (t (error "Syntax error: ~A" tokens))))

(defun parse-declaration (tokens)
  "Парсит определение отношения"
  (let* ((fun-name (first tokens))
         (sec (second tokens))
         (relation-expr-tokens (cddr tokens))
         (base-fun (gethash fun-name *relations-hash-table*)))
    (if (or (member fun-name '("parents" "children" "spouse" "m" "f") :test 'equal) (not (equal sec "=")))
        (error "Invalid syntax in relation declaration: ~A" tokens)
        (if (not base-fun)
            (setf (gethash fun-name *relations-hash-table*) (parse-relation relation-expr-tokens))
            (error "Function with name \"~A\" already exists" fun-name)))))


;; Регулярные выражения для различных токенов
(defparameter *relation-regex* "[a-zA-Zа-яА-Я_ё][a-zA-Z0-9а-яА-Я_ё]*")
(defparameter *operator-regex* "[=,()]")
(defparameter *whitespace-regex* "\\s+")
(defparameter *comments-regex* ";.*")

(defun lex (input)
  (labels ((match-regex (regex string)
             (multiple-value-bind (start end)
                 (ignore-errors (ppcre:scan regex string))
               (when (eq start 0)
                 (subseq string start end)))))
    (let ((tokens '())
          (remaining input))
      (loop while (not (zerop (length remaining)))
            do (let ((token nil))
                 (cond
                  ((setq token (match-regex *comments-regex* remaining))  ; Пропускаем комментарии
                   (setf remaining (subseq remaining (length token))))
                  ((setq token (match-regex *relation-regex* remaining))  ; Отношения
                   (push token tokens)
                   (setf remaining (subseq remaining (length token))))
                  ((setq token (match-regex *operator-regex* remaining))  ; Операторы
                   (push token tokens)
                   (setf remaining (subseq remaining (length token))))
                  ((setq token (match-regex *whitespace-regex* remaining))  ; Пропускаем пробелы
                   (setf remaining (subseq remaining (length token))))
                  (t (error "Invalid symbol: ~A" remaining)))))  ; Некорректный токен
      (reverse tokens))))

(defun parse-relations (filename)
  (with-open-file (stream filename)
    (let ((text (make-string (file-length stream) :initial-element #\Space)))
      (read-sequence text stream)
      (let ((lines (uiop:split-string text :separator '(#\newline))))
        (dolist (line lines)
          (let* ((tokens (lex line)))
            (if tokens (parse-declaration (lex line)))))))))

(defparameter *visited* nil)

(defun recursive-process-relation (nodes fun-list)
  (let* ((rels nodes))
      (dolist (fun fun-list)
        (let* ((primitive-fun (gethash fun *relations-primitives*))
               (inner-fun-list (gethash fun *relations-hash-table*)))
;          (print "----------------------")
;          (print fun-list)
;          (print fun)
;          (print primitive-fun)
;          (print-nodes rels)
;          (print inner-fun-list)
;          (print "++++++++++++++++++++++")
          (if primitive-fun
            (setf rels (funcall primitive-fun rels))
              (if inner-fun-list
                (setf rels (recursive-process-relation rels inner-fun-list))
                  (error "Function with name \"~A\" not exist" fun)))))
    (setf rels (remove-if (lambda (node) (member node *visited*)) rels))
;    (print "------")
;    (print-nodes rels)
;    (print "++++++")
    rels))

(defun process-relation (node fun-list)
  (let* ((first-fun (car fun-list))
         (res-nodes '()))
;    (print "??????")
;    (print first-fun)
;    (print "??????")
        (setf *visited* '())
    (if (member first-fun *primitive-fun-names* :test #'equal)
        (let* ((nodes (recursive-process-relation (list node) (list first-fun))))
;          (print-nodes nodes)
;          (print fun-list)
          (if (not (equal (length fun-list) 1)) (setf *visited* (list node)))
          (setf res-nodes (recursive-process-relation nodes (cdr fun-list))))
        (let* ((nodes (recursive-process-relation (list node) (list first-fun))))
;          (print-nodes nodes)
;          (print fun-list)
          (if (not (equal (length fun-list) 1)) (setf *visited* nodes))
          (setf res-nodes (recursive-process-relation nodes (cdr fun-list)))))
;    (print res-nodes)
    (remove-duplicates res-nodes :test #'equal)))

(defun process-node (node)
  (maphash (lambda (rel-name rel-list)
             (let ((result (process-relation node rel-list)))
                (format t "~a:~%" rel-name)
                (mapc (lambda (node)
                        (format t "    ~a~%" (node-name node)))
                    result)))
           *relations-hash-table*))


;; Тестовые отношения
(parse-relations "rels.txt")
(process-node *node-vova*)
