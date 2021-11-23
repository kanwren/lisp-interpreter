;;; List utilities

(defun foldr (f z xs)
  (if (null xs)
    z
    (f (car xs) (foldr f z (cdr xs)))))

(defun elem (e xs &key (test equal))
  (foldr (lambda (x rest) (or (test e x) rest)) #f xs))

(defun take (n xs)
  (if (or (<= n 0) (null xs))
    nil
    (cons (car xs) (take (- n 1) (cdr xs)))))

(defun drop (n xs)
  (if (or (<= n 0) (null xs))
    xs
    (drop (- n 1) (cdr xs))))

(defun append (xs ys)
  (cond
    ((null xs) ys)
    ((null ys) xs)
    (#t (cons (car xs) (append (cdr xs) ys)))))

(defun reverse (xs)
  (labels
    ((rec (acc ys)
          (if (null ys)
            acc
            (rec (cons (car ys) acc) (cdr ys)))))
    (rec nil xs)))

(defmacro push (x xs)
  (list 'setq xs (list 'cons x xs)))

;;; Conditionals

(defmacro cond (&rest conds)
  (foldr
    (lambda (clause1 rest)
      (list 'if
            (car clause1)
            (cons 'progn (cdr clause1))
            rest))
    nil conds))

; TODO: use defconst when implemented
(defvar otherwise #t)

;;; Loops

(defmacro while (cond &rest body)
  (list 'block nil
        (list 'tagbody
              'loop-begin
              'loop-continue
              (list 'unless cond (list 'go 'loop-end))
              (cons 'progn body)
              (list 'go 'loop-begin)
              'loop-end)))

(defmacro do-while (cond &rest body)
  (list 'block nil
        (list 'tagbody
              'loop-begin
              (cons 'progn body)
              'loop-continue
              (list 'when cond (list 'go 'loop-begin))
              'loop-end)))

(defmacro dotimes (counter &rest body)
  (let ((var (car counter))
        (bound (car (cdr counter)))
        (rest (cdr (cdr counter))))
    (list 'block nil
          (list 'let (list (list var 0))
                (list 'tagbody
                      'loop-begin
                      (list 'if (list '< var bound)
                            (cons 'progn body)
                            (list 'go 'loop-end))
                      'loop-continue
                      (list 'setq var (list '+ var 1))
                      (list 'go 'loop-begin)
                      'loop-end)
                (cons 'progn rest)
                ))))

(defmacro for-each (var xs &rest body)
  (let ((ys (gensym)))
    (list 'let (list (list ys (list 'the 'list xs)))
          (list 'block nil
                (list 'tagbody
                      'loop-begin
                      (list 'when (list 'null ys) (list 'go 'loop-end))
                      (list 'let (list (list var (list 'car ys)))
                            (cons 'progn body))
                      'loop-continue
                      (list 'setq ys (list 'cdr ys))
                      (list 'go 'loop-begin)
                      'loop-end
                      )))))

; TODO - other loops

(defmacro return (&optional val) (list 'return-from nil val))
(defmacro break () (list 'go 'loop-end))
(defmacro continue () (list 'go 'loop-continue))

;;; numeric utils

(defmacro incf (var &optional (delta 1))
  (list 'setq var (list '+ var delta)))
(defmacro decf (var &optional (delta 1))
  (list 'setq var (list '- var delta)))

(defun even (n) (= 0 (mod n 2)))
(defun odd (n) (/= 0 (mod n 2)))

nil
