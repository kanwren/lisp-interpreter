;;; List utilities

(defun lift-list (x)
  (if (listp x) x (list x)))

(defun foldr (f z xs)
  (if (null xs)
    z
    (f (car xs) (foldr f z (cdr xs)))))

(defun elem (e xs &key (test equal))
  (foldr (lambda (x rest) (or (test e x) rest)) #f xs))

(defmacro case-list (xs nil-case cons-case)
  `(if (null ,xs)
     (progn ,@(lift-list nil-case))
     (let ((,(car cons-case) (car ,xs))
           (,(car (cdr cons-case)) (cdr ,xs)))
       (progn ,@(cdr (cdr cons-case))))))

(defun range (lo &optional hi (step 1))
  (when (null hi)
    (setq hi lo)
    (setq lo 0))
  (let ((res nil) (i lo))
    (cond
      ((> step 0)
       (while (< i hi)
              (push i res)
              (incf i step)))
      ((< step 0)
       (while (> i hi)
              (push i res)
              (incf i step)))
      (otherwise nil))
    (reverse res)))

(defun take (n xs)
  (if (or (<= n 0) (null xs))
    nil
    (cons (car xs) (take (- n 1) (cdr xs)))))

(defun drop (n xs)
  (if (or (<= n 0) (null xs))
    xs
    (drop (- n 1) (cdr xs))))

(defun reverse (xs)
  (labels
    ((rec (acc ys)
          (if (null ys)
            acc
            (rec (cons (car ys) acc) (cdr ys)))))
    (rec nil xs)))

(defmacro push (x xs)
  `(setq ,xs (cons ,x ,xs)))

;;; Conditionals

(defmacro cond (&rest conds)
  (foldr
    (lambda (clause1 rest)
      `(if
         ,(car clause1)
         (progn ,@(cdr clause1))
         ,rest))
    nil conds))

; TODO: use defconst when implemented
(defvar otherwise #t)

;;; Loops

(defmacro forever (&rest body)
  `(block nil
          (tagbody
            loop-begin
            loop-continue
            (progn ,@body)
            (go loop-begin)
            loop-end)))

(defmacro while (cond &rest body)
  `(block nil
          (tagbody
            loop-begin
            loop-continue
            (unless ,cond (go loop-end))
            (progn ,@body)
            (go loop-begin)
            loop-end)))

(defmacro do-while (cond &rest body)
  `(block nil
          (tagbody
            loop-begin
            (progn ,@body)
            loop-continue
            (when ,cond (go loop-begin))
            loop-end)))

(defmacro dotimes (counter &rest body)
  (let ((var (car counter))
        (bound (car (cdr counter)))
        (rest (cdr (cdr counter))))
    `(block nil
            (let ((,var 0))
              (tagbody
                loop-begin
                (if (< ,var ,bound)
                  (progn ,@body)
                  (go loop-end))
                loop-continue
                (setq ,var (+ ,var 1))
                (go loop-begin)
                loop-end)
              (progn ,@rest)
              ))))

(defmacro for-each (var xs &rest body)
  (let ((ys (gensym)))
    `(let ((,ys (the list ,xs)))
       (block nil
              (tagbody
                loop-begin
                (when (null ,ys) (go loop-end))
                (let ((,var (car ,ys)))
                  (progn ,@body))
                loop-continue
                (setq ,ys (cdr ,ys))
                (go loop-begin)
                loop-end
                )))))

; TODO - other loops

(defmacro return (&optional val) `(return-from nil ,val))
(defmacro break () `(go loop-end))
(defmacro continue () `(go loop-continue))

;;; numeric utils

(defmacro incf (var &optional (delta 1))
  `(setq ,var (+ ,var ,delta)))
(defmacro decf (var &optional (delta 1))
  `(setq ,var (- ,var ,delta)))

(defun even (n) (= 0 (mod n 2)))
(defun odd (n) (/= 0 (mod n 2)))

nil
