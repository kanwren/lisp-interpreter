(defun foldr (f z xs)
  (if (null xs)
    z
    (f (car xs) (foldr f z (cdr xs)))))

; TODO: use defconst when implemented
(defvar otherwise #t)

(defmacro cond (&rest conds)
  (foldr
    (lambda (clause1 rest)
      (list 'if
            (car clause1)
            (cons 'progn (cdr clause1))
            rest))
    nil conds))

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
        (bound (car (cdr counter))))
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
                      'loop-end)))))

(defmacro return (&optional val) (list 'return-from nil val))
(defmacro break () (list 'go 'loop-end))
(defmacro continue () (list 'go 'loop-continue))

; TODO - other loops

(defmacro incf (var &optional (delta 1))
  (list 'setq var (list '+ var delta)))
(defmacro decf (var &optional (delta 1))
  (list 'setq var (list '- var delta)))

; utils

(defun even (n) (= 0 (mod n 2)))
(defun odd (n) (/= 0 (mod n 2)))

nil
