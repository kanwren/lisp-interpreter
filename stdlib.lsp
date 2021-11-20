(defmacro while (cond &rest body)
  (list 'block nil
        (list 'tagbody
              'begin
              (list 'unless cond (list 'go 'end))
              (cons 'progn body)
              (list 'go 'begin)
              'end)))

(defmacro do-while (cond &rest body)
  (list 'block nil
        (list 'tagbody
              'begin
              (cons 'progn body)
              (list 'when cond (list 'go 'begin)))))

; TODO - dotimes, other loops

(defmacro incf (var &optional (delta 1))
  (list 'setq var (list '+ var delta)))
(defmacro decf (var &optional (delta 1))
  (list 'setq var (list '- var delta)))

nil
