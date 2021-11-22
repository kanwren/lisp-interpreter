(load "stdlib.lsp")

(defun divides (n x) (= 0 (mod x n)))

(defun fizzbuzz (n)
  (flet ((single (x)
                 (cond
                   ((divides 15 x) "fizzbuzz")
                   ((divides  3 x) "fizz")
                   ((divides  5 x) "buzz")
                   (otherwise x))))
    (dotimes (i n)
      (print (single (+ i 1))))))

(fizzbuzz 100)
