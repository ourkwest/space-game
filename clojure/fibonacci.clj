(ns fibonacci)


;  sequence =
;       0, 1
;       x[n] = x[n-1] + x[n-2]
; e.g.  x[3] = x[2] + x[1]



(def n1 0)
(def n2 1)
(defn fib [n]
  (if (= n 1)
    n1
    (if (= n 2)
      n2
      (+ (fib (- n 2))
         (fib (- n 1))
         )
      )))

