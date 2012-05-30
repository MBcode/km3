;Make a few funcs that match up w/the base clojure utils that I see
; might not hurt to look at the clj code used to make it /to save time
;  even though most of it looks pretty easy
;=Sequences
;(drop 2 [1 2 3 4 5]) -> (3 4 5) 
(defun drop (n s) (subseq s n))
;(take 9 (cycle [1 2 3 4])) ;-> (1 2 3 4 1 2 3 4 1) 
;(defun cycle (s) ...) ;lazy promise
;(defun take (n s) ..) ;lazy demand?
;(interleave [:a :b :c :d :e] [1 2 3 4 5])
;-> (:a 1 :b 2 :c 3 :d 4 :e 5)
;(partition 3 [1 2 3 4 5 6 7 8 9])
;-> ((1 2 3) (4 5 6) (7 8 9))
(defun partition (n s) ;add (pre)test/s
  (cons (subseq 1 n s) (partition n (drop n s))))
;(map vector [:a :b :c :d :e] [1 2 3 4 5]) ;mapcar
;-> ([:a 1] [:b 2] [:c 3] [:d 4] [:e 5])
;(apply str (interpose \, "asdf")) ;also the same
;-> "a,s,d,f" (reduce + (range 100)) -> 4950
;=Maps and Sets
;(def m {:a 1 :b 2 :c 3}) (m :b) -> 2 ;also (:b m)
;new call convention for assoc on m
;(keys m) -> (:a :b :c)
(defun keys (al) (mapcar #'first al))
;(assoc m :d 4 :c 42) -> {:d 4, :a 1, :b 2, :c 42} 
;  "  appends2alst
;(merge-with + m {:a 2 :b 3}) -> {:a 3, :b 5, :c 3} 
;mapcar of fnc on aligned keys ..
;(union #{:a :b :c} #{:c :d :e}) -> #{:d :a :b :c :e}  ;union
;(join #{{:a 1 :b 2 :c 3} {:a 1 :b 21 :c 42}} #{{:a 1 :b 2 :e 5} {:a 1 :b 21 :d 4}})
;-> #{{:d 4, :a 1, :b 21, :c 42} {:a 1, :b 2, :c 3, :e 5}} ;mapcar #'union
