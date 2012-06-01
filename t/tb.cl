;might want a way to go from num in fl2, to setting *cx* and *cx2*
;defun setcx (an) actually those are atoms/num-names, want ft this time/as a start
;(defun setcx (n)
;  (let (;(atms (elt *fln* (1- n)))
;	(txt (cdr (assoc n *ft2*))))
;    (setf *cx* *oh*)
;    (setf *cx2* (fifth (second *cx*))) 
;    (format t "~&~a:~a" txt (mapcar #'len *cx2*))))
;-looks like stella has a diff parse--
;-lets start to parse the mm xml, esp the final mapping -more above already
;assume one UttText, and check it against the top Mappings

;st.cl too

;------------skip below:-
(defun nct-dir-nums (n9)
 ;(mapcar #'(lambda (s) (first-num (rm-ext (rm-pre3  s)))) (ls (str-cat "nct/"  n9)))
  (mapcar #'(lambda (s) (file2num s)) (ls (str-cat "nct/"  n9)))
  )

;(with-open-file (out "alln.cl" :direction :output) (loop for n9 from 0 to 9 do (print (mapcar #'(lambda (s) (first-num (rm-ext (rm-pre3  s)))) (ls (to-str n9))) out)))
;(with-open-file (out "alln.cl" :direction :output) (loop for n9 from 0 to 9 do (print (nct-dir-nums n9) out)))

; (defvar *n0* (mapcar #'(lambda (s) (first-num (rm-ext
;(loop for n9 from 0 to 9 collect (mapcar #'(lambda (s) (first-num (rm-ext (rm-pre3  s)))) (ls (to-str n9))))
;(loop for n9 from 0 to 9 collect (mapcar #'(lambda (s) (first-num (rm-ext (rm-pre3  s)))) (ls (str-cat "nct/" n9))))
;a few lines from nlm/mmt.lisp to replace the whole dir, now that we might just get mm xml

