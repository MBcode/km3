(lkm3)
;(defvar *fl2* (list-lines "../../vq.txt"))
;get a version w/csv of tsv file, to a list of vals ;then write acessors
(defvar *fl2* (mapcar #'csv-parse-str (list-lines "../../training_set.tsv")))
;(defun get-fl2 (n) (nth n *fl2*))
(ql 'nlp)
;(defun get-fl2 (n) (first-lv (nth n *fl2*))) ;so will pull str from list w/more info in it
;(defun get-fl2 (n) (second-lv (nth n *fl2*))) 
(defun get-fl2 (n) (aref (nth n *fl2*) 1)) 
(defvar *q*)
(defvar *a*)
(defvar *w*)
(defvar *a2rn* '(("A" . 4) ("B" . 5) ("C" . 6) ("D" . 7)))
;(defun a2rn (al) (first_lv (assoc al *a2rn* :test #'equal)))
(defun a2rn (al) (assoc_v al *a2rn*))
(defun set-fl2 (n) 
  "set record#n quest&answ.."
  (let* ((r (nth n *fl2*)) 
         (id (aref r 0))
         (q (aref r 1))
         (al (aref r 2))
         (an (1- (a2rn al)))
         (aa (loop for i from 3 to 6 collect (aref r i)))
         (a (aref r an))
         (wa (remove a aa :test #'equal)))
    ;set wrong ones too next
    (format t "~%id=~a~%q=~a~%a=~a~%w=~a~%" id q a wa)
    (setf *q* q)
    (setf *a* a)
    (setf *w* wa)
    al))

;echo $1|java -mx1900m -jar nlp/berk/BerkeleyParser-1.7.jar -tokenize -gr nlp/berk/eng_sm6.gr
(defun berk-n (n) (berk (get-fl2 n)))
;(defun berk2n (n) (berk2 (get-fl2 n)))
(defun berk2n (n) 
  (let ((s (get-fl2 n)))
    (format t "~%~a~%" s)
    (berk2 s)))

(defun tst (&optional (n 2))
  (set-fl2 n)
  (berk2 *q*))

;will brush off stanford parser work in nlp.lisp too, but want to move to corenlp (server)
; but will start w/calling it directly 1st:
;echo $1|./scnl/corenlp.sh -annotators tokenize,ssplit,pos,lemma,ner -outputFormat xml -
(ql 's-xml)
(defun ner-x (s)
  (let ((r (run-ext "nlp/ner-x.sh" (clean4echo s))))
    (when (> (len r) 99)
      (collect-if #'(lambda (x) (search "<" x)) (break2lines r)))))
;clean up a bit more then use s-xml to parse to sexp

(ql 'cl-json)
(defun jsonsp (s) (prefixp "{" s))
(defun collect-jsonsp (l) (mapcar_ #'json:decode-json-from-string (collect-if #'jsonsp l)))
(defun ner-j (s)
  (let ((r (run-ext "nlp/ner-j.sh" (clean4echo s))))
    (when (> (len r) 99)
      (collect-jsonsp (break2lines r)))))
;moving to server should allow for drakma call with just a json(-rpc)|xml return 
; if I don't get the client protocol down(as if via curl)then can clpython|run-ext2light client2start;(w/hy?)
(defun pyu-p (s) (search "u'" s))
(defun parsetree-p (s) (search "parsetree" s))
;This is from the server, and not necc just ner, more corenlp client in general now:
(defun ner-p (s) ;https://github.com/dasmith/stanford-corenlp-python.git  client.py altered to send str2parse
  (let ((r (run-ext "python" "cnlp/c3.py" (clean4echo s))))
    (when (> (len r) 99) 
      (let* ((l (break2lines r))
             (ul (collect-if #'pyu-p l)) 
             (pt (collect-if #'parsetree-p ul)))
        (format t "~%pt:~a~%" pt)
        (reduce #'str-cat2 ul)
      )))) ;still want {} so can use cl-json  ;also look at alt formats/py-printouts
;make a generic nlp.lisp which just needs one link to each nlp lib
;still want to use cl-nlp
