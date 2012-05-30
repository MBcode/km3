;full text
;(defvar *criteria_* "criteria_") was just for criteria
(defvar *criteria_* "sentence_") ;now wider use
(defun ft-str (n)
    "mk ins of fullTxt/criteria" 
  (let ((atms (elt *fln* (1- n)))
	(txt (cdr (assoc n *ft2*)))
	(header (cdr (assoc n *ie2*))))
    ;(format t "~&~a:~a:~a" n atms txt)
    (format nil "~&(*~a~a has (instance-of (fullTxt)) (txt (\"~a\")) (fullAtoms (~a)) (ie (\"~a\")))" 
	    *criteria_*
	                     ; n                                      txt   (ki atms)
	                       n                                 txt   (ids2km-seq atms t nil #'ki)
			       header)))
	;oh above no longer used, so have to add 'ie' slot below:
(defun crit-id (i)
  (ki (str-cat *criteria* i)))
(defun mk-ft (n)
  (ka (ft-str n)))
(defun mk-ft2 (n) ;replace str version above
  (let ((atms (elt *fln* (1- n)))
	(txt (cdr (assoc n *ft2*)))
	(header (cdr (assoc n *ie2*)))
	(id (str-cat *criteria_* n)))
    (sv-cls id "fullTxt")
    (sv id "txt" txt t)
    ;(sv id "ie" header t) ;new
    (sv id "SENT-TYPE" header t) ;new
    (svs2 id "fullAtoms" atms) ;not exactly like other last slot set ;try2fix
  id))
;(defun mk-ft3 (n) ;add columbia parse
;  (sv (mk-ft2 n) "cea" (cdr-lv (assoc n *cea*))))
(defun mk-ft3 (n) ;add columbia parse
 (let ((id (mk-ft2 n)))
  (when *canned-cNN* 
   (sv id "cea" (cdr-lv (assoc n *cea*)))
   (sv id "cea2" (cdr-lv (assoc n *cea2*)))
   (bestNPs2cNN n) ; columbia canned data
  )
  ))

(defun gcrit (n)
 (str-cat *criteria_* n))
(defun gvcrit (n sn)
  (gv (str-cat *criteria_* n) sn))

;could do one at a time, or all at once, really right after ssheet2 has run
(defun mk-all-ft () ;could pass global info dwn in an optional
  "make all criteria ins"
  (let ((mxn (len *fln*)))
    (loop for i from 1 to mxn do (mk-ft3 i))
    mxn))
;
(defun mapft (fnc)
  (mapcar- fnc (mapcar #'crit-id (range_1 (len *fln*)))))
;
(defun all-sent () "print objs all way dwn for whole study" (mapft #'id-sent))
;
;from ssheet3  ;not used, wrote&used ft.lisp instead, so move to there, from mmkm
(defun crit-str (id slots &optional (strm t)) ;later nil for asserts
  (let ((str (format nil "~&(*crit~a has (instance-of (criteria)) ~a)" id slots)))
    (when strm (write-string str strm)))) ;not used?


(defun bestNP (id) (gv id "bestNP"))
(defun bestNPs (n) (mapcar #'bestNP (rest (gvcrit n  "fullAtoms"))))
;(defun find-nn (l) (when (fulll l) (rfind-all 'NN l)))
(defun find-nn (l) (when (fulll l) (r-find 'NN l)))
(defun has-nn (l) (or (eq l 'NN) (and (fulll l) (r-find 'NN l))))
;use collect-if secondNNp isntead
