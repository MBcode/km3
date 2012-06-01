;just cleaner version of mmx_ that caches on subsections; rfinds, etc; &maybe a start of tripleAsserts
(defun under_f (str) ;better than cache version ;see m16_
  (let* ((wrds (string-to-words str))
         (iname (str-cat_l wrds)))
    iname))  ;finish off w/a much smaller/better cache/ing
;I could almost just restart just using km/utils and get the metamap in  **make it much smaller**
;defun metamap (s)
(defun new-mmt (xso path)
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out "~a" xso))
    )
(defun mmx_ (s)  ;right now cache/mmt/*.txt has the xml, from cluster-1
  "call and start mm processing" ;;pull out subsections and just cache those
  ;but that might means pulling it out/reconstructing a long1from parts then/think about
  (let* ((wrds (string-to-words s)) ;can skip&just use under_f
         (iname (str-cat_l wrds))
         (fx (str-cat2 iname ".xml"))
         (fx2 (str-cat2 "xml/" fx))
         (fc (str-cat2 iname ".cl"))
         (path (str-cat2 "cache/" fc))
         (nf (unless (file-p fx2) (new-mmt (mmt s) fx2))) ;make cached xml if doesn't exist;on lnx
         (xso (s-xml:parse-xml-file fx2))
         )
    (when *dbg* (format t "~&~a,len=~a" fx (len xso)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out "~a" xso))
    xso))
 
;-xml acess:
(defun rfinds (fl l)
    "rfind like xpath"     
    (if (eq (len fl) 1) 
      (let ((ffl (first fl)))  
	(if (listp ffl) (mapcar #'(lambda (ff) (rfind ff l))
				ffl)
	  (rfind ffl l)))
      (rfinds (rest fl) (rfind (first fl) l)))) 
;;
;; Duke:
;;
(defun outcome (l)
   (second-lv (rfinds '(:|primary_outcome| :|measure|) l)))
;;
(defun conditions (l)
  "all condition-s from NCT xml"
  (mapcar- #'second (rfind-all :|condition| l)))
;; 
(defun rm-pre3 (s)
  (if (> (len s) 3) (subseq s 3)
    ""))
(defun ones-plc (n)
  (when (integerp n) (mod n 10)))
(defun form8 (n)
  (format nil "~8,'0d"  n))
(defun nctfile (n)
  "nct number to the file path"
  (str-cat "nct/" (ones-plc n) "/NCT" (form8 n) ".xml"))
(defun nctxml (n)
  "nct[n]'s xml s-epr"
  (s-xml:parse-xml-file (nctfile n))) ;for nct

(defun file2num (s)
  "file path, to the nct number"
  (first-num (rm-ext (rm-pre3  s)))
   )
 
;-
(defvar *cx* *xb*) 
;-
(defun mm-cui (&optional (xl *cx*))
    (second-lv (rfinds '(:|Mapping| :|Candidate| :|CandidateCUI|) xl))
    )
(defun mm-check (&optional (xl *cx*))
  (let* ((utt (first-lv (mm-UttText xl)))
         (mapng- (mm-Mappings xl))
         (mapng (first-lv (first mapng-)))
         (cui (mm-cui xl))
         (td (tree-depth xl))
         (nm (num-Mappings xl))
         (ok (and (eq nm 1) (equal (string-downcase utt) (string-downcase mapng)))))
    (if ok (format t "~&assoc:~a:with:~a" utt cui) 
      (format t "~&~acheck: ~a ~a ~a" td nm utt mapng))
    ok))
;-eof
(defun nctNum2-CUIs (fn n)
  (let*  ((fn (nctfile n))
          (fx (s-xml:parse-xml-file fn)) ;for nct
          ;(cl (conditions fx))
          (cl (funcall fn fx))
          (prs (mapcar #'(lambda (c s) (cons (str-cat n s) c)) cl *alphs*))
          (xl (mmtx_pairs prs)) ;metamap xml
          )
    (format t "~&nct~a ~a ~a" n (len fx) (len xl))
    (mm-check xl)
          ))
(defun nctNum2conditionCUIs (n) (nctNum2-CUIs #'conditions n))
(defun nctNum2outcomeCUIs (n) (nctNum2-CUIs #'outcome n))
 
(defun nctNums2conditionCUIs (&optional (nums *try-nct-s*))
  (mapcar- #'nctNum2conditionCUIs nums)
  (mapcar- #'nctNum2outcomeCUIs nums)
  )

(defvar *allc* nil)
(trace nctNum2conditionCUIs nctfile conditions)
(trace s-xml:parse-xml-file str-cat rfind-all)
(defun tst1 () ;might load nct/1*.cl here?
  (setf *allc* (nctNums2conditionCUIs)))
 
