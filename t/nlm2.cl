;t.cl and nlm.cl(from nlm/mmt.lisp); all in this one nlm2.cl
(defvar *dbg* nil)
;====
;-parse mmtx-or-metamap output
(defvar *cid* nil) ;instead of num, set current-id
(defvar *cidn* nil) ;sPhr num
(defvar *cid2* nil) ;cid . cidn
(defvar *tst2* nil)
;=-=-=-=-=-=
(defun mmt (s)
  "was MMTx, now gives mm xml"
  ;(break2lines (run-ext  "mmt" s))
  (setf *tst2* (run-ext  "mmt" s))
  )
;=-=-=-=-=-=
(defun mmt-id (id str)
  (fnc-id_ "mmt" #'mmt id str)) ;(fnc-id "mmt" id str)
;(defun mnp-id (id str) ;  (fnc-id "mnp" id str))
;(trace mmt-id)
;=-=-=-=-=-= 
(defun mmtx_pair (pr &optional (strm t))
  (setf *cid* (car pr)) ;(setf *cid* id)
 (let* ((id (car pr))
        (str- (cdr pr))
        (str (rm-star str-))
	(mm (mmt-id id str))) ;still has sexpr w/everything
      (when *dbg* (format t "~&mm-len:~A" (len mm)))
         mm)) 

(defun mmtx_pairs (prs &optional (strm t))
  (mapcar #'mmtx_pair prs))
;====
;(al 's-xml-rpc)
;(defvar *xa* (s-xml:parse-xml-file "ha2.xml"))
;(defvar *cx* *xa*)
(defvar *cx* *xb*)
;(defvar *cx* *oh*)
(defvar *cx2* (fifth  (second *cx*)))
(defun under_f_ (str) ;better than cache version ;see m16_
  (let* ((wrds (string-to-words str))
	 (iname (str-cat_l wrds)))
    iname))  ;finish off w/a much smaller/better cache/ing
(defun mx250 (str)
    (subseq  str 0 (min (length str) 250)))
(defun str-cat_l2 (str)
   ;(subseq (str-cat_l str) 0 (min (length str) 250))
     (mx250 (str-cat_l str)))
;I could almost just restart just using km/utils and get the metamap in  **make it much smaller**
;defun metamap (s)
(defun new-mmt (xso path)
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out "~a" xso))
    )
(defun mmx_ (s)  ;right now cache/mmt/*.txt has the xml, from cluster-1
  "call and start mm processing"
  (let* ((wrds (string-to-words s)) ;can skip&just use under_f
	 ;(iname (str-cat_l2 wrds))
	 (iname (under_f2 wrds))
	 (fx (str-cat2 iname ".xml"))
	 (fx2 (str-cat2 "xml/" fx))
	 (fc (str-cat2 iname ".cl"))
	 (path (str-cat2 "cache/" fc))
	 ;(xs (run-ext "mmx" (format nil "\"~a\"" wrds))) 
	 ;(xso (s-xml:parse-xml-string xs))
	 ;(xso (s-xml:parse-xml-file (str-cat2 "xml/" fx)))
	 ;(xso (s-xml:parse-xml-file fx2))
	 (nf (unless (file-p fx2) (new-mmt (mmt s) fx2))) ;make cached xml if doesn't exist;on lnx
	 ;(xso (if (file-p fx2) (s-xml:parse-xml-file fx2)  ;....
	 ;  (mmt s)))
	 (xso (s-xml:parse-xml-file fx2))
	 ;-;(xs (format nil "mmx \"~a\">cache/~a" wrds fx))
	 )
    (when *dbg* (format t "~&~a,len=~a" fx (len xso)))
    ;-;(format t "~&~a" xs) ;&run by hand on domsetup this one time
    ;now make ins, and cache, like cache, **
    ;file:///Users/bobak/Downloads/lang/lsp/doc/book/HyperSpec/Body/m_w_open.htm
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out "~a" xso))
    xso))
;=-=
(defun mmt-id (id str)
  (when *dbg* (format t "~&skip:~a,use:~a" id str))
  (mmx_ str))
;=-=
;look@s-xml for stream proc
;-ut  ;how much better than ut/ fncs?
;defun rfind_ (s tree) ;work on
;defun rfind_all (key alst)
;-at end of ut
;-
;parse the xml
(defun UttText () (last-lv (rfind :|UttText| *cx*)))
(defun Phrases-Count () (first-lv (get_nums (last_lv (rfind :|Phrases| *cx*)))))
;(defun PhraseText () (last-lv (rfind :|PhraseText| *cx*)))
(defun PhraseText () (mapcar #'last-lv (rfind_all :|PhraseText| *cx2*)))
;(defun LexMatch () (last-lv (rfind :|LexMatch| *cx*)))
(defun LexMatch () (mapcar #'last-lv (rfind_all :|LexMatch| *cx*)))
(defvar *2tst* '( #'UttText #'Phrases-Count #'PhraseText #'LexMatch))
(defvar *s2tst* '(UttText Phrases-Count PhraseText LexMatch))
;function file:///Users/bobak/Documents/www/jtra.cz/stuff/lisp/sclr/function.html
;(defun funcalll (f arglst) (funcall f @arglist))
(defun fnc-l (fnc the-seq &key (start 0) (end (length the-seq)))
    (reduce fnc the-seq :start start :end end)) 
(defun funcalll (f arglst) (fnc-l f arglst)) ;if reduce-able
(defun fncall (f &rest args) (funcalll (symbol-function f) args))
(defun fncall0 (f) (funcall (symbol-function f)))
(defun tstt(&optional (tst *s2tst*))
  (mapcar #'(lambda (f) (format t "~&~a:~a" f (fncall0 f))) tst))
 
;-lets start to parse the mm xml, esp the final mapping -more above already
;assume one UttText, and check it against the top Mappings
(defun mm-UttText (&optional (xl *cx*)) (last-lv (rfind :|UttText| xl)))
(defun xlNum (l)
  (first-lv (get_nums (last_lv l))))
(defun xlCandidateMatched (l)
  (last-lv (rfind :|CandidateMatched| l)))
(defun xlCandidateMatches (l)
  (mapcar- #'last-lv (rfind-all :|CandidateMatched| l)))
(defun xlCandidateCUI (l) (xlNum (rfind :|CandidateCUI|)))
(defun num-Mappings (xl) (xlNum (rfind :|Mappings| xl)))
(defun mm-Mappings (&optional (xl *cx*))
  (let* ((n ;(xlNum (rfind :|Mappings| xl))
	  (num-Mappings xl))
         (mapngs (mapcar- #'xlCandidateMatched (rfind-all :|Mapping| xl)))
	 ;for condition 1cui is ~enough, but rest will need more, &best NP.. ;scone/etc/soon?
	 (mapng (first mapngs))
	 (cui (xlCandidateMatched (first mapng)))
	)
    (when (neq n 1) (format t "~&Have ~a mappings" n))
    (list mapng cui)
  ))

;defun rfinds (fl l)
;   "rfind like xpath"
;     (if (eq (len fl) 1) (rfind (first fl) l)
;         (rfinds (rest fl) (rfind (first fl) l)))) 
;now a version that looks for a tree,
;that is if(the tail)part of fl, is a list, get all of those values 
(defun rfinds (fl l)
    "rfind like xpath"
    (if (eq (len fl) 1) (let ((ffl (first fl)))  (if (listp ffl) (mapcar #'(lambda (ff) (rfind ff l))
                                                                           ffl)
                                                     (rfind ffl l)));(rfind (first fl) l)
            (rfinds (rest fl) (rfind (first fl) l))))
 ;make a rfinds-all version, but will have2mapcar
;this works for reg rfinds calls
;USER(7): (rfinds '(:|Mapping| :|Candidate| (:|UMLSConcept| :|MatchedWord|)) *cx*)
;((:|UMLSConcept| "Men") (:|MatchedWord| "men"))
 
(defun mm-cui (&optional (xl *cx*))
   ;(second-lv (rfinds '(:|Mapping| :|Candidate| :UMLSCUI) xl))
    (second-lv (rfinds '(:|Mapping| :|Candidate| :|CandidateCUI|) xl))
    ) 
(defun mm-check (&optional (xl *cx*)) 
  (let* ((utt (first-lv (mm-UttText xl)))
	 (mapng- (mm-Mappings xl))
	 (mapng (first-lv (first mapng-)))
	 ;(cui (second mapng-))
	 (cui (mm-cui xl))
	 (td (tree-depth xl))
	 (nm (num-Mappings xl))
         (ok (and (eq nm 1) (equal (string-downcase utt) (string-downcase mapng))))) 
    (if ok ;(and (eq nm 1) (equal utt (string-downcase mapng))) 
      (format t "~&assoc:~a:with:~a" utt cui) ;(format t "~&~aassoc ~a with ~a" td utt mapng-) 
      (format t "~&~acheck:~a,~a,~a,~a" td nm utt mapng cui))
    ok))
   
;now use this below in every nctNum2conditionCUIs call

;-
;; ==i would like to write the .ste version of this now
;;
;; Duke:
;;
(defun outcome (l)
   (second-lv (rfinds '(:|primary_outcome| :|measure|) l)))
;;
(defun conditions (l)  ;focus
  "all condition-s from NCT xml"
  (mapcar- #'second (rfind-all :|condition| l)))

(defun cnc (nct)
  "get current NCT" ;via dir or..?
  (s-xml:parse-xml-file nct))

(defun rm-pre3 (s) 
 ;(when (> (len s) 3) (subseq s 3))
  (if (> (len s) 3) (subseq s 3)
    "")
  ) 

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
  ;(subseq s 3 11)
  (first-num (rm-ext (rm-pre3  s)))
   )

(defun second2last (l) ;make a version that checks for MM/etc if don't parse it all now
  (second (last3 l)))
;--in ut
;(defvar *alph* '(#\a #\b #\c #\d #\e #\f #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s))
;(defvar *alphs* '("a" "b" "c" "d" "e" "f" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"))
(defun nctNum2conditionCUIs (n)
  (let*  ((fn (nctfile n))
	  (fx (s-xml:parse-xml-file fn)) ;for nct
	  (cl (conditions fx))
	  (ol (outcome fx))
	  ;(prs (mapcar #'(lambda (c s) (cons (str-cat n s) c)) cl *alphs*))
	  (prs (mapcar #'(lambda (c s) (cons (str-cat n s) c)) (append cl (list ol)) *alphs*))
	  ;(xml (mmtx_pairs prs)) ;for metamap
	  ;(xl (s-xml:parse-xml-string xml))
	  (xl (mmtx_pairs prs)) ;metamap xml
	  )
    (format t "~&~a,nct~a ~a ~a ~a ~a" (len prs) n (len fx) (len xl) cl ol)
    (mm-check xl)
	  ;xl 
	  ))
(defun nctNums2conditionCUIs (&optional (nums *try-nct-s*))
  (mapcar- #'nctNum2conditionCUIs nums))

(defvar *allc* nil)
;(trace nctNum2conditionCUIs nctfile conditions)
(defun tst1 () ;might load nct/1*.cl here?
  (setf *allc* (nctNums2conditionCUIs)))
;-eof
