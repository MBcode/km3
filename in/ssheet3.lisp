;(in-package "LISA") 
;(in-package "LISA-USER")
;(defvar *input-csv* "in/fm0401.csv") ;cfg
(defvar *input-csv* "") ;make sure can't see it till the end/later, so all computation redirectable
;ssheet2.cl parses simona's spreadsheet format, for preprocessed eligibility criteria   ;bobak
;  ;i believe that this should of been entered as s-expr/maybe even via a ka tool
;  ; there are still ambiguities w/the logic of the combination of the proprocessed phrases
;  ; i don't think this file is where most of the time should be spent ;after these instances are set..
;ld1.cl will get csv-parser.lisp &mapcare-csv-file now here are the accessors to get the parts2rearrange:
;In going from BULK load to no tmp files, less km&more sexpr output, this file lost 80 lines*
; I could still put out the km files, here during the one bulk load of the spreadsheet info
;  ----
;(defvar *t* (mapcar-csv-file *input-csv* #'nop)) 
(defvar *t* "you have to call init-csv")
;  ----
;do them on a per line/row basis, so can mapar over the *t* list
(defun mapt (f &optional (losl *t*)) (mapcar f losl)) ;list of str lists
(defun mapt2 (f) 
  (mapcar f (rest *t*))) ;get rid of col names
(defun mapt2f (fnc &optional (strm t)) 
  (mapcar #'(lambda (x) (funcall fnc x strm)) (rest *t*))) 
;(defun remove-nils (l) (remove-if #'null l))
;--use all of these to make a criteria ins
;(defun nct (l) (first l))
(defvar *cur-nct* nil)
;(defvar *cur-pp* nil) ;no it would be same a the ft
(defun nct (l) 
  (let ((n (first l)))
    (if n (progn (setf *cur-nct* n) n)
      *cur-nct*)))
;;(defun phrNum (l) (second l))
;(defun phrNum (l) (parse-number (second l)))
(defun phrNum (l) 
  (when (full l)
  (parse-number (second l)))) ;maybe "" if no num&print warn?
(defun ie (l) (third l)) ;was just Inclusion/Exclusion flag, but now could be subsection heading now
(defun fullTxt (l) (fourth l))
(defun preProc (l) (fifth l))
;(defun preProc (l) ;if nil don't use last
;  (let ((p (fifth l)))
;    (if p (progn (setf *cur-pp* p) p)
;      *cur-pp*)))
(defun cat (l) (sixth l))
(defun rewrite (l) (seventh l))
(defvar *crit-slots* '(nct ie ;fullTxt  ;txt already below
			   preProc cat rewrite))
;(defvar *crit-slots* '(|ie| |fullTxt| |preProc| |cat| |rewrite|))
(defun crit-slots (l)
 (reduce #'str-cat
  (mapcar #'(lambda (slt) (format nil " (~a (~a)) " slt (quote-str3 (funcall slt l))))
	  *crit-slots*)))
(defun p-crit (l &optional (strm t))
  ;format strm "~&(*crit~a has (instance-of (criteria)) ~a)" 
    (crit-str
	  (phrNum l) (crit-slots l)
	  strm))
;maybe make ins of I/E subclasses, and incl nct
;;(defvar *crit-ins* (mapt2 #'p-crit))
;(defvar *crit-ins* (with-open-file (strm "crit0.km" :direction :output) (mapt2f #'p-crit strm)))
 ;only if  cat==Complex, and parens in 'rewrite' then replace parened atoms w/ins that holds them
;have something w/a list of numbers get those nth values out if there
;(defun nth+ (ns ls)  ;in util-mb.cl now
; "nth from a list vs a single int"
;  (remove-nils ;assume nils only at end
;   (mapcar #'(lambda (n) (nth n ls)) ns)))
(defvar *ten-a* '(a b c d e f g h i j))

(defun renum-typ (l typ)
  (subst (gensym typ) typ l)
  )
(trace renum-typ)
;(defun list-difference (a b)
;  (mapcar #'(lambda (e) (remove e b :count 1)) a)) ;&reduce? ;not yet
;could also do positions for each type in cnct-list, and if get (> (len ret-lst) 2) then renum that type
(defun renum-dups (l) ;could just renum all of them right now
  (if (listp l)
   (let* ((rd (remove-duplicates l))
	  (sd (list-difference l rd)) ;won't work as still there, need a list-difference
	  )
      (if (< (len rd) (len l))
	(renum-typ l (first sd))
	l)) ;should map over if >1
    l))
;actually finish this later and use of parens not being made yet..
;(when (equal *input-csv* "fm0401.csv")
#+IGNORE
  (progn
 (defun atoms (l) (nth+ '(7 10 13 16 19 22 25 28 31 34) l))
 (defun ergos (l) (nth+ '(8 11 14 17 20 23 26 29 32 35) l))
 (defun cncts (l) (nth+ '(9 12 15 18 21 24 27 30 33 36) l)) 
 (defun match (l) (nth+ '(38 39 40 41) l)) ;new  ;not enough, so pprj still has all of it
);)
;(when (equal *input-csv* "fm2.csv")
  (progn
 (defun atoms (l) (nth+ '(7 10 13 16 19 22 25 28 31 34) l))
 (defun ergos (l) (nth+ '(8 11 14 17 20 23 26 29 32 35) l))
 (defun cncts (l) (nth+ '(9 12 15 18 21 24 27 30 33 36) l)) 
 (defun match (l) (nth+ '(42 43 44 45 46 47 48 49 50 51) l)) ;new  ;not enough, so pprj still has all of it
);)
; to keep logic correct, need to get parens from atoms
(defun atomparens (l) 
  "i would like a tree of nested paren logic by the end"
  ;consider asserting in KM as one big line of txt
  (format t "~&unfinished~a" l)
  )
;sc was going to do it this way for just a bit
;(defun atoms (l) (nth+  '(7 11 15 16 20 24 28 32 36 40) l))
;(defun ergos (l) (nth+  '(8 12 16 17 21 25 29 33 37 41) l))
;(defun match (l) (nth+  '(9 13 14 18 22 26 30 34 38 42) l))
;(defun cncts (l) (nth+ '(10 14 15 19 23 27 31 35 39 43) l)) 
;for final ins, might want to interleave atom parses w/cncts
;(defvar *cl* (mapt2 #'cncts)) ;in init-csv now
(defvar *cl* nil)
;maybe use/map over *fl*
(defun cnct-line (fn cl)
  "interleave" ;used for connectors between atoms
  (shuffle fn cl))

;and in the end cmp mainNPs w/the ones in the ergos
;
;;might be worth keeping the numbers around, like mk an assoc list of phrNum &txt
;;though mapt should keep it in line
;;(defvar *ft* (mapt #'fullTxt))
;;(defvar *pn* (mapt #'phrNum))
;;(defvar *ft2* (mapcar #'(lambda (t n) (cons t n)) *pn* *ft*))
;;(defvar *at* (mapt #'atoms)) ;use rest to get rid of headers:
(defvar *at* nil)
(defvar *erg* nil) ;new
(defvar *mth* nil)  ;new
(defvar *pn* nil)
(defvar *ft* nil)
;could flatten *at* but then wouldn't line up w/pn, but could spread out so in synch
;  could even sub#, so 1a etc ;only 10, so no further than a b c d e f g h i j
(defun flatt (&optional (at *at*)) 
  "flatten atoms/(or ?) and interleave even(sub)phrase number/lettering"
  (mapcar #'(lambda (n sl) 
	      ;(mapcar #'(lambda (s lt) (cons (str-cat  n lt) s)) sl *ten-a*)
	      (mapcar #'(lambda (s lt) (cons (str-cat  n lt) (trim-punct s))) sl *ten-a*)
	      )
   *pn* at))
;(trace flatt)
;did Atoms then Ergo columns, so connected like in pprj ;will need fix for how logic is connected/ask about
(defvar *fl*)
(defvar *fle*)
(defvar *fln*)
(defvar *fl2*)
(defvar *fl2e*)
(defvar *cncted*)
(defvar *nct*)
(defvar *pn*)
(defvar *ie*) ;new
(defvar *ie2*) ;new
(defvar *ft2*)
(defvar *ids*)
(defvar *fl2nd*)
(defvar *idnd*)
(defvar *mtha*)
(defun init-csv (&optional (input-csv *input-csv*))
  (format t "~&init-csv:~a" input-csv)
 (setf *t* (mapcar-csv-file input-csv #'nop)) 
 (setf *at* (mapt2 #'atoms))
 (setf *erg* (mapt2 #'ergos)) ;new
 (setf *mth* (mapt2 #'match))  ;new
 (setf *pn* (mapt2 #'phrNum))
 (setf *ft* (mapt2 #'fullTxt))
 (setf *cl* (mapt2 #'cncts))
 (setf *ie* (mapt2 #'ie)) ;new
 (setf *fl* (flatt))
 (setf *fle* (flatt *erg*)) ;new
 (setf *fln* (mapcar #'(lambda (sl) (mapcar #'first sl)) *fl*))
;(defvar *flne* (mapcar #'(lambda (sl) (mapcar #'first sl)) *fle*))
;;(defvar *fl2* (reduce #'nconc *fl*))
 (setf *fl2* (flat1 *fl*))  ;alst w/inputs to mmtx-tools* ;**
 (setf *fl2e* (flat1 *fle*))  ;Named ergo (gold-standard) ;**
 (setf *cncted* (mapcar #'cnct-line *fln* *cl*))  ;connected atoms
;(defvar *cnctede* (mapcar #'cnct-line *flne* *cl*))  ;connected atoms
;  ----took 80lines of the bulk run out
 (setf *nct* (mapt2 #'nct))
;(defvar *ft* (mapt2 #'fullTxt)) already above
 (setf *pn* (mapt2 #'phrNum))
 (setf *ft2* (mapcar #'(lambda (n txt) (cons n txt)) *pn* *ft*)) ;#-fTxt alst
 (setf *ie2* (mapcar #'(lambda (n txt) (cons n txt)) *pn* *ie*)) ;start to list headers-new
;(defun all-tx () (mapcar #'(lambda (ns) (tx (car ns) (cdr ns))) *ft2*))
 (setf *ids* (mapcar #'car *fl2*)) ;for spreadsheet only 
 (setf *fl2nd* (remove-duplicates *fl2* :key #'cdr :test #'string=)) ;no duplicates version
 (setf *idnd* (mapcar #'first *fl2nd*))
 (setf *mtha* (flatt *mth*)) ;partial but closer to usable np gold standard
 *ids*)
(defun f-id (id) (position id *ids* :test #'equal))
;if mappins could do mapt2 fncname of slotname
(defun pslt (n sn glb &optional (quotestr t)) 
  "nth of slot global" ;use weather should be quoted again
  (let ((val (nth n glb)))
    (format t "(~a ~a)" sn (if quotestr (quote-str val) val))))
(defun p-slt (sn val &optional (preproc #'quote-str)) 
  "pins sn val" ;~like km/etc
    (format t "(~a ~a)" sn (funcall preproc  val)))
;defun pins (id) 
(defun patm (id) 
  (let ((n (f-id id)))
    ;(format t "~&([atom~a] of  atom " n)
    (format t "~&([~a] of  atom " id)
    ;then map over an alst of slotname.*relatedGlobal* to print the slots
    (pslt n 'fullTxt *ft*)
    (pslt n 'byhand *erg*)
    ;handNP might even by a prs I do of the byhand input
    (format t ")")))
(defun cin (i) (str-cat "[" i "]"))
(defun pcrit (n)  ;already had p-crit but unused right now
  "print real/ful-fulltxt and cncts"
  (format t "~&([crit~a] of  criteria " n)
   ;(pslt n 'cncted *cncted* nil)
   ;(p-slt  'cncted (nth n *cncted*) #'cin)
    (p-slt  'cncted (nth n *cncted*) #'(lambda (l) (mapcar #'cin l)))
    (format t ")")) ;could patm from here
(defun ptrial (n) 
  "cnct trial to criteria"
  ;not being parsed yet
  ) ;could pcrit from here
;consider also (hash)|other ~oop/but-simple way of getting these globals w/in a *ct*"current trial"
;-eof
;put @end of ssheet2
(defun get_rec- (id &optional (alst *fl2*)) (assoc id alst :test #'str-eq2))
(defun id2a-cdr (id alst) (cdr (get_rec- id alst)))
 
; these should go in ssheet ;actually should go in test.lisp ;loaded in silver.lisp from nlm.asd
;(defun id2erg_ (id) (id2a-cdr id *fl2e*)) ;this is the correct global4 gold-stnd ;used in id2erg..
;(defun id2erg- (id) (id2a-cdr id *fl-2e*)) ;this is nlp NP only
;(defvar *rm-erg-strs* '( "[NP]" "[contextual modifier" "[context modifier" "[modifier]"
;                        "[NP" "[modifier" "]" ","))
;(defun id2erg (id) (no-dbl-spaces (trim-whitesp (rm-strs *rm-erg-strs* (id2erg_ id)))))
;(defun id2ergm (id) (cdr (assoc id *fl-2em* :test #'equal))) ;modifier/not checked/used yet
;
(defun alst-w (vec &optional (ids *idnd*)) ;or remove-nils
   (collect-if #'nop (mapcar #'(lambda (m i) (when m (cons i m))) vec ids)))
(defun alst_w (vec &optional (ids *idnd*))
   (mapcar #'cons vec ids))
(setf *input-csv* "in/fm0401.csv") ;cfg just to be sure it has a value
(defun mapidnd- (fnc) (mapcar fnc *idnd*))
(defun mapidnd (fnc) (format t "~&mapidnd:~a~&" fnc) (mapcar fnc *idnd*))
(defun mapids (fnc) (mapcar fnc *ids*))
(defun mapidnd2 (fnc) (mapcar #'cons (mapidnd fnc) *idnd*))
;-eof
;all in my sbclrc now:
;;(defvar *uptime* (run-ext "date"))
;(defvar *uptime* (date))
;(defun uptime () *uptime*)
;(defun lo () (format t "~&start:~a to-now:~a" (uptime) (date)) (ex))
