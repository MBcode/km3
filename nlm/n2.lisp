;was s14, now oct5; I really want this to go away; still simple alst4awhile but to objs..
;aug24 update/cleanup of accessors, incl clos; put heurstics in goog-doc(s)  bobak@computer.org
;-I don't like the prs_ssheet in waves&processing stuff, I like the pushnew; but still process?
(defvar *p* '()) ;pushnew this later
(defvar *ri* nil) ;(setf *ri* (mapcar #'mk-rec *ids*)) ;might start to use instead of *p*
(defvar *sheet2a* nil) ;was a3a.cl but getting really messy/again
;get rid of all accessors that only ret for part of 1 subphr unless it's used by the main1for the rec
;(defun head (l) (subseq l 0 4)) 
;(defun tail (l) (last l 4))
;(defun last3 (l) (last l 3))
;(defun ID2MMTX_PAIR (id) '("redefine later"))
(defun assocby2nd (a)
    (when (and (full a) (> (len a) 1)) (cons (second a) a))) 
(defun get-rec (id)  ;l used below 
  "keyed list of nlm/etc output"
  (let (;(frec (find (string-upcase id) *sheet2a* :test #'str-eq2 :key #'first))
        (frec (find (string-upcase id) *p* :test #'str-eq2 :key #'second)) ;try
	)
    ;(when (full frec) (first (cdr frec)))
    ;(when (full frec) frec)
    (if (full frec) frec
      (show id) ;(show-phr id) 
    ))) ;(assocby2nd (id2mmtx_pair id)) ;fix2get pnow ;should call fnc again/fix later
; try making a class&see if can extend accessors
(defclass subRec ()  ;can be a few of these per (MMTx parse of an)  atomic phrase 
  ((id :accessor id :initarg :id) ;new
   (mm-candidates :accessor mm-candidates :initarg :mm-candidates)  ;;In mmt.lisp now
   (mm-mappings :accessor mm-mappings :initarg :mm-mappings)));fill mmt w/these full of ca/map/etc
(defclass rec () ;was t
  ((id :accessor id :initarg :id)
   (txt :accessor txt :initarg :txt)
   (mmt_ :accessor mmt_ :initarg :mmt_) ;would have to defmethod mmt
   (mmtx :accessor mmtx :initarg :mmtx)
   (npparser :accessor npparser :initarg :npparser)
   ;new, after ins made w/mk-rec
   (modifier :accessor modifier)
   ))
;all these are still from alist:
(defmethod id ((l LIST)) (get1+ :id l)) 
(defmethod id ((s STRING)) (id (get-rec s))) 
(defmethod txt ((l LIST)) (get1+ :txt l)) 
(defmethod txt ((s STRING)) (txt (get-rec s))) 
(defmethod mmtx ((l LIST)) (get1+ :mmtx l)) 
(defmethod mmtx ((s STRING)) (mmtx (get-rec (rm-star s)))) 
(defun txt-2 (id) 
  "hyphenated input for nlp parsers"
  (let ((pv (first-lv (phr-pi id))))
    (typecase pv 
      (string  pv)
      (symbol (symbol-name pv))
      (t (to-str pv)))))
(defun txt2 (id) 
  (let ((tx (string-downcase (txt id)))) 
    (format t "~&using-txt:~a" tx) tx)) ;so can see when dflt2txt
;(defun frag-num (a) "not catching if candidates=0" (len (mmtx a)))
(defun frag-num (a)  (len (mm-candidates a)))
	;npparser not working, but just wrote has-np, so start to use that
(defmethod npparser ((l LIST)) (get1+ :npparser l))
;(defmethod npparser ((l LIST))  ;this is alst, need cdr to be downcased
;  (let ((np (get1+ :npparser l)))
;    (when (full np) (string-downcase np))))
(defmethod npparser ((s STRING)) (npparser (get-rec s))) 
;if i stored in ins, then also have the (5) :accessor(s) from that
(defun mk-rec (a) (make-instance 'rec :id (id a) :txt (txt a) :mmtx (mmtx a) :npparser (npparser a)))
;make another version that can be set just as the sexpr is being set
(defun mk-rec4 (id txt mmtx np) (make-instance 'rec :id id :txt txt :mmtx mmtx :npparser np))
(defun id2rins (id) ;or id2r, but start 2 fill up more slots, eg modifiers
    (let ((p (position id *ids* :test #'str-eq2)))
          (when (numberp p) (nth p *ri*))))
(defun set-modifier (id m) (setf (modifier (id2rins id)) m)) ;should check, might not have id@pnt..?
;mkm-rec (a) ;make in km, though already have a few, but .., also have cui, which can be useful

;could replace some of sexpr terms w/the subRec ones above

;=break n2 to n2&n3 below right here (for now)
;-eof
;put back in for a bit:
(defun l1-prs (a) (mapcar #'first (mmtx (if (stringp a) (get-rec a) a))))
(defun l1_prs (id)  (flat1 (mapcar #'butlast (l1-prs id)))) ;was in passert-prs
