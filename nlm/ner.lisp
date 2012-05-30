;local: /threec/nlm> tail -99 ben.lisp mmprs.lisp >ner.lisp 
;==> ben.lisp <==
;(in-package "LISA") ;(in-package "LISA-USER")

(defun arith-comparator-p (string)
  (and (stringp string)
       (find string '("==" ">=" "<=" ">" "=" "<") :test #'string-equal)))

(defun compound-arith-comparator-p (string)
  (and (stringp string)
       (find string '("==" ">=" "<=") :test #'string-equal)))

;; (compound-arith-comparator-pos-list "CD4 count >= 100 cells/mm3")
(defun compound-arith-comparator-pos-list (tokens)
  (loop for token 
      in (if (stringp tokens) (record->list #\space tokens) tokens)
      for i upfrom 0
      when (compound-arith-comparator-p token)
      collect (list i token)))

(defun arith-comparator-pos-list (tokens)
  (loop for token 
      in (if (stringp tokens) (record->list #\space tokens) tokens)
      for i upfrom 0
      when (arith-comparator-p token)
      collect (list i token)))

(defparameter *arithmetic-comparator-CUI-alist*
    '(
      (">=" "C0439091")
      ("<=" "C0439090") 
      ("==" "C0439089") 
      ("=" "C0439089")
      (">" "C0439093")
      ("<" "C0439092")
      ))

;; (arith-comparator-CUI 2 (compound-arith-comparator-pos-list "CD4 count >= 100 cells/mm3"))
(defun arith-comparator-CUI (n compound-arith-comparator-pos/id-pairs)
  (when (and (integerp n)
         (listp compound-arith-comparator-pos/id-pairs))
    (second (assoc (second (assoc n compound-arith-comparator-pos/id-pairs))
           *arithmetic-comparator-CUI-alist* :test #'string-equal))))

;; (rewrite-token-list "CD4 count >= 100 cells/mm3" "CD4  count >           =         100    cells" "noun noun greaterThan equalSign number noun")
;; ==>
;; (("CD4" "count" ">=" "100" "cells") ("noun" "noun" "C0439091" "number" "noun"))
;; (rewrite-token-list "CD4 count > 100 cells/mm3" "CD4  count >         100    cells" "noun noun  greaterThan number noun")
;; ==>
;; (("CD4" "count" ">" "100" "cells") ("noun" "noun" "C0439093" "number" "noun"))
(defun rewrite-MMTx-output (input-text mmtx-output mmtx-pos-output)
  (when (and (stringp input-text)
         (stringp mmtx-output)
         (stringp mmtx-pos-output))
    (let* ((input-text-tokens (record->list #\space input-text))
       (compound-arith-comparator-pos/id-pairs
        (compound-arith-comparator-pos-list input-text-tokens))
       (compound-comparator-positions
        (when compound-arith-comparator-pos/id-pairs
          (mapcar #'first compound-arith-comparator-pos/id-pairs)))
       (next-compound-positions
        (when compound-comparator-positions 
          (mapcar #'1+ compound-comparator-positions)))
       (mmtx-output-tokens (record->list #\space mmtx-output))
       (mmtx-pos-output-tokens (record->list #\space mmtx-pos-output)))
      (if (null compound-arith-comparator-pos/id-pairs)
      (list 
       mmtx-output-tokens
       (loop for mmtx-pos-output-token in mmtx-pos-output-tokens
           for mmtx-output-token in mmtx-output-tokens  
           for i upfrom 0
           collect 
         (if (arith-comparator-p mmtx-output-token)
             (second (assoc mmtx-output-token
                    *arithmetic-comparator-CUI-alist* :test #'string-equal))
           mmtx-pos-output-token)))
    (list 
     (loop for mmtx-output-token in mmtx-output-tokens
         for i upfrom 0 when (not (find i next-compound-positions))
         collect (if (find i compound-comparator-positions)
             (second (assoc i compound-arith-comparator-pos/id-pairs))
               mmtx-output-token))
     (loop for mmtx-pos-output-token in mmtx-pos-output-tokens
         for mmtx-output-token in mmtx-output-tokens  
         for i upfrom 0 when (not (find i next-compound-positions))
         collect 
           (if (find i compound-comparator-positions)
           (arith-comparator-CUI i compound-arith-comparator-pos/id-pairs)
         (if (arith-comparator-p mmtx-output-token)
             (second (assoc mmtx-output-token
                    *arithmetic-comparator-CUI-alist* :test #'string-equal))
           mmtx-pos-output-token)))))))) 
(defun record->list (delimiter seq)
  (let ((end (length seq)))
    (loop for left = 0 then (+ right 1)
        for right = (or (position delimiter seq :start left) end)
        if (< left right)
        collect (subseq seq left right)
        until (eq right end))))
 
;==> mmprs.lisp <==
;defun tuis_dscore (sl) 
;defun rescore-pair (pr) 
;-eof 
;==> b2.cl <== replaces ben.cl/ben's rewrite-MMTx-output
;l1-prs -> l1-ben output ;2replace 97 lines of ben.cl
(defvar *mathcmp* '("greaterThan" "lessThan" "equalSign"))
(defvar *mathcmp2*  ;new, or defparameter
  '("greaterThan" "lessThan" "equalSign"  "greaterThanEqual" "lessThanEqual" ">=" "<=" ">" "<" "="
 ;  "before" "after" "higher than" "lower than" ;pairs done below; see if after/before works though
 ;  "prior to" "within" ;dec after Mor's 1st scoring
    ))
;;Someday I'd like all input from the KB (ner.km) &run right off of that, maybe in v4?
;this is really time to start the bagOwords for concepts catching
(defvar *mathcmp2s*  ;search pairs
    '() ;'(("higher" "than") ("lower" "than"))
    ) 
;could catch this before parse(&keep together) &/or ofter parse when in parts
;mmt still breaks even w/hyphen, so will have to recognize from combo/pattern
;higher  than 
;adj     prep
(defun mathcmp-p (s) (position s *mathcmp2* :test #'equal))
;greaterThan equalSign  -> (">=" "C0439091") ; lessThan equalSign  -> ("<=" "C0439090")  
;get an l1, from: (l1-prs (get-rec "41C"))
;
(defun thaneq-p (id) ;not used
  "if > < + = in the txt" ;unused?
  (let* ((pl (mapcar #'mathcmp-p ;(mapcar #'cdr (first (l1-prs (get-rec id))))
		     (mm-prs id)
		     ))
	 (ge (search '(0 2) pl))
	 (le (search '(1 2) pl)))
    (full (remove-nils ;new
    (list ge le)))))

(defun subst2at (l at new)
  "replace pair at-position w/new-val in List"
  ;(list (subseq l 0 at) new (subseq l (+ at 2)))
  (if (numberp at) 
    (concatenate 'list (subseq l 0 at) (list new) (subseq l (+ at 2)))
    l))  ;think there is a subst that would do it, but no w/the at, still consider it

(defun l1-prs2b2 (l1)
  "prs2ben.cl like replacement" ;put in =, &expand ner in(a)general(way)
   (let* (;;(pl (mapcar #'mathcmp-p (mapcar #'cdr (first l1))))
	  ;(prs (first l1))
	  ;(prs1 (l1-prs l1))
	  (prs1 (mm-prs l1))
	  (prs (flat1 prs1))
	 (wl (collect-if #'full (mapcar #'car prs))) ;word list
	 (tl (collect-if #'full (mapcar #'cdr prs)))  ;tag list
	 (pl (mapcar #'mathcmp-p tl)) ;types of mach cmp-s
	 (ge (search '(0 2) pl)) ;>=
	 (le (search '(1 2) pl)) ;<=
	 (ee (search '(2) pl)) ;= ;new
	 ;maybe subst pr w/cui/etc directly
	 (plge (subst2at tl ge "C0439091"))
	 (plle (subst2at plge le "C0439090"))
	 (plge2 (subst2at wl ge "greaterThanEqual"))
	 (plle2 (subst2at plge2 le "lessThanEqual"))
	 )
     (when *dbg* (format t "~a ~a" ge le))
     (list plle2 plle)))
;
(defun l1-prs2ben- (l1) ;not used
  (let ((w+t (l1-prs2b2 l1)))
     (format t "~a" (first w+t))
    (second w+t))) ;like orig
;(l1-prs2ben- (l1-prs (get-rec "41C")))
;("noun" "noun" "noun" "C0439090" "number" "noun" "")
; ret (list plle2 plle)  &maybe use instead of orign prs;now called l1-prs2b2
(defun id2prs3 (id)
  "nlp3 and orig/(NpPrs/etc) cached prs"
  (let* ((cp (id2prs id)) ;cached parse
	 (l1 (get-rec id))
	 (prs (l1-prs l1))
	 (b2 (l1-ben l1))) ;want b2 simple/more ext version of ben's addition
    (list cp prs b2)))

(defun id2prs2 (id)
  (let* ((cp (id2prs id)) ;cached parse
	 (l1 (get-rec id))
	 (b2 (l1-prs2b2 l1)))
    (list b2 cp)))
;(mapcar #'(lambda (id) (list id (id2prs2 id))) *ids*)

;prefer to use subst, but not matching as expected
(defun  prs2ben- (id)
  (let* (;(prs (first (l1-prs (get-rec id))))
	 ;(prs1 (l1-prs (get-rec id))) ;new
	 (prs1 (mm-prs id))
	 (prs (flat1 prs1)) ;new
	 (wl (collect-if #'full (mapcar #'car prs)))
	 (tl (collect-if #'full (mapcar #'cdr prs)))  ;could use subst2at below instead
	 (plge (subst "C0439091" '("greaterThan"  "equalSign") tl :test #'equal))
	 (plle (subst "C0439090" '("lessThan"  "equalSign") tl :test #'equal)))
    (format t "~&~a~&~a~&~a~&~a" wl tl plge plle)
    ))
;aug10, tried this &only did 1st sub-fragment, &didn't see this output yet
(defun prs2ben (id) (l1-prs2b2 (get-rec id)))
(defun id2ben (id) (l1-prs2b2 (get-rec id)))

(defun than_p (id)
  (let* ((wl (first (id2ben id)))
	 (ret (remove-nils (mapcar #'(lambda (cmps) 
				       (when (search cmps wl :test #'equal) cmps))
				   *mathcmp2s*)))) 
    (when (full ret) (mapcar #'implode-l ret)))) ;str-cat makes ret more like others

(defun a2 (pr)
  (when (full pr) (cdr pr)))

(defun l1-prs2ben (l1) ;not used
   (let* ((pl (first l1))
	 (plge (subst "C0439091" '("greaterThan"  "equalSign") pl :test #'equal :key #'a2))
	 (plle (subst "C0439090" '("lessThan"  "equalSign") pl :test #'equal :key #'a2)))
     plle)) ;could reduce w/subst

;-was eof ;much unused, so clean up in a bit  ;;finish = above&a bit more ;but also generalize a bit
;=-=-=-= ;from s14.lisp
;-A comparison criterion ERGO Annotation has three components: noun phrase, operator, and quantity.
;defun cmp-p (id)
(defun acmp-p (id)
  "cmp-p for atoms w/mmtx..runs"
  (let* ((mcs (first (id2ben id)))
	 (in (intersection mcs *mathcmp2* :test #'equal)))
    (when (full in) in)))
(defun cmp_p (id)
  "math|txt comparator, 4atoms"
  (or (acmp-p id) (than_p id)))
(defun id2ner (id)
  "named entity recog, trail"
  (let* ((b (id2ben id))
	 (b2 (second b))
	 (bc (remove-nils (mapcar #'cui-p b2))))
    (when (full bc) (first-lv bc)))) ;might have >1 soon
(defun id2cmp (id)
  "see if cmp fragment&ret"
  (let* ((b (id2ben2 id))
	 (b1 (first b))
	 (b2 (second b))
	 (p (position-if #'cui-p b2)))
    (when p (list (split-nth p b1) (split-nth p b2))))) 
;these get cmp from ben's cui, but cmp-p just looks for the mathcmp2's  ;fix
(defun id2cmp2 (id)
  (let* ((b (id2ben2 id))
	 (a  (mapcar #'(lambda (a b) (cons a b)) (first b) (second b)))
	 (p (position-if #'cui-p a :key #'cdr)))
    (when p (split-nth p a)))) 
(defun id2cmp-2 (id)
  (or (id2cmp2 id) (than_p id)))
;;the auto-scoring of it need a fix
(defun alst-2 (l)
  (if (listp l) (alst2 l) l))
(defun id2cmp-3- (id) ;no broken
  (let ((l (id2cmp2 id)))
    (when l (mapcar #'alst-2 l))))
(defun id2cmp-3 (id) ;cmp w/silver below
  (let ((l (id2cmp2 id)))
    (when l
     (let ((lhs (alst2 (first l)))
	   (cmp (second l))
	   (rhs (third l)))
       (list lhs cmp rhs))))) 
(defun id2cmp-lhs (id)
  "cmp lhs ((words) (pos))"
  (let ((l (id2cmp2 id)))
    (when l ;(map car (first l))
      (alst2 (first l)
      ))))
(defun id2cmp_lhs (id) ;used in id2bestopNP
  "cmp lhs words"
  (let ((l (id2cmp-lhs id)))
    (when (full l) (implode-l (first l)))))
;not >1 np in lhs yet, but could mmtx&decide between
;48a 56a/b will need this along w/the next dblChecking on TUI time changes
;  sublis can be used to w/either part bak1 bak2 to make substitutions in a tree
(defvar *cmp-bak* '( ;for cui's defparameter *arithmetic-comparator-CUI-alist* 
    ("lessThanEqual" . "<=")  ;bak1
    ("greaterThanEqual" . ">=")
    ("equalSign" . "=")
    ("lessThan" . "<")
    ("greaterThan" . ">")
    ;for now try:
    ("<=" . "lessThanEqual")  ;bak2
    (">=" . "greaterThanEqual")
    ("=" . "equalSign")
    ("<" . "lessThan")
    (">" . "greaterThan")
    )) ;finish these off
(defun cmp-bak (bstr)
  (let ((a (assoc bstr *cmp-bak* :test #'equal)))
    (when a (cdr a))))
(defun id2_cmp (id) ;comparable w/id2erg_cmp
  "parsed comparison"
  (let* ((lhs (id2cmp_lhs id))
	 (l (id2cmp2 id))
	 (cmp1 (first (second l)))
	 (cmp (cmp-bak cmp1))
	 (rhs (implode-l (mapcar #'car (third l)))))
    (format t "~&~a" cmp)
    (list lhs cmp1 rhs))) 
(defun id2erg_cmp (id) ;assume cmp already
  "expected comparison"
 ;when (id2ner id)
 (let ((ergv (csv-parse-string (id2erg_ id))) )  ;this is silver
  (if (< (len ergv) 3) (format t "~&WARN:bad-silver:~a" ergv)
   (let (;(ergv (csv-parse-string (id2erg_ id)))  ;this is silver
	 (lhs (trim-whitesp (rm-strs *rm-erg-strs*
				     (elt ergv 0))))
	 ;(np (id2erg- id)) ;should be same as lhs
	 (cmp (elt ergv 1))
	 ;(cui (id2ner id)) ;instead of the cmp txt
	 (rhs (elt ergv 2)))
    (list lhs cmp rhs)) ) ))
;consider removing all spaces from each of the RHSides, to compare them
(defun strs-eq2 (l1 l2) (every #'nop (mapcar #'str-eq2 l1 l2))) ;mv2utils
(defun scoreID2cmp (id) ;will need version of above strs-eq2
  "compare comparisons"
  (if (not (acmp-p id))  ;want2try cmp_p ;(not (id2ner id)) 
    (format t "~&warn:~a not a cmp" id) ;could raise a (c)warn
    (strs-eq2 (id2_cmp id) (id2erg_cmp id))))
;;the auto-scoring of it need a fix   ;this&temporal catching might come in reorg
;=-=-=-=
;-eof
 
 
