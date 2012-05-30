;-see how many of these actually used, and how much it could be compressed/use utkm
;I think I will split it into more generic, &the parts that make the old cui string
; I did this w/cui.lisp then phr.lisp  as utkm works well, &could take over even more soon
;-I'd like most of the generic to focus on the asserting&have no str inbetween
;  mk/mv generic get/put & setting of id's to ../km/utkm.lisp then version this file&mk clean
;  ../km/utkm already has ki for km-id just mv fwd ;prob vers/~split file like did w/mnp
;     don't forget to finish asserting words from cui so can be linked
; -can keep some around, but could skip the making of cui w/s-expr, only have id &rest asserted..
;  clean this file up a-lot, have quick general id mk/set/finding, then for types, then get/put -slot.
;mkm.lisp KM interaction bobak@computer.org
;;-want2get rid of cui.lisp so see if this will work for now
; (tuis (cuistr2tui cs)) ;instead get tui from cui /ins if it has been initialized
(defun cui2tui (i) (gv+ i "instance-of"))
(defun c-ok (cs) (prefix "C" cs))
;(defun cuistr2cui (cs) (prefix "C" (subseq cs 2 10)))
(defun cuistr2cui (cs) (c-ok (subseq cs 2 10)))
(defun cuistr2kmcui (cs) (ki (cuistr2cui cs))) ;used2intern could get rid of this
;(defun  cuistr2tui- (cs) (cui2tui (cuistr2cui cs)))      ;in cui.lisp
(defun  cuistr2tui- (cs) (mapcar- #'symbol-name (cui2tui (cuistr2cui cs))))
(defun cui-p (c) (rm-star (c-ok c))) 
(defun cui-dscore (c) (gv c "dScore"))
;-
;-to get rid of phr.lisp
(defun phr-id_str (id) (ki id))
(defun phr-id (id) (ki id))
(defun phr-txt (id) (gv+ id "txt"))
;-
;(in-package "LISA") ;(in-package "LISA-USER")
; if done from ./km maybe don't need dir, but it did get it
(load-kb "km/mm.km")
(load-kb "km/dep.km")
;(load-kb "km/tui.km") 
(load-kb "km/tui2.km") ;more slots/need2check, jan2010 
(load-kb "km/scs.km") ;new 
(load-kb "km/scs2.km") ;n15 
(load-kb "km/ergo1.km") ;new 
(load-kb "km/ner.km") ;new 
(load-kb "km/pr.km") ;columbia pref rules 
(load-kb "km/trt2.km") ;metamap relations between tuis  ;assoc w/tui2
;the format of the preference rule is "A|B", where B is kept and preferred to A when assigning a semantic type to a concept.
(defun tui> (i1 i2) ;chould use all classes if >1, but try1st w/first-lv, then fix/finish
  (let* ((c1 (first-lv (class_of i1)))
	 (c2 (first-lv (class_of i2))))
    (if (eq c1 c2) 0 ;if eq, then go to the scores ;0 will signal this
      (let ((prl (gv-cls c1 "preferred")))
	(if *dbg* (format t "~&~a>~a,~a" c1 c2 prl)) ;rest dbg from a trace
	;do the pref replace, well ordering, by t/f if ordered this way
	(not (member c2 prl))))))
(defun tui3> (i_1 i_2)
  (let* ((i1 (first i_1))
	 (i2 (first i_2))
	 (pred (tui> i1 i2)))
    (if (neq pred 0) pred ;otherwise fall back to using scores 
      (let* ((cs1 (last_lv i_1))   ;using last which is altered=orig+delta
	     (cs2 (last_lv i_2)))   ;while was picking cui from max of bag of deltas
	(> cs1 cs2)))))			;still am  ;so far
(defun sort_can (atm)
  (let ((cans (mm_canr atm)))
    ;now sort them w/context of the atom
    (sort cans #'tui3>)))

;get rid of phr-passert below; only need to set cls once, then can just use id/sn/val
;mkm.cl  ;might not use
;-parse mmtx-or-metamap output  in mmtx3.cl now, this if from that, for the KM interaction
; only problem is that parsing&constructing objs is closely knit
(defvar *cid* nil) ;instead of num, set current-id
(defvar *tst2* nil)
;try going overboard in removing parsing fnc, just to look at km interation for a bit
;=in cui.lisp now: ;put back:
(defun cui-str (cui cls wrd pf-slt tc0 &optional (strm nil)) ;keep for sexpr only now w/assert-cui-
  "km cui instance"   ;only-use-for-str
     ;check that cuis.km is ok now  ;fix so not dumping to file from this w/o change /go back2old
   (if (full tc0)  
    (format strm "~&(*~a has (instance-of ~a) (txt (\"~a\")) (tc (\"~a\")) ~a)" 
	    cui cls wrd tc0 pf-slt)
    (format strm "~&(*~a has (instance-of ~a) (txt (\"~a\")) ~a)" cui cls wrd pf-slt))) 
;probably easier/well cleaner to get cui, and then just see which other vars aren't nil &set them
(defun assert-cui- (cui- cls wrd root tc0)
  "assert a cui" ;cui-str and ka of it
  (let ((cui (ki (c-ok cui-))))
    (sv-cls cui cls)
    ;(sv-cls cui "cui") ;new ;no ;try again4 mnp2km run ;or could do it on words below
    (sv cui "txt" wrd t)
    (svif cui "tc" tc0 t)
    (mnp2km wrd) ;try
    ;(mnp-parse2km wrd) ;try
    (svif cui "pref-txt" root t)))

(defun kmcui-cache-p (cui)  ;use more generic in-kb-p
  (gv (ki cui) "txt"))
;;-
;(defun atom-p (i) (is_a-p  i '|atomPhrase|)) ;in utkm is much better now o26
;defun atom-p (id)  ;want to get sPhr of only these
(defun atomnum-p (id)  ;want to get sPhr of only these
  "input phr id not_a_set_of_words"
  (digit_prefixp (rm-star (km-name (ki id)))))
(defun atom_p (id) ;only split out sPhr/frag's if >1 of them
  (when (and (atom-p id) (> (frag_num id) 1))
    (frag-num2 id)))
;-
;defun phr-id-str (id-)  ;vs. ki id "phr"  ;but can tell between 1A & txt ;someday phr only for1A etc
;just in case have something to turn 1A's into the related txt, and leave txt alone
;-
(defun idn (id n)
  "make id.n" ;could call ki
  (str-cat id "." n))
(defun idn2 (id n)
  (if n (idn id n)
    id))
;-
;defun id-str (id) ;might use this over phr-id* soon
;defun phr-id (id) 
; km accessors /for phrase, then..
(defun phr-lw (id)  (gv+ id "longestWords"))
(defun phr-mm- (id)  (gv+2 id "mm-mappings"))
(defun phr-mm (id) 
  (let ((mm (phr-mm- id)))
    (when mm (sort (mapcar #'rest mm) #'> :key #'last_lv))))
(defun phr_mm- (id)  (gv+2 id "mm_mappings"))
(defun phr_mm (id) 
  (let ((mm (phr_mm- id)))
    (when mm (sort (mapcar #'rest mm) #'> :key #'last_lv))))
;(mm-mappings has (instance-of (Slot)));final mapping tuples&it's score
;(mm_mappings has (instance-of (Slot)));~can of map, subset of can
;(mm_candidates has (instance-of (Slot)));cui of interest;bitMoIn ner
;=contin moving to phr.lisp
(defun mm_can (i) (gv+2 i "mm_candidates"))
(defun mm_can- (i) (mapcar- #'rest-lv (mm_can i))) ;new
(defun mm_canr (i) (reverse (mm_can- i))) ;makes copy
(defun mm_cans1 (i) "by orig score" (sort (mm_canr i) #'> :key #'second))
(defun mm_cans2 (i) "by ajusted score" (sort (mm_canr i) #'> :key #'third))
(defun mm_mapr (i) (reverse (phr-mm i))) ;makes copy
(defun mm_map1 (i) "by orig score" (sort (mm_mapr i) #'> :key #'second))
;-n2/acessor replacements:
(defgeneric mm-candidates (a))
(defgeneric mm-mappings (a))
(defmethod mm-candidates (i) (mm_cans1 i))
(defmethod mm-mappings (i) (mm_map1 i))
(defun mm-can-alsts (i) (mm-mappings i)) ;try
(defun first-map-pr (i) (first-lv (mm_map1 i))) ;n2 replacement
(defun first-map (i) (first-lv (first-map-pr i))) ;n2 replacement
(defun first-map-sc (i) (second-lv (first-map-pr i))) ;n2 replacement
(defun nth-cui (n i) (nth-lv n (mm_cans2 i))) ;replace n2 version
(defun nth-cui-sc (n i) ;replace n2 version, but might skip
  (let* ((coa (nth-cui n i))
	 (orig (second-lv coa))
	 (after (third-lv coa))
	 (delta (- after orig)))
    (format nil "(+ ~a ~a)" orig delta)))
(defun first-cui (i) (first-lv (mm_cans2 i))) ;replace n2 version
;-
(defun mm_can2 (i) (mapcar- #'second-lv (mm_can i)))
(defun phr_mc- (id) 
  (let ((mm (mm_can id)))
    (when mm (sort (mapcar #'rest (list+2 mm)) #'> :key #'second)))) ;is this safe?, use mm_canr
(defun phr_mc (id) 
  (let ((mm (mm_canr id)))
    (when mm (sort (list+2 mm) #'> :key #'second)))) ;is this safe?, use mm_canr
(defun phr_mc1 (id) 
  (let ((mc (phr_mc id)))
    (when mc (mapcar #'first mc))))
;-o5 version giving a show: o22_2- so:
(defun frag-num2 (id)
  (let ((pl (phr-phr id)))  ;can't do this,calling itself FIX ;was phr_phr, - vers ok
    (if (fulll pl) (len pl)
      0))) ;if main, then don't add .0 to end of id
;-
(defun phr-phr (id)  (gv id "phrases"))
(defun phr_phr (id) (if (atom_p id) (phr-phr id)
		      (ki id))) ;non atom id's are the phrase
(defun phr_types (id)
  "_PHRASE s"
   (type_of+ (phr_phr id)))
(defun has-np (id) 
 ;(member 'NOUN_PHRASE (list+ (type_of (phr_phr id))))
  (positions 'NOUN_PHRASE (phr_types id)))  ;position would give which sPhr
(defun np (id) ;w/position/s could get idn
  "sPhr that is a NP"
  (let ((n (has-np id)))
    (when n 
      (if (atom_p id) (mapcar #'(lambda (n-) (idn id n-)) n);(idn id n)
	id))))
(defun id2np-s (id)
  "like id2nps"
  (let ((np (np id)))
    (when np (list+ (ktxt np))))) ;ktxt's gv will work on lists as well
;
(defun show-phrs (id)
  "sPhrs of atom/phr"
  (show (phr_phr id))) 
(defun show_phrs (id)
  "show main&then sPhrs"
  (show id) (show-phrs id))
(defun words (id) (gv id "words")) ;txt words, which are used to find:    ;in main phr
(defun words- (i) (rest-lv (words i)))
;(defun word-s (id) (gv1 id "word-s")) ;txt words, which are used to find:    ;in sPhr
(defun word-s (id) (gv1 (phr_phr id) "word-s")) ;txt words, which are used to find:    ;in sPhr
(defun word-s-of (id) (gv1 id "word-s-of")) ; can go from txt2atom
(defun my-atom (i) 
  (if (atom-p i) i
    (first-lv (collect-if #'atom-p (word-s-of i)))))
;need to fix both my*atom
(defun my_atom (i) 
  "parent/full atom only." ;check
  (if (atom_p i) i
    (first-lv (collect-if #'atom_p (word-s-of i)))))
;I would like to pull more from the stanfordParser soon; or columbia's or PARC's ..
(defun replace-dot (str)
  (simple-replace-string "(. " "(#\\. " str))
(defun prs (id) 
  (gv id "prs"))
(defun prs2 (id)  ;should make this str safer, eg single char's to #\. ..
  (eval-str2 (replace-dot (prs id))))
(defun prs2pos (id) 
  (mapcar #'cdr (prs2 id)))
(defun my-prs (i) (prs (my-atom i)))
(defun words-of (id) (gv id "words-of")) ;cuis of the words
(defun words_of (id) 
  "sets of refs of *words in an id"
  (let ((wrds (words id)))
    (when (fulll wrds) ;might want a full-seq ?
      (mapcar #'words-of (rest wrds)))))
(defun words_of1 (id) 
  "everything that refs any *word w/in id"
  (let ((wo (words_of id)))
    (when (fulll wo) (remove-duplicates (flat1 wo)))))
;-
(defun cui-can-refs (id &optional (strm nil) (cuitxt nil))
  "distrib of cui candidates also refd in cui ins"
  (let* ((mc (phr_mc1 id))
	 (wo (words_of1 id))
	 (in (intersection mc wo))
	 (sd2 (set-difference  mc wo))
	 (sd1 (set-difference  wo mc))
	 (tx3 (format nil "~&~a,~a,~a" sd1 in sd2)))
    (if cuitxt (progn 
		 (mapcar #'print2pref-txt sd1) (princ " int:")
		 (mapcar #'print2pref-txt in) (princ " sd2:")
		 (mapcar #'print2pref-txt sd2) tx3)
    (format strm "~a" tx3) ;(print tx3 strm) ;(format strm "~&~a,~a,~a" sd1 in sd2)
    )))
;-
(defun phrases-of (id) (first-lv (gv id "phrases-of")))
(defun sphr-p (id)
  "sPhr predicate"
  (atom-p (phrases-of id)))
(defun atom-p- (id)
  "only main atom, no sPhrs"
  (and (atom-p id) (not (sphr-p id))))
(defgeneric frag_num (id))
(defmethod frag_num (id)
  "find#frags MMTx parsed atom into, if sPhr/fragment, ret that #"
  (if (atom-p- id) (frag-num (km-name id)) ;frag-num2 but can't depend on this to set if needed4it
    (when (position #\. id) (numstr (first-lv (last (explode- id #\.)))))))
(defmethod frag_num ((id symbol))
  (frag_num (symbol-name id)))
;-
(defun pref-txt (i) (gv i "pref-txt"))
(defun kmtxt (i) (gv i "txt")) ;2diff from txt method
(defun ktxt (i) (let ((txt (kmtxt i))) (if txt txt (to-str i))))
(defun ktxt+ (i) (gv+ i "txt")) ;2diff from txt method
;(defun using-txt-p (i) (equal (ktxt+ i) (ktxt+ (bnp i)))) ;in mnp2 w/bnp
(defun print2pref-txt (i)
  ;(format t "~&~a,~a,~a" (ktxt i) (pref-txt i) (type_of i))
  (let ((txt (ktxt i))
	(pt (pref-txt i))
	(cls (type_of i)))
    (if pt (format t "~&~a:~a,~a,~a" i txt pt cls)
      (format t "~&~a:~a,~a" i txt cls))
  txt))
;-
(defun get-sentence (id &optional (inclID t))
  (let* ((wrds (words id))  ;phr_phr above 2?
	 (wrd-s (word-s id))
	 (sent (make-sentence (or wrds wrd-s))) )
    (if inclID (format nil "~&~a:~a" (my-atom id) sent)
      (format nil "~&~a"  sent)))) 
;-look@my-atom, then use get-sentence below
(defun id-sent (id &optional (cuitxt nil)) 
  "show some pos/cncpt annotations for atom"
 ;when (not (equal id '#$:seq)) ;better to never send it
 (when (not (km-seqp+ id)) ;better to never send it
  (if (crit-p id) (mapcar- #'(lambda (i) (id-sent i cuitxt)) (gv id "fullAtoms")))
  (let* (;(wrds (gv id "words"))  ;words
	 (wrds (words id))  ;phr_phr above 2?
	 (wrd-s (word-s id))
	 (sent (make-sentence (or wrds wrd-s)))  ;was just wrds
	 (phr (phr_phr id))
	 (ty (type_of phr)) ;was of id
	 (pos (prs2pos id)) ;was of phr
	 (wo1 (words_of1 id))
	 ;(wo1 (remove-duplicates (flat1 wo)))
	 )
    ;get cui refs in too ;might have to look at words for cui refs
    ;(format t "~&~a~&~a~&~a" sent ty pos wo1) ;ty sent pos wo1
    ;(format t "~&~a~&~a~&~a~&~a" ty wrd-s sent pos wo1) 
    (if (not cuitxt) (progn
	    ;(format t "~&~a:~a~&~a~&~a~&~a~&~a" (my-atom id) sent wrd-s ty pos wo1)
	    (format t "~&~a:~a~&~a~&~a~&~a~&" (my-atom id) sent wrd-s ty pos)
		    ;(cui-can-refs id t)
		    (format t "~&~a" (cui-can-refs id))
		    ) 
	    (progn
		    (format t "~&~a:~a~&~a~&~a~&~a" (my-atom id) sent wrd-s ty pos) 
		    (cui-can-refs id nil cuitxt) ;(mapcar #'print2pref-txt wo1)
		    ))
    sent)))
(defun id-sent2 (id &optional (cuitxt nil)) 
  "show some pos/cncpt annotations for atom's sPhr/fragments"
  (mapcar #'(lambda (i) (id-sent (km-name2 (first-lv i)) cuitxt)) (list+ (phr_phr id))))
(defun id-sent_2 (id &optional (cuitxt nil)) 
  "main & sPhrs"
  (id-sent id cuitxt)
  (when (atom_p id) (id-sent2 id cuitxt)))
(defun id_sent (id) 
  (let ((phri (phr_phr id)))
    (if (fulll phri) (mapcar #'id_sent phri) 
      (let* (;(wrds (words id))
	     (wrds (word-s id))
	     (phr (make-phrase wrds))
	     (ty (type_of phri))
	     (wo1 (words_of1 id))
	     ;(wo1 (remove-duplicates (flat1 wo)))
	     )
	(format t "~&~a ~a ~a" phr ty wo1)))))
;- 
(defun phr-word-s (id)  (gv1 id "word-s"))
(defun phr-pi (id)  (gv id "parserInput")) ;cmp w/phr-txt 2see if id2prs=phr-nlp3
(defun phr-prs (id) (gv id "prs")) ;phr3.km has mmtx prs
(defun phr-nlp3 (id) (gv id "nlp3")) ;phr2.km has new med-vocab extNLP
(defun phr_nlp3 (id) (eval-str2 (first-lv (phr-nlp3 id))))
(defun phr-nlp4 (id) (gv id "nlp4")) ;phr4.km has new med-vocab extNLP
(defun phr-hyphen-txt (id &optional (mrgfnc #'find-merge-hyphens)) ;was find+merge-hyphens
  "create parserInput by hyphenating longestWords"
  (let* ((p (if (stringp id) (phr-id id) id))
	 (txt (string-downcase (first-lv (phr-txt p))))
	 (lw (phr-lw p)) ;should dwncase here
	 (lwl (mapcar #'string-downcase lw))
	 )
    ;(remove-duplicates (simple-hyphenate-strs lw txt)) ;close, but
    (if (full lw) (funcall mrgfnc lwl txt) 
      txt)
    ))
(defun bnp-hyphen-txt (id &optional (mrgfnc #'find-merge-hyphens)) ;was find+merge-hyphens
  "like pi but for best-np"
  (let ((txt (first-lv (ktxt+ (bnp id))))
	(lwl (mapcar #'string-downcase (phr-lw id))))
    (if (full lwl) (under_ (funcall mrgfnc lwl txt))
      txt)))
;- assert mmtx output in a usable way, throughout the atom/sPhrs phrases ;not sure NP's yet
;-
(defun mapal2kmarg-s (lm)
  "cui+scores minus tatal for the mm-mapping"
  (let ((sl (second-lv lm)))
    (mapcar #'(lambda (pr) (list (cdr pr) (car pr))) sl)))
 ;could of used :pair, but ok as is
(defun mapal2kmargs (lm)
  "a mm-mapping reorg w/score at end"
  (let* ((fs (first-lv lm))
	(sl (second-lv lm))
       ;(sl2 (list (mapcar #'(lambda (pr) (cons (cdr pr) (car pr))) sl) fs))
	(sl2 (list (mapcar #'cdr sl) fs)) ;cui's&main score2mm_mappings,  cui&localScore2mm-mapping
	)
    (flat1 sl2)))
(defun cui-map-args (id &optional (n nil))  ;not easy2generalize as sexpr so different ;assert
  "*cui&score candidates" ;args *cui score, also have for all/somehow
  (let* ((mmpl (mm-mappings id))
    	 (mmp (if n (elt mmpl n)
		(flat1 mmpl) ;mmpl
		))
	 (id- (if n  (idn id n) 
	       id))
	 (rng (range_ (len mmp))))
     (mapcar #'(lambda (i) (sva id- "mm-mappings" (mapal2kmargs (elt mmp i)))) rng)
     (mapcar 
      ;;;#'(lambda (i) (sv id- "mm_mappings" (elt mmp i) t))
      ;;#'(lambda (i) (sva id- "mm_mappings" (mapal2kmargs (elt mmp i))))
      ;#'(lambda (i) (mapcar #'(lambda (i2) (sva id- "mm_mappings" i2)) (mapal2kmarg-s (elt mmp i))))
       #'(lambda (i) (mapcar #'(lambda (i2) (sva id- "mm_mappings" i2)) (mapal2kmarg-s (elt mmp i))))
	     rng)
     ))
(defun phr-assert-mappings-2 (id)  ;main&sPhr mappings
  "main&sPhr mappings"
  (cui-map-args id)
  (let ((frag-n (atom_p id)))
    (when frag-n (mapcar #'(lambda (n) (cui-map-args id n)) (range_ frag-n)))))
;-
(defun cui-can-args (id &optional (n nil)) 
  "*cui&score candidates" ;args *cui pre post, can check w/dScore from cui's tui
  (let ((mmc (if n ;(or n (atom_p id)) 
	       (elt (mm-can-alst-s id) n)
	       (mm-can-alsts id)))
	(id- (if n  (idn id n) 
	       id)))
    (mapcar #'(lambda (pr) (sva id- "mm_candidates" 
				(list (string-upcase (alst_cui pr))  ;cui, rest is pre/post scores
					  (alst2can-pre-score pr) (alst2can-post-score pr)))) mmc)))
(defun phr-assert-candidates-2 (id)  ;main&sPhr candidates
  "assert cui&score in main&if atom_p then those too"
  ;(phr-assert-candidates2 id) ;to fill mm-candidates before filling mm_candidates below
  (cui-can-args id)
  (let ((frag-n (atom_p id)))
    (when frag-n (mapcar #'(lambda (n) (cui-can-args id n)) (range_ frag-n)))))
;-
(defun phr-assert-mmtx-2 (id)
  "assert all can/map cui w/scores"
  (phr-assert-mappings-2 id)
  (phr-assert-candidates-2 id))
;-
 ;would like to mv towards having min/max as aggregate slots; that are done automatically,&just qry
;want2get min delta from tui's as well, so can use it to get min-delta, fix/finish this.*
(defun phr-assert-deltas (id) ;fix by getting cui's &calling cui-dscore &taking min for min-delta *
  (let ((scores (rscl-can-score  id :retfnc #'nop)))
   (when (full scores) 
    (sv id '|max-delta| (apply #'max scores) nil)
    (sv id '|min-delta| (min-delta-can-score id) ;(apply #'min scores) 
		    nil))))
(defun phr-min (id) (gv id "min-delta"))
(defun phr-min2lowp (id) 
    (let ((mn (phr-min id))) 
          (and (full mn) (< (first-lv mn) -100)) ))
;remember, do not need to reassert slots/cls to set other slots
;defun sv-al (i al)   ;SetValue s from alist
;-this one looks like it sets the cls of Phrase o26, if atom-p set atomPhrase/no
(defun phr-passert-sn (ida sn &optional (qtval t)) ;print now, assert soon?
   (sv_ida ida sn qtval))
(defun phr-passert-pi (ida)  (sv_ida ida "parserInput")) ;should do this everytime/not phr2.km
(defun phr-set-pi (id)
  "set parserinput"
 (let ((prs-in (phr-hyphen-txt id)))
 ;(sv (phr-id-str id) "parserInput" prs-in) ;need to quote this
  (sv (phr-id_str id) "parserInput" prs-in t)
  ))
;(defun phr-passert-nlp3 (ida)  (phr-passert-sn ida "nlp3"))
(defun phr-passert-nlp3 (ida)  (sv_ida ida "nlp3"))
;keep prs as str for now
;(defun phr-passert-prs (ida)  (phr-passert-sn ida "prs"))
(defun phr-passert-prs (ida)  (sv_ida ida "prs"))
(defun phr-set-prs (id) (sv (phr-id_str id) "prs" (to-str (l1_prs id)))) ;in ld3 ;get rid of
(defun phr-set-prs2 (id prs) (sv (phr-id_str id) "prs" (to-str prs))) ;in ld3
(defun phr-set-nlp4 (id) (sv (phr-id_str id) "nlp4" (to-str (id2nlp4a id))))
(defun phr-init-after (id)
  "just like an init after for phr ins"
  (phr-set-nlp4 id)
  (phr-set-prs id) ;
  (phr-set-pi id)
  )
(defun phr-p-nlps (pi-a nlp-a)   ;not sure of use, getting nlp cached &will use that.........
  "2id alsts and get phr print/asserts for each"
  (mapcar #'(lambda (pia nla) (phr-passert-pi pia) (phr-passert-nlp3 nla)) pi-a nlp-a))

;=in cui/cache/e2old

;could be used for tui-dscore, as well if works in km ..
;is done w/o any instances w/even more terse: (the max of (the dScore of (a ?tui)))
;  (ka- "the max of (the dScore of (a " "Finding" "))")
(defun tui-dscore (cls)  ;ret val vs lst now
  (let ((c (if (symbolp cls) (symbol-name cls)
	                  (to-str cls)))) 
    ;(ka- "the max of (the dScore of (a " c "))")
    (gvs c "dScore")
    ))
;-
 ;use below w/str:  ;presently unused, would have to take max of present one
(defun tui_dscore (s) 
  "a tui as str gives all dScores"
  (tui-dscore (intern (format nil "~a" s))))

;should call &have arg determine which one
;(defun tui-d-gt (t1 t2) (> (first-nn (tui-dscore t1)) (first-nn (tui-dscore t2))))
(defun tui-d-gt (t1 t2) (> (first-nn (tui_dscore t1)) (first-nn (tui_dscore t2))))

;(defun v-p (s) (equal s "V"))
(defun small-str-p (s) (< (len s) 3))

;too much parsing from cui str, when now have cui-ins w/this info, so clean that up
; tui*dScore should be much easier too
;
; should be able2clean-up/simplify this:

(defun tuis_dscore (sl &optional (fnc #'max-fl))  ;this gets the max, but sometimes want the min
  "list of tui-names as strs, get max dScore"
  (let* ((sl1 (mapcar #'alph_char-only sl)) ;new, clean up strs
	 (sl2 (collect-if #'full sl1))
	 ;(sl3 (remove-if #'v-p sl2))
	 (sl3 (remove-if #'small-str-p sl2))
	 (tscs1 (mapcar #'tui_dscore sl3)) ;was tscs
	 ;(tscs1 (reduce #'append tscs))
	 (sc  (when (full tscs1) ;or mapcar-
	   (funcall fnc tscs1) ;(mapcar fnc tscs1) 
	   )) ) ;I should have KM always calc this
    (when *dbg* (format t "~a" tscs1))
    sc))
;(trace cui-dscore)
;  -look into taking km cui str to tui, so don't need it asserted; or assert right away
(defun rescore-pair (pr) 
  (let* (;(sc (parse-integer (first pr)))
	 (sc (first pr))
	 (cs (cdr pr))
	   ;try to go from cs 2 tui, then do a tui-dscore
	   (tuis (cuistr2tui- cs))  ;try o30
	   (dsl2 (tuis_dscore tuis))
	 (cui (cuistr2cui cs)) ;* version dangerous
	 ;(dsl1 (cui-dscore cui)) ;(dsl (or dsl2 dsl1))
	 (dsl (or dsl2 (cui-dscore cui))) ;try not to call both if can help it
	 (ds (first-nn dsl) ;(if (fulll dsl) (first dsl) 0)
	     ))
    (when *dbg* (format t "~&cui=~a,ds=~a,new=~a" cui ds (+ sc ds)))
    (if (< -9 ds 9)
      (cons (+ sc ds) cs) ;ret another pair w/updated score
      (cons (format nil "(+ ~a ~a)" sc ds) cs) ;ret another pair w/updated score-str ;just TRY
    )))
;==> b2.cl <==might look at doing some of this in km;;now in: nlm/mprs.lisp
;in cleanup consider obj that would wrap/shadow just enough to make the lisp side much eaiser
; all the objs would also get general obj/id cleanup specialized by class ;eg. class-slot for prefix
; Could even start say phr obj w/txt, then fill in as you go  ;consider clips/protege/alg interaction
;Could even give class/super &slots &get-/put- sn methods defn on the clos id/wrapper class that would
; just do the eval-str2km w/proper assoc id; the clos ins should also be easy to find/in cls-ht?
; then could be easily also used in lisa/ might save this for later, as some simple generic fncs 
; might let me mostly depend on km  ;note that cell could also be used if wanted2play a bit more..
;;Could easily have defmethods that like ones above that goto alist, just go to km instead
;; then it would always mainly be stored in an aread that wore easily allowed for reasoning
;;Can check to not re-assert cui's etc, but it already hadles this well; so can skip for a start
;was-eof
			;soon regularize/generalize &be able to assert right away 
;atomPhrase
;-this one looks like it sets the cls of Phrase o26, if atom-p set atomPhrase
(defun id2cls (id) ;was atom-p but not this goes offTheNew: atomPhrase cls
  (if (atomnum-p id)  "atomPhrase" "Phrase")) 

;moved2 utkm, in svs ;(defgeneric words-seq (w))
;(defmethod words-seq ((txt String)) (words-seq (explode- txt)))
;(defmethod words-seq ((wl List)) (ids2km-seq wl))
;
(defun phr-words-str (id str)  ;svs id  "words" str 
  ;;(format nil "~&(*phr~a has (words (~a)))" id (words-seq str))
  (svs i "words" str)
  ;(format nil "~&(~a has (words (~a)))" (phr-id-str id) (words-seq str))
  )
(defun phr-words-str2 (id str)  ;phr-passert-sn should also be able to assert them
  "assert phr 2km"
   ;(ka (phr-txt-str id str)) ;in mmkm-old  ;get txt  &cls
   (sv-cls id (id2cls id)) ;set (init) cls 
   (sv id "txt" str t) ;or auto if no t
  ;(eval-str2km (phr-words-str id str)) ;get words
   (svs id "words" str))
;
;now want words of the cui, so these 'lui'/words can link back to bot pos/_phrase &cui/tui
; might make one that takes a cui, get's txt, and asserts words, as ~of a :after initialize method
  ;_maybe find gen cui version of phr-passert-sn &use that_finish**** ;get-cui-slot c 'txt
;  cui-txt cui-pref-txt, then assert w/same cui id ; ; -use both txt&pref-txt..
 ;keep each word together, but assert each that are unified /check original cui getting all
(defun cui-words (cui wrd) ;called near/after cui-str
  "save words of txt in cui words slot, in a :seq" ;keep order but loose backlink
  (sv cui "words" (words-seq wrd) nil))
(defun cui_words (cui wrd) ;called near/after cui-str
  "save words of txt in cui words slot"
  (mapcar #'(lambda (w) (sv cui "words" w)) (mapcar #'ki (explode- (rm_comma wrd)))))
;
(defun assert-le (n cls txt from to)
  "assert lexElt"
  (let ((w-id (k_i txt)))
    (sv-cls w-id cls) ;want le cls of the word
   ;(sva w-id "le_of" (list (ki id-) n from to))
    w-id)) 

;subphr-str&le-str in mmkm-old.l
(defun assert-le- (id- n cls txt from to)
  "assert lexElt"
  (let ((w-id (k_i txt)))
    (sv-cls w-id cls) ;want le cls of the word
    (sva w-id "le_of" (list (ki id-) n from to))
    w-id)) ;got dups in ret b4/so try w-id here2

;(defun le_of (i) (gv+ i "le_of"))
(defun le_of (i) (gva i "le_of"))

(defun phr-sPhrs (id n)
  "add id.n to id"
 (let ((id- (ki (idn id n)))) ; (str-cat id "." n));ok if ret val/ins
  (sv id "phrases" id- 'auto t)
  id-)) ;so ret new id here
;defun sPhr-str (id n cls txt from to)  ;now just add words...  txt2words
;n3:could sv-cls id cls, sv "txt" txt, sva "word-s" (list (ki id-) n from to)
(defun assert_sPhr (n cls txt from to)  ;close but still get err
 (let* ((id (under_ txt)) ;try
	(id- (if (atom_p id) (phr-sPhrs id n)   ;not as clear as how to do, actually check if id ok..
	      (ki id))))
   (sv-cls id- cls)
   (sv id- "txt" txt t)
   (sva id- "word-s" (list (k_i txt) n from to))
   (ki id-)))
;if atom/part is the mnp _phrase then ok, otherwise shouldn't fill slots via it's id
; so try above where only the *phrwordid gets the slots ;but from/to context, but it's from mnp call
;Check, if txt is mnp context for from/to then only use that, above.
(defun assert-sPhr (id n cls txt from to)  ;close but still get err
  "assert a subPhr" ;try soon
 (let ((id- (if (atom_p id) (phr-sPhrs id n) 
	      (ki id))))
   (sv-cls id- cls)
   (sv id- "txt" txt t)
  ;;(sva id- "word-s" (list (ki id-) n from to))
   ;(sva id- "word-s" (list (ki txt) n from to))
   (sva id- "word-s" (list (k_i txt) n from to))
   ;"" ;so ka still dwnstream won't eval
   (ki id-)))
;
;29.1.1	Representing N-ary Predicates  ;might keep  ;or phr holds word/phrase w/ins w/extra context?
;  put in :args w/km-args now, &should get into orginal *phr if  cid is ok  ;;sva
;-put back:
(defun show-phr2 (id) (show  (under_ (txt id))))
(defun show-phr (id) (showme (phr-id id)))
;-eof
