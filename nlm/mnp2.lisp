;I envision this to be for the NLP tagger parts where the med-vocab is injected via hyphenation
;--generalize some of this so can go over any collection/:seq of word/phrases
;  right now km cache can hold tree/list, but not reason directly on; so turn to :seq tree in km..
;--like mmt.lisps mmt-id
;defun nlp-id (id str) ;want to just have str, will fix in cache.lisp as well
(defun nlp_id (id) ;want to just have str, will fix in cache.lisp as well
;;(fnc-id_ "nlp2" #'nlp2 id str)  ;might use another nlp fnc
 ;(fnc_id "nlp2" id)  ;might use another nlp fnc ;oops nlp2 puts sp&berk together but cached sep
  (list
  (fnc_id2 "sp2" id)
  (fnc_id2 "berk2" id)
  ))
;--
; rest of this is in ../nlp  ;could get cache-ing of this coherent w/rest, but low on time
;==might call below here, mnp2 for now
;-=-=;relating to cdkm3 a26_3 rfind-jj modifier study, which i should automate a bit more
;=-=-= from s14.lisp
;-more nlp related: aug
;(defun find-np (l) (rfind 'NP l)) 
;(defun find-npl (l) (rfind 'NPL l)) 
(defun find-np (l) (rfind-all 'NP l)) 
(defun find-npl (l) (rfind-all 'NPL l)) 
;(defun symbol_name (s) (string-downcase (symbol-name s)))
;(defvar *pmod* '(JJ ADVP ADJP NNP))
;(defvar *pmod* '(JJ ADVP ADJP VBG NNP))
;(defvar *pmod* '(JJ ADVP ADJP VBG VB NNP))
;(defvar *pmod* '(JJ ADVP ADJP VBG VB VBD NNP))
;(defvar *pmod* '(JJ ADVP ADJP VBG VB VBD))sep24meeting
(defvar *pmod* '(JJ ADVP ADJP VBG VB VBD RB))
(defun non-pmod-words (l) ;not exactly, actually want to rm each, or get set-complement
  (when (full l) (set-difference (flatten l) *pmod*)))
;(defun find-jj (l) (rfind-all 'JJ l)) ;use to break tie, in that NP w/modifier prob more specific 
;(defun rfind- (s l) (when (full l) (rfind-all s  l)))
(defun rfind- (s l) (when (fulll l) (rfind-all s  l)))
(defun rfind-l (sl l) (remove-nils (mapcar #'(lambda (s) (rfind- s l)) sl)))
(defun rfind-pmod (l) (rfind-l *pmod* l))
;assume stanford/berkley parse from id2sb
(defun id2sb (id) (format t "prs accessor not loaded yet,~a" id))
(defun id2sb-mod (id &optional (lt3 t))
  (let ((mod-wrds (mapcar #'to_str (non-pmod-words (mapcar #'rfind-pmod (id2sb id))))))
    (if lt3 (remove-if #'str-lt3 mod-wrds) mod-wrds)))
;-was in ld3
(defvar *pmsa* nil)
;;(setf *pmsa* (alst-w (mapcar #'id2sb-mod *idnd*))) ;just do this part in ld3 now
(defun id2b_mod (id) (id2a-cdr id  *pmsa*))         ;not best mod, but will do for a bit
(defun id2b-mod (id) 
  "modifier guessed for id"
  (let ((mods (id2b_mod id)))
    (when mods (remove-duplicates mods :test #'equal))))
(defun no-hyphen-match (modf) (str-cat modf " ")) ;hack2keep from matching w/hyphen next2word
(defun mod1np (mods opnp &optional (no-hyphen-match t))
  (if (and opnp (full mods))
    (let* ((mods1 (first mods))  ;want to use more than 1st  ;should iterate over* below
	   (mod1 (if no-hyphen-match (no-hyphen-match mods1) mods1))
	   (moded (add-modtag mod1 opnp)))
      ;if (equal modded opnp) ;mod1opnp id  ;pull out fnc, &call w/next if don't get 1st
      moded)
    opnp))
; ;(add-modtag (first (id2b-mod id)) (id2b-opnp id))
(defun mod1opnp (id &optional (no-hyphen-match t)) 
  (let* ((opnp (string-downcase (id2b-opnp id)))
	 (mods (id2b-mod id))
	 (moded (mod1np mods opnp no-hyphen-match)))
    (if (equal moded opnp) (mod1np (rest mods) opnp) ;*
      moded)))
(defun mod1op-np (id) 
 (if (cmp_p id) (format nil "[cmp]:~a;~a" (id2_cmp id) (cmp_p id))
  (let ((modstr (mod1opnp id)))
    (if (and (full modstr) (not (suffixp "[modifier]" modstr))) (str-cat modstr " [np]")
      modstr))))
;more in ld3 that sets a bestNP slot, which should then be broken dwn into modifier&lil-np ins
;  where as far as i can tell, lil-np is everything that isn't the modifier
;(trace mod1opnp)
;(format t "~&bestNPs:~&~a" (mapcar #'mod1op-np *idnd*)) 
;-below not used as much now that words are ins of POS classes, so can just check if is-a ...
(defun find-jj (l) (rfind- 'JJ l))
(defun find-advp (l) (rfind- 'ADVP l))
(defun find-adjp (l) (rfind- 'ADJP l))
(defun find-nnp (l) (rfind- 'NNP l))
(defun pre-np (s) (sprefixp 'np s)) ;maybe
(defun find-np- (l) (append (find-np l) (find-npl l))) ;maybe
(defun find-np_ (l) (let ((lwh (find-np- l))) (when (full lwh) (mapcar #'rest lwh))))
(defun fl (l) (frmt-l l))
;(defun str-lt3 (s) (< (len s) 3))
(defun str-lt3 (s) (< (len (to-str s)) 3)) ;use w/remove-if
(defun str-lt4 (s) (< (len (to-str s)) 4)) ;use w/remove-if
(defun str-gt3 (s) (> (len (to-str s)) 3)) ;maybe
(defun str-wo2 (s) "w/o 2char words"
  (implode-l (remove-if #'str-lt3 (explode- s))))
;-
;(defun rm_ (s) (butlast (explode- (to-str s) #\_)))
;(defvar *skip-pos* '("JJ"))
(defvar *skip-pos* '())
(defun rm_s (s &optional (fully-skip-l *skip-pos*)) 
  "rm last _bit, rm fully if in skip list"
  (let ((str (to-str s)))
    (if (position #\_ str)
      (let ((el (explode- str #\_)))
	(if (not (intersection (last-lv el) fully-skip-l :test #'equal)) (implode-l (butlast el))
	  ""))   ;member  last-lv would also work
       str))) ;try
;=-=-=  this was in o5, but probably belongs here too;=-=-=
;=-=-= ;-more nlp related: aug ;moved to mnp.lisp
(defun rm__ (s)
  "rm last _bit"
  (let ((str (to-str s)))
    (if (position #\_ str)
      (implode-l (butlast (explode- str #\_)))
      str)))
(defun coerce+ (obj typ)
  (if (equal typ 'symbol) (intern (to-str obj))
    (coerce obj typ)))
(defun rm_- (s)  ;usually w/sym, but..
  "rm last _bit"
  (let* ((str (to-str s))
	 (typ (type-of s)))
   (coerce+
    (if (position #\_ str)
      (underscore (implode-l (butlast (explode- str #\_))))
      str) typ)))
;(defun rm_ (s)  ;usually w/sym, but..
;  (intern (rm__ s))) 
(defun rm_ (s)  ;usually w/sym, but..
  (let ((r (rm_s s)))
    (when (full r) (intern r)))) 
(trace rm_) ;need to rm _jj's 1st soon too
(defun rm_s-nil (s) (rm-str "nil " s))
(defun rm-eb (s) (rm-str "]" s))
(defun id2opprs (id)
  "open-nlp parse"
  (get1+ :op (id2nlp4 id)))
(defun id2sb (id)
  "stanford/berkeley-nlp parse"
  (get1+ :sb (id2nlp4 id)))
(defun id2opprs- (id)
  "open-nlp parse w/hyphenated med-vocab"
  (first-lv (phr_nlp3 id)))
;defmethod opprs ((id STRING))
;;start to pull out _jj &if not that, other potential modifiers ;just get from bestNP ret parse
;do a posfixp _NNS filter, on terms in the NP (beyond sub NPs)
;(defun list+ (ml) (if (listp ml) ml (list ml))) in utils now
(defun id2opnp (id &optional (rm-small nil)) ;was t  ;ld3 has in (b)op  ;cache in objs
  "get open-nlp NPs"
  (let* ((nlp (id2opprs- id))
	 ;(nps (remove-nils (mapcar #'find-np- nlp))) 
	 (nps (remove-nils (find-np_ nlp))) 
	 (np-lol (remove-nils  ;make sure don't rm JJ before cui-annotation.. ;might be ok?
		   (mapcar #'(lambda (l) (remove-nils (mapcar #'rm_- (list+ l)))) nps)))
	 ;somewhere in here km phr ins made, but w/errs, eg. 'np ' ;was rm_ try other but no change yet
	 ;a call to mmtx_pair has it it in the call
	(np-nsd (remove-duplicates 
	     (if rm-small (mapcar #'(lambda (l) (remove-if #'str-lt3 l)) np-lol)
	       np-lol) :test #'equal)))
    (when *dbg* 
      (format t "~&~a,~&~a;" nps np-lol)
      )
 (let ((ret
    (mapcar #'implode_l 
	    (sort (copy-seq np-nsd) #'len>) ;but also fix/rm4now the bad lists
	 )))
   (if ret ret
     (list (txt2 id))))))
;break this into 2parts, one that gets the NP txt sets (w/id); the last that.. or just use above4that
;
;>1 valid erg annotation, though pref maximally specific; otherwise partial;Might still get w/txtMatch
	
;still work on this one sometime, but need2focus on opprs 1st, to be closer2old work 1st
(defun id2np- (id &optional (rm-small nil)) ;was t  ;maybe/not used
 (let* ((nlp4 (id2nlp4 id))  ;instead of 2searchs, could use pre-np
	(nps (remove-nils (mapcar #'find-np- nlp4)))
	(np- (mapcar #'rest (flat1 nps))) ;close but need2 rm_
	(np-lol 
		     (mapcar #'(lambda (l) (mapcar #'rm_ (flat1onlys l))) np-))
	(np-nsd (remove-duplicates 
	     (if rm-small (mapcar #'(lambda (l) (remove-if #'str-lt4 l)) np-lol)
	       np-lol)
		     :test #'equal)))
   (when *dbg* (format t "~&~a,~&~a;" nlp4 np-nsd)) ;np-lol
   (sort (copy-seq np-nsd) #'len>) ;but also fix/rm4now the bad lists
   )) ;could flat1onlys but keep ret a lol
;might add option to (sort * #'len>) if it's useful
(defun id2np-l (id)
  "get NP(L) from nlp parsers"
 (let* ((nlp4 (id2nlp4 id))  ;instead of 2searchs, could use pre-np
	(nps (remove-nils (mapcar #'find-np nlp4)))
	(npls (remove-nils (mapcar #'find-npl nlp4)))
	(np (last-lv (if (len> nps npls) nps npls)))) ;more parses of NPs than NPLs or vs.
   (if *dbg* ;use last (so don't have to parse word_nn etc right now
      ;(format t "~&gt-of:~a ~&~a=~&~a" nps npls np) 
      (format t "~&len>~a;~&~a,~&~a;" np nps npls) 
      )
      (flat1onlys
	;(last-lv (flat1 np))
	(rest-lv (flat1onlys np))
	)))
(defun id2np_l (id)
  "get NP(L) from nlp parsers, in under_ id form"
  (let* ((np-l (id2np-l id))
	 (words (if (listp np-l) (mapcar #'last-lv np-l) 
		  (butlast (explode- (to-str np-l) #\_)))) ;opprs
	 (wordsonly (flatten words)))
   ;(under_  ;do this externally, so can still be compared/re-used w/id2bestNP ;oh it can handle>1*fin
      ;(apply #'implode- wordsonly)
      ;(apply #'implode- (mapcar #'to-str+ wordsonly)) same as:
      ;(implode-l wordsonly)
      (implode-l (remove-if #'str-lt3 wordsonly)) ;stopped rm small above
      ));)

;;(defun flat1onlys (l) (if (and (listp l) (eq (len l) 1)) (flat1onlys (first-lv l)) l)) ;!4 '(thing)
;(defun flat1onlys (l) ;in ut now
;  (if (and (listp l) (eq (len l) 1) (listp (first-lv l))) (flat1onlys (first-lv l)) l))
;maybe a version that 
;-
;=-=-=
(defun add-modtag (mod2tag instr &optional (badNPguess nil))
  ;loops;(simple-replace-string mod2tag (str-cat mod2tag " [modifier]") instr)
  ;works:(replace-substrings instr mod2tag (str-cat ", " mod2tag " [modifier]"))
  (let ((moded (trim-commas 
		 (replace-substrings (rm-commas instr) mod2tag (str-cat ", " mod2tag " [modifier]")))))
    (if badNPguess (replace-substrings moded ", " "[np]") ;maybe do not do this
      moded)
  ))
;=-=-= now this is more obj based ;=-=-= from ld3:

;new/back again:   ;bnp in mnp2? cmp* in ner?
;(format t "~&bestNPs:mod1op-np:~&~a" (mapcar #'(lambda (id) (cons id (mod1op-np id))) *idnd*))
(defun mn2km (id) ;might put in mnp2
  "mod+np 2km modnp slot"
  (let ((mn (mod1op-np id)))
    (when (prefixp "[cmp]" mn) (sv id "instance-of" "numericComparator")) ;only1so far ;sv-cls
    (sv id "modnp" mn)
    (cons id mn)))
(defun id2bestNPkm (id)
  "bestNP slot in km"
 ;(sv-from id "bestNP" #'id2bestNP)
  (sv-from id "bestNP" #'(lambda (i) (ki_ (id2bestNP i)))))
(defun bnp (i) (gv i "bestNP"))
(defun bnp2 (i) (if (cmp-p i) "cmp"
		  (bnp i)))
(defun using-txt-p (i) (equal (ktxt+ i) (ktxt+ (bnp i))))
(defun bnpw (i) (words- (bnp i)))
(defun bnpw2 (i) 
  "bnp words that keep hyphenated together"
 ;(mapcar #'hyphen2under (explode- (phr-pi (bnp i))))
 ;(mapcar #'hyphen2under (explode- (phr-pi i)))
  (mapcar #'hyphen2under (explode- (bnp-hyphen-txt i)))
  )
;should interleave modifier&not, but might just output each right now 
(defun bnpm (i) (let ((bnpw (bnpw2 i))) (when bnpw (collect-if #'modifier-p bnpw))))
(defun bnp-m (i) (let ((bnpw (bnpw2 i))) (when bnpw (collect-if-not #'modifier-p bnpw))))
(defun pos (b l) 
  (if b (mapcar #'txt_type l) l))
(defun pos1 (l) 
  "print: word _ pos"
  (if l (mapcar #'txt_type (list+ l)) l))
(defun bnp_ (i &optional (pos t))
  (let ((bnpw (bnpw2 i)))
    (when bnpw
     ;(format nil "~&mod:~a,~a" (pos pos (bnpm i)) (pos pos (bnp-m i)))
      (format nil "~&mod:~a,~a" (pos pos (collect-if #'modifier-p bnpw)) 
	                        (pos pos (collect-if-not #'modifier-p bnpw))))))
(defun bnp- (i &optional (pos t))
  "bestNP mod,lilnp"
 ;(format nil "~&mod:~a,~a" (bnpm i) (bnp-m i))
  (format nil "~&mod:~a,~a" (pos pos (bnpm i)) (pos pos (bnp-m i)))
  )
(defun bnp-2 (i &optional (pos t))
  "bestNP mod,lilnp, then:silver"
  (str-cat (bnp- i pos) "," (bnp-silver i)))
;(defun print-cmp (i) (format nil "~&cmp:~a,~a" (cmp-p i) (collect-if #'cmp-p (mm_can2 i)))) ;notInCan
(defun get-cmp- (i) (collect-if #'cmp-p (words- i)))
(defun get-cmp (i &optional (txt t))
  (let* ((w- (words- i))
         (p (position-if #'cmp-p w-))
         (r (if (numberp p) (list (subseq w- 0 p) (subseq w- p (1+ p)) (subseq w- (1+ p)))
                                  w-)))
    (if txt (format nil "~{~a~^, ~}" r) r)))
;;consider checking that LHS is a NP
(defun print-cmp (i) (format nil "~&cmp:~a:~a" (cmp-p i) (get-cmp i))) ;notInCan
(defun c-np (i &optional (pos t))
  "cmp or np"
  (if (cmp-p i) (print-cmp i) (bnp-2 i pos)))
(defun p-c-np (i &optional (pos t))
  "print c-np" ;cmp or mod+NP
  (format t "~&~a:~a" i (c-np i)))
;(defun bnp2km (i) (let ((bnp (bnp i))) (when bnp (sv-cls (ki_ bnp) "Noun_PHRASE"))))
(defun bnp2km (i) (let ((bnp (bnp i))) (when bnp (sv-cls bnp "Noun_PHRASE"))))
 
;-eof
