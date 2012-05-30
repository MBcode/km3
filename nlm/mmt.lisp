;local: /threec/nlm> head -345 mmprs.lisp | cat>mmt.lisp   ;bobak@computer.org
;mprs.lisp parse MMTx/MetaMap output  bobak@computer.org
;(in-package "LISA") ;(in-package "LISA-USER")
(defvar *dbg* nil)  ;t if need be when testing positions/subseqs over Phrase2Taging sets 
(defvar *tst* nil) 
(defvar *pnow* nil) ;to get over not passing :mmtx output to list that is getting pushnew-d to *p*
;-want2get rid of cui.lisp so see if this will work for now
(defun c-ok (c) c)
;-pnow not used
(defun ID2MMTX_PAIR (id) (cons id *pnow*)) ;try oops only ok if while processing ;getting lng input**
;(defvar *cui-strm* (open "mid/cuis.km" :direction :output :if-exists :append)) ;new
(defvar *cui-strm* t) ;already full
(defun fcs () (FORCE-OUTPUT *cui-strm*))
;most recenlty called mmtx3.cl, but broke out just parsing to this: mprs.cl
;==> mmtx2km.cl <==was it's name in version ./two   ;;will need mkm.cl loaded/1st
;mmtx2km.cl assume km only,as have taken mmobj.cl clos stuff out&won't usually load, so have1dummy fnc:
;-parse mmtx-or-metamap output
(defvar *cid* nil) ;instead of num, set current-id
(defvar *cidn* nil) ;sPhr num
(defvar *cid2* nil) ;cid . cidn
(defvar *tst2* nil)
;=-=-=-=-=-=
(defun mmt (s) 
  "MMTx"
  ;(tokens (run-external  "mmt" s))
  ;(tokens (run-ext  "mmt" s))
  (break2lines (run-ext  "mmt" s))
  )
(trace mmt) ;as not set up to call it here yet; going from cache
;=-=-=-=-=-=
(defun mmt-id (id str)
  (fnc-id_ "mmt" #'mmt id str)) ;(fnc-id "mmt" id str)
;(defun mnp-id (id str) ;  (fnc-id "mnp" id str))
;=-=-=-=-=-=

;-retry w/parsing cui as the focus
(defun prs-cui-line (line &optional (strm t))  ;save to: *cui-strm*
  "parse a cui line" ;break into the list of what you want then have printing be seperate
  (let* ((pp (positions- '(#\( #\)) line))
	 (root (if (full pp) (subseq line (+ (first pp) 1) (second pp))  ;pref-txt(opt)
		 ""))
         (bp (positions- '(#\[ #\]) line)) ;has to be in one of these lines
	 (cls0 (subseq line (+ (first bp) 1) (second bp))) ;break by, then underscore
	 ;/, /s//__/   then break by remaining ,'s  -finish*   test on tx/phr6.tmp
	 (clss (underscore (simple-replace-string ", " "__" cls0))) ;break by, then underscore
	 (cls (split-string clss #\,))	;can be >1
	 (mn (parse-number (subseq line 1 6))) ;could use a parse-number (generic?)
	 (wrd+ (subseq line 6 (- (or (first pp) (first bp)) 1)))
	 (wrd-a (split-str2by wrd+ #\:))
	 (cui- (if (full wrd-a) (trim-whitesp (first wrd-a)) "")) ;id
	 (cui (c-ok cui-)) ;used2 assure started w/C 
	 ;(dScore (cui-dScore (cuistr2kmcui cui))) ;go from ins, as can have >1 parent
	 (wrd- (if (full wrd-a) (cdr wrd-a) ""))
	 (wrd (string-downcase (rm-str "[V]" wrd-))) ;try
         (tp (positions- '(#\{ #\}) line)) ;new treecode stuff
	 (tc0 (subseq line (+ (first tp) 1) (second tp))) ;break by, then underscore
	 (longP (position #\Space wrd)) ;if longestWord,then hyphenate in the parse txt of the atom
	 (pf-slt (if (full root) (format nil " (pref-txt (\"~a\")) " root) ""))
	 (cuistr (cui-str cui cls wrd pf-slt tc0)  ))
   ;(unless dScore (eval-str2km cuistr)) ;do if not cuis.km add it now
    (when (not (kmcui-cache-p cui)) ;new/try  ;try to move into the km side though
      (assert-cui- cui cls wrd root tc0) ;does cui-str &ka of it
      (cui_words cui wrd) ;(sv cui "words" wrd)  ;check  ;sort of an init :after for cui
      ) 
    (when longP (sv *cid* "longestWords" wrd t t)) ;new ;2nd opt=t will print as above
  (cons mn cuistr)))
;-
;write a mmtx-lines-parser 
; Meta Cendidtates (#) &get score&cui in an alst; also parse those cui to km ins
; Meta Mapping will already use a parsed cui, so just get score&cui in an alst
;  _could see if already have cui when making one; &try asserting to km right away; &save at end
;    -can worry about aui/etc cnncts later  ;maybe a time to try agraph/racer? ;km triple dump?
;    
;(defun phrase-p (line) ; (prefixp "Phrase:" line))
(defun prs-meta-p (line) ;defun prs-meta (line)
  "either of the mmtx meta lines"
 (when (prefixp "Meta " line) 
  (let* ((metaparts (explode- line #\Space)) 
	 (n (getnum (third metaparts))))
   (when *dbg*
    (format t "~&;meta map:~a:~a" n line))
 n)))

(defun get-cui-str (cl)
  "pull cui-str from list of strs"
  (let ((cs (member "C" cl :test #'prefixp))  ;cui-str:
        ;(cs (member "C" (explode- cl) :test #'prefixp))
	)  ;cui-str:
    (first cs)))
(defun get-cui (cl)
  "get cui str from str"
  (let* (;(cs (if (listp cl) (get-cui-str cl) cl))  ;cui-str:
	  (cs (get-cui-str (explode- cl)))  ;cui-str:
          (wrd-a (split-str2by cs #\:))
	  (wrd (if (full wrd-a) (car wrd-a) "")))
     (trim-whitesp wrd)))
;
(defun prs_phrase (line)
  "line2 just phrase from it"
  (let* ((wrd-a (split-str2by line #\:))
	 (wrd (if (full wrd-a) (cdr wrd-a) "")))
    (trim-whitesp wrd)))
;
(defun get-phrs (lines)
  "pull out mmtx input phrase from its output lines"
  (let* ((pl (member "Phrase:" lines :test #'prefixp)) 
	 (phrs (prs_phrase (first pl))) ;though tagging version is better
	 ) 
    (string-trim '(#\") phrs)))

(defun mmtxl-prs-meta (mml &optional (strm t))  ;new, right from mmtxl-prs-alst in mmtx-phrs-parser
  (let* ((mmp (positions "Meta Mapping " mml)) ;position of meta on meta-map-list
	 (tp (position ">>>>> Tagging" mml :test #'equal)) ;position of tagging in mml
	 (mmend (if (> (len mmp) 1) (second mmp) tp)) ;end of 1st mm, or start of tp
	 (mmn (prs-meta-p (first mml))) ;Meta Mapping (#) number
	 (mcl (subseq mml 0 mmend))
	 (mcnums (remove-nils (mapcar #'first-num (rest mcl))))
	 (mccuis (remove-nils (mapcar #'get-cui (rest mcl))))
         (mcui-al (mapcar #'cons mcnums mccuis)))
 (when *dbg*
  (format strm "~&;mmn(~a)=mx:~a,~a"  mmn mcnums mccuis)
  (format strm "~&;mnp=~a,tp=~a,mmend=~a"  mmp tp mmend)
  (format strm "~&;mml~a:~a,~a:~a." (len mml) mml (len mcui-al) mcui-al)
  (setf *tst* mcui-al) ;dbg
  (setf *tst2* (list mml mmp tp mmend mmn mcl)) ;dbg
  )
 (list mmn mcui-al)))
;above works, was working on mmtxl-prs-metas (mml &optional (strm t));try to get all meta's this time

;- ;new, try to get all meta's this time ;looks good so will skip upper ones once finish prs-meta
(defun prs-meta (aml &optional (strm t)) 
  "take1meta mapping list and parse it"
  (let* ((mcl aml)
	 ;(mmn (prs-meta-p (first mml))) ;Meta Mapping (#) number
	 (mmn (prs-meta-p (first aml))) ;Meta Mapping (#) number
	 (mcnums (remove-nils (mapcar #'first-num (rest mcl))))
	 (mccuis (remove-nils (mapcar #'get-cui (rest mcl))))
         (mcui-al (mapcar #'cons mcnums mccuis)))
    (list mmn mcui-al) ;want this for each mapping
  )) ;could be make/asserting part of a sub-obj here
(defun prs-metas (mml &optional (strm t)) 
  "take list of all str segs w/meta-mappings in it, and parse them"
  (let* (;(mmp (positions- '("Meta Mapping" ">>>>> Tagging") mml)) ;position of meta on meta-map-list
	 (mmp1 (positions "Meta Mapping " mml))
	 (mmp2 (positions  ">>>>>" mml)) ;> Tagging
	 (mmp (append mmp1 mmp2))
	 (ml (subseqs mml mmp)))
    (when *dbg* (format t "~&~a=~a ~a~&~a" mmp mmp1 mmp2 ml) )
    (when (full ml) (mapcar #'prs-meta ml))))
;-mk-rec& *ri* are in n2.lisp
(defvar *sri* '()) ;subRec ins
(defclass subRec_ ()  ;can be a few of these per (MMTx parse of an)  atomic phrase 
  ((mm-candidates :accessor mm_candidates :initarg :mm-candidates)
   (mm-mappings :accessor mm_mappings :initarg :mm-mappings)
   (id :accessor id :initarg :id) ;new
  ));fill mmt w/these full of ca/map/etc
;-
(defmethod print-object ((sr subRec_) strm) ;could use with-slots
    (format strm "~&<~a:~a>" (mm_candidates sr) (mm_mappings sr)))
;-after turned sexpr accessors into methods can use other version
;make methods to transform mmtx output saved in this into km, carefully; also could get rid of sexpr
;ld3.cl:;(defvar *ri* (mapcar #'mk-rec *ids*))
;o5 mk-rec (a) (make-instance 'rec :id (id a) :txt (txt a) :mmtx (mmtx a) :npparser (npparser a)))
;instead of (mmtx a) have subRec ins 

;finish   ;inputs to these from (mmt str)
(defun mmtxl-prs-alst (lines &optional (strm t)) 
  "parse mmtx outputlines into set of (a)lsts of all types of annotated output"
  (let* ((pl (member "Phrase:" lines :test #'prefixp))
	 (phrs (prs_phrase (first pl))) ;though tagging version is better
	 (mcn (prs-meta-p (second pl))) ;Meta Candidates (#) number
	 (cl+ (subseq pl 2))  ;cui lst &more
	 (cl (subseq cl+ 0 mcn)) ;cui list only
	 (cr (mapcar #'prs-cui-line cl)) ;was prs-c-match  ;still print/save/now assert? cuis ;ok2here
	 (mml (member "Meta " cl+ :test #'prefixp))  ;Meta Mapping list  ;~redo/fix below
	 ;;use mml4meta parsing in: mmtxl-prs-metas
	 (rs (mapcar #'rescore-pair cr)) ;done before cui's become an ins ;could do after though
	 (mp (prs-metas mml strm))  ;new  ;subPhr was next
	  ) 
 (when *dbg* (format strm "~&;phrs~a:~a=~a<~a,~a,cr=~&~a" phrs mcn (len cl) (len cl+) cl cr))
 (pushnew (make-instance 'subRec :mm-candidates rs :mm-mappings mp
			 :id *cid2*) *sri*) ;then maybe w/index/id?
 ;cui-prs-ret & 1st:meta-mapping's ;cr                  ;rescore added
 ;put: phr-assert- here for *cid*, check if cid2 needed for sPhr: yes got it:
 (format t "~&phr-assert-for:~a" (setf *cid2* (idn2 *cid* *cidn*)))
 ;is sPhr clos ins made at the ret of this?
 (setf *pnow*
  (list 'Candidates mcn 
	phrs ;add (sub)phrase right after candidates  ;will have2change accessor a bit
	rs ;cr ;had unscored too
	'Mapping    ;mmn mcui-al
	mp)) ;want a redone version that is even more of a full embedded km ins
  ;subPhr ;no this is messing things up, use km as side-effect only
  ))
;-=-=
(defun mmtxl-tag-alst (lines)
  "pull out -T (mmtx tagging) output, from all of its output"
  (let  ((tl (member ">>>>> Tagging" lines :test #'equal)))
    (when tl
      (list ;1st is alst, then 2longer args
       (mapcar #'cons (explode- (second tl)) (explode- (third tl)))
       (second tl)
       (third tl))))) 
;defun mmtx-lines-parser (lines &optional (strm t))
(defun mmtx-phrs-parser (lines &optional (strm t))
  "mmtx raw output2 tagging&more"
  ;find w/# meta*-p    ;starts to look like ben's  ;could even pull some of these/or just use mine
  ;  really could re-knit a set of (a)lsts just like in 'ni'
 (when *cidn* (incf *cidn*)) ;incr
 (let ((tag3 (mmtxl-tag-alst lines)))  ;do this for merged version of segments as well**
   ;do here: phr-set-prs2 
   (list  ;cons
    (first tag3)  ;(mmtxl-tag-alst lines)
    (rewrite-MMTx-output (get-phrs lines)    ;ben's
      (second tag3) (third tag3))
    (mmtxl-prs-alst lines strm) 
    )))
(defun mmtx-lines-parser (lines &optional (strm t))
  "parse output lines from mmtx"
  (let* ((pp (positions "Phrase: " lines))
	 (pll (subseqs lines pp :end t)))
    (when *dbg* (format t "~&;pp=~a,ppl=~a" pp pll))
    (setf *cidn* (if (< (len pp) 2) nil 0))
    (mapcar #'(lambda (pl) (mmtx-phrs-parser pl strm)) pll)))  ;do sPhr's/cid2 from here?  
;-=-=
(defvar *mnp-ed* nil) ;id's already run through mnp
;(defun pushnew2 (thing stack) ;this would have2be a macro for the var
;  (if (listp thing) (mapcar- #'(lambda (a) (pushnew a stack)) 
;			     (remove-duplicates (flat1 thing)  :test #'equal))
;    (pushnew thing stack)))
(defun have-mnp-p (i) (position (ki_ i) (flat1 *mnp-ed*) :test #'equal))
(defun have-bnp-mnp-p (i) (have-mnp-p (bnp i)))
;-try a tagged version  ;vs ones now in mmt-old
(defun mmtx_pair (pr &optional (strm t))
  (setf *cid* (car pr)) ;(setf *cid* id)
 (let* ((id (car pr))
        (str- (cdr pr))
	(str (rm-star str-))
	(mmtx (mmtx-lines-parser (mmt-id id str) strm)) ;still has sexpr w/everything
	(npparser (mnp_lines-parser id strm)) ;try instead; getting some unexpected npl output 
        (new-rec (list :id id :txt str :mmtx mmtx :npparser npparser))
	) ;phrstr not needed as asserted through phr-words-str2
  (phr-words-str2 id str) ;does txt(/cls)&words &asserts
  (format t "~&phr:~a len=~a" *cid* (len new-rec)) 
  (pushnew new-rec *p*) ;keep old sexpr version this way
  (phr-assert-mmtx-2 *cid*) ;n3 hopefully can get info to assert right now;try
  (pushnew (mk-rec4 id str mmtx npparser) *ri*) ;new n3
  (let ((sn (id2sn id))) (when sn (format t "~&Caught the relation(s):~a,assert-soon" sn))) ;new
  (pushnew (remove-duplicates (flat1  ;wanted pushnew2
    (mnp_phrase2km id))  :test #'equal)
	  *mnp-ed*) ;is more general, will get _PHRASE ins ;no longer ret phrstr
  ))
(defun mnp4bnp (i)
  (when (not (have-bnp-mnp-p i))
    (let ((mnpo (remove-duplicates (flat1  (mnp_phrase2km i))  :test #'equal)))
      (pushnew mnpo *mnp-ed*)
      mnpo)))

(defun mmtx_pairs (prs &optional (strm t))
  (mapcar #'mmtx_pair prs)) 
;start:
(defun prs_ssheet (&optional (pairs *fl2*))
  "actually parse any mmtx pairs"
  (mmtx_pairs pairs)) ;small tst2start, then full sheet: (subseq *fl2* 110) 

;(defun v-p (s) (equal s "V"))  ;not used here, but prs related
