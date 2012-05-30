;mnp MMTx -T tagger, 	;;start by (now)calling on atoms, but will all go on/over smaller chuncks/NPs..
; this has both parsing to s-expr and to km   ;should get a NLP like tree of word/phr objs  /skip from/to
;---clean out the unused versions of this soon
;pulled by hand from mmt.lisp, so can focus on combining this phrasing w/nlp routines ;bobak@computer
; this is different from mmt's >tag< which will be in each subPhr, NpParser is only for main/atmPhr
;Cache: ;put back in
(defun mnp-id (id str)
  (fnc-id_ "mnp" #'mnp id str)) ;  (fnc-id "mnp" id str)
(defun mnp2 (str) (when (full str) (mnp-id str str))) ;like mnp-id2
;I want this to tak over mnp-id:
(defun npl-id (id str) ;try w/new/next cache?
 ;(fnc-id_ "npl" #'npl id str) ;already call under_ on id; as txt is only id now, for files
 (let ((mnp (fnc-id_ "npl" #'npl (under_ id) str)))
   (if (full mnp) mnp 
     (list mnp))))
;=-=-=-=-=-=
;===BULK npParse*.cl goes away, as well as parts of this; in just a bit/prob edit it out
;some from rsc2.cl so can put that away until want other parsers again
;(defun mmt (s)  "MMTx"
;  ;(tokens (run-external  "mmt" s))
;  ;(tokens (run-ext  "mmt" s))
;  (break2lines (run-ext  "mmt" s)))
;ExtCalls:
(defun mnp (s) 
  "NpParser"
  ;(explode- (run-external  "mnp" s) #\|)
  (explode- (run-ext  "mnp" s) #\|)
  ) ;now it's: nprs2km   ;== (mapcar #'prs-le (np2km s) )
;-
(defun npl (s) 
  "renaming as not new version yet"
  (break2lines (run-ext  "mnp" s)))
;;(defun npl (s) (format t "~&dummy4new mnp on:~a" s) s) ;see if ret is similar to mnp;expected redefn
;(defun npl (s) (format t "~&dummy4new mnp on:~a" s) (list s)) ;fix/finish 
;-
;=======had to go w/npl for a bit to get mnp, wanted to read/cache text as is, &then read2lol 2prs2km
;  ==clean this all up/make much smaller  ;feeding off of mem/cache.lisp
;-try
;via ID:
(defun mnp_id (id) (mnp-id id (txt id)))
(defun mnp_id2 (id) 
  (let ((lbs (npl-id id (txt id))))
    (when (fulll lbs) (mapcar #'explode-bar lbs))))
;I should write something like the fnc that calls mnp when there is no cache, do it(for?)new cach/ing
;(defun npl_id (id) (npl-id id (txt id)))
(defun npl_id (id) 
 (if (not (full id)) '("")
  (let ((tx (txt id)))
    (npl-id (under_ tx) tx))))
;-
; in other files, diff wrapping of the run-ext call is done; so it is easier to parse
; lx2.cl has lx2km ;tle.cl has np2km
;../prs.cl if we try parsers again
;for ref here are the cmnd files run-ext is calling above:
;==> lexA <== echo $1 |lexAccess 
;==> mmt <== echo $1 |mmtx/bin/MMTx -I 
;==> mnp <== echo $1 |mmtx/bin/NpParser 
;and these either have to be in the path or ln -s to mmtx
;;some has gone to m2km.cl to work on interacting w/km
;- had old breakdown, which is similar2what ended up in the final s-expr
;;it's all phrases, sentence->diff levels of phr, to longest/Words, -have (downcase)wrd-ins 2play w/
;;-mmtx/metamapn ;atom/in -> m-phrases ;m-phrase -> m-matches ;m-match -> cuis
;;cui has id tui txt pref-txt(opt)  ;if txt has ' ', then longestWord &needs hypen before parsing  
;--
;afterBULK, now processing all lines in a single mmt/mnp call,(at once)&produce an s-expr w/alsts
;--
;note we start w/atom now, but it is really input-txt, so could of been full-txt
;-=-=
;-mmtx's np section: ;calls mnp
(defun get-phrs-typ (s)
  "get *_PHRASE from NpParser output line"
  (first (break2lines s)))

(defun mnp-lines-parser (sl &optional (strm t)) ;do here to catch phrases before broken by mmtx
  "parse [PHRASE] output lines from NpParser" ;just _PHRASEs via -T (mmtx tagger does rest better)
  (let* (;(sl (mnp str)) ;NpParser output as list of strs
	 (mnp (positions "_PHRASE" sl :test #'search)) ;just find phrase markers,&then pull that info:
	  (mns (mapcar #'(lambda (p) (nth p sl)) mnp)) ;raw phrs-typ strs
	  (ptl (mapcar #'get-phrs-typ mns)) ;phrs-typs, now also get the phrases 
	  (mns2 (mapcar #'(lambda (p) (nth (- p 3) sl)) mnp)) ;raw phrs-strs
	 )
    (mapcar #'cons ptl mns2)
    ))
;(defun le-p (str) (equal str "Lexical Element")) ;could also use prefixp or search if in larger str
;(defun le-p (str) (equal (first-lv str) "Lexical Element"))  ;could have vers that warns if off
(defun le-p (strl) (and (equal (first-lv strl) "Lexical Element") (len-gt strl 7))) 
;(defun phr-p (str) (equal (first-lv str) "Phrase")) 
(defun phr-p (strl) (and (equal (first-lv strl) "Phrase") (len-gt strl 8))) 
;(defun np-p (str) (equal (first-lv str) "NOUN_PHRASE")) 
(defun np-p (al) (equal (car al) "NOUN_PHRASE"))  ;from s14
  ;as below positions uses search
 ;Consider naming le's by phrID.le# then it looks like '.' access ;so (str-cat *cid* ".le" n)
(defun mnp-lines-parser2 (sl &optional (strm t)) ;do here to catch lex-elts before broken by mmtx
  "parse Lexical_Element-s out of NpParser output"
  (let* ((lp (positions "Lexical Element" sl :test #'search))
	 (le (mapcar #'(lambda (p) (nth p sl)) lp)) ;raw le strs
	 (mns2 (mapcar #'(lambda (p) (nth (+ p 2) sl)) lp)) ;raw le-strs
	 ) 
    (format t "~&le_l:~a~&~a~&~a" lp le mns2)
    )) 
;USER(16): ;just cached some new mnp txt in mem/npl, see if new mnp-*parser 4all would be better?
;defun mnp-parser (id) ;though maybe txt is better if not indexed that way, maybe overload?
;defgeneric mnp-parser  ;how did I sep out ID from txt, objs of either type would also wrk
;quickly as fnc, then..?  use output from: USER(1): (npl_id "1A")
;("Phrase|0|0|4|adult|adult|1|true|NOUN_PHRASE" "Lexical Element|0|LEXICON|noun|adult|0|4|true")
(defun parser-npl (bstr) ;str w/|bars, LexElts
  "parse 1 | sep ret str from NpParser, get all LexElts this time"
  (let ((pl (break-by-bar bstr)))
    ;format t 
    pl)) ;for now
(defun mnp-parser (id)
  "parse all ret strs from NpParser, get all LexElts this time"
   (let ((npl (npl_id id))) ;np lex strings
     (mapcar #'parser-npl npl)))

(defun  parse_le (le) ;list repr a lexElt
   (assert-le (elt le 1) (elt le 3) (elt le 4) (elt le 5) (elt le 6)))
(defun  parse-le (le &optional (id *cid*)) ;list repr a lexElt
  "lexElt list, to a km repr" ;start w/format sketch, then nil&assert str 2km
  ;format nil "~&(*~a.le~a has (instance-of (~a)) (txt (\"~a\")) (from (~a)) (to (~a)))"
   ;le-str
   (assert-le- ;new try to assert
	  id (elt le 1) (elt le 3) (elt le 4) (elt le 5) (elt le 6))
   "" ;so nothing evaled dwnstream
   )
;try using just for new lexElts 2km ins; sketch up a format, trace if not enough in output
(defun mnp-parse2km (id)  ;could have parser-npl do it ;for now..?  ;called in mmtx_pair 
  (let* ((lel (collect-if #'le-p (mnp-parser id))) ;lexElts now, assume NP handled elsewhere4now
	;;(les (mapcar #'parse-le lel))
	 (les (mapcar #'(lambda (le) (parse-le le id)) lel)))
    (when (fulll les)
      (when *dbg* (format t "~&mnp:~a" les))
      ;;(mapcar #'eval-str2km les) ;finish the instantiation
      ;(mapcar #'(lambda (le) (eval-str2km (first-lv le))) les)
      (mapcar #'(lambda (le) (ka (first-lv le))) les) ;try so can skip ""
      )));will probably replace w/more general version below
;-above already?for np, but want for all _PHRASE, starting below:
(defgeneric phrase-key-p (s))
(defmethod phrase-key-p ((s STRING))
  (suffixp "_PHRASE"))
(defmethod phrase-key-p ((s SYMBOL))
  (phrase-key-p (symbol-name s)))
;			in both of these cid could be in a slot!,so can see links
;;try as sPhr just for now
(defun  parse_mnp-phr (le) ;list repr a _PHRASE lexElt
    (assert_sPhr (elt le 1) (elt le 8) (elt le 4) (elt le 2) (elt le 3)))
(defun  parse-mnp-phr (le &optional (id *cid*)) ;list repr a _PHRASE lexElt
  "phrase lexElt list, to a km repr" ;start w/format sketch, then nil&assert str 2km
  ;format nil "~&(*~a.sPhr~a has (instance-of (~a)) (txt (\"~a\")) (from (~a)) (to (~a)))"
 ;(when (atom-p id) ;new, try to get w/in the atom's phrases slot
    (assert-sPhr 
      ;sPhr-str  ;always called just don't put in main phr if atom-p
	  id (elt le 1) (elt le 8) (elt le 4) (elt le 2) (elt le 3)));)
;	;or assert-sPhr n3  ;move all below from ka, to asserting right away
;mnp_id2* fncs
; -now in mnp-old.cl-
;-vs:  using below:	both differntiate LexElt and _Phrase  ;though mnp_id2 = mnp-parser
; simplify/vs 2 versions
;
(defun mnp_parser (w-id) ;take txt from here instad of looking up, as no ins yet
    ;(mapcar #'parser-npl (list+ w-id)) ;no/well from cache file &I'm sending txt right now/fix
    (list+2 (mnp2 w-id)) ;was mnp
     )

(defun mnp2km (w-id)  ;could replace mnp-parse2km as more general
  "like mnp_phrase2km but no id2assoc w/other than the word/s getting mnp info for"
  (let* ((lel (collect-if #'nop (mnp_parser w-id))) ;lexElts now, assume NP handled elsewhere4now
	 (les (mapcar #'(lambda (le) 
			  (if (le-p le) (parse_le le)  ;check if lexElt or _Phrase
			    (when (phr-p le) (parse_mnp-phr le)))) lel)))
    les))
 ;could call mnp2km then add info for the atom/.. id that it came from after<--* instead of below
	;in o6tx working on it; want to enter anything get mnp from cache &see/(then)mk it*
(defun mnp_phrase2km (id)  ;could replace mnp-parse2km as more general
  (let* (;(lel (collect-if #'le-p (mnp-parser id))) ;lexElts now, assume NP handled elsewhere4now
	 (lel (collect-if #'nop (mnp-parser id))) ;lexElts now, assume NP handled elsewhere4now
	 (les (mapcar #'(lambda (le) 
			  (if (le-p le) (parse-le le id)  ;check if lexElt or _Phrase
			    (when (phr-p le) (parse-mnp-phr le)))) lel)))
   ;(when (fulll les)
   ;  (when *dbg* (format t "~&_phrase:~a" les))
      ;(mapcar #'(lambda (le) (eval-str2km (first-lv le))) les)
      ;(mapcar #'(lambda (le) (ka (first-lv le))) les) ;n5 no longer need2eval str
   ;  )
     les));doesn't look different from above/finish/fix/be able to play w/*;got rid of le-p
;-
		;'npl' here too,  finish/fix s29_3 error ;oh, npl dflt output
;defun mnp_lines-parser (id)  ;not quite the same, as it calls 'npl' for some reason
(defun mnp_lines-parser (id &optional (strm t))
  "try mnp-lines-parser w/new cache"
  (let* ((lel (mnp-parser id))
	 (npl (collect-if #'(lambda (l) (np-p (last l))) lel)))
   ;(make-mnp-ins lel) ;;turns out a good time to make _PHRASE ins,;NOjust mnp_phrase2km
    (when npl 
      (mapcar #'(lambda (el) (cons "NOUN_PHRASE" (nth 5 el))) npl))))

;==might call below here, mnp2 for now
