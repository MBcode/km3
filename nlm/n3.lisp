
;=break n2 to n2&n3 below right here (for now)

; keep stnd cui, and cui-lnk w/score/etc
;; these should go in ssheet id2erg*
;;
(defun mm-prs (a) (mapcar #'first (mmtx a)))
(defun mm-prsb (a) (mapcar #'second (mmtx a)))
(defun mm-ben (a) (flat1onlys (mm-prsb a))) ;new
(defun id2ben2 (a) (flat1onlys (mm-prsb a))) ;new
;(defun mm-candidates (a) (mapcar #'third (mmtx a)))
;(defgeneric mm-candidates (a))
;(defgeneric mm-mappings (a))
(defmethod mm-candidates (a) (mapcar #'third (mmtx a)))
(defun candidate_access (c fnc) (if (and (full c) (equal (first c) 'CANDIDATES)) (funcall fnc c) 
			    (warn "not can")))
(defun can_txt (c) (candidate_access c #'third))
(defun can_alst (c) (candidate_access c #'fourth))
(defun can_map (c) (candidate_access c #'(lambda (l) (get1+ 'MAPPING l)))) ;or last3
(defun mm-can-txts (a) (mapcar #'can_txt (mm-candidates a)))
(defun mm-can-alst-s (a) (mapcar #'can_alst (mm-candidates a)))
(defun mm-can-alsts (a) (flat1 (mm-can-alst-s a)))
;(defun mm-mappings (a) (mapcar #'can_map (mm-candidates a))) ;start using these
(defmethod mm-mappings (a) (mapcar #'can_map (mm-candidates a))) ;start using these
(defun alst2can-post-score (pr) (first-numstr pr))
(defun alst2can-pre-score (pr) (first2num-in-str pr))
;(defun mm-can-post-scores (a) (mapcar #'first-numstr (mm-can-alsts a)))
;(defun mm-can-pre-scores (a) (mapcar #'first2num-in-str  (mm-can-alsts a)))
(defun mm-can-post-scores (a) (mapcar #'alst2can-post-score (mm-can-alsts a)))
(defun mm-can-pre-scores (a) (mapcar #'alst2can-pre-score  (mm-can-alsts a)))
;-phr-passert*: parserInput nlp3 prs nlp4, &what to add: can/map but both w/*cui-id . score(s)
(defun alst_post (pr) (first-numstr  pr))
(defun alst_pre (pr) (first2num-in-str pr))
(defun alst_cui (pr) (cuistr2cui (cdr pr)))
(defun alst_scorestr (pr) (first pr))
(defun alst_km_cui-scorestr (pr) (format nil "(cui-score ~a)" (cons (alst_cui pr) (alst_scorestr pr))))
(defun alst_km_str (pr)  ; instance-of (cui-score) ?
  (format nil " (cui ~a) (post ~a) (pre ~a)" (alst_cui pr) (alst_post pr) (alst_pre pr)))
(defun mm-can-cuis (a) (mapcar #'alst_cui (mm-can-alsts a)))
;(defun mm-can-kmstrs (a) (mapcar #'alst_km_str (mm-can-alsts a)))
(defun mm-can-kmstrs (a) (mapcar #'alst_km_cui-scorestr (mm-can-alsts a)))
;-
(defmethod print-object ((r rec) strm)
  (format strm "~&<~a:~a>" (id r) (txt r)))
;-
(defmethod print-object ((sr subRec) strm) ;could use with-slots
      (format strm "~&<~a:~a>" (mm-candidates sr) (mm-mappings sr)))
;-
;make sure can find the number of sub-fragments in an atom
;-
(defun mm-can-nums (l1)
    (let ((al (mm-can-alsts l1)))
         (when al (format nil "~a->~a" (mm-can-pre-scores l1) (mm-can-post-scores l1)))))
(defun mm-can-nums- (l1)
    (let ((al (mm-can-alsts l1))
	  (pre (mm-can-pre-scores l1))
	  (post (mm-can-post-scores l1)))
         (when al (format nil "~a->~a:~a" ;"~&~a->~&~a:~&~a" 
			                      pre post (mapcar #'- post pre)))))
(defun l2nalst (l)
    (loop for i in l
	          for n from 0
		          collect  (cons i n)))
(defun mm-can-nums-2 (l1)
    (let ((al (mm-can-alsts l1))
	  (pre (mm-can-pre-scores l1))
	  (post (mm-can-post-scores l1)))
         (when al (format nil ;"~a->~a:~a~&~a" 
			  "~&~a~a->~&~a:~&~a"  l1 ;send in id now
	      pre post (mapcar #'- post pre) (sort 
					       (copy-seq (l2nalst post)) #'> :key #'first-numstr)))))
;-can_map checks for mapping w/in mm-mappings
;clean up the rest of these
(defun map_p (m) (eq (first m) 'MAPPING))
(defun map_n (m) (when (map_p m) (second m)))
(defun map_r (m) (when (map_p m) (third m)))
(defun map-r (m) (when (numberp (map_n m)) (map_r m)))
(defun can-map+2 (l1) (collect-if #'full (mapcar #'map-r (mm-mappings l1))))
(defun can-map_2 (l1) (cons (id l1) (can-map+2 l1)))
(defun sort-alst (al &optional (key #'first-numstr)) "can give opt alst accessor"
  (sort (copy-seq al)  #'> :key key)) 
(defun can-sort-alst (l1) (sort-alst (mm-can-alsts l1)))
(defun first-cui (l1) (cuistr2cui (cdr (first (can-sort-alst l1)))))
(defun find-cui-pr> (n l1)  "cui-strs above n"
  (collect-if #'(lambda (x) (> (first-numstr x) n)) (can-sort-alst l1)))
(defun nth-cui-pr (n l1)  (nth n (can-sort-alst l1)))
(defun nth-cui (n l1) (cuistr2cui (cdr (nth-cui-pr n l1))))
(defun nth-cui-sc (n l1) (car (nth-cui-pr n l1)))
(defun first-map-pr (l1) (first (third (mm-mappings l1))))
(defun first-map (l1) (cuistr2kmcui (cdr (first-map-pr l1))))
(defun first-map-sc (l1) (car (first-map-pr l1)))
;=mv some of less used comparisons /to a sep file
(defun top_equal (l1)
    "see if rescoring changes top CUIs"
      (let* ((maptop (can-map+2 l1))
	     (lt (len maptop))
	     (can-srt (can-sort-alst l1))
	     (cantop+ (subseq can-srt 0 (min (len can-srt) 3)))
	     (cantop (mapcar #'(lambda (pr) (cons (car pr) (cuistr2cui (cdr pr)))) cantop+))
	     (int (intersection maptop cantop))
	     (intn (len int)))
	    (when (neq lt intn) (format nil "~a:~a,~a" int maptop cantop))))
(defun first-equal (l1) (equal (first-cui l1) (first-map l1)))
(defun first_equal (l1)
    "see if rescoring changes 1st cui"
      (let ((cui1 (first-cui l1))
	            (map1 (first-map l1)))
	    (when (not (equal cui1 map1)) (format nil "~&~a,~a" cui1 map1))))
(defun first_equal2 (l1)
    (let ((cui1 (first-cui l1))
	  (map1 (first-map l1)))
          (when (not (equal cui1 map1))
	          (format nil "~&~a:~a,~a:~a" cui1 (nth-cui-sc 1 l1) map1 (first-map-sc l1)))))
(defun cmp-mnp-erg (l1)
    "see if NP is w/in gold-standard"
      (let* ((id (id l1))
	              (gold (id2erg id)))
	    (list id (npparser l1) gold (l1-NPs-p l1 gold) )))
(defun numtxt-pair (l) (cons (id l) (txt l)))
(defun cmp_mnp-erg (l1) ;held in *tgs*
    (let* ((id (id l1))
	   (txt (txt l1))
	   (gold (id2erg id))
	   (gold2 (id2ergm id))
	   )
     ;(when *dbg*
      (format t "~&~a ~a ~a" id gold (txt l1)) ;)
        (when (full txt)
	      (list id txt ;(cons id txt) 
		    (npparser l1) :np gold
		              (l1-NPs-p l1 gold) ;(l1-np-p l1 gold)
			      :mod gold2
			                (get-rec2 id) ;(get-rec+ id) ;
					          ))))
(defun cmp_mnp-erg2 (l1) ;new
    (let* ((id (id l1))
	   (txt (txt l1))
	   (gold (id2erg id))
	   (gold2 (id2ergm id))
	   (a1 (bnpw2 id))
	   (a2 (bnp-m id)))
      (format t "~&bnp:~a=~a,mod:~a=~a" a1 gold a2 gold2)))
(defun id2mnp-erg (id) (get_rec- id *tgs*))
;could generalize to send gold&NP then gold2&? adj/adv? not clear from what I see
(defun l1-np (l1)
    "find the NP from NpParser output"
      (let ((na (assoc "NOUN_PHRASE" (npparser l1) :test #'equal)))
	    (when na (cdr na))))
;(defun id2np (id) (l1-np (get-rec id)))
;(defun np-p (al) (equal (car al) "NOUN_PHRASE")) ;in mnp
(defun id2np (id) (collect-if #'np-p (npparser id)))
;(defun l1-NPs (l1)
;    "get all the NPs" ;will check the 1st pick next
;      (mapcar #'cdr (collect-if #'np-p (npparser l1))))
(defun l1-NPs (l1)
    "get all the NPs" ;will check the 1st pick next
    (let ((nps (mapcar #'cdr (collect-if #'np-p (npparser l1)))))
      (when (full nps) (mapcar #'string-downcase nps))))
(defun id2NPs (id) (l1-NPs (get-rec id)))
(defun l1-np-p (l1 tx)  ;so send in gold/erg
    (let ((np (l1-np l1)) ;(tx (id2erg id)) ;(l1-txt l1)
	              )
         (when (and np tx) ;turn into str-intersect
	       (cons (st np tx) (st tx np)))))
;(defvar *pctint* '())
;-
;more moved to ../test/test.lisp
;-
(defun l1-NPs-p (l1 tx)
    "cmp NPs w/ergo/gold"
      (let ((NPs (l1-NPs l1)))
	(mapcar #'(lambda (np) (str+intersect-match np tx)) NPs)))
;next2unused, &should come from same fnc, &pref id based now
(defun l1-NPs_p (l1)
    (let* ((NPs (l1-NPs l1))
	   (id (id l1))
	   (gold (id2erg id)))
          (mapcar #'(lambda (np) (str-intersect-match np gold)) NPs)))
(defun l1-NPs+p (l1)
    (let* ((NPs (l1-NPs l1))
	   (id (id l1))
	   (gold (id2erg id)))
          (mapcar #'(lambda (np) (list id (str-intersect-match np gold))) NPs)))
;-
(defun t3 (&optional (p *p*))
    "check CUI 1st/top picks after re-rank, and Np-v-gold"
     (list ;2cui tests, then the final(real)test
         (mapcar #'first_equal2 p)  ;checking of 1st meta-map, so look@this more&use...-fin
	 (mapcar #'top_equal p)
	 (mapcar #'cmp_mnp-erg  p)))  ;gold-stnd
(defun tgs (&optional (p *ids* ;*p*
			 ))
    "test w/gold standard"
      (mapcar #'cmp_mnp-erg  p))  ;gold-stnd
(defun nums-chng (&optional (p *p*))
    "just thec(cui) score changes"
      (mapcar #'mm-can-nums- p))
(defun t4 (&optional (p *p*))
    (cons (t3 p) (nums-chng p)))

;-
(defun rec4cuirev- (id) 
  (let ((l (get-rec id)))
    (format t "~&id=~a txt=~a mmtx=~a" id (txt l) (mmtx l))))

(defun rec4cuirev_ (id) 
  (let* ((l (get-rec id))
	 (can (mm-candidates l)) ;has mapping@end
	 (np (npparser l))
	 )
    (format t "~&---------------------------------------------------------------")
    (format t "~&id=~a txt=~a ~&~a ~&~a" id (txt l) can np)))
;none of these used, just rec4cuirev2 now
(defun rec4cuirev (id) 
  ;(let ((l (get-rec id)))
   (when id (let* ((can (mm-candidates id)) ;has mapping@end
		  (np (npparser id)))
    (format t "~&---------------------------------------------------------------")
    (format t "~&id=~a txt=~a ~&~a ~&~a" id (txt id) can np))));)

(defun print-can-a (cp)
  "print candidate assoc" ;cui for candidates
  (let ((score (first cp))
	(cui (cuistr2cui (cdr cp))))
    (when *dbg*
      (format t " ~a~a;" ;"~&~a ~a" 
	  ;(first cp)  (cuistr2cui (cdr cp))
	     score     cui))
  	 (cons score  cui)))

;;could always make strs and ret /(for)/printing
;in cui.lisp now
;(defun print-cui (cui &optional (strm t))
;  "full cui info" ;used in mapping now
;  (let ((pt (cui-pref-txt cui))
;	(tx (cui-txt cui))
;	(cls (cui-class cui))
;	(defn (cui-defn-str (cui-p cui))))
;    ;(format strm "~a~a:~a:~a;~&" cui (or pt tx) cls defn)
;    (format strm "~a~a:~{~a~^, ~}:~a;~&" cui (or pt tx) cls defn)
;    ))
;(defun print-cui-a (ca &optional (strm t))
;  "print cui assoc" ;for mapping  /~= print 1 potential mapping
;  (let* ((sc (first ca))  ;or accessor/above
;	 (cui (cdr ca))
;	 (c (sym-cat '* cui)))
;    (format strm " ~a:" sc)
;    (print-cui c)))

(defun print-can-map (cm1 &optional (pre-str "") (strm t))  ;finish this1
  "print 1 mapping"
  (let ((cal (second cm1))) ;cui alst for a mapping  ;*could reorder b4 redoing?
          ;if i do: get the cui's rescore&see if can redo final scoring/look@related docs2
    ;;(format t "~&~a" cal) ;finish this now
   (format strm "~&~a" pre-str) 
    (mapcar #'print-cui-a cal)
  ))
(defun print-can-maps (can &optional (strm t))  ;fin
  "print all mappings"
  (let ((cm (sixth can)))  ;or accessor/above
    ;;(format t "~&~a" cm) ;just get back in now, then add longer cui info to this next
    ;(format t "~&") ;fnc incls defn now
    (mapcar #'print-can-map cm)
    ))
;or
(defun print-can-map1 (n cm)  ;fin
 (let ((cm1 (nth n cm)))
  ;(format t "~&map~a :~a" (1+ n) (when cm1 (print-can-map cm1)) )
	  (when cm1 (print-can-map cm1 (format nil "MAP~a:" (1+ n) )) )
  ))
(defun print-can-map3 (can)  ;fin
 (let ((cm (sixth can)))
   (loop for n from 0 to 2 do (print-can-map1 n cm))
 ))

(defvar *canl1* nil) ;from this get just the pre/post rescore ordering ;for ranking
;deal w/tie's later
;(defun rscl-a-post (al) (eval-str (car al)))
(defun rscl-a-post (al)
  "(+ pre delta)->sum"
  (let ((str (if (listp al) (car al) al)))
    (eval-str str)))
;(defun rscl-a-post2 (al) (eval-str (cdr al)))
(defun rscl-a-pre_ (al)
  "(+ pre delta)->pre"
  ;if (full al) (first-num (car al)) (numstr al)
 (numstr (first-lv
   (second (explode- (rm-ws-parens al)))
   )))
(defun rscl-a-pre (al)
  (if (full al) (first-num (car al)) (numstr al)))
(defun rscl-a-delta (al)
  "(+ pre delta)->delta"
 ; (second-num (car al))
 (numstr (first-lv
   (last (explode- (rm-ws-parens al)))
  )))
(defun rscl-a-delta_ (al)
  (second-num (car al)))
(defun pr-dupl (n nl)
	;(when (> (count pr pre) 1) (format t "/~a" (count pr pre)))
  (let ((cnt (count (cdr n) nl :key #'cdr)))
    ;(when cnt (format t "/~a" cnt))
    (when (and cnt (> cnt 1)) (format t "/~a" cnt)
    )))
(defun sortpscore (al)
  "alst->just scores"
  (let* ((sc (mapcar #'cdr al))
	 (scu (remove-duplicates sc)))
    scu))
(defun scoreaposit (scal sl)
  "alst-> alst w/position"
 (mapcar #'(lambda (sca)
  (cons (car sca)
	(1+ (position (cdr sca) sl))))
  scal))
(defun scoreapositions (sl)
  (scoreaposit  sl (sortpscore sl)))
(defun cui-ranks (&optional canlst *canl1*) ;helped show that rescoreing didn't mv positions much
  "alist hold pre/post scores for cui's turns into the ranking changes"
  (let* ((pre (mapcar #'(lambda (a) (cons (cdr a) (rscl-a-pre a))) canlst))
	 (pl (sort (copy-list pre) #'> :key #'cdr)) ;#'rscl-a-post2
	 (post (mapcar #'(lambda (a) (cons (cdr a) (rscl-a-post a))) canlst))
	 ;(pre-s (sortpscore pre))
	 ;(post-s (sortpscore post))
	 (pre-s (scoreapositions pre))
	 (post-s (scoreapositions post))
	 (len (len post)))
    ;(format t "~&~a~a" pre post)
    ;(format t "~&~{~a~^, ~}~&~{~a~^, ~}" pre post)
    ;(mapcar #'(lambda (a) ( )) pre)
    (format t "~&")
   ;(format t "~&~a" pre-s)
   ;(format t "~&~a" post-s)
    ;#+IGNORE
    (loop for pr in pre 
	  for y from 1
	 ;po in post
	 do
	 ;(format t "~a->~a" pr po)
	(progn
	 (format t "~&~a:~a(~a)" y pr (cdr (assoc (car pr) pre-s)))
	 ;(when (> (count pr pre) 1) (format t "/~a" (count pr pre))) ;need :key #'cdr
	 (pr-dupl pr pre)
	 (let ((po (assoc (car pr) post)))
	  (format t "->~a(~a)"  po (cdr (assoc (car po) post-s)))  
	      ;want2get change in position instead/fix/fin
	  (pr-dupl po post)
	  ))
	)
  ))
;might print as position#/if dups
;	below id can mean under_ txt
;-access can scores ;aug
;make a id2can-scores ;maybe use? id2can-map
(defun can2score (can) (first (fourth can)))
(defun can2cui (can) (cdr (fourth can)))
(defun id2can-strs (id)  ;try to get away from processing this str, want min sometimes too/cleanup
  (let* ((id- (if (has-space-p id) (underscore_ id) id))
	 (can-strls (mapcar #'fourth (mm-candidates id-)))
	 (can-strs (flat1 can-strls)))
    can-strs))
(defun id2can-scores (id) 
  (mapcar #'first (id2can-strs id)))
(defun id2can-cuis (id) 
  (mapcar #'cuistr2cui (mapcar #'cdr (id2can-strs id))))
(defun id2can-scores_ (id)  ;try to get away from processing this str, want min sometimes too/cleanup
  (let* ((id- (if (has-space-p id) (underscore_ id) id))
	 ;;(scores (mapcar #'rscl-a-post (fourth (first (id2can+ id-)))))
	 ;(sscores (mapcar #'first (fourth (first (mm-candidates id-))))
	 (sscores (mapcar #'can2score (mm-candidates id-)))
	 ) sscores))   
;fix, not getting all of the sets of candidates, see s16b_3
(defun id2can-cuis_ (id);new ;try to get away from processing this str, want min sometimes too/cleanup
  (let* ((id- (if (has-space-p id) (underscore_ id) id))
	 (cuistrs (mapcar #'cdr (fourth (first (mm-candidates id-)))))
	 (cuis (mapcar #'cuistr2cui cuistrs))
	 ) cuis))
;defun max-post-can-score (id) ;which is often underscored txt now
(defun max-mix-can-score (id) ;which is often underscored txt now
  "max-score for rescored candidate CUIs, from tui dScores" ;used as 1st cut for pref of annotated txt
  (let* (;(id- (if (has-space-p id) (underscore_ id) id))
	 ;;(scores (mapcar #'rscl-a-post (fourth (first (id2can+ id-)))))
	 ;(sscores (mapcar #'first (fourth (first (id2can+ id-)))))
	 (sscores (id2can-scores id))
	 (scores-s  (collect-if #'stringp sscores)) ;try
	 ;(scores (mapcar #'rscl-a-post (collect-if #'stringp sscores)))
	 (pre-scores (mapcar #'rscl-a-pre scores-s))
	 (delta-scores (mapcar #'rscl-a-delta scores-s))
	 (scores (mapcar #'(lambda (pre delta) (+ delta (* 0.01 pre))) ;to break ties
			     pre-scores delta-scores))
	 )
    (when *dbg* (format t "~&~{~a~^, ~}" scores) )
    (when (full scores) 
     ;ave (ave-l scores)
     (+ (* 0.01 (ave-l scores))
      (apply #'max scores)
      ))))
;;;Consider also a ave-delta, maybe ave-pre ;also ave of top few/ 1/2 of scores
;both of these have been returning nils, so FIX
(defun rscl-can-score (id &key (rfnc #'rscl-a-delta) (retfnc #'max-l))  ;was max
   (let* ((sscores (id2can-scores id))
	 (scores (mapcar rfnc (collect-if #'stringp sscores))))
    (when *dbg* (format t "~&~{~a~^, ~}" scores) )
    (when (full scores) (funcall retfnc scores))))
;defun max-delta-can-score (id &optional (rfnc #'rscl-a-delta)) ;which is often underscored txt now
(defun max-rscl-can-score (id &optional (rfnc #'rscl-a-delta)) ;which is often underscored txt now
  "largest delta score from all candidate cuis from an id"
   (let* ((sscores (id2can-scores id))
	 (scores (mapcar rfnc (collect-if #'stringp sscores))))
    ;(when *dbg* 
      (format t "~&~{~a~^, ~}" scores) ;)
    (when (full scores) (apply #'max scores))))
(defun max-rscl-can-score_ (id &optional (rfnc #'rscl-a-delta))  ;nope/not yet ;well..
  (rscl-can-score id :rfnc rfnc :retfnc #'max-l))
(defun min-rscl-can-score (id &optional (rfnc #'rscl-a-delta))  ;but want min dScore as input
  (rscl-can-score id :rfnc rfnc :retfnc #'min-l))  ;so going through cuistr not the way
;not really, need the min dscores, so use below, like phr-assert-deltas does
(defun max-delta-can-score (id) (max-rscl-can-score id))
(defun min-delta-can-score_ (id) (min-rscl-can-score id)) ;new so can catch things2toss
(defun min-delta-can-score (id) 
  (let* ((scorels (mapcar #'cui-dscore (id2can-cuis id)))
	 (scores (flat1 scorels)))
    (min-l scores)))
  ;clean up a bit more, &use in more than just a pool/but will start w/just ret meta-mappings -finish
(defun max-post-can-score (id) (max-rscl-can-score id #'rscl-a-post))
;(defun id2opNPs (id) ;redefined in ld3 where ;now in test.lisp
;  (princ "use ld3 version"))
;probably have a id2NP.. that just calls this and returns the best  ;;want2keep parses&allow other cmp
;defun id2bestNP (id &optional (id2NPfnc #'id2NPs))
(defun id2bestNP (id &optional (id2NPfnc #'id2NP-s)) ;oct21
  "get highest candidate cui rescore for id's NPs" ;2extend use mappings as tie-breaker
  (let* (;(NPs (id2NPs id))
	 (NPs (funcall id2NPfnc id))
	 (bestNPs (when (and (full NPs) (> (len NPs) 1))  ;was when NPs
	      (sort (mapcar #'(lambda (np) 
				;(cons (max-post-can-score np) np)
				(cons (max-delta-can-score np) np)
				;(cons (max-mix-can-score np) np)
				) (copy-list NPs))  ;#'>
		    #'nn> :key #'first)
	      ))
	 (topNP (if (full bestNPs) (cdr (first-lv bestNPs))
		      (first-lv NPs))))
    ;(if (prefixp "np " topNP) (rm-str "np " topNP)  ;kludge, figure out the problem
    ;  topNP)
    ;;If top2have the same score, break tie w/top from max-post -fix/finish *** ;now just warn
    (when (and (full bestNPs) (> (len bestNPs) 1) (equal (first-lv bestNPs) (second-lv bestNPs)))
      (format t "~&FIX since ther is a tie, use max-post as backup"))
    ;topNP
    (if topNP (string-downcase topNP)
      (txt2 id))
    ))
;if the top scores of the NPs are the same, then could go to mappings, too
(defun id2bestopNP (id)
  "open-nlp best NP"
  (let ((b-np (rm-str "np " ;turned out rm-small words got rid of this before
		  (id2bestNP id #'id2opNPs)))
	(cmp (id2cmp_lhs id)))
    (if cmp cmp ;cmp-p
     (if (full b-np) b-np
      (txt2 id))))) ;if no NP ret txt


(defun print-can (can_)
  "print atm-frag candidates +mappings"
  (let* ((can can_ ;(flat1onlys can_)
	      ) ;new
	 (top (subseq can 0 3))
	 (ca (fourth can)))  ;or accessor/above
    ;(format t "~%~%~a;" top) ;
    (format t "~&~a;" top) ;
    (cui-ranks
     (setf *canl1* (mapcar #'print-can-a ca)) ;no longer prints
     )
    ;(print-can-maps can) ;have2call here
    (print-can-map3 can) ;have2call here
    ))
;find alt mappings too?

(defun rec4cuirev2 (id)  ;finish w/next as gets rid of extra cui info in 'can' but doesn't add2'map'
  (let ((l id ;(get-rec id)
	   ))
   (when l (let* ((can (mm-candidates l)) ;has mapping@end
		  (np (npparser l)))
    ;(format t "~%~%~%~%~%~%---------------------------------------------------------------")
    (format t "~&---------------------------------------------------------------")
    (format t "~&=id=~a ;txt=~a " id (txt l))
    (mapcar #'print-can can)
    ;(format t "~%~&=~a" np)
    (format t "~&=~a" np)
    can))))
 ;I should make an attempt to pick new mappings, &maybe number the original as well
(defun rec4cuirev3 (id)  ;finish/not yet used
  (let ((l (get-rec id)))
   (when l (let* ((can (mm-candidates l)) ;has mapping@end
		  (np (npparser l)))
    (format t "~&---------------------------------------------------------------")
    (format t "~&id=~a txt=~a " id (txt l))
    (mapcar #'print-can can)
    (format t "~&~a" np)
    ))))
;=-=-= ;-more nlp related: aug ;moved to mnp.lisp
;-
;was in ld3 then o5: id2match id2fixmatch-p medvocab-*-p fix-w*-medvodb-p
;=-=-=-= ;cmp parts in ner.lisp closer to related code, but still uses some of these accessors
(defun atom-w-str (str)  (collect-if #'(lambda (pr) (search str  (cdr pr) :test #'equal)) *fl2*))
;-try mnp_id npl npl_id, moved to mnp.lisp
;-
;;put back in for a bit:
;(defun l1-prs (a) (mapcar #'first (mmtx (if (stringp a) (get-rec a) a))))
;(defun l1_prs (id)  (flat1 (mapcar #'butlast (l1-prs id)))) ;was in passert-prs
;(trace l1_prs)
;-eof 
