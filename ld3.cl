(load "ld2.cl") ;ld3.cl continues w:
;varsNotUsed: *sheet4* *fl2* *sheet2a*
(mapids #'phr-init-after) ;all phr-set-*
(defvar *missing* '("planned pregnancy" "renal failure" "asthma medication"))
(defvar *op* (mapidnd #'id2opnp)) ;runs phr_nlp3 from km just loaded above
(defvar *opal* (txtl2tal (append *op* *missing*))) ;get opNP txt into get-rec lookup
(defvar *sheet4* (prs_ssheet *opal*))
(setf *fl2* (append *fl2* *opal*)) ;try
(defvar *fl4* (mapcar #'(lambda (tx) (cons (underscore_ tx) tx)) (flatten *op*))) ;2get txt cache
(setf *opa* (mapcar #'cons *idnd* *op*))  ;alst of id.np-s to order to tui> 
;-get these mapcar's done on each obj as it's made ;w/phr-assert-mmtx-2
;(mapidnd #'phr-assert-mappings-2)  ;would have2put re-scored candidates below
;(mapidnd #'phr-assert-candidates-2)  ;
;(trace  max-post-can-score max-delta-can-score max-mix-can-score) ; id2bestNP
(defvar *bop* (mapidnd #'id2bestopNP)) ;best op np-s
(setf *bopa* (mapcar #'cons *idnd* *bop*)) ;can use cmp-alst against *fl-2e*
(trace str-intersect-match)
(setf *silver* (alst-w (mapidnd #'id2erg)))
(setf *silverm* (alst-w (mapidnd #'id2ergm)))
(defvar *sheet4* (prs_ssheet *fl4*))
(setf *sheet2a* (mapcar #'(lambda (a) (cons (second a) a)) *p*)) ;just do again on pushnew-d p
;n3 has this, so skip for now
(defvar *tgma* (mapcar #'cons *idnd* *tgm*)) ;don't want to recount duplicates
(format t "~&tgm,uses:cmp_mnp-erg(NpParser NOUN_PHRASES):~&~a" *tgm*)
;;
(count-mpm)
(defvar *mthaf* (flat1 *mtha*))
(format t "~&mtha:~a" (len *mtha*))
(format t ",mthaf:~a" (len *mthaf*))
(princ "matches for best open-nlp np-s")
(defvar *cb* (cmp-bop))
(defvar *cb2* (cmp-bop2))
(setf *cba* (alst_w (mapcar #'cdr *cb*)))
(format t "~&best open-nlp matches~&~a" (mapcar #'cdr *cb*))
(load "mid/crit_atm.cl" :print t) ;no get a 1-to-1 comparison
;(trace p-score)
(format t "~&opprs gen.v.hand:~&~a" (cmp-alst-ids #'equal *cba* *ca* *idnd*)) ;need this soon
(format t "~&p-score~&")
(mapidnd #'p-score) ;to help debug mis:es
(mapidnd #'phr-assert-deltas)  ;should have re-scored candidates to fill phr w/
(mapidnd #'mnp4bnp) ;new n5
(format t "~&cb=~a" *cb*)
(defvar *min2l* (collect-if  #'phr-min2lowp (eval-str2km "(the instances of Phrase)")))
(format t "~&Phrases w/subPhrases that should NOT be considered:~&~a" ;working on time expr 
	*min2l*) ;calc min2low if ask km ins for str-id
(format t "~&str-intersect of id2bestopNP and silver:")
(count-mpm (mapcar #'cdr *cb*)) 
(count-mpm (mapcar #'cdr *cb2*)) 
(let ((hand (mapidnd #'id2hand-m)))
  (format t "~&hand-m:~a~&unique atoms hand scores:" hand)
  (count-mpm hand)
  (p-count 'miss hand))
(format t "~&pct-int=~f" (ave-l *pctint*))
(defvar *cids* (collect-if #'cmp_p *idnd*))
(let ((frac (countInAlst 1 *silverCUImapNum*)))
  (format t "~&frac cui's caught:~a ~$ ~%" frac frac)) 
(ss-out) ;ask to write csv output file in out/ss-out
(setf *pmsa* (alst-w (mapidnd #'id2sb-mod))) ;just do this part in ld3 now
(format t "~&bestNPs:[np]== everything not marked as a modifer from:~a" *pmod*) 
(format t "~&bestNPs:id2sb-mod:~&~a" (alst-w (mapidnd #'id2sb-mod))) 
;(load-kb "km/trt.km") ;try2use loading trt2 instead
;fix:
(trace strs-eq2)
(format t "~&Check score for cmp:~a" (mapcar #'scoreID2cmp *cids*))
;now new obj based col-E bestNP(mod+lil-np)/cmp is at bottom of nlm/mnp2.lisp
;(defun bnp2km (i) (let ((bnp (bnp i))) (when bnp (sv-cls bnp "Noun_PHRASE"))))
;(format t "~&bestNPs:mod1op-np:~&~a" (mapcar  #'mn2km
;	       ;#'(lambda (id) (let ((mn (mod1op-np id))) (sv id "modnp" mn) (cons id mn))) 
;		*idnd*))
(format t "~&bestNPs:mod1op-np:~&~a" (mapidnd  #'mn2km))
(mapidnd #'id2bestNPkm) ;I still want this to be more init-after /well after it's calculated
(mapidnd #'bnp2km) ;get the assoc ins set to Noun_PHRASE
(untrace)
(mapidnd #'p-c-np) ;print cmp &NP breakdown for col-E  ; print-cmp's finding the cmp sub ins/wrd
