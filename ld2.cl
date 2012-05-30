(load "ld.cl")
;(trace parser-npl mnp-parser mnp_phrase2km)
 ;(trace tuis_dscore max-l)
;(trace parse_le parse_mnp-phr mnp2km)
;(trace mnp_parser)
;(trace mnp-parser)
;(trace assert-sPhr)
;(trace assert-le) ; list-lines
;;(trace le-p phr-p)
;(trace assert-le-)
;;(trace assert-cui- rescore-pair mmtxl-prs-alst tuis_dscore cuistr2cui) ;got rid of cui.lisp
;;(trace mmtxl-tag-alst l1_prs mm-mappings)
;(trace phr-assert-mappings-2 phr-assert-candidates-2 cui-map-args cui-can-args)
;(trace assert-sPhr sv-from) ;sv-cls sva idn
;(trace id2NPs id2NP-s id2opNPs)
;(load "colu.cl" :print t) ;provides *cea* alist: crit# . columbia parse
;now it &parsed.lisp are in asd file
; (format t "~&made all criteria:~a" (mk-all-ft)) ;make fullText/criteria, just to organize for now
(defun id2nps (id) (id2np-s id)) ;just for now    ;get out of here/reconcile w/mmkm version
;(trace id2np-s np tui> tui3>)
;(trace can-sort-alst sort-alst);hopefully not calling
;(trace find+merge-hyphens phr-passert-sn phr-hyphen-txt) ;(trace mmt-id) 
;(trace find-merge-hyphens)
(format t "~&dbg2phr-at:~a" (eval-str2km "(the instances of Phrase)")) ;dbg
(defvar *pctint* '()) ;str-int ave overlap pct 
(defvar *sheet2* (prs_ssheet)) ;will parse up from-cache t, but could do slower from fnc-calls
(setf *sheet2a* (mapcar #'(lambda (a) (cons (second a) a)) *p*))  
;(setf *ri* (mapcar #'mk-rec *ids*)) ;this gives list of nil:nil now  ;n3 try to push w/*p*
;(trace mk-rec4)
(trace id2NPs)
(defvar *fl3* (mapcar #'(lambda (tx) (cons (underscore_ tx) tx)) (flatten (mapcar #'id2NPs *idnd*))))
;(defvar *fl3* (mapcar #'(lambda (tx) (cons (underscore_ tx) tx)) 
;		     (remove-nils  (mapcar #'first-lv (mapcar #'id2NPs *idnd*)))))
(defvar *sheet3* (prs_ssheet *fl3*))
(setf *fl2* (append *fl2* *fl3*))
(setf *sheet2a* (mapcar #'(lambda (a) (cons (second a) a)) *p*))  
;n3 has this, so skip for now
(defvar *tgs* (tgs))  ;Check that using *idnd* instead of *ids* so don't recount
(defvar *tgm* (mapcar #'(lambda (tg) (first-lv (last-lv (first-lv (get2+ :np tg))))) *tgs*))
;;
(princ "auto NP-match from amia sub data")
(princ " w/NP gold-stnd, now need it for cuis")
(count-mpm) ;now send in vector2score, but could add a list of what2score2 
