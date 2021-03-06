;from r*l but w/o tmp files now
;all of these take strings instead of filenames now:  stanford,berkeley&open-parsers work
(defun rootp (s) (prefix_p "(ROOT" s))
(defun parsingp (s) (prefix_p "Parsing" s)) ;unused
(defun echo-sp (s) (run-ext "echo" (clean4echo s) ">" "/tmp/sp.txt"))
;(defun run-sp (s) (break2lines (run-ext "nlp/sp_2" (clean4echo s)))) ;w/old setup
(defun run-sp (s) 
  (echo-sp s) ;(run-ext "echo" (clean4echo s) ">" "/tmp/sp.txt") 
  (break2lines 
    (run-ext "sp/lp.sh" "/tmp/sp.txt")
   ;(run-ext "sp/lp.sh" "/tmp/sp.txt" "|" "agrep"  "-d'^P'" "ROOT")
    )) 
(defun sp (s)
  "stanford parser"
    (let* ((prs (run-sp s) ;(break2lines (run-ext "nlp/sp_2" (clean4echo s)))
             )
	   (p1 (position-if #'rootp prs))
	   (p2 (position-if-not #'full prs))
           (p12 (subseq prs p1 p2)))
      (apply #'str-cat p12)))
;make out of stnd-cachable&parse
(defun sp_raw (s)
  (run-sp s) ;(break2lines (run-ext "nlp/sp_2" (clean4echo s)))
  )
(trace echo-sp) (trace run-sp)
(defun sp1 (s)
    (let* ((prs (sp_raw s))
	   (p1 (position-if #'rootp prs))
	   (p2 (position-if-not #'full prs))
           (p12 (subseq prs p1 p2)))
      (apply #'str-cat p12)))
(defun sp2 (s)
  (eval-str2 (clean-se (sp1 s))))
(defun clean-seval (s)
  (eval-str2 (clean-se s)))
;
;-skip sp fncs above, as the sp_2 does the most right now
;might write positions-if, oh postions already does this, no need for parsingp fnc
;need a fnc to take a list of elt, &a position-list, &go dwn it in pairs &create a lol of elts between them;have it subseqs
(defun sp-1 (prs)
  "get the tree only"
  (let* ((p1 (position-if #'rootp prs))
	     (p2 (position-if-not #'full prs))
         (p12 (subseq prs p1 p2))
        ;(p2- (subseq prs p2))
         )
      (apply #'str-cat p12)))
(defun sp-2 (prs)
  "prs tree+dependencies"
  (let* ((p1 (position-if #'rootp prs))
	     (p2 (position-if-not #'full prs))
         (p12 (subseq prs p1 p2))
         (p2- (subseq prs p2))
         (rl (clean-seval 
              (apply #'str-cat p12)))
         )
      (cons rl  p2-)
      ))
(defun sp_0 (s)
  "break sentence output 1st"
    (let* (;(prs (sp_raw s))
           (prs (run-sp s))
	  ;(p1 (position-if #'parsingp prs))
	  ;(ps (positions "Parsing" prs))
	   (ps+ (positions "Pars" prs))
       (ps (when (full ps+) (butlast (rest ps+))))
           )
      (when (full ps) (subseqs prs ps)))) ;ret lol   list-of-sentences which are lists of all output strlines relating2the sentence
(defun sp_1 (s)
  "get the tree only/all sent"
  (let ((sl (sp_0 s)))
    (mapcar #'sp-1 sl)))
(defun sp_2 (s)
  "prs tree+dependencies/all sent"
  (let ((sl (sp_0 s)))
    (mapcar #'sp-2 sl)))

;get a version that keeps the adjacency info, &transforms to a usable format (eg.set of triples)

;(defun rm-str (rs s)
;  (simple-replace-string rs "" s)) 
 ;in utils now
;(defun rm-strs (rsl s)
;  ;reduce #'rm-str
;  (if (and (full rsl) (> (len rsl) 1)) (rm-str (first rsl) (rm-strs (rest rsl) s))
;    (rm-str (first rsl) s) ))
;;(defun rm-strs (rsl s)
;;  (when (full rsl)
;;    (let ((rs1 (r

(defun clean-sp2 (s) ;reduce if more
  (simple-replace-string "(, ,)" "" 
    (simple-replace-string "(. .)" "" s)))
;could have () w/: . .   : -   , ,    so might want to change in parts?
(defun clean-se (s) 
 (simple-replace-string "(. " "( "
  (rm-strs '("(. .)" "(, ,)" "(: -)" "(: ;)"  "," ":") s)))

;(defun clean4echo (s) (rm-strs '("*") s)) ;in utl

;might also just turn into the char equiv if a list has single char/s, while still a str
;/(?a ?a)/s//#\?a/
(defun pr2chr (s)
  (simple-replace-string "(. .)" "#\\."
   (simple-replace-string "(, ,)" "#\\,"  s)))

;(defun eval-str2 (s) ;in utils
;  (eval-str (str-cat "'" s)))

(defun sp2_ (s)
  (eval-str2 (clean-se (sp s))))

(defun berk (s)
  "berkeley parser"
    (run-ext "nlp/brk1_" (clean4echo s)))

(defun berk2 (s)
  (eval-str2 (clean-se (berk s))))
;-
(defun nlp2= (pr) 
 (format t "nl2=~a" (equal (second (first pr)) (second pr)))
 pr)

(defun nlp2 (s) 
  "2nlp parses that are easy to get s-expr from"
  (nlp2= ;consider if same only return one .. otherwise ret diff, (need2fin that)
   (list (sp2 s) (berk2 s))))

(defun f1 (l) (when (fulll l) (if (equal (len l) 1) (first l) l)))
;could just ret b1, so even if 1 or 2, .. ;so len tells # ret
(defun nlp-2 (s) 
  (let* ((s2 (second (sp2 s)))
	 (b1 (berk2 s))
	 (b2 (first-lv b1 ))) ;was first
    (format t "~&~a ~a ~a ~a ~a"  (mapcar #'equal s2 b2) (every #'equal s2 b2)
	    (len s2) (len b2) (list-diff s2 b2))
    (if (and (full s2) (full b2))
      (if (every #'equal s2 b2) b1 ;(f1 b1) ;s2
        (list s2 b2))
     (or s2 b2)) ;ret just full one
    ))
	 
(defun flatNP (l)
  (collect-if #'(lambda (x) (when (fulll x) (equal (first x) 'np))) l))
;-
(defun opprs- (s)  ;try not to use this anymore
  "openNLP parser "
    (rsc (format nil "echo \"~a\"|cat> text" s))
    (run-ext (format nil "chnk.sh")))
;now w/o tmp file: ;v3 make so can run from main dir
(defun op_tk (s)
  "open parser, tokenizer"
    (run-ext "nlp/tk.sh" s))
 
(defun op_sd (s)
  "open parser, SentenceDetector"
    (run-ext "nlp/sd.sh" s))
 
(defun op_pt (s)
  "open parser, PosTagger"
    (run-ext "nlp/pt.sh" s))
 
(defun op_ch (s)  ;might want to focus on this
  "open parser, TreebankChunker"
    (run-ext "nlp/ch.sh" s))
 
(defun opprs (s)
  "openNLP parser "
  (op_ch (op_pt (op_tk (op_sd (clean4echo s))))))

;could make this w/a list of fncs to compose
; could even mk/compose ext fncs on fly /labels..
(defun opprs-ch (s) ;try
  (op_ch ;(op_pt (op_tk (op_sd 
			 (clean4echo s)));)))
;
(defun op-prs (s &optional (ch nil))
  (let* (;(prs (opprs s)) ;or opprs-ch
         (prs (if ch (opprs-ch s) (opprs s))) ;or opprs-ch
	 (needends (search "] [" prs))
         (pprs  (rm-strs '("," ":")
	   (simple-replace-string "/" "_" 
	      (simple-replace-string "[" "(" 
				      (simple-replace-string "]" ")" prs))))))
    (if needends (strcat "(" pprs ")")
      pprs)))
(defun op_prs (s)
  (eval-str2 (op-prs s)))
;
(defun nlp-3 (s)
  (list (op_prs s) (nlp-2 s)))

;have2fix or get rid of this like nyu stuff
(defvar *fl2* '())
(defun get-fl2 (n) 
 ;(cdr (assoc n *fl2* :test #'equal))
  (nth n *fl2*)
  )
;-
;(defun id2nyu (id) nil) ;need nyu.cl  ;also fully cached version in nlp3.cl
(defun id2nyu (id) id) ;need real version again
(defun id2nlp4 (id)
 (let ((s 
         ;(cdr (assoc id *fl2* :test #'equal))
         (get-fl2 id)
         ))
  (list :nyu (id2nyu id) :op (op_prs s) :sb (nlp-2 s))))
;-
(load "diff-sexp.lisp" :print t) ;http://tehran.lain.pl/stuff/diff-sexp.lisp
(defun id2nlp4diff (id) (let ((n2s (id2nlp4 id))) (diff-sexp (second n2s) (sixth n2s))))
;-
;could make a nlp23 where it looks @w/&w/o med-vocab(hypens)&puts out all options/maybe even between
;  when there is >1 hypenation option, that maybe shouldn't be fully merged ;focus on NP match though
;skip below  ;try not to use this anymore
(defun lex (fn) ;still needs2be broken dwn2words, think there is a tool for this
  "might want to try combo's or oba longestWords, try grep/etc from oba?"
    (run-ext (format nil "lexAccess -i:~a |cat> l.~a.tmp" fn fn)))
;-eof
