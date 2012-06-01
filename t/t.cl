;(al 's-xml-rpc)
;(defvar *xa* (s-xml:parse-xml-file "ha2.xml"))
;(defvar *cx* *xa*)
(defvar *cx* *xb*)
;(defvar *cx* *oh*)
(defvar *cx2* (fifth  (second *cx*)))
(defun under_f (str) ;better than cache version ;see m16_
  (let* ((wrds (string-to-words str))
	 (iname (str-cat_l wrds)))
    iname))  ;finish off w/a much smaller/better cache/ing
;I could almost just restart just using km/utils and get the metamap in  **make it much smaller**
;defun metamap (s)
(defun mmx_ (s)
  "call and start mm processing"
  (let* ((wrds (string-to-words s)) ;can skip&just use under_f
	 (iname (str-cat_l wrds))
	 (fx (str-cat2 iname ".xml"))
	 (fc (str-cat2 iname ".cl"))
	 (path (str-cat2 "cache/" fc))
	 ;(xs (run-ext "mmx" (format nil "\"~a\"" wrds))) 
	 ;(xso (s-xml:parse-xml-string xs))
	 (xso (s-xml:parse-xml-file (str-cat2 "xml/" fx)))
	 ;-;(xs (format nil "mmx \"~a\">cache/~a" wrds fx))
	 )
    (format t "~&len=~a" (len xso))
    ;-;(format t "~&~a" xs) ;&run by hand on domsetup this one time
    ;now make ins, and cache, like cache, **
    ;file:///Users/bobak/Downloads/lang/lsp/doc/book/HyperSpec/Body/m_w_open.htm
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out "~a" xso))
    ))
;look@s-xml for stream proc
;-ut
(defun rfind_ (s tree) ;work on
  (cond ((null tree) nil)
        ((atom tree)
         (if (equal s tree) tree nil))
        ((listp tree)
         (if (find s tree :test #'equal)
             (member s tree :test #'equal) ;(assoc s (list tree) :test #'equal)
           (or (rfind_ s (car tree))
               (if (cdr tree) (rfind_ s (cdr tree))))))))

(defun rfind_all (key alst)
  (let* ((tree (copy-tree alst))
         (temp-result (rfind_ key tree))
         (results
          (loop while (and tree temp-result)
              collect temp-result into list-of-sub-alsts
              do (setq tree
                   (let ((test-tree (remove temp-result tree
                                            :test #'equal)))
                            (if (equal test-tree tree)
                                nil test-tree))
                     temp-result (rfind_ key tree))
              finally (return list-of-sub-alsts))))
    results)) 
;-
;parse the xml
(defun UttText () (last-lv (rfind :|UttText| *cx*)))
(defun Phrases-Count () (first-lv (get_nums (last_lv (rfind :|Phrases| *cx*)))))
;(defun PhraseText () (last-lv (rfind :|PhraseText| *cx*)))
(defun PhraseText () (mapcar #'last-lv (rfind_all :|PhraseText| *cx2*)))
;(defun LexMatch () (last-lv (rfind :|LexMatch| *cx*)))
(defun LexMatch () (mapcar #'last-lv (rfind_all :|LexMatch| *cx*)))
(defvar *2tst* '( #'UttText #'Phrases-Count #'PhraseText #'LexMatch))
(defvar *s2tst* '(UttText Phrases-Count PhraseText LexMatch))
;function file:///Users/bobak/Documents/www/jtra.cz/stuff/lisp/sclr/function.html
;(defun funcalll (f arglst) (funcall f @arglist))
(defun fnc-l (fnc the-seq &key (start 0) (end (length the-seq)))
    (reduce fnc the-seq :start start :end end)) 
(defun funcalll (f arglst) (fnc-l f arglst)) ;if reduce-able
(defun fncall (f &rest args) (funcalll (symbol-function f) args))
(defun fncall0 (f) (funcall (symbol-function f)))
(defun tstt(&optional (tst *s2tst*))
  (mapcar #'(lambda (f) (format t "~&~a:~a" f (fncall0 f))) tst))
 
;might want a way to go from num in fl2, to setting *cx* and *cx2*
;defun setcx (an) actually those are atoms/num-names, want ft this time/as a start
;(defun setcx (n)
;  (let (;(atms (elt *fln* (1- n)))
;	(txt (cdr (assoc n *ft2*))))
;    (setf *cx* *oh*)
;    (setf *cx2* (fifth (second *cx*))) 
;    (format t "~&~a:~a" txt (mapcar #'len *cx2*))))
;-looks like stella has a diff parse--
;;stella can  (read-xml-expressions "adult.xml")
;; go w/this even though just like s-xml w/more
;; as there might be more2wrk off this
;(defun UttText2 () (rfind '<UttText> *cx2*))
(defun Utterance () (rfind '<Utterance> *cx*))
(defun Phrases () (rfind '<Phrases> *cx*))
;(defun PhrasesCount () (len (rfind '<Phrases> *cx*)))
(defun PhrasesCount () (len (Phrases)))
(defun LexMatch2 () (mapcar #'last-lv (rfind_all '<LexMatch> *cx*)))
;-end stella-
;-lets start to parse the mm xml, esp the final mapping -more above already
;assume one UttText, and check it against the top Mappings
(defun mm-UttText (&optional (xl *cx*)) (last-lv (rfind :|UttText| xl)))
(defun xlNum (l)
  (first-lv (get_nums (last_lv l))))
(defun xlCandidateMatched (l)
  (last-lv (rfind :|CandidateMatched| l)))
(defun xlCandidateCUI (l) (xlNum (rfind :|CandidateCUI|)))
(defun num-Mappings (xl) (xlNum (rfind :|Mappings| xl)))
(defun mm-Mappings (&optional (xl *cx*))
  (let* ((n ;(xlNum (rfind :|Mappings| xl))
	  (num-Mappings xl))
         (mapngs (mapcar- #'xlCandidateMatched (rfind-all :|Mapping| xl)))
	 (mapng (first mapngs))
	 (cui (xlCandidateMatched (first mapng)))
	)
    (when (neq n 1) (format t "~&Have ~a mappings" n))
    (list mapng cui)
  ))
(defun mm-check (&optional (xl *cx*)) 
  (let* ((utt (first-lv (mm-UttText xl)))
	 (mapng- (mm-Mappings xl))
	 (mapng (first-lv (first mapng-)))
	 (cui (second mapng-))
	 (nm (num-Mappings xl)))
    (if (and (eq nm 1) (equal utt (string-downcase mapng))) 
      (format t "~&assoc ~a with ~a" utt mapng-) ;(format t "~&assoc ~a with ~a" utt cui)
      (format t "~&check: ~a ~a ~a" nm utt mapng))))
   
;now use this below in every nctNum2conditionCUIs call

;-
;; ==i would like to write the .ste version of this now
;;
;; Duke:
;;
(defun conditions (l)
  "all condition-s from NCT xml"
  (mapcar- #'second (rfind-all :|condition| l)))

(defun cnc (nct)
  "get current NCT" ;via dir or..?
  (s-xml:parse-xml-file nct))

(defun rm-pre3 (s) 
 ;(when (> (len s) 3) (subseq s 3))
  (if (> (len s) 3) (subseq s 3)
    "")
  )


(defun ones-plc (n) 
  (when (integerp n) (mod n 10)))
(defun form8 (n)
  (format nil "~8,'0d"  n))
(defun nctfile (n)
  "nct number to the file path"
  (str-cat "nct/" (ones-plc n) "/NCT" (form8 n) ".xml"))

(defun file2num (s)
  "file path, to the nct number"
  ;(subseq s 3 11)
  (first-num (rm-ext (rm-pre3  s)))
   )

(defun second2last (l) ;make a version that checks for MM/etc if don't parse it all now
  (second (last3 l)))
;--
(defvar *alph* '(#\a #\b #\c #\d #\e #\f #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s))
(defvar *alphs* '("a" "b" "c" "d" "e" "f" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"))
(defun nctNum2conditionCUIs (n)
  (let*  ((fn (nctfile n))
	  (fx (s-xml:parse-xml-file fn))
	  (cl (conditions fx))
	  (prs (mapcar #'(lambda (c s) (cons (str-cat n s) c)) cl *alphs*))
	  )
    (mmtx_pairs prs)))
(defun nctNums2conditionCUIs (&optional (nums *try-nct-s*))
  (mapcar- #'nctNum2conditionCUIs nums))

(defvar *allc* nil)
(trace nctNum2conditionCUIs nctfile conditions)
(defun tst1 () ;might load nct/1*.cl here?
  (setf *allc* (nctNums2conditionCUIs)))

;------------skip below:-
(defun nct-dir-nums (n9)
 ;(mapcar #'(lambda (s) (first-num (rm-ext (rm-pre3  s)))) (ls (str-cat "nct/"  n9)))
  (mapcar #'(lambda (s) (file2num s)) (ls (str-cat "nct/"  n9)))
  )

;(with-open-file (out "alln.cl" :direction :output) (loop for n9 from 0 to 9 do (print (mapcar #'(lambda (s) (first-num (rm-ext (rm-pre3  s)))) (ls (to-str n9))) out)))
;(with-open-file (out "alln.cl" :direction :output) (loop for n9 from 0 to 9 do (print (nct-dir-nums n9) out)))

; (defvar *n0* (mapcar #'(lambda (s) (first-num (rm-ext
;(loop for n9 from 0 to 9 collect (mapcar #'(lambda (s) (first-num (rm-ext (rm-pre3  s)))) (ls (to-str n9))))
;(loop for n9 from 0 to 9 collect (mapcar #'(lambda (s) (first-num (rm-ext (rm-pre3  s)))) (ls (str-cat "nct/" n9))))
