;dep2rdf.lisp bobak@gmail
;take stanford-parser/etc's dependency output from CoNLL-X format to a KM-like RDF/triples
;(defvar *trying* nil)
;(defun try () (setf *trying* 't)) (try)
(lkmq) ;(lkm2) ;2 loads u2.lisp ;km utils
;(defun km (a) (km::km a))
;(ql 'km) ;lkmq does this
(use-package :km)
;lcc.lisp ;(load "ls.cl" :print t) ;(load "le.cl" :print t) ;(load "lt.cl" :print t)
(load "lcc.lisp" :print t) ;(km::load-kb "sd.km")

;get rid of ref2 in u2.lisp:
(defun words-of (id) (gv id "words-of")) 
;in fact overhaul show-c* want show-cls ;use show-

;will use sp_2 parse&assert
;a few accessors
(defvar *sl* '()) ;get an assoc mk-sentence
(defun ssp (str) (setf *sl* (sp_2 str)))
(defun sn (n &optional (sl *sl*)) "sent num" (nth n sl))
 ;from stanford-parser ret
(defun g-st (spr) "sentence tree" (first spr))
(defun g-sd (spr) "depend" ;(rest (rest spr))
  (rm-nils (rest spr)))
  ;start w/printing but parse/assert after
(defun pre-upto-paren (str)  
  (let ((pn (search "(" str)))
    (when (numberp pn) (subseq str 0 pn))))
;add next2 to utils
(defun butfirst (s &optional (n 1)) (butfirst-n s n))
(defun butfirstlast (s &optional (n 1)) (butlast-n (butfirst s n) n))
(defun mk-word (w)
  (sv-cls w "Word"))
(defun print-dep (d) ;unused/get rid of
  "mk dep ins" ;fin filling slots:d1,d2 /rename
  (let* ((typ (clean_se (pre-upto-paren d)))
         (snl (positionsl '(#\( #\, #\)) d))
         (pr (subseq-s d snl))
         (d1 (string-trim " " (butfirstlast (first-lv pr)))) ;(d1, )
         (d2 (string-trim " " (butfirstlast (second-lv pr)))) ;(,d2 )
         (cd (clean_se d))
         (n ;(when typ (gentemp typ))
            (under_ cd) ;(gentemp cd) ;want gentemp that doesn't add anything if not there/find/mk1         
            ));want consisten wrd ins ids
    ;will need more gentemp, but only once per(sentence),then reuse w/in
     ;could prefix w/s# for sentence obj
    ;(print d) ;next break apart w/in parens
    (format t "~%~a,~a" d n)
    (mk-word d1)
    (mk-word d2)
    (sv n "d1" d1)
    (sv n "d2" d2)
    (sv-cls n typ) ;fix
    ))
(defun prs-dep (d)
  (let* ((snl (positionsl '(#\( #\, #\)) d))
         (pr (subseq-s d snl))
         (d1 (string-trim " " (butfirstlast (first-lv pr)))) ;(d1, )
         (d2 (string-trim " " (butfirstlast (second-lv pr)))) ;(,d2 )
         )
    (list d1 d2)))
(defun mk-dep (d)
  (let* ((typ (clean_se (pre-upto-paren d)))
         (cd (clean_se d))
         (n (under_ cd))
         (pr (prs-dep d))
         (d1 (first pr))
         (d2 (second pr)))
    (format t "~%~a,~a" d n)
    (mk-word d1)
    (mk-word d2)
    (sv n "d1" d1)
    (sv n "d2" d2)
    (sv-cls n typ) ;fix
    ))
(defun mk-sentence (spr)
  (let ((sn (gentemp "s")))
    (sv-cls sn "Sentence")
    (svs sn "spr" (clean_se (g-sd spr))) ;was spr, revisit
    )) ;if use gentemp above use here too
(defun mk-sentence-dep (di)
  (let ((sn (gentemp "s")))
    (svs sn "dep" di)
    (sv-cls sn "Sentence")))
(defun ptd (spr) "print tree+dep" 
  (print "tree") (print (g-st spr)) 
  (print "depend") 
  ;start w/mk-Communicate  incl who from/to,  cc/Communicate.km
  (let ((di (mapcar #'mk-dep (g-sd spr)))) ;was print-dep
    ;might test di (mk-sentence spr) 
    (mk-sentence-dep di) 
  ))
;would be easier to mk dep ins, then put in sent ins
(defun pas (&optional (sl *sl*)) "print all sentences" (mapcar #'ptd sl))
(defun s+p (str) "set&print" (ssp str) (pas))
