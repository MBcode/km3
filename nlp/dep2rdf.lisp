;dep2rdf.lisp bobak@gmail
;take stanford-parser/etc's dependency output from CoNLL-X format to a KM-like RDF/triples
(lkm2) ;2 loads u2.lisp ;km utils
;lcc.lisp
(load "ls.cl" :print t)
(load "le.cl" :print t)
(load "lt.cl" :print t)
(load-kb "sd.km")

;get rid of ref2 in u2.lisp:
(defun words-of (id) (gv id "words-of")) 
;in fact overhaul show-c* want show-cls ;use show-

;will use sp_2 parse&assert
;a few accessors
(defvar *sl* '())
(defun ssp (str) (setf *sl* (sp_2 str)))
(defun sn (n &optional (sl *sl*)) "sent num" (nth n sl))
 ;from stanford-parser ret
(defun g-st (spr) "sentence tree" (first spr))
(defun g-sd (spr) "depend" (rest (rest spr)))
  ;start w/printing but parse/assert after
(defun pre-upto-paren (str)  
  (let ((pn (search "(" str)))
    (when (numberp pn) (subseq str 0 pn))))
(defun print-dep (d)
  (let* ((typ (pre-upto-paren d))
         (n (gentemp typ)))
    (print d)
    ;(sv-cls n typ) ;fix
    ))
(defun ptd (spr) "print tree+dep" 
  (print "tree") (print (g-st spr)) 
  (print "depend") (mapcar #'print-dep (g-sd spr)))
(defun pas (&optional (sl *sl*)) "print all sentences" (mapcar #'ptd sl))
(defun s+p (str) "set&print" (ssp str) (pas))
