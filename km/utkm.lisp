#+km25
(eval-when (:execute :load-toplevel :compile-toplevel)
  (setq *readtable* *km-readtable*))	; So that the dispatch macro #$ is recognized 

;todo: give full path from a class all the way to the top
;
;only completely generic thing taken out of mmkm.lisp
;it should build on more generic work, some of which will be in ../c/tools ..
(defun eval-str2km (s)  ;now ka below as well, starting to prefer ka-, even through sv/gv
  (when (stringp s)
      (eval-str (str-cat "(km '#$" (rm_comma s) ")"))))

;consider a eval2km that could also take a list,  which could be tricky
;well at least get a :set formatting4assert:
;defun ids2km-set (idl &optional (add-star t))
; "take a set of str/sym and turn into km :set of ins IDs"
 
;defun get-cui-slot (c sn)
(defun get-ins-slot (i sn)  ;(gv i sn)
     (km (append '(|the|) (list sn) '(|of|) (list i))))

(defun  is-a-p (ins cls)
  (first-lv ;was full , should ki/symbol
   ;(km (list ins '|&+| (list '|a| cls)))
    (km (list (kin ins) '|&+| (list '|a| cls))) ;dec
    ))
;useful, might put in mmkm/mkm
(defun noun-p (ins) (is-a-p ins '|noun|))
(defun modifier-p (ins) (is-a-p ins '|modifier|))
(defun lil-np-p (ins) (not (modifier-p ins))) ;only defn I have of the 'np' w/in the NP
;(defun noun_phrase-p (ins)
;  (is-a-p ins 'NOUN_PHRASE))
(defgeneric type_of (a))
(defmethod type_of (a)  (gv a "instance-of"))
(defmethod type_of ((l List))
    (mapcar #'type_of l))
(defun type_of+ (i)
    "assure list+"
      (list+ (type_of i))) 
(defun type_of1 (i) (first-lv (type_of i)))
(defgeneric txt_type (i))
(defmethod txt_type (i) (when i (str-cat (ktxt i) "_" (type_of1 i))))
(defmethod txt_type ((l List)) (mapcar #'txt_type l))
;type_of could also be gv ins "classes"
(defun atom-p2 (i) (is-a-p (kin i) '|atomPhrase|))

(defun is_a-p (i cls)
  (member cls (type_of+ i)))
;(defun atom-p (i) (is_a-p (kin i) '|atomPhrase|))
(defun atom-p (i) (is_a-p  i '|atomPhrase|))
(defun crit-p (i) (is_a-p  i '|fullTxt|))
(defun cmp-p (i)  ;keep this way till can be sure it works for is-a-p, might have to fix it
 ;(member '|numericComparator| (list+ (type_of i)))
  (is_a-p i '|numericComparator|))
(defun noun_phrase-p (ins)
 ;(member 'NOUN_PHRASE (type_of+ ins))
  (is_a-p ins 'NOUN_PHRASE))
(defun tui-p (ins)
  (is_a-p (ki ins) '|tui|))
;consider alst2km ((:pair k1 v1) (:pair k2 v2))
;already: defun affix-pair-prefix(input) in controller-miscellaneous.lisp
;
;generic/macro: the-of sn ins (km0 `(|the| sn |of| (|a| ,concept))
;  (car (km0 `(|the| |text-gen| |of| ,inst))))
;;;Issues a KM slot lookup
;;;(defun ps-slot-lookup(query-frame query-slot)
;;;  (ps-km-query `(|the| ,QUERY-SLOT |of| ,QUERY-FRAME)))
;;;  from "controller-km-bridge.lisp" 58L, 1913C                         30,3-10       All
;;;--
;Might be interesting to turn any alist of (s v)'s into an ins, of either &/or clos/km
;look@ *rins* construction right now, might go off that?
;
;-might pull in bits about class/ins naming ;want some generic things to assure even ins names
;new:
(defun txsnr () (taxonomy '|snr-Group|)) ;all     semantic relation/slots
(defun txisa () (taxonomy '|is_a|)) ;hier part of semantic relation/slots
(defun txlui () (taxonomy '|lui|))  ;anything that can ground out to umls lui TUI->CUI->lui<-POS
(defun txusr () (taxonomy '|user|))  ;ergo1 modifier->Modifier so diff from POS version
(defun txphr () (taxonomy '|Phrase|)) ;subsumed by txlui
(defun txaphr () (taxonomy '|atomPhrase|)) ;subsumed by txlui
(defun txnc () (taxonomy '|numericComparator|)) ;subsumed by txlui
(defun txnp () (taxonomy 'NOUN_PHRASE)) ;subsumed by txlui
(defun txbnp () (taxonomy '|Noun_PHRASE|)) ;subsumed by txlui
(defun txnoun () (taxonomy '|noun|)) ;subsumed by txlui
(defun txmodifier () (taxonomy '|modifier|)) ;subsumed by txlui
;-
(defun km-name2 (i)
    (string-upcase (km-name i)))
;-
;generalize the pre-append if not prefixp ;KM ID
(defgeneric ki (s &optional pre)) ;"make sure km ins always starts w/*"  ;keep original type
;I could make an s-cat to call str|symbol&havein 1fnc, but would have to decide about mixed inputs
(defmethod ki ((s String) &optional (pre ""))
  (if (prefixp "*" s) s
    (str-cat "*" pre s)))
(defmethod ki ((s Symbol) &optional (pre ""))
  ;if (prefixp "*" s) s
  (if (prefixp "*" (symbol-name s)) s
    (sym-cat "*" pre s)))
(defmethod ki ((s List) &optional (pre ""))
  (mapcar #'ki s))
;
(defun ki_ (s) ;str  ;from/to #s left along, but for ins-names, still want it
  "clean up 2 KM id str_" ;like txt2kmId
  ;(ki (under_ (trim-punct2 s)))
  (let ((str  (underscore ;under_ 
		(trim-punct2 (to-str s)))))
    (if (digit_prefixp str) ;(numberp (numstr str)) 
      str
      (ki str))) ;so #'s not *ed
  ) ;if numberp numstr, then skip ki,  ;in :arg might mix ins &#s
(defun k_i (s) ;str  ;from/to #s left along, but for ins-names, still want it
  "km id"
  (ki (under_ (trim-punct2 (to-str s)))))
;
(defun kin (s)
  "km id  interned"
  (let ((i (ki s)))  ;2bad can't make where cls&relation names not *ed
    (if (symbolp i) i 
      (intern i))))
;
(defun show- (s)
  "w/o*"
  (showme (intern s)))
;
(defun show-all (s)
  (mapcar #'show (explode- s)))
(defun show_all (s)
  (mapcar #'show (explode- (clean-se s)))) ;clean more agressively/finish
(defun show-c (s)
  "print out our word ins like the columbia format"
  (let* ((wo (words-of s))
	 (wc (when wo (first-lv (type_of wo))))  ;pos1 also get _pos part at least, via txt_type
	 (wa (when wc (gv- wc "abbrev"))))
    (format nil "~&~a~a/~a" s wa (type_of s))))
(defun show-c1 (s)
  "print out our word ins like the columbia format" ;little less like it but better print
  (let* ((wo (words-of s))
	 (tt (txt_type s))
	 (wc (when wo (first-lv (flatten- (type_of wo)))))  ;pos1 get _pos part , via txt_type
	 (wa (when wc (gv- wc "abbrev"))))
    (format nil "~&~a~a" tt wa))) ;not like columbia any longer
(defun gv-abbrev (wc) (gv- wc "abbrev"))
(defun show-cl (s) ;maybe rename
  "get parts for cmp w/columbia version of a word/phrase" ;which will be done in colu.lisp
  (let* ((wo (words-of s))
	 (tt (txt_type s))
	 (wca (when wo (flatten- (type_of wo))))  ;pos1 get _pos part , via txt_type
	 (wc (first-lv wca))
	 (wa (when wc (gv- wc "abbrev"))) ;should mapcar to get them all -finish
	 (waa (mapcar-  #'gv-abbrev wca))
	 ) 
    (list wo tt wca waa)))
;(defun cmp-cw (s) "cmp my&columbia parse for a word") ;diff version in colu.lisp 2cmp w/cea-parts lst
(defun clean_se (s)
   (simple-replace-string "(. " "( "
    (rm-strs '("(. .)" "(, ,)" "(: -)" "(: ;)"  "," ":" "(" ")" ";" "\\" "\"" "'") s)))
(defun show_c (s)
  (mapcar #'show-c1 (explode- (clean_se s)))) ;
(defgeneric show (s))
;defun show (s)
(defmethod show (s)
  "km showme for sym|str even w/o *"
  (showme (kin s)))
(defmethod show ((l List))
  (mapcar #'show l)) ;was show already a fnc?check/looks ok
;
(defun txt2kmId (txt &optional (idfnc #'under_f)) ;txt2ki, ki_
  "txt->km *id"
  (ki (funcall idfnc txt)))
;
(defun strl (s)
  "str about 2eval to (km slt) lst should have () around it"
  (format nil "(~a)" s))
(defun quote-strl (s)
  "str about 2eval to lst should have '() around it"
  ;(format nil "'(~a)" s)
  (str-cat "'" (strl s)))
;
(defgeneric ka (sl)) ;assert string or list
;KM assert
;(defmethod ka ((s String))  ;like eval-str2km above
;  "KM assert string"
;  (let ((sl (if (prefixp "(" s) s
;	      (strl s))))
;    (eval-str (str-cat "(km-unique '#$" (rm_comma sl) ")"))))
;could of sent in optional for getMulti, but just get back &if just one ret first-lv of it
(defun ka1 (sl) "for a single value" ;needs parens
  (eval-str (str-cat "(km-unique '#$" sl ")")))
(defun full1 (l) (and (fulll l) (eq (len l) 1)))
(defmethod ka ((s String))  ;like eval-str2km above
  "KM assert string"
 (when (full s) ;so no ""
  (let* ((sl (if (prefixp "(" s) s
	      (strl s)))
	 (ret (eval-str (str-cat "(km '#$" (rm_comma sl) ")"))))
    (if (full1 ret) (first-lv ret) 
      ret)))) ;if 1thing get it, else get list, ka+ 
(defmethod ka ((s List))
  "assert list2 km"
 (when (fulll s)
  (ka (implode-l s))))
;
(defun ka- (&rest args) ;fix
  "km assert w/o a list, just args"
  (when *dbg* (format t "~&ka:~a,~a"  args (type-of args)))
 (when (fulll args)
  (funcall #'ka args)))
  ;had apply, but ka is a fnc that works on 1list, not takes n args
;
;2.4.0 - The semantics of KM expressions:
;   (the instances of ...)
;   (the all-instances of ...)
;   and the equivalent Lisp functions:
;        (all-instances <class>)
;	 (immediate-instances <class>)
(defun ins-of (cls) ;maybe rename as could be short for instance-of; or might write as 'class'
  (ka- "the instances of " cls))
(defun ins_of (cls) (all-instances cls)) ;2.4.0
(defun ins_of_only (cls) (immediate-instances cls)) ;2.4.0
(defun an-ins-of (cls) (first-lv (ins-of cls)))
(defun tmp-ins-of (cls) ;try2avoid
      (ka- "an instance of " cls))
(trace tmp-ins-of)
(defun an_ins-of (cls)
  (let ((try (an-ins-of cls)))
    (if try try
      ;(ka- "an isntance of " cls)
      (tmp-ins-of cls))))
(defun gvs (cls sn) ;gv-cls
  "get-value cls slot"
  (let ((c (if (symbolp cls) (symbol-name cls)
	     (to-str cls))))
   ;(gv (an_ins-of cls) sn)) ;found err n25
    (gv (an_ins-of c) sn)))
;
(defun isa-p (i cls)
  (ka- i '|&+| (list '|a| cls)))
(defun cmp-p- (i) ;check on
  (isa-p i "numericComparator"))
;
(defun gv- (i sn)  ;GetValue could be a cls/relation, so don't add *
  "get of cls/relation"
  (ka- "the" sn "of" i))
(defun gv-cls (cls sn) ;gv-cls instead of gvs, by skipping an ins
  (let ((c (if (symbolp cls) (symbol-name cls)
	     (to-str cls))))
    (gv- c sn)))
;defun gv (i sn)  ;GetValue
(defgeneric gv (i sn))
(defmethod gv (i sn)  ;GetValue
  "get km value"
 ;(ka- "the" sn "of" (ki i))
  (gv- (ki i) sn))
(defmethod gv ((l List) sn)
  (mapcar #'(lambda (i) (gv i sn)) l))
(defun gv+ (i sn) "GetValue always as a list" (list+ (gv i sn))) ;as ka returns firs-lv if only 1
(defun gv+2 (i sn) "GetValue args/etc as a list" (list+2 (gv i sn))) ;as ka returns firs-lv if only 1
(defun gva (i sn) "get-value for :args" (mapcar- #'rest (gv+2 i sn)))
(defgeneric gv1 (i sn))
(defmethod gv1 (i sn)  (ka- "the1" sn "of" (ki i)))
(defmethod gv1 ((l List) sn) (mapcar #'(lambda (i) (gv1 i sn)) l))
(defun gv2 (i sn)  (ka- "the2" sn "of" (ki i)))
(defun gv3 (i sn)  (ka- "the3" sn "of" (ki i)))
(defun gv4 (i sn)  (ka- "the4" sn "of" (ki i)))
;
(defun class_of (i)
  (gv i "instance-of"))
;
(defun quoteable-p (s) ;str
  "if space then put quotes before print/assert"
  (when (stringp s) (position #\Space s)))
(defun snvs (sn val &optional (qtval 'auto))
  "slotname value string for km"
 ;let ((sval (if (and (eq qtval 'auto) (quoteable-p val)) (quote-str val) val)))
 (let ((sval (if (or (not qtval) (and (eq qtval 'auto) (not (quoteable-p val))))  val
	       (quote-str val))))
  (format nil "(~a (~a))" sn sval)))
;-add something for quoting strings ;&optional (qtval t) ;or the quoateable-p
(defun sv (i sn val &optional (qtval 'auto) (also nil))  ;SetValue
  "set km value"
 (let ((has (if also "also-has" "has")))
  (ka- (ki i) has (snvs sn val qtval))))   ;consider "also-has", so multislot but not unified&&
(defun svif (i sn val &optional (qtval 'auto) (also nil))  ;Set if Value 
  (when val (sv i sn val qtval also)))
(defun sv-from (i sn fnc &optional (qtval 'auto) (also nil))   ;SetValue s from a fnc
  "set km values from a fnc on i"
  (sv i sn (funcall fnc i) qtval also))
(defun sv_from (i sn &optional (qtval 'auto) (also nil))   ;SetValue s from a fnc
  "set km values from a fnc on i, w/same name as sn"
  (let ((idfnc (symbol-function sn)))
    (sv-from i sn idfnc qtval also)))
;sval, sv using an alist
(defun sv_ida (ida sn &optional (qtval t))   ;SetValue s from a cons pair ;id.val
   (sv (car ida) sn (cdr ida) qtval)) 
(defun sv-pr (i pr &optional (qtval t))   ;SetValue s from a cons pair ;sn.val
   (sv i (car pr) (cdr pr) qtval))
(defun sv-al (i al)   ;SetValue s from alist
  "set km values from alst"
  (mapcar #'sv-pr ;(lambda (pr) (sv i (car pr) (cdr pr)))
	  al))
;	make sure there is a gv-cls to get a class value, by not putting a *in ins name; it is: gvs
;(defun sv-cls (i cls) (sv i "instance-of" cls))
(defun sv-cls (i cls-) 
  "set class(es) of instance"
  (let ((cls (implode-l (list+ cls-))))
    (sv i "instance-of" cls nil))) ;don't quote
;(defun sv-cls (i cls) 
;  (when (fulll cls) (mapcar- #'(lambda (c) (sv-cls i c)) cls))
;  (sv i "instance-of" cls 'auto t)) ;also

;;			add-star=ki_ instead of just under_
;;defun ids2km-seq (idl &optional (add-star t) (add-quote nil))
(defun ids2km-seq (idl &optional (add-star t) (add-quote nil) (idfnc #'under_f))
  "take a seq of str/sym and turn into km :seq of ins IDs" ;used in words-seq and put-slot-cui
  (when (fulll idl)
    (let* ((idls (mapcar #'to-str idl))
	   (id-l (append '(":seq ") (if add-star (mapcar #'(lambda (i) (txt2kmId i idfnc)) idls)
				  (mapcar idfnc idls))))
	   (bare-l (implode-l id-l)))
      (if add-quote (quote-strl bare-l)
	(strl bare-l))))) 
;
(defgeneric words-seq (w))
(defmethod words-seq ((txt String))
              (words-seq (explode- txt)))
(defmethod words-seq ((wl List))
              (ids2km-seq wl))
;
(defun svs (i sn vals)  ;also -has
  "sv :seq"
  (sv i sn (words-seq vals) nil t))

(defun svs2 (i sn vals)  ;also -has
  (sv i sn (ids2km-seq vals t nil #'ki) nil t))

;also a test of making above more simple
(defun km-args (al &optional (add-quote nil))
  "list to go to :args"
  (let* ((als (mapcar #'to-str (list+ al)))
	 (a-l (append '(":args ") (mapcar #'ki_ als)))
	 (bare-l (implode-l a-l)))
      (if add-quote (quote-strl bare-l) ;see how oft used
	(strl bare-l)))) 
;setVal :args
(defun sva (i sn vals)  ;also -has
  "sv :args"
 ;(ka- (ki i) "has" (km-args vals)) ;dwn a ()s , &didn't have sn
  (sv i sn (km-args vals) nil t) ;was auto
  )

