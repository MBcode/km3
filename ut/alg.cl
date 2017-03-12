;start to test asserting km kb through algernon-tab to a protege pprj
;(tell ((:OR ((:IS-CLASS criteria)) 
;                     ((:USE-KB :KB1 Protege  "/Users/bobak/wrk/sf/appli/km/run/j/km.pprj")))))
(defun p-alg-top (&optional (strm t) (file "alg.pprj")) ;(file "km.pprj")
  "print top of tmp alg file"
 (format strm "(tell ((:OR ((:IS-CLASS criteria)) ((:USE-KB :KB1 Protege  \"~a\")))))" file))
 
(defun alg (fn)
  "assert the tmp algernon file"
   (rsc (format nil "alg ~a" fn)))

(defun lstr (l)
  (if (listp l) (format nil "~{~a~^ ~}" l)
    l))
;-
;might want general way to put a general ins, it any format
(defun p-alg-slt (s v &optional (vname "tname") (strm t))
  "print an alg slot/value"
 (formatstrmt "(~a ?~a ~a)" s vname ;v
	 (quote-str3 (if (listp v) (lstr v) v))
	 ))
(trace p-alg-slt)

(defun p-alg-nom (nom &optional (vname "tname") (strm t))
  "print alg ins head"
 (formatstrmt "(:add-instance (?~a ~a)" vname nom))

(defun p-alg-ins (sl vl nom &optional (vname "tname"))
  "print alg ins"
  (p-alg-nom nom vname)
  (mapc #'(lambda (s v) (p-alg-slt s v vname)) sl vl))

;-
(defun km-sym-plist (str)
  (km-symbol-plist (intern str)))

(defun km-plist-svs (pl)
  (case (first pl)
    ('Done (fourth pl))
    ('OWN-PROPERTIES (second pl))
    ))
;asserting some things as instances when they aren't

(defmethod km-concept-svs ((concept String))
  (km-concept-svs (intern concept)))
(defmethod km-concept-svs ((concept Symbol))
  (km-plist-svs (dereference (km-symbol-plist concept))))

(defvar *sstypes* '(|instance-of| |instances| |superclasses| |subclasses|))
(defun sstypep (slt)
  "is is a standard slot type"
  (member slt *sstypes*))
(defun km-assoc-sv (key sv &optional (dflt '|Thing|))
  (let ((sa (assoc key sv)))
    (if sa (first (first (cdr sa)))
      dflt)))
(defun km-super-sv (sv &optional (cls '|Thing|))
  (km-assoc-sv '|superclasses| sv cls))
(defun km-ins-sv (sv &optional (cls '|Thing|))
  (km-assoc-sv '|instance-of| sv cls))

;http://algernon-j.sourceforge.net/doc/commands/
;:ADD-RELATION	(name (from-class to-class))
;(format stream "~&(tell ((:add-relation (~a ~a))))" concept svstrs) 
;Format is (:ADD-RELATION name (from-class to-class) [options])
(defvar *dflt-relate* '(Thing Thing))
;     (do-objects concept       - No, need the dereferenced list
;; setting the symbol-plist was not safe because a symbol's property list is a global
;; resource that can contain information established by unrelated programs - for example
;; by the LW compiler (and probably other Lisp compilers).
;; (print `(setf (symbol-plist ',concept) ',(symbol-plist concept)) stream)
;(print `(put-list ',concept ',(dereference (km-symbol-plist concept))) stream)
;check if cls has to be just one,  ;could use Thing as a tmp?
;(print `(tell (((:add-instance (?t ',cls) (:name ?t ',concept) ',(dereference (km-symbol-plist concept))) stream)
                ;(print `(km-add-to-kb-object-list ',concept) stream) ;no alg analog
(defun p-alg-concept (concept &optional (stream t))  ;was a lambda being worked up in fastsave-alg-kb
  "going to focus on km instance-of to alg :add-instance" ;might also of slot 2 :add-relation
  (let* (;(cls 'Thing) ;find fnc to get cls from concept
	 (svl (km-concept-svs concept))
	 (cls (km-ins-sv svl)) ;was super, but just want instance-of
	;(typ (if (eq cls '|Slot|) "relation" "instance"))
	 ;(nsvl (fkm2alg-sv svl)) ;insert ?t etc, incl mcard-list
	 (nsvl (if (eq cls '|Slot|) *dflt-relate* ;(mapcar #'(lambda (x) (first (cdr x))) svl) 
		 (fkm2alg-sv svl))) 
	 ;(svstrs (p-svs nsvl))
	 ;(svstrs (p-svs (if (eq cls '|Slot|) svl nsvl))) ;no var in relations
	 (svstrs (if (eq cls '|Slot|) nsvl (p-svs nsvl))) ;no var in relations
	  )  ;print ins, like p-alg-ins above
	;;(print `(tell (((:add-instance (?t ',cls) (:name ?t ',concept) ',nsvl)))) stream)
	;;            			check if name should be a str
	(if (eq cls '|Slot|) 
	 (format stream "~&(tell ((:add-relation ~a ~a)))" concept svstrs) 
	 (format stream "~&(tell ((:add-instance (?T ~a) (:name ?T ~a) ~a)))" cls concept svstrs) 
	 )
	#+IGNORE 
	(if (eq cls '|Slot|) ;if relation don't need fkm2alg-sv's ?t in middle
	 (print `(tell (:add-relation (',concept) ',svstrs)) stream)
	 ;(print `(tell (:add-relation (?t ',cls) (:name ?t ',concept) ',svstrs)) stream)
	 (print `(tell (:add-instance (?t ',cls) (:name ?t ',concept) ',svstrs)) stream)
	);could insert the (proper) start list into the sv list 
	))

;to get taxonomy, maprecursive on make-tax
(defun algtax (v)
  (cond 
    ((prefixp "I" v) nil) ;skip ins for now
    ((prefixp "?" v) nil) ;skip unk ins for now
    (t (strcat "(" v)) ;will have to close it
    ))
(defun p-alg-tax ()
  (map-recursive #'remove-nils
   (map-recursive #'algtax (make-tax))))
;-
(defun km2clpInsname (in)
  (if (prefixp "*" (if (symbolp in) (symbol-name in) in)) (format nil "[~a]" in) 
    in))
(defun mcard-list (pl)
  (when pl
    (if (listp pl) (mapcar #'km2clpInsname (remove '&& (flatten- pl)))
      (quote- pl) ;(if (stringp pl) (quote-str3 pl) pl)
      )))
;(trace km2clpInsname)
;(trace quote-str3)
;maybe see if [] w/in and not quote?

(defun mcard_list (pl)
  "check cardinality, for now just if >1 value for list, otherwise value"
 (when pl
  (if (not (listp pl)) 
    (if (stringp pl) (quote-str3 pl)
      pl)
    (if (> (length pl) 1) pl
      (mcard-list (first pl))))))
;(trace mcard-list)  ;;iterate through sets, and link *ins as [*ins] ;finish
;someplace still have to only have alg :add-instance if '|instance-of| exits

(defun fkm2alg-sv (svl)
  "give alst of ins slot/val-s in fkm ret in alg format"
  (mapcar #'(lambda (sv) (let* ((s (car sv)) (v (cdr sv))
					     (v- (quote-str3 (lstr (mcard-list v)))))
			   `(,s ?t ,v-))) svl)) ;or use p-alg-slt above
;some similar km things, like km-format? might look there too?
(defun p-sv (sv)
  (if (sstypep (first sv)) ""
    (format nil "~a" sv)))
;could ret nil & remove-nils
(defun p-svs (svl)
  "slot val lst, turned into str, if an ins"
  (format nil "~{~a~^ ~}" (mapcar #'p-sv svl)))

;(trace p-alg-concept)
;(trace get-all-concepts)
;from loadkb.lisp part of KM
;;; Thanks to Francis Leboutte for this.
;;; This new version:
;;; - uses km-symbol-plist to make fastsave-kb portable (see comment below)
;;; - produces a more compact file
;;; - has a compile argument: to compile the fkm file.
;;;  Loading the compiled file should be faster. On LispWorks 4.4, fastloading a compiled file
;;;  instead of a fkm file is about 20% faster.
;defun fastsave-kb (file &key (reset-kb t) (compile nil))
(defun fastsave-alg-kb (file &key (reset-kb t) (compile nil))
  (let ((stream (tell file)))
    (p-alg-top stream) ;new
    (when *using-km-package*
      (print '(in-package :km) stream))
    (let ((*package* (if *using-km-package* (find-package :km) *package*)))
      (when reset-kb
        (print '(reset-kb) stream))
;     (do-objects concept       - No, need the dereferenced list
      (mapc #'(lambda (c) (p-alg-concept c stream))  ;was (get-all-concepts) finish by doing others
	    (all-instances '|Thing|)
	    )
      ;(mapc #'(lambda (km-parameter) ;alg won't use the globals/etc
      ;          (print `(setq ,km-parameter ',(eval km-parameter)) stream))
      ;      (append *km-behavior-parameters* *km-state-parameters*))
      (close stream)
      (format t "~a saved!~%NOTE: Load this file using (fastload-kb ~s), not (load-kb ~s)~%"
              file file file)
      (when compile
        (compile-file file))
      t)))

(defun t1 ()  (p-alg-tax))
(defvar *tax* (p-alg-tax))
(defun t2 ()  (fastsave-alg-kb "t.km"))
 
