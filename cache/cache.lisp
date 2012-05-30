; -would like to get away from 1A like id's and only use inputs-
;mem.lisp has too much that isn't used; &a problem, so a good time to clean house
; only spec cache subdir&fnc-name once, by using symbol-function /etc
;defgeneric cache-fnc-i-val (fnc i val &optional (cdir "cache")) ;i==id/input 
;defun cache-fnc-i-val (fnc i val &optional (cdir "cache")) ;i==id/input 
(defparameter +cdir+ "cache/")
;(defparameter +hyph+ (list #\/ #\.))
;(defparameter +hyph+ (list #\/ #\. #\[ #\]))
(defparameter +hyph+ (list #\/ #\. #\[ #\] #\> #\<))
;
(defun cache-dir (fnc &optional (cdir +cdir+))  
  (str-cat cdir (string-downcase (symbol-name fnc)))) 

(defun under_f (str)  ;might to to utils
  "make the filename safer, w/underscores and get rid of some chars"
  (under_ (all2hyphen str +hyph+) ;(slash2hyphen str)
	  ))
(defun under_f2 (str)  
  "for osx file lengths"
  ;(mx250 (under_f str))
  (mx250 (under_f (str-cat_l str)))
  )
(defun sl-p (sl) (and (fulll sl) (stringp (first-lv sl))))
(defun cache-fi (fnc i  &optional (cdir +cdir+)) ;i==id/input 
  "sym-fnc id/in get or run fnc&save"
  (let ((file (str-cat (cache-dir fnc) "/"  (str-cat (under_f2 i) ".txt"))))
    (if (file-exists-p file) (progn (format t "~&list-lines:~a" file) ;dbg
			       (list-lines file))  ;assume full for now
      (let* ((vall (funcall (symbol-function fnc) i)) ;want ret list just like list-lines
	     (valsl (if (sl-p vall) vall (list (to-str vall))))) ;-list of strings-
	(if (fulll valsl) (save-lines valsl file)
	  (warn (format nil "can't save:~a to ~a" vall file)))))))

(defun internup (str)
  (intern (string-upcase str)))
;also
(defun str2fnc (fs) (symbol-function (intern fs))) ;so don't have2send in 2versions of same name
;-now to replace mem.lisp fnc&get this going again
(defun file+id-file (fnc i)
  (cache-fi (internup fnc) i))
;&over this:   ,here i split into id &str (input value), for most
(defun fnc-id_ (fnc-str fnc id str) ;str is the real Input-value
  (cache-fi (internup fnc-str) str))
;mv2below:
(defun fnc_id (fnc-str id)  ;use str2fnc to call fnc-id_ if needed
  (let ((str (txt id)))  ;instead use accessor to id-> txt/str
    (cache-fi (internup fnc-str) str)))
;or just use the version from mem.lisp:
;
;(defun fnc+id-file (fnc id) ;used to just get, but above sets as well, so cache-txt below not needed
;  (list-lines (format nil "txt\/~a\/~a.txt" fnc id)))
(defun fnc+id-file (fnc i)
  (cache-fi (intern fnc) i))
;
(defun fnc-id_2 (fnc-str fnc id str) ;allows cache-dir and backup fnc to be different
  (let ((cache (fnc+id-file fnc-str id)))
    (if (full cache) cache
     (let ((retstrl (funcall fnc str)))
      (when (full retstrl) (cache-txt fnc-str retstrl id))
      ;retstr
      (if (full (first-lv retstrl)) retstrl str) ;really a list,&need to ret input as not calling fnc*
      )))) ;actually this is wrong, problem is I should have a cache for everything
 
(defun fnc_id2 (fnc-str id)  ;use str2fnc to call fnc-id_ if needed
  ;(fnc+id-file fnc-str id)
  (let (; (fnc (symbol-function (internup fnc-str)))
	(fnc  (internup fnc-str))
	;(str (txt id))
	(str (txt-2 id)) ;only for nlp
	)
   ;(fnc-id_2 fnc-str fnc id str)
    (fnc-id_ fnc-str fnc id str)
    ))
;-eof
;above is going w/ID as str, instead of ID now; which would allow a mixed cache
;&not having to put under nct.. which would cut down reuse; even reuse of ID&str mix now used
;remember I wanted mnp to be as-is from the java like mmt, so went w/npl as a replacement
; want to be sure to switching to mnp as-is asap, probably call mnp again
;
; from mem.lisp w/1change, used in ld3
(defun txtl2tal (l)
  "in: lst of txt, ret alst of under_ . txt"
  (mapcar #'(lambda (txt) (cons (under_f2 txt) txt)) (flatten l) ;l
          ))
 
;-
(defun cache-txt (fnc txt id &optional (memdir "mem")) ;use opt
  "run fnc on txt &cache in txt/fnc/id.txt" ;cache-dir
  (rsc (format nil "echo \"~a\"|cat>cache\/~a\/~a.txt" txt fnc id))
  txt)
;-eof 
(defun cache1 (fnc in alst) ;mv2 cache.lisp ;tried w/dScore but then not needed, see e2old.cl
 (let ((val (assoc in alst))) 
   (if val (cdr val)
     ;(setf (assoc in alst) (funcall fnc in))
     ;(cdr (pushnew (cons in (funcall fnc in)) alst))
     (let ((out (funcall fnc in)))
       (pushnew (cons in out) alst)
       out)
     )))
