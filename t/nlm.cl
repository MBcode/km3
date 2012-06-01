;a few lines from nlm/mmt.lisp to replace the whole dir, now that we might just get mm xml
;-parse mmtx-or-metamap output
(defvar *cid* nil) ;instead of num, set current-id
(defvar *cidn* nil) ;sPhr num
(defvar *cid2* nil) ;cid . cidn
(defvar *tst2* nil)
;=-=-=-=-=-=
(defun mmt (s)
  "was MMTx"
  ;(break2lines (run-ext  "mmt" s))
  (setf *tst2* (run-ext  "mmt" s))
  )
;=-=-=-=-=-=
(defun mmt-id (id str)
  (fnc-id_ "mmt" #'mmt id str)) ;(fnc-id "mmt" id str)
;(defun mnp-id (id str) ;  (fnc-id "mnp" id str))
;=-=-=-=-=-= 
(defun mmtx_pair (pr &optional (strm t))
  (setf *cid* (car pr)) ;(setf *cid* id)
 (let* ((id (car pr))
        (str- (cdr pr))
        (str (rm-star str-))
	(mm (mmt-id id str))) ;still has sexpr w/everything
      (format t "~&mm-len:~A" (len mm))
         mm))


(defun mmtx_pairs (prs &optional (strm t))
  (mapcar #'mmtx_pair prs))
 
;--eof 
