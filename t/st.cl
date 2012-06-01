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
 
