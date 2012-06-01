(defun rfinds (fl l)
    "rfind like xpath"
      (if (eq (len fl) 1) (rfind (first fl) l)
            (rfinds (rest fl) (rfind (first fl) l))))
;now a version that looks for a tree,
;that is if(the tail)part of fl, is a list, get all of those values 
(defun rfinds (fl l)
    "rfind like xpath"
      (if (eq (len fl) 1) (let ((ffl (first fl)))  (if (listp ffl) (mapcar #'(lambda (ff) (rfind ff l))
									   ffl)
						     (rfind ffl l)));(rfind (first fl) l)
            (rfinds (rest fl) (rfind (first fl) l))))
;this works for reg rfinds calls
;USER(7): (rfinds '(:|Mapping| :|Candidate| (:|UMLSConcept| :|MatchedWord|)) *cx*)
;((:|UMLSConcept| "Men") (:|MatchedWord| "men"))
