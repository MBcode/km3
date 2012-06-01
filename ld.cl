(defvar *rad* nil) ;not working on radiology right now
(al 's-xml-rpc)
(defvar *xa* (s-xml:parse-xml-file "t/adult.xml")) 
(defvar *xb* (s-xml:parse-xml-file "t/bcq.xml")) 
(defvar *xh* (s-xml:parse-xml-file "t/ha2.xml")) 
;(defvar *xh2* (s-xml:parse-xml-file "ha2.xml" :output-type :lxml)) 
(defvar *xh3* (s-xml:parse-xml-file "t/ha2.xml" :output-type :xml-struct)) 
(defvar *oh* (s-xml:parse-xml-file "t/ot2.xml")) 
(defvar *nc* (s-xml:parse-xml-file "nct/0/NCT00930410.xml")) 
(defvar *cnc* *nc*)
(load "t/bcq.cl" :print t) ;cmp plm xml &s-xml:
(load "t/bcq-s.cl" :print t)
(load "nct/1000.cl" :print t) ;then move to larger tests sets for: *try-nct-s* 
;(lt2) 
(load "t/allm.cl" :print t)
(al 'km3) ;(al 'nlm) 
(load "t/nlm2.cl" :print t) ;still uses cache
;(load "nlm3.cl" :print t) ;still uses cache
;(lt) has t.cl
(defvar *canned-cNN* nil) ;columbia data is in one file, not from csv/inputs yet
  ;so should inhibit if the init-csv input isn't the default
(when *rad*
 (trace nct crit-slots mapt2)
 (trace init-csv) ;mapt2
 ;(trace mapcar-csv-file flatt flat1 atoms)
 ;(init-csv "in/fm0401hd.csv") ;instead
 ;(init-csv) ;for full
 (init-csv "in/office.csv") ;for radiology
 ;(init-csv "in/ts113009b.csv") ;dec10
 ;(init-csv "in/testsetlist_113009_w_March_matches.csv") ;for new 59crit
)
;could keep even though ERGO cols not filled yet, as skipping it anyway;dbg when more data&time
(load-kb "mtui-ht.km")
