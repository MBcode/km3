;(eval-when (:execute :load-toplevel :compile-toplevel)
; (setq *readtable* *km-readtable*))	; So that the dispatch macro #$ is recognized 

(defsystem nlm
  :name "nlm"
  :version "0.0.1"
  :maintainer "bobak"
  :author "nlm/km"
  :licence "GPL"
  :description "nlm.gov+km" 
  :components (
     (:module ut :components (
                (:file "csv-np") ;
                ;(:file "sys") ;new&in util in this version
                (:file "util_mb")
                (:file "diff-sexp")
                ) :serial t)
     (:module c ;km 
	      :components (
                ;(:file "km_2-2-29") ; (:file "km_2-2-33") ; (:file "km_2-3-0") ;(:file "km_2-4-5") ;
                (:file "km_2-5-1") ;
                ) :serial t)
     (:module km 
	      :components (
                (:file "utkm") ;new pulled from mmkm
                (:file "sn2") ;new find relations
                ) :serial t)
     (:module sc 
	      :components (
                (:file "scone-loader") ;cmu nlp KR, w/snomed contrib
                ) :serial t)
     (:module in :components (
                 (:file "ssheet3") ;requires csv  ;has globals needed later
                 (:file "silver") ;for NP+mod    ;might goto ./test
                 (:file "j27") ;silver for CUIs    ;might goto ./test
                ) :serial t)
     (:module test :components (
                 (:file "test") ;was in in/..
                ) :serial t)
     (:module out :components (
                 (:file "ss-out") ;was in ld3d
                ) :serial t)
     (:module alg :components (
              ;  (:file "alg") ;from v2, output to alg-tab
                 (:file "plm") ;start w/a plm dump, first
                ) :serial t)
     (:module cache :components ( ;renamed dir
                ;(:file "mem") ;mtxt finishs this off now ;in ld2 now, but fix/dumped, in mmt now
		(:file "cache") ;to replace above w/much less code
                ) :serial t)
     (:module nlm :components (
                ;;;(:file "txt") ;in ld2
                 ;;(:file "lexA") ;old file2be integrated into mnp ;not useful redid in mnp
                 ;;(:file "mnp") ;from mmt, &will get nlp in here  ;Split:
                 ;(:file "n2") ;moved up so will get redefined
                 ;(:file "n3") ;get rid of ;moved up so will get redefined
                 (:file "mnp1") ;just -T tagger
                 (:file "mnp2") ; nlp w/med-vocab via hyphens
                 (:file "mmt") ;(:file "mmprs")
                 (:file "mmkm") ;requieres km
                 (:file "ner") ;(:file "ben")
                 ;;;(:file "lc")
                 ;;(:file "n2" ;"o5" ;:depends-on (;"util_mb" ; "ssheet3"))
                 (:file "n2") ;moved up so will get redefined
                 (:file "n3") ;get rid of ;moved up so will get redefined
                 (:file "ft") ;criteria ins
                ;(:file "cui") ;cui stuff, trying to get rid of most of it
                ;;(:file "phr") ;phr stuff, trying to get rid of most of it
                 (:file "colu") ;columbia f3
                 (:file "parsed") ;columbia f17
                ) :serial t)
     (:module nlp :components (
                (:file "nlp") ;live
                (:file "nlp3") ;canned
                (:file "nyu") ;new canned
                ) :serial t)
)) 
;how2depend on outside a module/read more on them
;
;want to try a smaller version where call mm get xml and skip most of nlm&nlp modules
