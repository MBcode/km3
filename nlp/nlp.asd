;lkm3 "~/.sbclrc" 644L, 26054C                                     100,8         12%
;(lkm3) will load utils, km and km-uitls

(defsystem nlp
  :name "nlp"
  :version "0.0.1"
  :maintainer "bobak"
  :author "nlp/km"
  :licence "GPL?"
  :description "nlp+km" 
  :components (
;    (:module ut :components (
;               (:file "csv-np") ;
;               ;(:file "sys") ;new&in util in this version
;               (:file "util_mb")
;               (:file "diff-sexp")
;               ) :serial t)
;    (:module c ;km 
;      :components (
;               ;(:file "km_2-2-29") ; (:file "km_2-2-33") ; (:file "km_2-3-0") ;(:file "km_2-4-5") ;
;               (:file "km_2-5-1") ;
;               ) :serial t)
;    (:module km 
;      :components (
;               (:file "utkm") ;new pulled from mmkm
;               (:file "sn2") ;new find relations
;               ) :serial t)
;    (:module cache :components ( ;renamed dir
;               ;(:file "mem") ;mtxt finishs this off now ;in ld2 now, but fix/dumped, in mmt now
;	(:file "cache") ;to replace above w/much less code
;               ) :serial t)
    (:module nlp :components ( (:file "nlp") ;live
               (:file "nlp3") ;canned
               (:file "nyu") ;new canned
               ) :serial t)
)) 
