;from 6_27_AMIA_submission_test_set_s_CUI_annota not sure all markings are totally unambiguous
;problem, cui's are given for segment/fragments of an atom; &I only showed SC the top mappings
; if cdr gives a list then that has ""
; Also looks like a map was reconstructed from(eg 12a) parts of others, maybe just pull out then
;  in the end probably want the cui's for each;but this helps pnt out where it differes/needs change
;(defvar *n1* '(0 (1) (1 1) (1 1 1) (1 1 1 1) (1 1 1 1 1)))
;(mapcar #'(lambda (id) (cons id (nth (frag-num id) *n1*))) *ids*)
;num/fragment, and if cui-collection for one fragment then in a list
;	everything below w/a ;comment has a yellow marking, &I might not be sure of the silver CUIs
;	0 means that there is NO silver mapping
;	yellow & green [y|g] comments below mean there might be a change from default/but not always
(defvar *silverCUImapNum* ;1=pick top cui mapping, 2, the 2nd,  0=NO-mapping
'(("1A" 1) ("1B" 1) ("1C" 1) ("2A" 1) 
   ("2B"  (C0010068 C0035648)) ;y
   ("3A" 1) 
   ("3B" (C0332154) 0 1) ;yy  
   ("4A" 1) ;gg
 ("5A" 1 1) ;gggg
 ("5B" 1 2) ;gg
 ("6A" 1 1) ;gy
 ("6B" 1 1) ("6C" 1 1) ("7A" 1) ("8A" 1)
 ("9A" (C0702113 C0011849)) ;yg
 ("10A" 1) 
 ("11A" 3) ;g
 ("11B" 1) ("11C" 1) 
 ("11D" 1 C0449851 0 1)  ;ygy
 ("12A" 1) ;gyg ;C0521116 &?
 ("13A" (C0205191 C0010068))  ;=C1555457 cCHD too specific? ;g
 ("14A" 1) 
 ("14B" (C1619636 C0087111) 1) ;ygy
 ("14C" (C0151691)) ;gy
 ("14D" 1 2 1) ;gyg
 ("14E" (C0425310) 3 1) ;yg ;? Stop [422117008(SNOMEDCT)] Stop (Instruction Imperative) [C1947925] 
     ;covers 1st 2
 ("14F" (C0011900 C1563320) 1) ;gyyg
 ("14G" 1 1) ("14H" 1) 
 ("14I" 1 1 (C0008972 C0008972)) ;gyyyy ?
 ("14J" 1) ("15A" 1) ("15B" 1)
 ("15C" (C1301732)) ;y
 ("15D" (C1301732 C0205164)) ;gyg
 ("16A" 1) 
 ("17A" (C0392756 C0205082) 1) ;yyyyy
 ("17B" 1 1) 
 ("17C" 1) ;gy
 ("18A" (C0018801)) ;yyy
 ("19A" 1 1) ;gy
 ("19B" 0 (C0870811)) ;yy
 ("19C" 2 (C0003364)) ;gy ;? C0003364 C0013227
 ("20A" (C1552867)) ;or?(C1546953 C0012634) ;yyg
 ("20B" 1) ("21A" 1) 
 ("21B" (C0004096)) ;y
 ("21C" 1) ;g
 ("22A" 1) 
 ("23A" 1 1) ;gg
 ("23B" 1 1) ("23C" 1 1) ("24A" 1 1)
 ("24B" 1 1) ("24C" 1) ("24D" 1) 
 ("25A" 1) ;g
 ("26A" 1) ("27A" 1) ("27B" 1)
 ("27C" 1) 
 ("27D" (C0015260)) ;yy
 ("28A" 1) 
 ("29A" (C0262926) 1) ;yyy ;C0019664 C0262512 C0262926
 ("30A" 1) 
 ("30B" (0009814) 0 0 0 0 0) ;g 
 ("31A" (C0232219)) ;y
 ("32A" 3) ;yg
 ("33A" 1) ("34A" 1) ("35A" 1) ("36A" 1) 
 ("36B" 2 1) ;g 
 ("37A" 1 1) ;g
 ("38A" 1) ;g
 ("39A" 1) 
 ("39B" 3) ;g
 ("39C" 1) ("40A" 1) ("40B" 1) 
 ("41A" 1) ;g
 ("41B" 1 (C1305855)) ;gy 
 ("41C" 1 1) ("42A" 1) 
 ("42B" 1 1 2) ;yg
 ("42C" 1) ("42D" 1 1)
 ("42E" 1) 
 ("42F" (C0015260)) ;yg 
 ("43A" 1) ("43B" 1 1 1) ("44A" 1) ("45A" 1)
 ("46A" 1 1 1 0) ;g ;only4
 ("46B" 1 1 1) ;g
 ("47A" 3 2) ;gyg
 ("47B" (C1552469)) ;y
 ("47C" 1 0 0 1 0 0 2) ;("47C") ;y
 ("48A" (C0205272 C0205272) 1 1 (C0008972) 1) ;yyg
 ("49A" (C0019665 C1608518) 1) ;yy
 ("49B" 1 2) ;g
 ("49C" 1 1) ("49D" 1 1) 
 ("49E" 0 0 0 0 1 2) ;yyyyyyyg ;("49E") 
 ("50A" 1 2) ;ygyyg
 ("51A" 1)
 ("51B" 1) ;gy
 ("52A" 1 1) ;g
 ("53A" 1) 
 ("54A" 1 1) ;g
 ("55A" 1 1 1 1) ;gg
 ("55B" 1 1 1 1) ;g
 ("55C" (C0005135 C0332256) 1) ;ggyy
 ("55D" 3 1)  ;g
 ("56A" 2 1 1 1) ;gggg
 ("56B" 2 1 1 1) ;gg 
 ("56C" 2 1 1 1) ;gg
 ("56D" 2 1 1 1) ;gg
 ("56E" 2 C0242295 1 1 1)  ;gyg
 ("56F" 2 1 1 1)) ;gg
)
