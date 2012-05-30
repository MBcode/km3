;(load "ld.cl" :print t)
;(load "c/km_2-3-0.fasl") ;will get loaded later, but need for tools
(load "c/km_2-5-1.fasl")
(al 'tools) ;could just load this after ld3
;(load "ld2.cl" :print t)
(load "ld3.cl" :print t) ;3 calls 2 that calls 1
;(taxonomy)
(when *canned-cNN*
 (mapcar #'cmp0cea2bnp *fln*) ;parse columbia ;do for predictions soon
 )
