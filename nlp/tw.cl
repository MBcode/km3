;test IBM blumix (Watson) NLP via AlchemyAPI
(load ".aid.cl" :print t) ;(defvar *aid* "") ;Alchemy/bluemix ID
(defun key-str (&optional (k *aid*)) (str-cat "?apikey=" k))
;http://go.alchemyapi.com/getting-started  http://www.alchemyapi.com/api/combined/urls.html
;/home/bobak/dwn/ai/nl/w/src/msc/alchemy/src/alchemyapi_python
(defvar *base_url* "http://access.alchemyapi.com/calls") 
(defconstant +typ2us+ '((url . "/url/URLGet") 
                        (text . "/text/TextGet") 
                        (html . "/html/HTMLGet")
                        (image . "/image/ImageGet"))) ;url-str
(defun in-typ-url-str (typ) (assoc-v typ +typ2us+))
(defconstant +srvc2us+ '((sentiment . "TextSentiment") 
                         (sentiment-targeted . "TargetedSentiment") 
                         (author . "Author") 
                         (keywords . "RankedKeywords") 
                         (concepts . "RankedConcepts") 
                         (entities . "RankedNamedEntities") 
                         (category . "Category") 
                         (relations . "Relations") 
                         (language . "Language") 
                         (text . "Text") 
                         (text-raw . "RawText") 
                         (title . "Title") 
                         (feeds . "Feeds") 
                         (microformats . "Microformats") 
                         (combined . "CombinedData") 
                         (imagetagging . "RankedImageKeywords") 
                         (facetagging . "RankedImageFaceTags") 
                         (taxonomy . "RankedTaxonomy") 
                         (combined . "CombinedData") ;prefer 
                         )) ;url-str
(defun srvc-typ-url-str (typ) (assoc-v typ +srvc2us+))

(defun get-url (in-type srvc-type &optional (base *base_url*))
  (str-cat base (in-typ-url-str in-type) (srvc-typ-url-str srvc-type) (key-str)))

(defun prefixp (pre str)
 (and (stringp pre) (stringp str) (> (length str) (length pre)) 
         (string= pre str :end2 (length pre)))) 
(defun httpify (u)
    (if (prefixp "http" u) u (str-cat "http://" u)))

;defun get-combined (in-typ url) ;url|text
(defun get-combined (in-typ &optional path (url-base *mlb*)) ;url|text
 (let ((url (str-cat url-base path)))
  (str-cat (get-url in-typ 'combined) "&extract=entity,keyword,taxonomy,concept,relation" 
           ;page-image,image-kw,feed,title,author,pub-date,doc-sentement
           ;&optputMode=json&knowledgeGraph=1
           (str-cat "&url=" (httpify url))))
 )

(ql 'drakma)
(defun hr (u)
  "get the page as 1str"
  (drakma:http-request (httpify u) ))

(trace in-typ-url-str srvc-typ-url-str get-combinded httpify)
(defun t1 (&optional (file "test.txt"))
  (get-combined 'url (str-cat "w/" file)))
(ql 'flexi-streams)
(ql 's-xml)
(defun h1 (&optional (file "test.txt"))
 (s-xml:parse-xml-string
  (flexi-streams:octets-to-string (hr (t1 file))) ;this also works
  :output-type :sxml))
;For now, Instead of (hr (t1)) just pasted t1's str into browser&load output xml file:
(defvar *u1* (s-xml:parse-xml-file "URLGetCombinedData.txt"))
;Switch to h1 soon, but also throw in funcall caching to save on repeat calls
 (ql 'fare-memoization)  ;then call w/ memoized-funcall #'h1     ;other fnc/libs2look@too
 (defun tc1 (&optional (fn "test.txt")) (memoized-funcall #'h1 fn))
;1st query only recognizes a few words, & I think missed a few sentences; Really underwhelming.
