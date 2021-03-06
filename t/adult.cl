(:|MMOs|
 (:MMO
  (:|CmdLine|
   (:|Command|
    "MetaMap -Z 09 --negex --longest_lexicon_match -ITyQxa -% format")
   ((:|Options| :|Count| "12")
    (:|Option| (:|OptName| "mm_data_year") (:|OptValue| "09"))
    (:|Option| (:|OptName| "negex"))
    (:|Option| (:|OptName| "longest_lexicon_match"))
    (:|Option| (:|OptName| "show_cuis"))
    (:|Option| (:|OptName| "tagger_output"))
    (:|Option| (:|OptName| "word_sense_disambiguation"))
    (:|Option| (:|OptName| "quick_composite_phrases"))
    (:|Option| (:|OptName| "syntax"))
    (:|Option| (:|OptName| "all_acros_abbrs"))
    (:|Option| (:|OptName| "XML") (:|OptValue| "format"))
    (:|Option| (:|OptName| "infile") (:|OptValue| "user_input"))
    (:|Option| (:|OptName| "outfile") (:|OptValue| "user_output"))))
  ((:|AAs| :|Count| "0")) ((:|Negations| :|Count| "0"))
  ((:|Utterances| :|Count| "1")
   (:|Utterance| (:PMID "00000000") (:|UttSection| "tx") (:|UttNum| "1")
    (:|UttText| "adult patients, 18-75 years of age") (:|UttStartPos| "0")
    (:|UttLength| "34")
    ((:|Phrases| :|Count| "2")
     (:|Phrase| (:|PhraseText| "adult patients,")
      ((:|SyntaxUnits| :|Count| "3")
       (:|SyntaxUnit| (:|SyntaxType| "mod") (:|LexMatch| "adult")
        (:|InputMatch| "adult") (:|LexCat| "noun")
        ((:|Tokens| :|Count| "1") (:|Token| "adult")))
       (:|SyntaxUnit| (:|SyntaxType| "head") (:|LexMatch| "patients")
        (:|InputMatch| "patients") (:|LexCat| "noun")
        ((:|Tokens| :|Count| "1") (:|Token| "patients")))
       (:|SyntaxUnit| (:|SyntaxType| "punc") (:|InputMatch| ",")
        ((:|Tokens| :|Count| "0"))))
      (:|PhraseStartPos| "0") (:|PhraseLength| "15")
      ((:|Candidates| :|Count| "3")
       (:|Candidate| (:|CandidateScore| "-861") (:|CandidateCUI| "C0030705")
        (:|CandidateMatched| "Patients") (:|CandidatePreferred| "Patients")
        ((:|MatchedWords| :|Count| "1") (:|MatchedWord| "patients"))
        ((:|SemTypes| :|Count| "1") (:|SemType| "podg"))
        ((:|MatchMaps| :|Count| "1")
         (:|MatchMap| (:|TextMatchStart| "2") (:|TextMatchEnd| "2")
          (:|ConcMatchStart| "1") (:|ConcMatchEnd| "1") (:|LexVariation| "0")))
        (:|IsHead| "yes") (:|IsOverMatch| "no")
        ((:|Sources| :|Count| "10") (:|Source| "LCH") (:|Source| "MSH")
         (:|Source| "MTH") (:|Source| "NCI") (:|Source| "PSY")
         (:|Source| "ICNP") (:|Source| "LNC") (:|Source| "SNOMEDCT")
         (:|Source| "AOD") (:|Source| "CCPSS"))
        ((:|ConceptPIs| :|Count| "1")
         (:|ConceptPI| (:|StartPos| "6") (:|Length| "8"))))
       (:|Candidate| (:|CandidateScore| "-694") (:|CandidateCUI| "C0001675")
        (:|CandidateMatched| "Adult") (:|CandidatePreferred| "Adult")
        ((:|MatchedWords| :|Count| "1") (:|MatchedWord| "adult"))
        ((:|SemTypes| :|Count| "1") (:|SemType| "aggp"))
        ((:|MatchMaps| :|Count| "1")
         (:|MatchMap| (:|TextMatchStart| "1") (:|TextMatchEnd| "1")
          (:|ConcMatchStart| "1") (:|ConcMatchEnd| "1") (:|LexVariation| "0")))
        (:|IsHead| "no") (:|IsOverMatch| "no")
        ((:|Sources| :|Count| "9") (:|Source| "ICNP") (:|Source| "MSH")
         (:|Source| "MTH") (:|Source| "NCI") (:|Source| "NDFRT")
         (:|Source| "RCD") (:|Source| "SNOMEDCT") (:|Source| "AOD")
         (:|Source| "CSP"))
        ((:|ConceptPIs| :|Count| "1")
         (:|ConceptPI| (:|StartPos| "0") (:|Length| "5"))))
       (:|Candidate| (:|CandidateScore| "-694") (:|CandidateCUI| "C1706450")
        (:|CandidateMatched| "Adult") (:|CandidatePreferred| "Legal Adult")
        ((:|MatchedWords| :|Count| "1") (:|MatchedWord| "adult"))
        ((:|SemTypes| :|Count| "1") (:|SemType| "humn"))
        ((:|MatchMaps| :|Count| "1")
         (:|MatchMap| (:|TextMatchStart| "1") (:|TextMatchEnd| "1")
          (:|ConcMatchStart| "1") (:|ConcMatchEnd| "1") (:|LexVariation| "0")))
        (:|IsHead| "no") (:|IsOverMatch| "no")
        ((:|Sources| :|Count| "2") (:|Source| "MTH") (:|Source| "NCI"))
        ((:|ConceptPIs| :|Count| "1")
         (:|ConceptPI| (:|StartPos| "0") (:|Length| "5")))))
      ((:|Mappings| :|Count| "2")
       (:|Mapping| (:|MappingScore| "-888")
        ((:|Candidates| :|Count| "2")
         (:|Candidate| (:|CandidateScore| "-694") (:|CandidateCUI| "C0001675")
          (:|CandidateMatched| "Adult") (:|CandidatePreferred| "Adult")
          ((:|MatchedWords| :|Count| "1") (:|MatchedWord| "adult"))
          ((:|SemTypes| :|Count| "1") (:|SemType| "aggp"))
          ((:|MatchMaps| :|Count| "1")
           (:|MatchMap| (:|TextMatchStart| "1") (:|TextMatchEnd| "1")
            (:|ConcMatchStart| "1") (:|ConcMatchEnd| "1")
            (:|LexVariation| "0")))
          (:|IsHead| "no") (:|IsOverMatch| "no")
          ((:|Sources| :|Count| "9") (:|Source| "ICNP") (:|Source| "MSH")
           (:|Source| "MTH") (:|Source| "NCI") (:|Source| "NDFRT")
           (:|Source| "RCD") (:|Source| "SNOMEDCT") (:|Source| "AOD")
           (:|Source| "CSP"))
          ((:|ConceptPIs| :|Count| "1")
           (:|ConceptPI| (:|StartPos| "0") (:|Length| "5"))))
         (:|Candidate| (:|CandidateScore| "-861") (:|CandidateCUI| "C0030705")
          (:|CandidateMatched| "Patients") (:|CandidatePreferred| "Patients")
          ((:|MatchedWords| :|Count| "1") (:|MatchedWord| "patients"))
          ((:|SemTypes| :|Count| "1") (:|SemType| "podg"))
          ((:|MatchMaps| :|Count| "1")
           (:|MatchMap| (:|TextMatchStart| "2") (:|TextMatchEnd| "2")
            (:|ConcMatchStart| "1") (:|ConcMatchEnd| "1")
            (:|LexVariation| "0")))
          (:|IsHead| "yes") (:|IsOverMatch| "no")
          ((:|Sources| :|Count| "10") (:|Source| "LCH") (:|Source| "MSH")
           (:|Source| "MTH") (:|Source| "NCI") (:|Source| "PSY")
           (:|Source| "ICNP") (:|Source| "LNC") (:|Source| "SNOMEDCT")
           (:|Source| "AOD") (:|Source| "CCPSS"))
          ((:|ConceptPIs| :|Count| "1")
           (:|ConceptPI| (:|StartPos| "6") (:|Length| "8"))))))
       (:|Mapping| (:|MappingScore| "-888")
        ((:|Candidates| :|Count| "2")
         (:|Candidate| (:|CandidateScore| "-694") (:|CandidateCUI| "C1706450")
          (:|CandidateMatched| "Adult") (:|CandidatePreferred| "Legal Adult")
          ((:|MatchedWords| :|Count| "1") (:|MatchedWord| "adult"))
          ((:|SemTypes| :|Count| "1") (:|SemType| "humn"))
          ((:|MatchMaps| :|Count| "1")
           (:|MatchMap| (:|TextMatchStart| "1") (:|TextMatchEnd| "1")
            (:|ConcMatchStart| "1") (:|ConcMatchEnd| "1")
            (:|LexVariation| "0")))
          (:|IsHead| "no") (:|IsOverMatch| "no")
          ((:|Sources| :|Count| "2") (:|Source| "MTH") (:|Source| "NCI"))
          ((:|ConceptPIs| :|Count| "1")
           (:|ConceptPI| (:|StartPos| "0") (:|Length| "5"))))
         (:|Candidate| (:|CandidateScore| "-861") (:|CandidateCUI| "C0030705")
          (:|CandidateMatched| "Patients") (:|CandidatePreferred| "Patients")
          ((:|MatchedWords| :|Count| "1") (:|MatchedWord| "patients"))
          ((:|SemTypes| :|Count| "1") (:|SemType| "podg"))
          ((:|MatchMaps| :|Count| "1")
           (:|MatchMap| (:|TextMatchStart| "2") (:|TextMatchEnd| "2")
            (:|ConcMatchStart| "1") (:|ConcMatchEnd| "1")
            (:|LexVariation| "0")))
          (:|IsHead| "yes") (:|IsOverMatch| "no")
          ((:|Sources| :|Count| "10") (:|Source| "LCH") (:|Source| "MSH")
           (:|Source| "MTH") (:|Source| "NCI") (:|Source| "PSY")
           (:|Source| "ICNP") (:|Source| "LNC") (:|Source| "SNOMEDCT")
           (:|Source| "AOD") (:|Source| "CCPSS"))
          ((:|ConceptPIs| :|Count| "1")
           (:|ConceptPI| (:|StartPos| "6") (:|Length| "8"))))))))
     (:|Phrase| (:|PhraseText| "18-75 years of age")
      ((:|SyntaxUnits| :|Count| "6")
       (:|SyntaxUnit| (:|SyntaxType| "shapes") (:|InputMatch| "18")
        ((:|Tokens| :|Count| "1") (:|Token| "18")))
       (:|SyntaxUnit| (:|SyntaxType| "punc") (:|InputMatch| "-")
        ((:|Tokens| :|Count| "0")))
       (:|SyntaxUnit| (:|SyntaxType| "shapes") (:|InputMatch| "75")
        ((:|Tokens| :|Count| "1") (:|Token| "75")))
       (:|SyntaxUnit| (:|SyntaxType| "head") (:|LexMatch| "years")
        (:|InputMatch| "years") (:|LexCat| "noun")
        ((:|Tokens| :|Count| "1") (:|Token| "years")))
       (:|SyntaxUnit| (:|SyntaxType| "prep") (:|LexMatch| "of")
        (:|InputMatch| "of") (:|LexCat| "prep")
        ((:|Tokens| :|Count| "1") (:|Token| "of")))
       (:|SyntaxUnit| (:|SyntaxType| "mod") (:|LexMatch| "age")
        (:|InputMatch| "age") (:|LexCat| "noun")
        ((:|Tokens| :|Count| "1") (:|Token| "age"))))
      (:|PhraseStartPos| "16") (:|PhraseLength| "18")
      ((:|Candidates| :|Count| "6")
       (:|Candidate| (:|CandidateScore| "-797") (:|CandidateCUI| "C1510829")
        (:|CandidateMatched| "Age-Years") (:|CandidatePreferred| "Age-Years")
        ((:|MatchedWords| :|Count| "2") (:|MatchedWord| "age")
         (:|MatchedWord| "years"))
        ((:|SemTypes| :|Count| "1") (:|SemType| "tmco"))
        ((:|MatchMaps| :|Count| "2")
         (:|MatchMap| (:|TextMatchStart| "5") (:|TextMatchEnd| "5")
          (:|ConcMatchStart| "1") (:|ConcMatchEnd| "1") (:|LexVariation| "0"))
         (:|MatchMap| (:|TextMatchStart| "3") (:|TextMatchEnd| "3")
          (:|ConcMatchStart| "2") (:|ConcMatchEnd| "2") (:|LexVariation| "0")))
        (:|IsHead| "yes") (:|IsOverMatch| "no")
        ((:|Sources| :|Count| "1") (:|Source| "NCI"))
        ((:|ConceptPIs| :|Count| "2")
         (:|ConceptPI| (:|StartPos| "22") (:|Length| "5"))
         (:|ConceptPI| (:|StartPos| "31") (:|Length| "3"))))
       (:|Candidate| (:|CandidateScore| "-760") (:|CandidateCUI| "C0439234")
        (:|CandidateMatched| "Years") (:|CandidatePreferred| "year")
        ((:|MatchedWords| :|Count| "1") (:|MatchedWord| "years"))
        ((:|SemTypes| :|Count| "1") (:|SemType| "tmco"))
        ((:|MatchMaps| :|Count| "1")
         (:|MatchMap| (:|TextMatchStart| "3") (:|TextMatchEnd| "3")
          (:|ConcMatchStart| "1") (:|ConcMatchEnd| "1") (:|LexVariation| "0")))
        (:|IsHead| "yes") (:|IsOverMatch| "no")
        ((:|Sources| :|Count| "7") (:|Source| "HL7V3.0") (:|Source| "MTH")
         (:|Source| "NCI") (:|Source| "RCD") (:|Source| "SNOMEDCT")
         (:|Source| "CCPSS") (:|Source| "ICNP"))
        ((:|ConceptPIs| :|Count| "1")
         (:|ConceptPI| (:|StartPos| "22") (:|Length| "5"))))
       (:|Candidate| (:|CandidateScore| "-726") (:|CandidateCUI| "C0439508")
        (:|CandidateMatched| "/year") (:|CandidatePreferred| "/year")
        ((:|MatchedWords| :|Count| "1") (:|MatchedWord| "year"))
        ((:|SemTypes| :|Count| "1") (:|SemType| "tmco"))
        ((:|MatchMaps| :|Count| "1")
         (:|MatchMap| (:|TextMatchStart| "3") (:|TextMatchEnd| "3")
          (:|ConcMatchStart| "1") (:|ConcMatchEnd| "1") (:|LexVariation| "1")))
        (:|IsHead| "yes") (:|IsOverMatch| "no")
        ((:|Sources| :|Count| "2") (:|Source| "RCD") (:|Source| "SNOMEDCT"))
        ((:|ConceptPIs| :|Count| "1")
         (:|ConceptPI| (:|StartPos| "22") (:|Length| "5"))))
       (:|Candidate| (:|CandidateScore| "-593") (:|CandidateCUI| "C0001779")
        (:|CandidateMatched| "Age") (:|CandidatePreferred| "Age")
        ((:|MatchedWords| :|Count| "1") (:|MatchedWord| "age"))
        ((:|SemTypes| :|Count| "1") (:|SemType| "orga"))
        ((:|MatchMaps| :|Count| "1")
         (:|MatchMap| (:|TextMatchStart| "5") (:|TextMatchEnd| "5")
          (:|ConcMatchStart| "1") (:|ConcMatchEnd| "1") (:|LexVariation| "0")))
        (:|IsHead| "no") (:|IsOverMatch| "no")
        ((:|Sources| :|Count| "8") (:|Source| "LNC") (:|Source| "MTH")
         (:|Source| "NCI") (:|Source| "RCD") (:|Source| "SNOMEDCT")
         (:|Source| "AOD") (:|Source| "CCPSS") (:|Source| "SNMI"))
        ((:|ConceptPIs| :|Count| "1")
         (:|ConceptPI| (:|StartPos| "31") (:|Length| "3"))))
       (:|Candidate| (:|CandidateScore| "-521") (:|CandidateCUI| "C0001792")
        (:|CandidateMatched| "Aged") (:|CandidatePreferred| "Elderly")
        ((:|MatchedWords| :|Count| "1") (:|MatchedWord| "aged"))
        ((:|SemTypes| :|Count| "1") (:|SemType| "popg"))
        ((:|MatchMaps| :|Count| "1")
         (:|MatchMap| (:|TextMatchStart| "5") (:|TextMatchEnd| "5")
          (:|ConcMatchStart| "1") (:|ConcMatchEnd| "1") (:|LexVariation| "3")))
        (:|IsHead| "no") (:|IsOverMatch| "no")
        ((:|Sources| :|Count| "18") (:|Source| "MDR") (:|Source| "MSH")
         (:|Source| "MTH") (:|Source| "NCI") (:|Source| "DXP")
         (:|Source| "AOD") (:|Source| "CSP") (:|Source| "ICPC2P")
         (:|Source| "LCH") (:|Source| "ICPC2ICD10ENG") (:|Source| "MTHICD9")
         (:|Source| "RCDSY") (:|Source| "RCD") (:|Source| "SNOMEDCT")
         (:|Source| "SNMI") (:|Source| "SNM") (:|Source| "ICNP")
         (:|Source| "NDFRT"))
        ((:|ConceptPIs| :|Count| "1")
         (:|ConceptPI| (:|StartPos| "31") (:|Length| "3"))))
       (:|Candidate| (:|CandidateScore| "-521") (:|CandidateCUI| "C0242449")
        (:|CandidateMatched| "Ageism") (:|CandidatePreferred| "Ageism")
        ((:|MatchedWords| :|Count| "1") (:|MatchedWord| "ageism"))
        ((:|SemTypes| :|Count| "1") (:|SemType| "socb"))
        ((:|MatchMaps| :|Count| "1")
         (:|MatchMap| (:|TextMatchStart| "5") (:|TextMatchEnd| "5")
          (:|ConcMatchStart| "1") (:|ConcMatchEnd| "1") (:|LexVariation| "3")))
        (:|IsHead| "no") (:|IsOverMatch| "no")
        ((:|Sources| :|Count| "3") (:|Source| "ICNP") (:|Source| "MSH")
         (:|Source| "PSY"))
        ((:|ConceptPIs| :|Count| "1")
         (:|ConceptPI| (:|StartPos| "31") (:|Length| "3")))))
      ((:|Mappings| :|Count| "1")
       (:|Mapping| (:|MappingScore| "-797")
        ((:|Candidates| :|Count| "1")
         (:|Candidate| (:|CandidateScore| "-797") (:|CandidateCUI| "C1510829")
          (:|CandidateMatched| "Age-Years") (:|CandidatePreferred| "Age-Years")
          ((:|MatchedWords| :|Count| "2") (:|MatchedWord| "age")
           (:|MatchedWord| "years"))
          ((:|SemTypes| :|Count| "1") (:|SemType| "tmco"))
          ((:|MatchMaps| :|Count| "2")
           (:|MatchMap| (:|TextMatchStart| "5") (:|TextMatchEnd| "5")
            (:|ConcMatchStart| "1") (:|ConcMatchEnd| "1")
            (:|LexVariation| "0"))
           (:|MatchMap| (:|TextMatchStart| "3") (:|TextMatchEnd| "3")
            (:|ConcMatchStart| "2") (:|ConcMatchEnd| "2")
            (:|LexVariation| "0")))
          (:|IsHead| "yes") (:|IsOverMatch| "no")
          ((:|Sources| :|Count| "1") (:|Source| "NCI"))
          ((:|ConceptPIs| :|Count| "2")
           (:|ConceptPI| (:|StartPos| "22") (:|Length| "5"))
           (:|ConceptPI| (:|StartPos| "31") (:|Length| "3")))))))))))))
 
