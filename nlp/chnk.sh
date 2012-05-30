java opennlp.tools.lang.english.SentenceDetector \
  opennlp.models/english/sentdetect/EnglishSD.bin.gz < text |
java opennlp.tools.lang.english.Tokenizer \
  opennlp.models/english/tokenize/EnglishTok.bin.gz |
java opennlp.tools.lang.english.PosTagger -d \
  opennlp.models/english/parser/tagdict opennlp.models/english/parser/tag.bin.gz |
java opennlp.tools.lang.english.TreebankChunker \
  opennlp.models/english/chunker/EnglishChunk.bin.gz
 
