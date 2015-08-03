#' ---
#' title: Utility function for corpus preparation and management
#' 
#' ideas for this code coming from 
#' http://www.rdatamining.com/docs/RDataMining-slides-text-mining.pdf
#'
#' @param Corpus 
#' @return tdm
#' 
#' output: pdf_document
#' ---
textMiningTools <- function(Corpus)
{
  # convert to lower case
  # tm v0.6
  Corpus <- tm_map(Corpus, content_transformer(tolower))
  
  # remove URLs
  removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
  # tm v0.6
  Corpus <- tm_map(Corpus, content_transformer(removeURL))
  
  # remove anything other than English letters or space
  removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
  Corpus <- tm_map(Corpus, content_transformer(removeNumPunct))
  
  # remove stopwords from corpus
  Corpus <- tm_map(Corpus, removeWords, stopwords("english"))
  
  # remove punctuations. preserve dashes
  # other possibilities here: 
  # http://stackoverflow.com/questions/27951377/tm-removepunctuation-except-hashtag
  Corpus <- tm_map(Corpus, removePunctuation,
                   preserve_intra_word_dashes=TRUE)
  
  # remove numbers
  Corpus <- tm_map(Corpus, removeNumbers)
  
  # remove extra whitespace from corpus
  Corpus <- tm_map(Corpus, stripWhitespace)
  
  #--------- Stemming and Completion -----------
  # keep a copy of corpus to use later as a dictionary for stem completion
  CorpusCopy <- Corpus
  # stem words
  Corpus <- tm_map(Corpus, stemDocument)
  
  # stem completion
  Corpus <- lapply(Corpus, stemCompletion2, dictionary=CorpusCopy)
  Corpus <- Corpus(VectorSource(Corpus))
  #----------------------------------------
  
  # create a TermDocumentMatrix. Word length control
  term_doc_matrix <- TermDocumentMatrix(Corpus,
                                        control = list(wordLengths = c(wordLength, Inf)))
  
  return(term_doc_matrix)
}

#' 
#' helper function for stem completion
#' ideas from http://stackoverflow.com/questions/25206049/stemcompletion-is-not-working
#'
#' @param x 
#' @param dictionary 
#'
stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  # Unexpectedly, stemCompletion completes an empty string to
  # a word in dictionary. Remove empty string to avoid above issue.
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  
  PlainTextDocument(stripWhitespace(x))
}