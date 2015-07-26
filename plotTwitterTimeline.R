  # load packages
  require(twitteR)
  require(tm)
  require(ggplot2)
  
  # convert tweet list to data frame
  tweet.df <- twListToDF(tweets)
  
  # build a corpus of text from source
  myCorpus <- Corpus(VectorSource(tweets.df$text))
  
  # convert to lower case
  # tm v0.6
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  
  # remove URLs
  removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
  
  # tm v0.6
  myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
  
  # remove anything other than English letters or space
  removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
  myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
  
  #   remove punctuation
  myCorpus <- tm_map(myCorpus, removePunctuation)
  #   remove numbers
  myCorpus <- tm_map(myCorpus, removeNumbers)
  
  # remove stopwords from corpus
  myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))
  
  # remove extra whitespace
  myCorpus <- tm_map(myCorpus, stripWhitespace)
  
  #--------- Stemming and Completion -----------
  # keep a copy of corpus to use later as a dictionary for stem completion
  # myCorpusCopy <- myCorpus
  # stem words
  # myCorpus <- tm_map(myCorpus, stemDocument)
  
  # tm v0.5-10
  # myCorpus <- tm_map(myCorpus, stemCompletion)
  # tm v0.6
  #  ideas from http://stackoverflow.com/questions/25206049/stemcompletion-is-not-working
  #stemCompletion2 <- function(x, dictionary) {
  #  x <- unlist(strsplit(as.character(x), " "))
    # Unexpectedly, stemCompletion completes an empty string to
    # a word in dictionary. Remove empty string to avoid above issue.
  #  x <- x[x != ""]
  #  x <- stemCompletion(x, dictionary=dictionary)
  #  x <- paste(x, sep="", collapse=" ")
  #  PlainTextDocument(stripWhitespace(x))
  # }
  # myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
  #----------------------------------------
  
  # create a TermDocumentMatrix
  tdm <- TermDocumentMatrix(myCorpus,
                            control = list(wordLengths = c(1, Inf)))
  
  # create a data frame from tdm
  term.freq <- rowSums(as.matrix(tdm))
  term.freq <- subset(term.freq, term.freq >= 15)
  df <- data.frame(term = names(term.freq), freq = term.freq)
  
  ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
    xlab("Terms") + ylab("Count") + coord_flip()