# ideas for this code coming from 
# http://www.rdatamining.com/docs/RDataMining-slides-text-mining.pdf

# helper function for stem completion
# ideas from http://stackoverflow.com/questions/25206049/stemcompletion-is-not-working
stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  # Unexpectedly, stemCompletion completes an empty string to
  # a word in dictionary. Remove empty string to avoid above issue.
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  
  PlainTextDocument(stripWhitespace(x))
}

#
# function plotTwitterTimeline 
# tweets: output from userTimeline (twitteR package)
# twitHandle: handle of twitter user (without '@')
# wordLength: the length of words to be plotted
# freqTerm: the frequency of word in the tweet
#
plotTwitterTimeline <- function(tweets, twitHandle, wordLength = 3, freqTerm = 15){  
# load packages
  require(twitteR)
  require(tm)
  require(ggplot2)
  
  #------------------------------------
  # request twitter handle
  # userHandle <- readline("what is the twitter handle name? ")
  
  # request number of tweets
  # n <- readline("how many tweets (<=3200)? ")
  
  # get timeline tweets of specified handle user
  # tweets <- userTimeline(as.character(userHandle), as.integer(n))
  #------------------------------------
  
  # convert tweet list to data frame
  tweets.df <- twListToDF(tweets)
  
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
  
  # remove stopwords from corpus
  myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))
  
  # remove punctuations
  myCorpus <- tm_map(myCorpus, removePunctuation)
  
  # remove numbers
  myCorpus <- tm_map(myCorpus, removeNumbers)
  
  # remove extra whitespace from corpus
  myCorpus <- tm_map(myCorpus, stripWhitespace)
  
  #--------- Stemming and Completion -----------
  # keep a copy of corpus to use later as a dictionary for stem completion
   myCorpusCopy <- myCorpus
  # stem words
   myCorpus <- tm_map(myCorpus, stemDocument)
   
   # stem completion
   myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
   myCorpus <- Corpus(VectorSource(myCorpus))
  #----------------------------------------
  
  # create a TermDocumentMatrix
  tdm <- TermDocumentMatrix(myCorpus,
                            control = list(wordLengths = c(wordLength, Inf)))
  
  # create a data frame from tdm
  term.freq <- rowSums(as.matrix(tdm))
  term.freq <- subset(term.freq, term.freq >= freqTerm)
  df <- data.frame(term = names(term.freq), freq = term.freq)
  
  # plot a bar plot with ggplot
  g <- ggplot(df, aes(x = term, y = freq)) 
  g <- g + geom_bar(stat = "identity") 
  g <- g + xlab("Terms") 
  g <- g + ylab("Count") 
  g <- g + coord_flip()
  g <- g + ggtitle(paste("Tweets of @", as.character(twitHandle), sep = ""))
  
  # save the plot
  ggsave(paste(twitHandle, "png", sep = "."), width = 10, height = 6)
  
  return(g)
}