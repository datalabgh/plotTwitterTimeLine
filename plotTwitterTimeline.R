#
# 
# tweets: 
# twitHandle: 
# wordLength: 
# freqTerm: 
#
#' Function to plot text from the timeline of a named
#' twitter user 
#'
#' @param tweets: (list) output from userTimeline from twitteR package. 
#'        Max <= 3200
#' @param twitHandle: (str) handle of twitter user --- without '@' 
#' @param wordLength: (int) word lengths below this will not be plotted
#' @param freqTerm: (int) the frequency of word in tweets 
#'
plotTwitterTimeline <- function(tweets, 
                                twitHandle, 
                                wordLength = 3, 
                                freqTerm = 15)
  {  
  # load textMiningTools function
  source('~/R/Code4GH/functions/textMiningTools.R')
  
  # load packages
  require(twitteR)
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
  tweetsCorpus <- Corpus(VectorSource(tweets.df$text))
  
  tdm <- textMiningTools(tweetsCorpus)
  
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
  
  # save a 10 X 6 png file. 
  # ggsave(paste(twitHandle, "png", sep = "."), width = 10, height = 6)
  
  return(g)
}