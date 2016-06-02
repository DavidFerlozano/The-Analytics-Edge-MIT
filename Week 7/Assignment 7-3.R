library(wordcloud)
library(tm)
tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
frequencies = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(frequencies))

wordcloud(colnames(allTweets), colSums(allTweets),scale=c(2, 0.25))

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
frequencies = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(frequencies))
wordcloud(colnames(allTweets), colSums(allTweets),scale=c(2, 0.25))

negTweets = subset(allTweets, tweets$Avg <= -1)
wordcloud(colnames(negTweets), colSums(negTweets))

display.brewer.all()