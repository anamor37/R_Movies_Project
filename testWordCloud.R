install.packages("tm")
install.packages("NLP")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("wordcloud2")

library(tm)
library(NLP)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)


genreQ <- read.csv('IMDb movies.csv', stringsAsFactors = FALSE)
text <- genreQ$genre
genreCorpus <- Corpus(VectorSource(text))
genreCorpus [[1]][1]

genreCorpus <- tm_map(genreCorpus, removePunctuation)
genreCorpus <- tm_map(genreCorpus, stripWhitespace)
genreCorpus <- tm_map(genreCorpus, removeWords, stopwords("english"))
genreCorpus [[1]][1]

dtm <- TermDocumentMatrix(genreCorpus)
m <- as.matrix(dtm)
words <- sort(rowSums(m), decreasing=TRUE)
df <- data.frame(word = names(words), freq=words)
#head(d,10) 

#set.seed(1234)
wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
wordcloud2(data=df, size=1.4, color='random-dark')


