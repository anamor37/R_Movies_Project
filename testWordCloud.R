install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("wordcloud2")

library(tm)
library(SnowballC)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)

genreQ <- read.csv('IMDb movies.csv', stringsAsFactors = FALSE)
text <- genreQ$genre
genreCorpus <- Corpus(VectorSource(text))

genreCorpus <- gengreCourpus
genreCorpus <- tm_map(genreCorpus, content_transformer(tolower))
genreCorpus <- tm_map(removePunctuation)
genreCorpus <- tm_map(stripWhiteSpace)
genreCorpus <- tm_map(genreCorpus, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(genreCorpus)
matrix <as.matrix(dtm)
words <- sort(rowSums(matrix), decreasing=TRUE)
df <- data.frame(word = names(words), freq=words)
head(d,10) 

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))



