library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(Cairo)

setwd("C:/Users/icbmadmin/Desktop/")
text = readLines("Wordcloudtest.txt") # Read in the text from a .txt file

docs = Corpus(VectorSource(text))
inspect(docs)
toSpace = content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs = tm_map(docs, toSpace, "/")
docs = tm_map(docs, toSpace, "@")
docs = tm_map(docs, toSpace, "\\|")

#docs = tm_map(docs, content_transformer(tolower)) # Convert the text to lower case
docs = tm_map(docs, removeNumbers) # Remove numbers
docs = tm_map(docs, removeWords, stopwords("english")) # remove small "stopwords" (e.g. the, when, to, elc.)
docs = tm_map(docs, removeWords, c("blabla1", "blabla2")) # remove self-defined words from the text
docs = tm_map(docs, removePunctuation) # remove punctuation
docs = tm_map(docs, stripWhitespace) # Eliminate additional white spaces

dtm = TermDocumentMatrix(docs)
m = as.matrix(dtm)
v = sort(rowSums(m),decreasing=TRUE)
d = data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
Cairo(file = "WordcloudHE425_results.svg",
      type = "svg",
      width = 8,
      height = 8,
      bg = "white",
      units = "in",
      dpi = 96)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=F, rot.per=0.35, 
          colors=brewer.pal(11, "Spectral"))
dev.off()
