library("tm")
library("magrittr")
library("wordcloud")

setwd("/home/ruben/Development/swiftkey_capstone/work")
txt <- "./en_US/samples"

data_ori <- VCorpus(DirSource(txt, encoding = "UTF-8"), readerControl = list(language = "lat"))

doc_stats <- system("wc -wml en_US/samples/*.txt", intern=TRUE)
stats <- strsplit(doc_stats, " ")

# Extracting some basic stats
res_nm <- c(stats[[1]][8],stats[[2]][8], stats[[3]][10])  
res_lc <- c(stats[[1]][4],stats[[2]][4], stats[[3]][4])
res_wc <- c(stats[[1]][6],stats[[2]][6], stats[[3]][7])
res_cc <- c(stats[[1]][7],stats[[2]][7], stats[[3]][9])

# remove stopwords, badwords and other words
# http://www.cs.cmu.edu/~biglou/resources/bad-words.txt
bad_words <- read.csv("bad-words.txt", stringsAsFactors=FALSE, header = TRUE)
bad_words <- paste(bad_words, collapse=" ")
stop_words <- c("http", stopwords("english"), bad_words)

data <- tm_map(data_ori, removeWords, stop_words)
data <- tm_map(data, content_transformer(tolower))
data <- tm_map(data, stripWhitespace)
data <- tm_map(data, removePunctuation)
data <- tm_map(data, removeNumbers)
data <- tm_map(data, function(x) iconv(x, "latin1", "ASCII", sub=""))
data <- tm_map(data, PlainTextDocument)
data_clean <- tm_map(data, stripWhitespace)

dtm <- DocumentTermMatrix(data_clean)
#tdm <- TermDocumentMatrix(data_clean)

res_wcl <- rowSums(as.matrix(dtm))

summary_1 <- data.frame(res_nm,res_lc, res_wc, res_wcl, res_cc)
names(summary_1)<- c("File Name","Line Counts", "Word Counts", "Clean Word Counts", "Character Counts")
summary_1

dtm2 <- as.matrix(dtm)
frequency_tot <- colSums(dtm2)
frequency_tot <- sort(frequency, decreasing=TRUE)
frequency_blog <- sort(dtm[[1]], decreasing=TRUE)


terms_freq_1 <- findFreqTerms(dtm, highfreq = 1)

#histogram 
hist(frequency,right=FALSE, main="Words", xlab="Counts in Documents")  


library(ggplot2)   
p <- ggplot(frequency[1:100], aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p 

wf <- data.frame(word=names(freq_blog), freq_blog=freq_blog)
ggplot(wf[wf$freq_blog>4000, ], aes(x=word, y=freq_blog)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("") +
  ylab("Frequency") +
  ggtitle("Words with court > 60,000\n in blog.txt")

wordcloud(names(freq_blog), freq_blog, min.freq=1000,
          max.words=Inf, random.order=FALSE, rot.per=.15,colors=brewer.pal(8,"Dark2"))


wordcloud(data_clean, max.words = 100, random.order = FALSE)
wordcloud(data_clean[[1]], max.words = 100, random.order = FALSE)
wordcloud(data_clean[[2]], max.words = 100, random.order = FALSE)
wordcloud(data_clean[[3]], max.words = 100, random.order = FALSE)

inspect(tdm[1:10,1:3])







# writeLines(as.character(data[[1]]))
# identical(data[2], data_clean[2])

findFreqTerms(dtm, 5) # Terminos que aparecen mas de X
findAssocs(dtm, "home", 0.8)
removeSparseTerms(dtm, 0.4)
inspect(DocumentTermMatrix(data_clean, list(dictionary = c("year", "york"))))

library('wordcloud')
words <- names(frequency)
wordcloud(words[1:100], frequency[1:100])
