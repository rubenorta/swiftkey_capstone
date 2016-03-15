library("tm")
library("magrittr")

setwd("/home/ruben/Development/swiftkey_capstone/work")
txt <- "./en_US/samples"

# http://www.cs.cmu.edu/~biglou/resources/bad-words.txt

data_ori <- VCorpus(DirSource(txt, encoding = "UTF-8"),  
                readerControl = list(language = "lat"))

# remove stopwords, badwords and other words
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

doc_stats <- system("wc -wml en_US/samples/*.txt", intern=TRUE)
stats <- strsplit(doc_stats, " ")


# Extracting some basic stats
res_nm <- c(stats[[1]][7],stats[[2]][7], stats[[3]][8])  
res_lc <- c(stats[[1]][3],stats[[2]][3], stats[[3]][3])
res_wc <- c(stats[[1]][5],stats[[2]][5], stats[[3]][5])
res_cc <- c(stats[[1]][6],stats[[2]][6], stats[[3]][7])
#res_wc <- rowSums(as.matrix(dtm))

summary_1 <- data.frame(res_nm,res_lc, res_wc, res_cc)
names(summary_1)<- c("File Name","Line Counts", "Word Counts", "Character Counts")
summar_1


res_line <- sapply(data_ori, length) 

dtm2 <- as.matrix(dtm)w
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)



# inspect
# inspect(data[])
# writeLines(as.character(data[[1]]))
# identical(data[2], data_clean[2])

findFreqTerms(dtm, 5) # Terminos que aparecen mas de X
findAssocs(dtm, "home", 0.8)
removeSparseTerms(dtm, 0.4)
inspect(DocumentTermMatrix(data_clean, list(dictionary = c("year", "york"))))

library('wordcloud')
words <- names(frequency)
wordcloud(words[1:100], frequency[1:100])
