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
data_clean <- tm_map(data, removeWords, stop_words)

data <- tm_map(data_ori, content_transformer(tolower))
data <- tm_map(data, stripWhitespace)
data <- tm_map(data, removePunctuation)
data <- tm_map(data, removeNumbers)
data <- tm_map(data, stripWhitespace)

dtm <- DocumentTermMatrix(data_clean)
dtm2 <- as.matrix(dtm)
frequency2 <- colSums(dtm2)

# inspect
# inspect(data[])
# writeLines(as.character(data[[1]]))
# identical(data[2], data_clean[2])

findFreqTerms(dtm, 5) # Terminos que aparecen mas de X
findAssocs(dtm, "home", 0.8)
removeSparseTerms(dtm, 0.4)
inspect(DocumentTermMatrix(data_clean, list(dictionary = c("year", "york"))))
