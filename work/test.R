library("tm")

setwd("/home/ruben/Development/swiftkey_capstone/work")
txt <- "./en_US/samples"
 
data <- VCorpus(DirSource(txt, encoding = "UTF-8"),  
                readerControl = list(language = "lat"))

data <- tm_map(data, content_transformer(tolower))
data <- tm_map(data, stripWhitespace)
data_clean <- tm_map(data, removePunctuation)
data_word <- tm_map(data_clean, removeWords, stopwords("english"))

dtm <- DocumentTermMatrix(data_word)

# inspect
# inspect(data[])
# writeLines(as.character(data[[1]]))
# identical(data[2], data_clean[2])

findFreqTerms(dtm, 5)
findAssocs(dtm, "home", 0.8)
removeSparseTerms(dtm, 0.4)
inspect(DocumentTermMatrix(data_clean, list(dictionary = c("year", "york"))))