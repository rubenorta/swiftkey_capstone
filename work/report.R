library("tm")
library("ggplot2")
library("RColorBrewer")

setwd("/home/ruben/Development/swiftkey_capstone/work")

# Retrieve some basic stats about the files
doc_stats <- system("wc -wml en_US/master_data/*.txt", intern=TRUE)
stats <- strsplit(doc_stats, " ")

res_nm <- c(stats[[1]][8],stats[[2]][7], stats[[3]][7])  
res_lc <- c(stats[[1]][4],stats[[2]][3], stats[[3]][3])
res_wc <- c(stats[[1]][6],stats[[2]][5], stats[[3]][5])
res_cc <- c(stats[[1]][7],stats[[2]][6], stats[[3]][6])

summary_1 <- data.frame(res_nm,res_lc, res_wc, res_cc)
names(summary_1)<- c("File Name","Line Counts", "Word Counts", "Character Counts")
summary_1

# Load Datasets & Cleaning
txt <- "./en_US/master_data"
data_ori <- VCorpus(DirSource(txt, encoding = "UTF-8"), readerControl = list(language = "lat"))

# Extract 10% of data to analysis
set.seed(1)
data_ori[[1]]$content <- data_ori[[1]]$content[as.logical(rbinom(length(data_ori[[1]]$content), 1, prob=0.1))]
data_ori[[2]]$content <- data_ori[[2]]$content[as.logical(rbinom(length(data_ori[[2]]$content), 1, prob=0.1))]
data_ori[[3]]$content <- data_ori[[3]]$content[as.logical(rbinom(length(data_ori[[3]]$content), 1, prob=0.1))]

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

dtm_blog <- DocumentTermMatrix(data_clean[1])
dtm_twitter <- DocumentTermMatrix(data_clean[2])
dtm_news <- DocumentTermMatrix(data_clean[3])

dtm_blog <- removeSparseTerms(dtm_blog, 0.95)
dtm_twitter <- removeSparseTerms(dtm_twitter, 0.95)
dtm_news <- removeSparseTerms(dtm_news, 0.95)

#
# Blog
#
freq_blog <- sort(colSums(as.matrix(dtm_blog)), decreasing=TRUE)
freq_blog[1:10]
terms_freq_1 <- findFreqTerms(dtm_blog, highfreq = 1)
length(terms_freq_1)

wf <- data.frame(word=names(freq_blog), freq_blog=freq_blog)
ggplot(wf[wf$freq_blog > 5000, ], aes(x=word, y=freq_blog)) +
  geom_bar(stat="identity") +
  xlab("") +
  ylab("Frequency") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle("Words Counts > 5,000")

pal <- brewer.pal(8,"Dark2")
wordcloud(names(freq_blog), freq_blog, min.freq=1000,
          max.words=Inf, random.order=FALSE, rot.per=.15,colors=pal,vfont=c("sans serif","plain") )

#
# Twitter
#
freq_twitter <- sort(colSums(as.matrix(dtm_twitter)), decreasing=TRUE)
freq_twitter[1:10]

wf <- data.frame(word=names(freq_twitter), freq_blog=freq_twitter)
ggplot(wf[wf$freq_twitter > 5000, ], aes(x=word, y=freq_twitter)) +
  geom_bar(stat="identity") +
  xlab("") +
  ylab("Frequency") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle("Words Counts > 5,000")

#
# News
#

freq_news <- sort(colSums(as.matrix(dtm_news)), decreasing=TRUE)
freq_news[1:10]

wf <- data.frame(word=names(freq_news), freq_news=freq_news)
ggplot(wf[wf$freq_news > 5000, ], aes(x=word, y=freq_news)) +
  geom_bar(stat="identity") +
  xlab("") +
  ylab("Frequency") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle("Words Counts > 5,000")
