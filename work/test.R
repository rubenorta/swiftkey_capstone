library("tm")

setwd("/home/ruben/Development/swiftkey_capstone/work")
txt <- "./en_US"

ovid <- VCorpus(DirSource(txt, encoding = "UTF-8"),  
                readerControl = list(language = "lat"))