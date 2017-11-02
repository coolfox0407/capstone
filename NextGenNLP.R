
## This project is part of Coursera's "Data Science Capstone: SwiftKey Project" module
## "NextGen NLP" App: Text Prediction model from HC Corpora dataset
## Description: This R script loads "HC Corpora" dataset from 
## "Coursera-SwiftKey.zip" and the English language dataset is loaded from
## the files "en_US.blogs.txt", "en_US.twitter.txt", "en_US.news.txt"
## Dataset is then pre-processed, cleaned, exploratory data analysis is performed
## design word prediction using "n-gram" model

## R File Name: "NextGenNLP.R"
## Input Files: "en_US.blogs.txt", "en_US.twitter.txt", "en_US.news.txt"
## Ouput Files: "sampleData.txt", "corpus.RData", "unigram.RData", "unigram.csv",
## "bigram.RData", "bigram.csv", "trigram.RData", "trigram.csv"
## "quadgram.RData", "trigram.csv"

## Date of submission of assigment: 02-November-2017
## Author: Hariharan D


# Load the necessary R libraries.

# Note: It is assumed that the below R libraries are already installed.


options(java.parameters = "-Xmx2048m")

suppressWarnings(library(downloader))
suppressWarnings(library(plyr))
suppressWarnings(library(dplyr))
suppressWarnings(library(tm))
suppressWarnings(library(ggplot2))
suppressWarnings(library(RWeka))
suppressWarnings(library(SnowballC))
suppressWarnings(library(wordcloud))
suppressWarnings(library(stringi))
suppressWarnings(library(NLP))
suppressWarnings(library(openNLP))
suppressWarnings(library(rJava))
suppressWarnings(library(RWekajars))
suppressWarnings(library(RColorBrewer))
suppressWarnings(library(qdap))

set.seed(12345)

setwd("C:/Users/Welcome/Desktop/Data Science/R/Coursera Reading Material/Course 10 - Data Science Capstone/Project/")

fileUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

if(!file.exists("./DataSet"))
{
  dir.create("./DataSet")
}

if(!file.exists("./DataSet/Coursera-SwiftKey.zip"))
{
  download.file(fileUrl, destfile = "./DataSet/Coursera-SwiftKey.zip",mode = "wb")
  Download_Date <- Sys.time()
}

if(!file.exists("./DataSet/final"))
{
  unzip(zipfile="./DataSet/Coursera-SwiftKey.zip",exdir="./DataSet")
}

pathFiles <- file.path("./DataSet/final", "en_US")
files <- list.files(pathFiles, recursive=TRUE)
files

enUSTwitter <- "./DataSet/final/en_US/en_US.twitter.txt"
enUSBlogs <- "./DataSet/final/en_US/en_US.blogs.txt"
enUSNews <- "./DataSet/final/en_US/en_US.news.txt"



con <- file(enUSTwitter, "r") 
lineTwitter <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

con <- file(enUSBlogs, "r") 
lineBlogs <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

con <- file(enUSNews, "r") 
lineNews <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)


fileSizeMB <- function(lns)
{
  file.info(lns)$size/(1024^2)
}

lineCount <- function(lns)
{
  length(lns)
}

wordCount <- function(lns)
{
  sum(sapply(gregexpr("\\S+", lns), length))
}

meanSentenceLength <- function(lns)
{
  mean(sapply(gregexpr("\\S+", lns), length))
}

maxChars <- function(lns)
{
  max(nchar(lns))
}

countTwitter <- c(round(fileSizeMB(enUSTwitter),0), lineCount(lineTwitter), wordCount(lineTwitter), meanSentenceLength(lineTwitter), maxChars(lineTwitter))
countBlogs <- c(round(fileSizeMB(enUSBlogs),0), lineCount(lineBlogs), wordCount(lineBlogs), meanSentenceLength(lineBlogs), maxChars(lineBlogs))
countNews <- c(round(fileSizeMB(enUSNews),0), lineCount(lineNews), wordCount(lineNews), meanSentenceLength(lineNews), maxChars(lineNews))

countEnUS <- rbind(countTwitter, countBlogs, countNews)
rownames(countEnUS) <- c("Twitter", "Blogs", "News")
colnames(countEnUS) <- c("File-Size(MB)", "Lines-Count", "Words-Count", "Mean-Words-Per-Line", "Max-Chars-Per-Line")

countEnUS

options(mc.cores=1)
lineTwitter <- iconv(lineTwitter, "UTF-8", "ASCII", sub="")
lineBlogs <- iconv(lineBlogs, "UTF-8", "ASCII", sub="")
lineNews <- iconv(lineNews, "UTF-8", "ASCII", sub="")

set.seed(123456)

sampleData <- c(sample(lineTwitter, length(lineTwitter) * 0.1), sample(lineBlogs, length(lineBlogs) * 0.1), sample(lineNews, length(lineNews) * 0.1))
length(sampleData)

writeLines(sampleData, "./sampleData.txt")


corpus <- VCorpus(VectorSource(sampleData))
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
corpus <- tm_map(corpus, toSpace, "@[^\\s]+")
corpus <- tm_map(corpus, content_transformer(tolower), lazy = TRUE)
corpus <- tm_map(corpus, content_transformer(removePunctuation), preserve_intra_word_dashes=TRUE)
corpus <- tm_map(corpus, content_transformer(removeNumbers))
profanities <- readLines("./Profanity.txt", skipNul = TRUE)
corpus <- tm_map(corpus,removeWords, profanities)
# profanityFilter <- function(termList)
# {
#   profanities <- readLines("./Profanity.txt", skipNul = TRUE)
#   lapply(termList, setdiff, y=profanities)
# }
# corpus <- profanityFilter(corpus)
# corpus <- VCorpus(VectorSource(corpus))
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
corpus <- tm_map(corpus, content_transformer(removeURL))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, PlainTextDocument)

saveRDS(corpus, file = "./corpus.RData")

# corpus <- readRDS(file = "./corpus.RData")

corpus <- data.frame(text=unlist(sapply(corpus,`[`, "content")),stringsAsFactors = FALSE)


#sampleData <- unlist(strsplit(sampleData, "[\\.\\,!\\?\\:]+"))

#wordcloud(corpus, max.words=100, random.order=FALSE, colors=brewer.pal(8,"Accent"))

# Uni-Gram

unigram <- NGramTokenizer(corpus, Weka_control(min = 1, max = 1,delimiters = " \\r\\n\\t.,;:\"()?!"))
unigram <- data.frame(table(unigram))
unigram <- unigram[order(unigram$Freq,decreasing = TRUE),]

names(unigram) <- c("word1", "freq")
# head(unigram)
unigram$word1 <- as.character(unigram$word1)

write.csv(unigram[unigram$freq > 1,],"./unigram.csv",row.names=F)
unigram <- read.csv("./unigram.csv",stringsAsFactors = F)
saveRDS(unigram, file = "./unigram.RData")

# Bi-Grams

bigram <- NGramTokenizer(corpus, Weka_control(min = 2, max = 2,delimiters = " \\r\\n\\t.,;:\"()?!"))
bigram <- data.frame(table(bigram))
bigram <- bigram[order(bigram$Freq,decreasing = TRUE),]

names(bigram) <- c("words","freq")
# head(bigram)
bigram$words <- as.character(bigram$words)
str2 <- strsplit(bigram$words,split=" ")
bigram <- transform(bigram, 
                    one = sapply(str2,"[[",1),   
                    two = sapply(str2,"[[",2))
bigram <- data.frame(word1 = bigram$one,word2 = bigram$two,freq = bigram$freq,stringsAsFactors=FALSE)

write.csv(bigram[bigram$freq > 1,],"./bigram.csv",row.names=F)
bigram <- read.csv("./bigram.csv",stringsAsFactors = F)
saveRDS(bigram,"./bigram.RData")

# Tri-Grams

trigram <- NGramTokenizer(corpus, Weka_control(min = 3, max = 3,delimiters = " \\r\\n\\t.,;:\"()?!"))
trigram <- data.frame(table(trigram))
trigram <- trigram[order(trigram$Freq,decreasing = TRUE),]

names(trigram) <- c("words","freq")
# head(trigram)
trigram$words <- as.character(trigram$words)
str3 <- strsplit(trigram$words,split=" ")
trigram <- transform(trigram,
                     one = sapply(str3,"[[",1),
                     two = sapply(str3,"[[",2),
                     three = sapply(str3,"[[",3))
trigram <- data.frame(word1 = trigram$one,word2 = trigram$two, 
                      word3 = trigram$three, freq = trigram$freq,stringsAsFactors=FALSE)

write.csv(trigram[trigram$freq > 1,],"./trigram.csv",row.names=F)
trigram <- read.csv("./trigram.csv",stringsAsFactors = F)
saveRDS(trigram,"./trigram.RData")


# Quad-Grams

quadgram <- NGramTokenizer(corpus, Weka_control(min = 4, max = 4,delimiters = " \\r\\n\\t.,;:\"()?!"))
quadgram <- data.frame(table(quadgram))
quadgram <- quadgram[order(quadgram$Freq,decreasing = TRUE),]

names(quadgram) <- c("words","freq")
head(quadgram)
quadgram$words <- as.character(quadgram$words)

str4 <- strsplit(quadgram$words,split=" ")
quadgram <- transform(quadgram,
                      one = sapply(str4,"[[",1),
                      two = sapply(str4,"[[",2),
                      three = sapply(str4,"[[",3), 
                      four = sapply(str4,"[[",4))

quadgram <- data.frame(word1 = quadgram$one,
                       word2 = quadgram$two, 
                       word3 = quadgram$three, 
                       word4 = quadgram$four, 
                       freq = quadgram$freq, stringsAsFactors=FALSE)

write.csv(quadgram[quadgram$freq > 1,],"./quadgram.csv",row.names=F)
quadgram <- read.csv("./quadgram.csv",stringsAsFactors = F)
saveRDS(quadgram,"./quadgram.RData")


