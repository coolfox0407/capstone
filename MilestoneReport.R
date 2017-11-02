
## This assigment is part of Coursera's "Data Science Capstone: SwiftKey Project" module
## Week 2 Milestone Report: Text Prediction model from HC Corpora dataset
## Description: This R script loads "HC Corpora" dataset from 
## "Coursera-SwiftKey.zip" and the English language dataset is loaded from
## the files "en_US.blogs.txt", "en_US.twitter.txt", "en_US.news.txt"
## Dataset is then pre-processed, cleaned, exploratory data analysis is performed
## design word prediction using "n-gram" model

## R File Name: "MilestoneReport.R"
## Input Files: "en_US.blogs.txt", "en_US.twitter.txt", "en_US.news.txt"
## Ouput Files: 

## Date of submission of assigment: 28-October-2017
## GitHub User Name: coolfox0407
## Author: Hariharan D


# Load the necessary R libraries.

# Note: It is assumed that the below R libraries are already installed.

library(downloader)
library(plyr)
library(dplyr)
library(tm)
library(ggplot2)
library(RWeka)
library(SnowballC)
library(wordcloud)

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

countTwitter <- c(round(fileSizeMB(enUSTwitter),0), lineCount(lineTwitter), wordCount(lineTwitter), meanSentenceLength(lineTwitter))
countBlogs <- c(round(fileSizeMB(enUSBlogs),0), lineCount(lineBlogs), wordCount(lineBlogs), meanSentenceLength(lineBlogs))
countNews <- c(round(fileSizeMB(enUSNews),0), lineCount(lineNews), wordCount(lineNews), meanSentenceLength(lineNews))

countEnUS <- rbind(countTwitter, countBlogs, countNews)
rownames(countEnUS) <- c("Twitter", "Blogs", "News")
colnames(countEnUS) <- c("File-Size(MB)", "Lines-Count", "Words-Count", "Mean-Words-Per-Line")

countEnUS

lineTwitter <- iconv(lineTwitter, "UTF-8", "ASCII", sub="")
lineBlogs <- iconv(lineBlogs, "UTF-8", "ASCII", sub="")
lineNews <- iconv(lineNews, "UTF-8", "ASCII", sub="")





set.seed(123456)

sampleData <- c(sample(lineTwitter, length(lineTwitter) * 0.02), sample(lineBlogs, length(lineBlogs) * 0.02), sample(lineNews, length(lineNews) * 0.02))
sampleData <- unlist(strsplit(sampleData, "[\\.\\,!\\?\\:]+"))

corpusData <- sampleData

# corpusData <- VCorpus(VectorSource(sampleData), readerControl=list(reader=readPlain,language="english"))
# corpusData <- unlist(strsplit(corpusData, "[\\.\\,!\\?\\:]+"))
# toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
# corpusData <- tm_map(corpusData, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
# corpusData <- tm_map(corpusData, toSpace, "@[^\\s]+")

corpusData <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+"," ",corpusData)
corpusData <- gsub("@[^\\s]+"," ",corpusData)
corpusData <- tolower(corpusData)
# corpusData <- removeWords(corpusData, stopwords("english"))
corpusData <- removePunctuation(corpusData)
corpusData <- removeNumbers(corpusData)
corpusData <- trimws(corpusData)
xcorpusData3 <- unlist(strsplit(corpusData,"\\s"))
xcorpusData2 <- strsplit(corpusData,"\\s")
# corpusData <- stripWhitespace(corpusData)
# corpusData11 <- stemDocument(corpusData)
# corpusData <- PlainTextDocument(corpusData)

wordcloud(xcorpusData3, max.words=100, random.order=FALSE, colors=brewer.pal(8,"Accent"))

profanityFilter <- function(termList)
{
  profanities <- readLines("./Profanity.txt", skipNul = TRUE)
  lapply(termList, setdiff, y=profanities)
}

# samcorpusData1 <- profanityFilter(xcorpusData2)
samcorpusData <- profanityFilter(xcorpusData2)

# stopwordFilter <- function(termList)
# {
#   stopwords <- readLines("./StopWords.txt", skipNul = TRUE)
#   lapply(termList, setdiff, y=stopwords)
# }  
# 
# samcorpusData <- stopwordFilter(samcorpusData)  

head(samcorpusData,5)
length(samcorpusData)
sum(sapply(samcorpusData, length))

# options(mc.cores=1)

# getFreq <- function(tdm)
#   {
#     freq <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
#     return(data.frame(word = names(freq), freq = freq))
#   }

getFreq <- function(termList)
{
  term <- data.frame(unlist(termList))
  grouped <- as.data.frame(table(term))
  freq <- grouped[order(-grouped$Freq),]
  rownames(freq) <- 1:nrow(freq)
  
  total <- sum(freq$Freq)
  freq$CumFreq <- cumsum(freq$Freq)
  freq$Coverage <- freq$CumFreq/total
  
  return(freq)
}	

sampleENTermFrequency <- getFreq(samcorpusData)
# sampleENTermFrequency1 <- getFreq(samcorpusData1)

tmp <- sampleENTermFrequency[1:50,]
tmp$termLength <-  nchar(as.character(tmp$term))

ggplot(tmp, aes(x=reorder(term,Freq), y=Freq, fill=termLength)) +
  geom_bar(stat="identity") +
  coord_flip() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank())


filterFrequencyTable <- function(freqTable, binCoverageShift=0.01)
{
  shiftTotal <- 0
  
  freqTable$keep <- FALSE
  
  for (n in 1:nrow(freqTable)) 
  {
    shiftTotal <- shiftTotal + freqTable$Coverage[n]
    
    if(shiftTotal >= binCoverageShift)
    {
      freqTable$keep[n] <- TRUE
      shiftTotal <- 0
    }
  }
  
  freqTable[freqTable$keep == TRUE,]
}


# unigram <- function(x)
# {
#   NGramTokenizer(x, Weka_control(min = 1, max = 1))
# }
# 
# bigram <- function(x)
# {
#   NGramTokenizer(x, Weka_control(min = 2, max = 2))
# }
# 
# trigram <- function(x)
# {
#   NGramTokenizer(x, Weka_control(min = 3, max = 3))
# }
# 
# quadgram <- function(x)
# {
#   NGramTokenizer(x, Weka_control(min = 4, max = 4))
# }

# sampleENUniGram <- unigram(samcorpusData)
# sampleENUniGramFrequency <- getFreq(sampleENUniGram)
# 
# plotUniGram <- filterFrequencyTable(sampleENUniGramFrequency, 0.005)
# ggplot(plotUniGram, aes(y=as.integer(rownames(plotUniGram)), x=Coverage)) +
#   geom_line() +
#   coord_flip() + 
#   labs(x="Coverage",y="Observations") +
#   theme(panel.border = element_blank(), 
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), 
#         panel.background = element_blank()
#   )


createNgram <- function(vec, n=2){
  l <- length(vec) 
  if(l < n){
    return(c())
  }else if(l == n){
    return(paste(vec, collapse = " "))
  }else{
    numNgrams <- l-n+1
    mtrx <- matrix(nrow=numNgrams, ncol=n)
    for(i in 1:n){
      m <- l - n + i
      mtrx[,i] <- vec[i:m]
    }
    ngrams <- apply(mtrx, 1, paste, collapse=" ")
    return(ngrams)
  }
} 

transformNGram <- function(termList, n=2){
  lapply(termList, createNgram, n=n)
}

sampleENBiGrams <- transformNGram(samcorpusData,2)
sampleENBiGramsFrequency <- getFreq(sampleENBiGrams)

plotBiGram <- filterFrequencyTable(sampleENBiGramsFrequency, 0.005)
ggplot(plotBiGram, aes(y=as.integer(rownames(plotBiGram)), x=Coverage)) +
  geom_line() +
  coord_flip() + 
  labs(x="Coverage",y="Observations") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()
  )


sampleENTriGrams <- transformNGram(samcorpusData,3)
sampleENTriGramsFrequency <- getFreq(sampleENTriGrams)

plotTriGram <- filterFrequencyTable(sampleENTriGramsFrequency, 0.005)
ggplot(plotTriGram, aes(y=as.integer(rownames(plotTriGram)), x=Coverage)) +
  geom_line() +
  coord_flip() + 
  labs(x="Coverage",y="Observations") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()
  )

# 
# sampleENQuadGrams <- transformNGram(samcorpusData,4)
# sampleENQuadGramsFrequency <- getFreq(sampleENQuadGrams)
# 
# plotQuadGram <- filterFrequencyTable(sampleENQuadGramsFrequency, 0.005)
# ggplot(plotQuadGram, aes(y=as.integer(rownames(plotQuadGram)), x=Coverage)) +
#   geom_line() +
#   coord_flip() + 
#   labs(x="Coverage",y="Observations") +
#   theme(panel.border = element_blank(), 
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), 
#         panel.background = element_blank()
#   )


coverageFactor <- function(freqTable, coverage)
  {
    pos <- nrow(freqTable[freqTable$Coverage < coverage,])
    pos / nrow(freqTable) 
  }


coverageFactorValues <- c(0.1,0.5,0.9)
uniCov <- sapply(coverageFactorValues, coverageFactor, freqTable=samcorpusData)
biCov <- sapply(coverageFactorValues, coverageFactor, freqTable=sampleENBiGramsFrequency)
triCov <- sapply(coverageFactorValues, coverageFactor, freqTable=sampleENTriGramsFrequency)
# quadCov <- sapply(coverageFactorValues, coverageFactor, freqTable=sampleENQuadGramsFrequency)

infoCov <- rbind(uniCov, biCov, triCov)
rownames(infoCov) <- c("uni-gram", "bi-gram", "tri-gram")
colnames(infoCov) <- coverageFactorValues
infoCov
