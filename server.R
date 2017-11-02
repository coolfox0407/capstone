

suppressWarnings(library(shiny))
suppressWarnings(library(tm))
suppressWarnings(library(stringr))

set.seed(12345)

big <- readRDS("bigram.RData")

trig <- readRDS("trigram.RData")

quadg <- readRDS("quadgram.RData")

message <- "" 


predictWord <- function(inputWord) 
  {
  word_add <- tolower(inputWord)
  word_add <- removePunctuation(word_add)
  word_add <- removeNumbers(word_add)
  word_add <- stripWhitespace(word_add)
  
  inputWord <- strsplit(word_add, " ")[[1]]
  n <- length(inputWord)
  
  if (n>= 3) 
    {
      inputWord <- tail(inputWord,3)
    
        if (identical(character(0),head(quadg[quadg$word1 == inputWord[1] & quadg$word2 == inputWord[2] & quadg$word3 == inputWord[3], 4],1)))
          {
            predictWord(paste(inputWord[2],inputWord[3],sep=" "))
          }
      else 
          {
            message <<- "Next word is predicted using Quad-Gram"
            head(quadg[quadg$word1 == inputWord[1] & quadg$word2 == inputWord[2] & quadg$word3 == inputWord[3], 4],1)
          }
    }
  else if (n == 2)
    {
      inputWord <- tail(inputWord,2)
    
      if (identical(character(0),head(trig[trig$word1 == inputWord[1] & trig$word2 == inputWord[2], 3],1))) 
        {
          predictWord(inputWord[2])
        }
      else 
        {
          message <<- "Next word is predicted using Tri-Gram"
          head(trig[trig$word1 == inputWord[1] & trig$word2 == inputWord[2], 3],1)
        }
    }
  else if (n == 1)
    {
      inputWord <- tail(inputWord,1)
    
      if (identical(character(0),head(big[big$word1 == inputWord[1], 2],1))) 
        {
          message <<- "No match was found. Most common word 'the' is returned."
          head("the",1)
        }
      else 
        {
          message <<- "Next word is predicted using Bi-Gram"
          head(big[big$word1 == inputWord[1],2],1)
        }
    }
  }


# Shiny server logic

shinyServer(function(input, output) 
    {
      output$prediction <- renderPrint(
        {
          result <- predictWord(input$inputString)
          output$text2 <- renderText(message)
          result
        });
  
      output$text1 <- renderText(
        {
          input$inputString
        });
    }
  )
