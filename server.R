### Capstone server.R
#setwd("/Users/peggyfan/Downloads/R_data/Capstone/corpus_en/textPrediction")

library(tm)
library(stringr)
library(data.table)
library(shiny)
library(shinyIncubator)
library(SnowballC)
options(mc.cores=1)

options(shiny.maxRequestSize = 355000000)

source("global.R")

shinyServer(function(input, output, session) {
  withProgress(session, {
    setProgress(message = "Loading, please wait",
               detail = "This may take a few moments...")
    Sys.sleep(5)
    setProgress(detail = "Still working...")
    Sys.sleep(5)
    setProgress(detail = "Just a few more moments...")
    Sys.sleep(5) 
    setProgress(detail = "Almost there...")
    Sys.sleep(5) 
    
############## GT TEXT INPUT ROWS ##############
observe ({
  predictGT <- function(sentence) {
    predictList <- NULL
    
    ######################## 4GRAMS
    sl <- tolower(sentence)
    punct <- '[]\\?!\"’”“‘#$%&(){}+*/:;,._`|~\\[@\\<=>^-]'
    sl <- gsub(punct, "", sl)
    sl <- unlist(strsplit(sl," "))
    sl <- stemDocument(sl)
    len <- length(sl)
    
    trigram <- paste(sl[len-2], sl[len-1], sl[len])
    elems <- unlist(strsplit(trigram, " " ))
    
    if (is.null(elems)) {return(predictList)}
    
    m <- data.table(matrix(elems, ncol = 3, byrow = TRUE))
    setnames(m, c("word1", "word2", "word3"))
    m$start <- paste(m$word1, m$word2, m$word3)
    
    match_Q <- subset(n4_table, start == m$start)
    #match_Q <- n4_table[n4_table$start == m$trigram,]
    
    if(nrow(match_Q) > 0) { 
      # use the counts in the Simple GT table to extract the probability
      setkey(match_Q, count)
      setkey(n4_SGT_DT, r)
      match_Q[, Pgt := n4_SGT_DT[match_Q]$p]
      
      match_Q <- match_Q[order(-Pgt)]
      predictList <- match_Q$word4
    }
    
    if (!is.null(predictList)) {  
      wordList <- list()
      for (i in predictList[1:10]) {
        words <- lapply(i, stemCompletion_mod)[[1]]
        words <- unlist(words$content)
        if (!words=="NA") {wordList <-unlist(c(wordList, words))}
      }
      return(wordList)
    }  
    
    
    ############## TRIGRAMS
    if(nrow(match_Q) == 0) {
      bigram <- paste(sl[len-1], sl[len])
      elems <- unlist(strsplit(bigram, " " ))
      m <- data.table(matrix(elems, ncol = 2, byrow = TRUE))
      setnames(m, c("word1", "word2"))
      m$start <- paste(m$word1, m$word2)
      
      match_T <- subset(n3_table, start == m$start)
      
      if(nrow(match_T) > 0) { 
        # use the counts in the Simple GT table to extract the probability
        setkey(match_T, count)
        setkey(n3_SGT_DT, r)
        match_T[, Pgt := n3_SGT_DT[match_T]$p]
        
        match_T<- match_T[order(-Pgt)]
        predictList <- match_T$word3
        #maxP <- max(match_Q$Pgt)
        #predictList <-  match_Q$word4[match_Q$Pgt == maxP]
      }
      
      if (!is.null(predictList)) {  
        wordList <- list()
        for (i in predictList[1:10]) {
          words <- lapply(i, stemCompletion_mod)[[1]]
          words <- unlist(words$content)
          if (!words=="NA") {wordList <-unlist(c(wordList, words))}
        }
        return(wordList)
      }  

        ################## BIGRAMS
        if (nrow(match_T)==0) {
          unigram <- paste(sl[len])
          elems <- unlist(strsplit(unigram, " " ))
          m <- data.table(matrix(elems, ncol = 1, byrow = TRUE))
          setnames(m, c("word1"))
          m$start <- paste(m$word1)
          #m <- data.frame(lapply(m, as.character), stringsAsFactors=FALSE)
          
          match_B <- subset(n2_table, start == m$start)
          
          if(nrow(match_B) > 0) { 
            # use the counts in the Simple GT table to extract the probability
            setkey(match_B, count)
            setkey(n2_SGT_DT, r)
            match_B[, Pgt := n2_SGT_DT[match_B]$p]
            
            match_B <- match_B[order(-Pgt)]
            predictList <- match_B$word2
          }
          
          if (!is.null(predictList)) {  
            wordList <- list()
            for (i in predictList[1:10]) {
              words <- lapply(i, stemCompletion_mod)[[1]]
              words <- unlist(words$content)
              if (!words=="NA") {wordList <-unlist(c(wordList, words))}
            }
            return(wordList)
          }  
          
          ######################## UNIGRAM
          
          if(nrow(match_B) == 0) {
            n1_table <- n1_table[order(-Pgt)]
            predictList <-  n1_table[1:5]
            
            wordList <- list()
            for (i in predictList[1:10]) {
              words <- lapply(i, stemCompletion_mod)[[1]]
              words <- unlist(words$content)
              if (!words=="NA") {wordList <-unlist(c(wordList, words))}
            }
            return(wordList)
          }
        }}}

  ##### TEXT INPUT 1
  tx1 <- predictGT(input$text1)
  tx1 <- tx1[1:5]
  tx1word1 <- tx1[1]
  if (is.na(tx1word1)) {tx1word1<-""}
  tx1word2 <- tx1[2]
  if (is.na(tx1word2)) {tx1word2<-""}
  tx1word3 <- tx1[3]
  if (is.na(tx1word3)) {tx1word3<-""}
  tx1word4 <- tx1[4]
  if (is.na(tx1word4)) {tx1word4<-""}
  tx1word5 <- tx1[5]
  if (is.na(tx1word5)) {tx1word5<-""}
  tx1list <- c(tx1word1, tx1word2, tx1word3, tx1word4, tx1word5)
  
  output$text2 <- renderText({
    withProgress(session, min=1, max=15, expr={
      for(i in 1:15) {
        setProgress(message = 'Loading in progress',
                    detail = 'Please wait...',
                    value=i)
        print(i)
        Sys.sleep(0.1)
      }
      })
    if(input$updateGT==0) (return("")) 
    else (return(tx1list[1])) #top 2 predicted word
  }) 
  
  output$text3 <- renderText({ 
    if(input$updateGT==0) (return("")) 
    else (return(tx1list[2])) #top 2 predicted word
  
})
  
  output$text4 <- renderText({ 
    if(input$updateGT==0) (return("")) 
    else (return(tx1list[3])) # top 3 predicted word
  })
})  

############## KN TEXT INPUT ROWS ##############

observe ({
  predictKN <- function(sentence) {
    predictList <- NULL
    
    ######################## 4GRAMS
    sl <- tolower(sentence)
    punct <- '[]\\?!\"’”“‘#$%&(){}+*/:;,._`|~\\[@\\<=>^-]'
    sl <- gsub(punct, "", sl)
    sl <- unlist(strsplit(sl," "))
    sl <- stemDocument(sl)
    len <- length(sl)
    
    trigram <- paste(sl[len-2], sl[len-1], sl[len])
    elems <- unlist(strsplit(trigram, " " ))
    
    if (is.null(elems)) {return(predictList)}
    
    m <- data.table(matrix(elems, ncol = 3, byrow = TRUE))
    setnames(m, c("word1", "word2", "word3"))
    m$start <- paste(m$word1, m$word2, m$word3)
    
    match_Q <- subset(new_table4, start == m$start)
    
    if(nrow(match_Q) > 0) { 
      # find the largest probability
      match_Q <- match_Q[order(-Pkn)]
      predictList <- match_Q$word4
    }
    
    if (!is.null(predictList)) {  
      wordList <- list()
      for (i in predictList[1:10]) {
        words <- lapply(i, stemCompletion_mod)[[1]]
        words <- unlist(words$content)
        if (!words=="NA") {wordList <-unlist(c(wordList, words))}
      }
      return(wordList)
    }  
    
    ############## TRIGRAMS
    if(nrow(match_Q) == 0) {
      bigram <- paste(sl[len-1], sl[len])
      elems <- unlist(strsplit(bigram, " " ))
      m <- data.table(matrix(elems, ncol = 2, byrow = TRUE))
      setnames(m, c("word1", "word2"))
      m$start <- paste(m$word1, m$word2)
      
      match_T <- subset(new_table3, start == m$start)
      
      if(nrow(match_T) > 0) { 
        # find the largest probability
        match_T <- match_T[order(-Pkn)]
        predictList <- match_T$word3
      }
      
      if (!is.null(predictList)) {  
        wordList <- list()
        for (i in predictList[1:10]) {
          words <- lapply(i, stemCompletion_mod)[[1]]
          words <- unlist(words$content)
          if (!words=="NA") {wordList <-unlist(c(wordList, words))}
        }
        return(wordList)
      }  
      
      ################## BIGRAMS
      if (nrow(match_T)==0) {
        unigram <- paste(sl[len])
        elems <- unlist(strsplit(unigram, " " ))
        m <- data.table(matrix(elems, ncol = 1, byrow = TRUE))
        setnames(m, c("word1"))
        m$start <- paste(m$word1)
        
        match_B <- subset(new_table2, start == m$start)
        
        if(nrow(match_B) > 0) { 
          # find the largest probability
          match_B <- match_B[order(-Pkn)]
          predictList <- match_B$word2
        }
        
        if (!is.null(predictList)) {  
          wordList <- list()
          for (i in predictList[1:10]) {
            words <- lapply(i, stemCompletion_mod)[[1]]
            words <- unlist(words$content)
            if (!words=="NA") {wordList <-unlist(c(wordList, words))}
          }
          return(wordList)
        }  
        
        ######################## UNIGRAM
        
        if(nrow(match_B) == 0) {
          new_table1 <- new_table1[order(-Pkn)]
          predictList <-  new_table1[1:5]
          
          wordList <- list()
          for (i in predictList[1:10]) {
            words <- lapply(i, stemCompletion_mod)[[1]]
            words <- unlist(words$content)
            if (!words=="NA") {wordList <-unlist(c(wordList, words))}
          }
          return(wordList)
        }
      }}}
  
  ##### TEXT INPUT 2
  tx2 <- predictKN(input$text5)
  tx2 <- tx2[1:5]
  
  tx2word1 <- tx2[1]
  #if (is.null(tx2word1)) {tx2word1<-""}
  if (is.na(tx2word1)) {tx2word1<-""}
  tx2word2 <- tx2[2]
  #if (is.null(tx2word2)) {tx2word2<-""}
  if (is.na(tx2word2)) {tx2word2<-""}
  tx2word3 <- tx2[3]
  #if (is.null(tx2word3)) {tx2word3<-""}
  if (is.na(tx2word3)) {tx2word3<-""}
  tx2word4 <- tx2[4]
  #if (is.null(tx2word4)) {tx2word4<-""}
  if (is.na(tx2word4)) {tx2word4<-""}
  tx2word5 <- tx2[5]
  #if (is.null(tx2word5)) {tx2word5<-""}
  if (is.na(tx2word5)) {tx2word5<-""}
  
  tx2list <- c(tx2word1, tx2word2, tx2word3, tx2word4, tx2word5)
  
  output$text6 <- renderText({ 
    withProgress(session, min=1, max=15, expr={
      for(i in 1:15) {
        setProgress(message = 'Loading in progress',
                    detail = 'Please wait...',
                    value=i)
        print(i)
        Sys.sleep(0.1)
      }
    })
    if(input$updateKN==0) (return("")) 
    else (return(tx2list[1])) #top 1 predicted word
  }) 
  output$text7 <- renderText({ 
    if(input$updateKN==0) (return("")) 
    else (return(tx2list[2])) # top 2 predicted word
  })
  output$text8 <- renderText({ 
    if(input$updateKN==0) (return("")) 
    else (return(tx2list[3])) # top 3 predicted word
  })
}) 

####### GT TEXT AREA CODES ##########
dataset <- reactive ({  
  predictGT <- function(sentence) {
    predictList <- NULL
    
    ######################## 4GRAMS
    sl <- tolower(sentence)
    punct <- '[]\\?!\"’”“‘#$%&(){}+*/:;,._`|~\\[@\\<=>^-]'
    sl <- gsub(punct, "", sl)
    sl <- unlist(strsplit(sl," "))
    sl <- stemDocument(sl)
    len <- length(sl)
    
    trigram <- paste(sl[len-2], sl[len-1], sl[len])
    elems <- unlist(strsplit(trigram, " " ))
    
    if (is.null(elems)) {return(predictList)}
    
    m <- data.table(matrix(elems, ncol = 3, byrow = TRUE))
    setnames(m, c("word1", "word2", "word3"))
    m$start <- paste(m$word1, m$word2, m$word3)
    
    match_Q <- subset(n4_table, start == m$start)
    #match_Q <- n4_table[n4_table$start == m$trigram,]
    
    if(nrow(match_Q) > 0) { 
      # use the counts in the Simple GT table to extract the probability
      setkey(match_Q, count)
      setkey(n4_SGT_DT, r)
      match_Q[, Pgt := n4_SGT_DT[match_Q]$p]
      
      match_Q <- match_Q[order(-Pgt)]
      predictList <- match_Q$word4
    }
    
    if (!is.null(predictList)) {  
      wordList <- list()
      for (i in predictList[1:10]) {
        words <- lapply(i, stemCompletion_mod)[[1]]
        words <- unlist(words$content)
        if (!words=="NA") {wordList <-unlist(c(wordList, words))}
      }
      return(wordList)
    }  
    
    
    ############## TRIGRAMS
    if(nrow(match_Q) == 0) {
      bigram <- paste(sl[len-1], sl[len])
      elems <- unlist(strsplit(bigram, " " ))
      m <- data.table(matrix(elems, ncol = 2, byrow = TRUE))
      setnames(m, c("word1", "word2"))
      m$start <- paste(m$word1, m$word2)
      
      match_T <- subset(n3_table, start == m$start)
      
      if(nrow(match_T) > 0) { 
        # use the counts in the Simple GT table to extract the probability
        setkey(match_T, count)
        setkey(n3_SGT_DT, r)
        match_T[, Pgt := n3_SGT_DT[match_T]$p]
        
        match_T<- match_T[order(-Pgt)]
        predictList <- match_T$word3
        #maxP <- max(match_Q$Pgt)
        #predictList <-  match_Q$word4[match_Q$Pgt == maxP]
      }
      
      if (!is.null(predictList)) {  
        wordList <- list()
        for (i in predictList[1:10]) {
          words <- lapply(i, stemCompletion_mod)[[1]]
          words <- unlist(words$content)
          if (!words=="NA") {wordList <-unlist(c(wordList, words))}
        }
        return(wordList)
      }  
        
        ################## BIGRAMS
        if (nrow(match_T)==0) {
          unigram <- paste(sl[len])
          elems <- unlist(strsplit(unigram, " " ))
          m <- data.table(matrix(elems, ncol = 1, byrow = TRUE))
          setnames(m, c("word1"))
          m$start <- paste(m$word1)
          #m <- data.frame(lapply(m, as.character), stringsAsFactors=FALSE)
          
          match_B <- subset(n2_table, start == m$start)
          
          if(nrow(match_B) > 0) { 
            # use the counts in the Simple GT table to extract the probability
            setkey(match_B, count)
            setkey(n2_SGT_DT, r)
            match_B[, Pgt := n2_SGT_DT[match_B]$p]
            
            match_B <- match_B[order(-Pgt)]
            predictList <- match_B$word2
          }
          
          if (!is.null(predictList)) {  
            wordList <- list()
            for (i in predictList[1:10]) {
              words <- lapply(i, stemCompletion_mod)[[1]]
              words <- unlist(words$content)
              if (!words=="NA") {wordList <-unlist(c(wordList, words))}
            }
            return(wordList)
          }  
          
          ######################## UNIGRAM
          
          if(nrow(match_B) == 0) {
            n1_table <- n1_table[order(-Pgt)]
            predictList <-  n1_table[1:5]
            
            wordList <- list()
            for (i in predictList[1:10]) {
              words <- lapply(i, stemCompletion_mod)[[1]]
              words <- unlist(words$content)
              if (!words=="NA") {wordList <-unlist(c(wordList, words))}
            }
            return(wordList)
          }
        }}}

  x <- predictGT(input$textGT)
  x <- x[1:5]
  word1 <- x[1]
  if (is.null(word1)) {word1<-""}
  word2 <- x[2]
  if (is.null(word2)) {word2<-""}
  if (is.na(word2)) {word2<-""}
  word3 <- x[3]
  if (is.null(word3)) {word3<-""}
  if (is.na(word3)) {word3<-""}
  word4 <- x[4]
  if (is.null(word4)) {word4<-""}
  if (is.na(word4)) {word4<-""}
  word5 <- x[5]
  if (is.null(word5)) {word5<-""}
  if (is.na(word5)) {word5<-""}
  
 list <- c(word1, word2, word3, word4, word5)
})

   output$b1 <- renderUI({ 
     withProgress(session, min=1, max=15, expr={
       for(i in 1:15) {
         setProgress(message = 'Loading...',
                     detail = 'Please wait until the buttons below the text box pop up.',
                     value=i)
         print(i)
         Sys.sleep(0.1)
       }
     })
     actionButton("word1", label = dataset()[1]) # return top 1 predicted word
   })
   output$b2 <- renderUI({ 
     actionButton("word2", label = dataset()[2]) # return top 2 predicted word
   })
   output$b3 <- renderUI({ 
     actionButton("word3", label = dataset()[3]) # return top 3 predicted word
   })
   output$b4 <- renderUI({ 
     actionButton("word4", label = dataset()[4]) # return top 4 predicted word
   })
   output$b5 <- renderUI({ 
     actionButton("word5", label = dataset()[5]) # return top 5 predicted word
   })

####### KN TEXT AREA CODES ##########
dataset1 <- reactive ({  
  predictKN <- function(sentence) {
    predictList <- NULL
    
    ######################## 4GRAMS
    sl <- tolower(sentence)
    punct <- '[]\\?!\"’”“‘#$%&(){}+*/:;,._`|~\\[@\\<=>^-]'
    sl <- gsub(punct, "", sl)
    sl <- unlist(strsplit(sl," "))
    sl <- stemDocument(sl)
    len <- length(sl)
    
    trigram <- paste(sl[len-2], sl[len-1], sl[len])
    elems <- unlist(strsplit(trigram, " " ))
    
    if (is.null(elems)) {return(predictList)}
    
    m <- data.table(matrix(elems, ncol = 3, byrow = TRUE))
    setnames(m, c("word1", "word2", "word3"))
    m$start <- paste(m$word1, m$word2, m$word3)
    
    match_Q <- subset(new_table4, start == m$start)
    
    if(nrow(match_Q) > 0) { 
      # find the largest probability
      match_Q <- match_Q[order(-Pkn)]
      predictList <- match_Q$word4
      #maxP <- max(match_Q$Pgt)
      #predictList <-  match_Q$word4[match_Q$Pgt == maxP]
    }
    
    if (!is.null(predictList)) {  
      wordList <- list()
      for (i in predictList[1:10]) {
        words <- lapply(i, stemCompletion_mod)[[1]]
        words <- unlist(words$content)
        if (!words=="NA") {wordList <-unlist(c(wordList, words))}
      }
      return(wordList)
    }  
    
    ############## TRIGRAMS
    if(nrow(match_Q) == 0) {
      bigram <- paste(sl[len-1], sl[len])
      elems <- unlist(strsplit(bigram, " " ))
      m <- data.table(matrix(elems, ncol = 2, byrow = TRUE))
      setnames(m, c("word1", "word2"))
      m$start <- paste(m$word1, m$word2)
      
      match_T <- subset(new_table3, start == m$start)
      
      if(nrow(match_T) > 0) { 
        # find the largest probability
        match_T <- match_T[order(-Pkn)]
        predictList <- match_T$word3
      }
      
      if (!is.null(predictList)) {  
        wordList <- list()
        for (i in predictList[1:10]) {
          words <- lapply(i, stemCompletion_mod)[[1]]
          words <- unlist(words$content)
          if (!words=="NA") {wordList <-unlist(c(wordList, words))}
        }
        return(wordList)
      }  
      
      ################## BIGRAMS
      if (nrow(match_T)==0) {
        unigram <- paste(sl[len])
        elems <- unlist(strsplit(unigram, " " ))
        m <- data.table(matrix(elems, ncol = 1, byrow = TRUE))
        setnames(m, c("word1"))
        m$start <- paste(m$word1)
        
        match_B <- subset(new_table2, start == m$start)
        
        if(nrow(match_B) > 0) { 
          # find the largest probability
          match_B <- match_B[order(-Pkn)]
          predictList <- match_B$word2
          
        }
        
        if (!is.null(predictList)) {  
          wordList <- list()
          for (i in predictList[1:10]) {
            words <- lapply(i, stemCompletion_mod)[[1]]
            words <- unlist(words$content)
            if (!words=="NA") {wordList <-unlist(c(wordList, words))}
          }
          return(wordList)
        }  
        
        ######################## UNIGRAM
        
        if(nrow(match_B) == 0) {
          new_table1 <- new_table1[order(-Pgt)]
          predictList <-  new_table1[1:5]
          wordList <- list()
          for (i in predictList[1:10]) {
            words <- lapply(i, stemCompletion_mod)[[1]]
            words <- unlist(words$content)
            if (!words=="NA") {wordList <-unlist(c(wordList, words))}
          }
          return(wordList)
        }
      }}}
  
  y <- predictKN(input$textKN)
  y <- y[1:5]
  KNword1 <- y[1]
  if (is.null(KNword1)) {KNword1<-""}
  KNword2 <- y[2]
  if (is.null(KNword2)) {KNword2<-""}
  if (is.na(KNword2)) {KNword2<-""}
  KNword3 <- y[3]
  if (is.null(KNword3)) {KNword3<-""}
  if (is.na(KNword3)) {KNword3<-""}
  KNword4 <- y[4]
  if (is.null(KNword4)) {KNword4<-""}
  if (is.na(KNword4)) {KNword4<-""}
  KNword5 <- y[5]
  if (is.null(KNword5)) {KNword5<-""}
  if (is.na(KNword5)) {KNword5<-""}
  
  KNlist <- c(KNword1, KNword2, KNword3, KNword4, KNword5)
})

output$b6 <- renderUI({ 
  withProgress(session, min=1, max=15, expr={
    for(i in 1:15) {
      setProgress(message = 'Loading...',
                  detail = 'Please wait until the buttons below the text box pop up.',
                  value=i)
      print(i)
      Sys.sleep(0.1)
    }
  })
  actionButton("word1", label = dataset1()[1]) # return top 1 predicted word
})
output$b7 <- renderUI({ 
  actionButton("word2", label = dataset1()[2]) # return top 2 predicted word
})
output$b8 <- renderUI({ 
  actionButton("word3", label = dataset1()[3]) # return top 3 predicted word
})
output$b9 <- renderUI({ 
  actionButton("word4", label = dataset1()[4]) # return top 4 predicted word
})
output$b10 <- renderUI({ 
  actionButton("word5", label = dataset1()[5]) # return top 5 predicted word
  
})

})
})


