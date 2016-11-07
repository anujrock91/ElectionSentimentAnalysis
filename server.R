#INSTALL THE PACKAGES IF REQUIRED


#install.packages('bitops')
#install.packages('RCurl')
#install.packages('RJSONIO')
#install.packages('twitteR')
#install.packages('ROAuth')
#install.packages('jsonlite')
#install.packages('plyr')
#install.packages('stringr')
#install.packages('streamR')



library(bitops)
library(RCurl)
library(RJSONIO)
library(twitteR)
library(ROAuth)
library(jsonlite)
library(plyr)
library(stringr)
library(streamR)

shinyServer(
  function(input, output, session){
    
    output$myPlot <- renderPlot({
      
     candidateName <- input$CandidateName
     radioSelect <- input$timeSelect
     
     if(candidateName == 'Donald Trump' & radioSelect == 'Weekly Trend'){
         
         a = readLines('DonaldTrump.json',warn = FALSE)
         x = jsonlite::fromJSON(a)
         pos = scan('positive-words.txt', what = 'character', comment.char = ';')
         neg = scan('negative-words.txt', what = 'character', comment.char = ';')
         
         #This file sentiment_new.R along with the list of positive and negative words has been taken from url: https://github.com/mjhea0/twitter-sentiment-analysis
         source('sentiment_new.R')
         y = x$text
         cleaned = iconv(y, 'UTF-8','ASCII') 
         analysis = score.sentiment(cleaned, pos.words = pos, neg.words = neg)
         hist(analysis$score, xlab = 'Positive Vs Negative Tweets', las = 1, main = 'Trend of Tweets' , col = c('blue','yellow','green'), probability = T)
         lines(density(analysis$score), col = 2, lwd = 4)
         box()
         
       }
       
    if(candidateName == 'Donald Trump' & radioSelect == 'Now'){
         load('my_oauth.Rdata')
         #serching TWitter
         filterStream(file.name = "Donald.json", # Save tweets in a json file
                   track = c("Donald Trump","president","election"), # Collect tweets mentioning either Affordable Care Act, ACA, or Obamacare
                   language = "en",
                   timeout = 10, # Keep connection alive for 60 seconds
                   oauth = my_oauth) # Use my_oauth file as the OAuth credentials
         
         tweets_df <- parseTweets('Donald.json', simplify = FALSE)
         pos <- scan('positive-words.txt' ,what = 'character', comment.char = ';')
         neg <- scan('negative-words.txt' ,what = 'character', comment.char = ';')
         y = tweets_df$text
         cleaned = iconv(y,'UTF-8','ASCII')
         analysis = score.sentiment(cleaned, pos.words = pos, neg.words = neg)
         
         #This file sentiment_new.R along with the list of positive and negative words has been taken from url: https://github.com/mjhea0/twitter-sentiment-analysis
         source('sentiment_new.R')
         hist(analysis$score, xlab = 'Positive Vs Negative Tweets', las = 1, main = 'Trend of Tweets' , col = c('blue','yellow','green'), probability = T)
         lines(density(analysis$score), col = 2, lwd = 4)
         box()
       }
     
     
     
     if(candidateName == 'Hillary Clinton' & radioSelect == 'Weekly Trend'){
         
          a = readLines('HillaryClinton.json',warn = FALSE)
          x = jsonlite::fromJSON(a)
          pos = scan('positive-words.txt', what = 'character', comment.char = ';')
          neg = scan('negative-words.txt', what = 'character', comment.char = ';')
          
          #This file sentiment_new.R along with the list of positive and negative words has been taken from url: https://github.com/mjhea0/twitter-sentiment-analysis
          source('sentiment_new.R')
          y = x$text
          cleaned = iconv(y,'UTF-8','ASCII')
          analysis = score.sentiment(cleaned, pos.words = pos, neg.words = neg)
          positiveAnalysis <- subset(analysis, analysis$score>=0)
          negativeAnalysis <- subset(analysis, analysis$score<0)
          hist(analysis$score, xlab = 'Positive Vs Negative Tweets', las = 1, main = 'Trend of Tweets' , col = c('blue','yellow','green'), probability = T)
          lines(density(analysis$score), col = 2, lwd = 4)
          box()
       }
      
     if(candidateName == 'Hillary Clinton' & radioSelect == 'Now'){
       load('my_oauth.Rdata')
       #serching TWitter
       filterStream(file.name = "Hillary.json", # Save tweets in a json file
                    track = c("Hillary Clinton","election"), # Collect tweets mentioning either Affordable Care Act, ACA, or Obamacare
                    language = "en",
                    timeout = 10, # Keep connection alive for 60 seconds
                    oauth = my_oauth) # Use my_oauth file as the OAuth credentials
       
       tweets_df <- parseTweets('Hillary.json', simplify = FALSE)
       pos <- scan('positive-words.txt' ,what = 'character', comment.char = ';')
       neg <- scan('negative-words.txt' ,what = 'character', comment.char = ';')
       y = tweets_df$text
       cleaned = iconv(y,'UTF-8','ASCII')
       analysis = score.sentiment(cleaned, pos.words = pos, neg.words = neg)
       
       #This file sentiment_new.R along with the list of positive and negative words has been taken from url: https://github.com/mjhea0/twitter-sentiment-analysis
       source('sentiment_new.R')
       
       hist(analysis$score, xlab = 'Positive Vs Negative Tweets', las = 1, main = 'Trend of Tweets' , col = c('blue','yellow','green'), probability = T)
       lines(density(analysis$score), col = 2, lwd = 4)
       box()
       }
     
     if(candidateName == 'Bernie Sanders' & radioSelect == 'Now'){
       load('my_oauth.Rdata')
       #serching TWitter
       filterStream(file.name = "Bernie.json", # Save tweets in a json file
                    track = c("Bernie Sanders","president","election"), # Collect tweets mentioning either Affordable Care Act, ACA, or Obamacare
                    language = "en",
                    timeout = 10, # Keep connection alive for 60 seconds
                    oauth = my_oauth) # Use my_oauth file as the OAuth credentials
       
       tweets_df <- parseTweets('Bernie.json', simplify = FALSE)
       pos <- scan('positive-words.txt' ,what = 'character', comment.char = ';')
       neg <- scan('negative-words.txt' ,what = 'character', comment.char = ';')
       y = tweets_df$text
       cleaned = iconv(y,'UTF-8','ASCII')
       analysis = score.sentiment(cleaned, pos.words = pos, neg.words = neg)
       
       #This file sentiment_new.R along with the list of positive and negative words has been taken from url: https://github.com/mjhea0/twitter-sentiment-analysis
       source('sentiment_new.R')
       
       hist(analysis$score, xlab = 'Positive Vs Negative Tweets', las = 1, main = 'Trend of Tweets' , col = c('blue','yellow','green'), probability = T)
       lines(density(analysis$score), col = 2, lwd = 4)
       box()
       
     }
     
     if(candidateName == 'Bernie Sanders' & radioSelect == 'Weekly Trend'){
       
       a = readLines('BernieSanders.json',warn = FALSE)
       x = jsonlite::fromJSON(a)
       pos = scan('positive-words.txt', what = 'character', comment.char = ';')
       neg = scan('negative-words.txt', what = 'character', comment.char = ';')
       
       #This file sentiment_new.R along with the list of positive and negative words has been taken from url: https://github.com/mjhea0/twitter-sentiment-analysis
       source('sentiment_new.R')
       y = x$text
       cleaned = iconv(y,'UTF-8','ASCII')
       analysis = score.sentiment(cleaned, pos.words = pos, neg.words = neg)
       hist(analysis$score, xlab = 'Positive Vs Negative Tweets', las = 1, main = 'Trend of Tweets' , col = c('blue','yellow','green'), probability = T)
       lines(density(analysis$score), col = 2, lwd = 4)
       box()
     }
     
     if(candidateName == 'Ted Cruz' & radioSelect == 'Now'){
       load('my_oauth.Rdata')
       #serching TWitter
       filterStream(file.name = "Ted.json", # Save tweets in a json file
                    track = c("Ted Cruz","president","election"), # Collect tweets mentioning either Affordable Care Act, ACA, or Obamacare
                    language = "en",
                    timeout = 10, # Keep connection alive for 60 seconds
                    oauth = my_oauth) # Use my_oauth file as the OAuth credentials
       
       tweets_df <- parseTweets('Ted.json', simplify = FALSE)
       pos <- scan('positive-words.txt' ,what = 'character', comment.char = ';')
       neg <- scan('negative-words.txt' ,what = 'character', comment.char = ';')
       y = tweets_df$text
       cleaned = iconv(y,'UTF-8','ASCII')
       analysis = score.sentiment(cleaned, pos.words = pos, neg.words = neg)
       
       #This file sentiment_new.R along with the list of positive and negative words has been taken from url: https://github.com/mjhea0/twitter-sentiment-analysis
       source('sentiment_new.R')
       
       hist(analysis$score, xlab = 'Positive Vs Negative Tweets', las = 1, main = 'Trend of Tweets' , col = c('blue','yellow','green'), probability = T)
       lines(density(analysis$score), col = 2, lwd = 4)
       box()
       
     }
     
     if(candidateName == 'Ted Cruz' & radioSelect == 'Weekly Trend'){
       
       a = readLines('TedCruz.json',warn = FALSE)
       x = jsonlite::fromJSON(a)
       pos = scan('positive-words.txt', what = 'character', comment.char = ';')
       neg = scan('negative-words.txt', what = 'character', comment.char = ';')
       
       #This file sentiment_new.R along with the list of positive and negative words has been taken from url: https://github.com/mjhea0/twitter-sentiment-analysis
       source('sentiment_new.R')
       y = x$text
       cleaned = iconv(y,'UTF-8','ASCII')
       analysis = score.sentiment(cleaned, pos.words = pos, neg.words = neg)
       hist(analysis$score, xlab = 'Positive Vs Negative Tweets', las = 1, main = 'Trend of Tweets' , col = c('blue','yellow','green'), probability = T)
       lines(density(analysis$score), col = 2, lwd = 4)
       box()
     }
    
      
      
    }
    )
  }
 
)