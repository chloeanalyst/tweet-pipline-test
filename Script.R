library(rtweet)
library(tidyverse)
library(tidytext)
library(textdata)
library(lubridate)
#library(splitstackshape)
library(RSQLite)


#------------------------------------------------------------------------------- Create Data Base
conn <- dbConnect(RSQLite::SQLite(), "Twitter_USOpen_Sentiment.db")

 dbExecute(conn, "CREATE TABLE Twitter_USOpen_Sentiment(
                   created_at DATE,
                   avg_sentiment NUMERIC,
                   total_sentiment NUMERIC
                   )")

 
 
 conn2 <- dbConnect(RSQLite::SQLite(), "Twitter_USOpen_Tweets.db")
 
 dbExecute(conn2, "CREATE TABLE Twitter_USOpen_Tweets(
                   text TEXT,
                   created_at TEXT,
                   word_count NUMERIC
                   )")
 
 #------------------------------------------------------------------------------- Loop Start

 i = 1
 
 while(i < 10) {
 
 
 #------------------------------------------------------------------------------- Collect Tweets
 


tweets <- search_tweets(  # Collect Tweets.
  "#USOpen",  # Hashtag you would like to analyse
  n = 10, # Increase to collect the number of tweets you are interested in.
  lang = "en", # Language of tweets
  include_rts = FALSE, # Exclude duplicated retweeted Tweets
  retryonratelimit = TRUE # If TRUE script will sleep until rate limit has reset.
) %>% select(text, created_at) %>%
  mutate(word_count = str_count(text,"\\S+")) #Calculates the word count of each tweet (not used in output but is useful for checking quality).


#------------------------------------------------------------------------------- Analyse Sentiment



output <-  tweets %>% unnest_tokens(word,text, drop=FALSE) %>% # Calculate Sentiment.
  inner_join(get_sentiments("afinn")) %>% # Using AFINN lexicon for sentiment score.
  group_by(text,created_at) %>%
  summarise(sentiment_score = sum(value)) # Calculates score for each tweet.
output$created_at <- as.Date(output$created_at) # Change date to date format.



daily_overall <- output %>% group_by(created_at) %>% # Sentiment score per day.
  summarise(avg_sentiment = mean(sentiment_score),
            total_sentiment = sum(sentiment_score)) #Calculates avg and total score.



#------------------------------------------------------------------------------- Update database


dbWriteTable(conn, "Twitter_USOpen_Sentiment", daily_overall, append = T)
dbWriteTable(conn2, "Twitter_USOpen_Tweets", tweets, append = T)

print(i)
i = i+1
Sys.sleep(450)

}




