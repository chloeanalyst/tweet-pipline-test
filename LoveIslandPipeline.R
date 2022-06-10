library(rtweet)
library(tidyverse)
library(tidytext)
library(textdata)
library(lubridate)
library(RSQLite)



run <- function() {

conn <- dbConnect(RSQLite::SQLite(), "LoveIslandIsBack.db")

dbExecute(conn, "CREATE TABLE LoveIslandTweets(
                                           created_at TEXT,
                                           text TEXT,
                                           favorite_count NUMERIC,
                                           retweet_count NUMERIC
                                           )")



pull <- search_tweets(  # Collect Tweets.
  "#LoveIsland",  # Hashtag you would like to analyse
  n = 100000, # Increase to collect the number of tweets you are interested in.
  lang = "en", # Language of tweets
  include_rts = FALSE, # Exclude duplicated retweeted Tweets
  retryonratelimit = TRUE # If TRUE script will sleep until rate limit has reset.
                        ) #Calculates the word count of each tweet (not used in output but is useful for checking quality).


tweets <- pull %>% select(created_at, text, favorite_count, retweet_count)
tweets$created_at <- as.character(as.Date(tweets$created_at))
tweets <- as.data.frame(tweets)


dbWriteTable(conn, "LoveIslandTweets", tweets, append = T)

dbDisconnect(conn)

}
