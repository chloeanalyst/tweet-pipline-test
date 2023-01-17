# Love Island Winter 2023

require(rtweet)
require(tidyverse)
require(tidytext)
require(textdata)
require(lubridate)
require(RSQLite)
require(googlesheets4)
require(DBI)
require(RPostgres)

setwd("/Users/chloe/Documents/GitHub/tweet-pipline-test")


analyse()
run_and_analyse()


contestants <- c( 'Will'
                 ,'Tanyel'
                 ,'Tanya'
                 ,'Shaq'
                 ,'Ron'
                 ,'Olivia'
                 ,'Kai'
                 ,'Lana'
                 ,'Haris'
                 ,'Anna-May'
                 ,'Maya'
                 ,'Iain'
                 ,'Tom'
                 )


#-------------------------------
# Set up tables and workspace
#-------------------------------

# 1. Connect to Supabase

con <- dbConnect(RPostgres::Postgres(),dbname = 'postgres', 
                 host = 'db.oggqwqgajtiglcpuncqo.supabase.co', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                 port = 6543, # or any other port specified by your DBA
                 user = 'postgres',
                 password = 'XwW2Gd7Us63sbe9')  


 dbExecute(con, "CREATE TABLE LoveIsland_23Winter(
                                          created_at TEXT,
                                          text TEXT,
                                          favorite_count NUMERIC,
                                          retweet_count NUMERIC
                                          )")
 
 

 dbExecute(con, "CREATE TABLE Daily_23Winter (       created_at TEXT,
                                                      avg_weighted_sentiment NUMERIC,
                                                      episodised_total_sentiment NUMERIC,
                                                      total_tweets NUMERIC,
                                                      negative NUMERIC,
                                                      neutral NUMERIC,
                                                      positive NUMERIC
                                            )")
 
 
 
  dbExecute(con, "CREATE TABLE Contestant_23Winter (  created_at TEXT,
                                                      contestant TEXT,
                                                      avg_sentiment NUMERIC,
                                                      avg_weighted_sentiment NUMERIC,
                                                      total_tweets NUMERIC,
                                                      negative NUMERIC,
                                                      positive NUMERIC,
                                                      neutral NUMERIC
                                           )")
            
  
  
  
#-------------------------------
# Functions
#-------------------------------
  
# Run and Analyse  
  
  run_and_analyse <- function(){
    run()
    Sys.sleep(30)
    analyse()
  }
  
  
  
# Run - Collect & Store raw tweet data  
  
  run <- function() {
    
    con <- dbConnect(RPostgres::Postgres(),dbname = 'postgres', 
                     host = 'db.oggqwqgajtiglcpuncqo.supabase.co', 
                     port = 6543, 
                     user = 'postgres',
                     password = 'XwW2Gd7Us63sbe9') 
    
    
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
    
    dbWriteTable(con, "LoveIsland_23Winter", tweets, append = T)  
    
    dbDisconnect(con)
    
  }
  
  
  
  
  analyse <- function(){
    
    
    #conn <- dbConnect(RSQLite::SQLite(), "LoveIslandIsBack.db")
    
    con <- dbConnect(RPostgres::Postgres(),dbname = 'postgres', 
                     host = 'db.oggqwqgajtiglcpuncqo.supabase.co', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                     port = 6543, # or any other port specified by your DBA
                     user = 'postgres',
                     password = 'XwW2Gd7Us63sbe9')    
    
    
    #table <- dbReadTable(conn, "LoveIsland_23Winter")
    
    table <- dbReadTable(con, "LoveIsland_23Winter")
    
    table <- table[!duplicated(table), ] #Remove any duplicate rows
    
    output <-  table %>% unnest_tokens(word,text, drop=FALSE) %>% # Calculate Sentiment.
      inner_join(get_sentiments("afinn")) %>% # Using AFINN lexicon for sentiment score.
      group_by(text,created_at) %>%
      summarise(sentiment_score = sum(value),
                weighted_score = sum(value)*sum(favorite_count))  # Calculates score for each tweet.
    
    
    
    output$created_at <- as.Date(output$created_at) # Change date to date format.
    
    
    output$sentiment_value <- case_when( 
      output$sentiment_score > 0 ~ "positive",
      output$sentiment_score < 0 ~ "negative",
      output$sentiment_score == 0  ~ "neutral")
    
    
    
    daily_overall <- output %>% group_by(created_at) %>% # Sentiment score per day.
      summarise(
        avg_weighted_sentiment = mean(weighted_score),
        episodised_total_sentiment = sum(sentiment_score)^n_distinct(created_at),
        total_tweets = n()
      ) #Calculates avg and total score.
    
    sentiment_breakdown <- output %>% group_by(created_at, sentiment_value) %>% summarise(test = n()) %>% pivot_wider(names_from = sentiment_value, values_from = test)
    
    daily_overall <- daily_overall %>% left_join(sentiment_breakdown, by = "created_at")
  
    
    
    for (i in contestants) {
      output[[i]] <- ifelse(grepl(i, output$text),i,'') # Creates a column for each contestant. 
    }                                                 # Contestant name will appear in the row if they appear in the tweet.
    
    
    output$contestant <- apply(output[6:(length(contestants)+5)], 1, function(x) paste(x[!is.na(x) & x != ""], collapse = ", ")) # Stack columns in to 1 column, 
    # this puts tweets about multiple contestants
    # onto one row separated by a comma
    x <- as.numeric(length(contestants)+6) # Calculating a value to use to subset the output.
    # 1. Total number of contestants.
    # 2. Total number of columns from the left that we want to subset +1. 
    
    output <- output[c(1:5,x)] # Select required columns (created at, ).
    
    
    output_a <- output %>% mutate(contestant = strsplit(gsub("[][\"]", "", contestant), ", ")) %>% # Create a new row for any tweets that contain multiple contestants and group to find avg sentiment.
      unnest(contestant) 
    
    
    sentiment_contestant_breakdown <- output_a %>% group_by(created_at, contestant, sentiment_value) %>% summarise(test = n()) %>% pivot_wider(names_from = sentiment_value, values_from = test)
    
    
    output <- output_a %>% group_by(created_at,contestant) %>%
      summarise(avg_sentiment = mean(sentiment_score),
                avg_weighted_sentiment = mean(weighted_score),
                total_tweets = n_distinct(text)) 
    
    output$join_key <- paste(output$created_at, output$contestant)
    sentiment_contestant_breakdown$join_key <- paste(sentiment_contestant_breakdown$created_at, sentiment_contestant_breakdown$contestant)
    
    sentiment_contestant_breakdown <- sentiment_contestant_breakdown %>% ungroup() %>% select(join_key, positive, negative, neutral)  
    
    output <- output %>% left_join(sentiment_contestant_breakdown, by = "join_key") %>% select(created_at
                                                                                               , avg_sentiment
                                                                                               , avg_weighted_sentiment
                                                                                               , total_tweets
                                                                                               , negative
                                                                                               , positive
                                                                                               , neutral
                                                                                               , contestant)
    
    
    
    #Add any additional calculations here.
    
    
    
    get_word_cloud <- function(name) {
      
      result <- table %>% filter(grepl(name, table$text)) %>%
        unnest_tokens(word,text, drop=FALSE) %>%
        anti_join(get_stopwords()) %>%
        select(word) %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE)
      
      result$contestant <- name
      
      return(result)
      
    }
    
    
    get_word_clouds <- function() {
      
      word_cloud <- list()
      
      
      for( i in contestants ) {
        
        word_cloud[[i]] <-  get_word_cloud(i)
        
      }
      
      cloud_results <- as.data.frame(do.call(rbind,word_cloud))
      
      return(cloud_results)
      
    }
    
    contestant_word_clouds <- get_word_clouds()
    
    output$created_at <- as.character(output$created_at)
    daily_overall$created_at <- as.character(daily_overall$created_at)

    dbWriteTable(con, "Daily_23Winter", daily_overall, overwrite = T)
    dbWriteTable(con, "Contestant_23Winter", output, overwrite = T)
    
    dbDisconnect(con)
    
    
    write_sheet(output, ss = "https://docs.google.com/spreadsheets/d/1aDm3FxqcAbRNZQL6tC1oBYnROUQAsTcr9hBFuo4A5jk/edit#gid=0"
                , sheet = "Contestant")
    
    
    write_sheet(daily_overall, ss = "https://docs.google.com/spreadsheets/d/1aDm3FxqcAbRNZQL6tC1oBYnROUQAsTcr9hBFuo4A5jk/edit#gid=1796703791"
                , sheet = "Daily")
    
    
    write_sheet(contestant_word_clouds, ss = "https://docs.google.com/spreadsheets/d/1aDm3FxqcAbRNZQL6tC1oBYnROUQAsTcr9hBFuo4A5jk/edit#gid=310560987"
                , sheet = "Wordcloud")
    
    
    print('Daily Overall')
    print(daily_overall)
    print('Output')
    print(output)
  }
  