library(rtweet)
library(tidyverse)
library(tidytext)
library(textdata)
library(lubridate)
library(RSQLite)
library(googlesheets4)


analyse()
run_and_analyse()


# i <- 1
# 
# while( i == 1  ) {
#   
#   if( hour(Sys.time()) %in% 22:00 ){
#                                       run_and_analyse()
#                                       break
#                                     } else Sys.sleep(abs((hour(Sys.time()) - 22)*60*60))
#   
#                                     } 


#-------------------FUNCTIONS--------------------------

run_and_analyse <- function(){
run()
sys.sleep(300)
analyse()
}
run <- function() {
  
  conn <- dbConnect(RSQLite::SQLite(), "LoveIslandIsBack.db")
  
  # dbExecute(conn, "CREATE TABLE LoveIslandTweets(
  #                                          created_at TEXT,
  #                                          text TEXT,
  #                                          favorite_count NUMERIC,
  #                                          retweet_count NUMERIC
  #                                          )")
  
  
  
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
analyse <- function(){


conn <- dbConnect(RSQLite::SQLite(), "LoveIslandIsBack.db")


table <- dbReadTable(conn, "LoveIslandTweets")

table <- table[!duplicated(table), ] #Remove any duplicate rows

# List of contestants
contestants <- c('Ekin-Su'
                 ,'Afia'
                 ,'Luca'
                 ,'Amber'
                 ,'Andrew'
                 ,'Ikenna'
                 ,'Gemma'
                 ,'Davide'
                 ,'Tasha'
                 ,'Liam'
                 ,'Indiyah'
                 ,'Dami'
                 ,'Paige'
              
                 )



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

write.csv(daily_overall,"./data/daily.csv") # Save to CSV.



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



write.csv(output,paste0("./data/result-",today(),".csv"))


table %>% filter(grepl("Tasha", table$text))


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
 
dbWriteTable(conn, "Daily", daily_overall, overwrite = T)
dbWriteTable(conn, "Contestant", output, overwrite = T)

dbDisconnect(conn)


write_sheet(output, ss = "https://docs.google.com/spreadsheets/d/1GeTY81rBFv3fm95WMClPm_zDjAa88ykNyPkiX8NkTyU/edit#gid=0"
                  , sheet = "Contestant")


write_sheet(daily_overall, ss = "https://docs.google.com/spreadsheets/d/1GeTY81rBFv3fm95WMClPm_zDjAa88ykNyPkiX8NkTyU/edit#gid=0"
            , sheet = "Daily")


write_sheet(contestant_word_clouds, ss = "https://docs.google.com/spreadsheets/d/1GeTY81rBFv3fm95WMClPm_zDjAa88ykNyPkiX8NkTyU/edit#gid=0"
            , sheet = "Wordcloud")


print('Daily Overall')
print(daily_overall)
print('Output')
print(output)
}




# --------- Setting up additional tables.
# 
# conn <- dbConnect(RSQLite::SQLite(), "LoveIslandIsBack.db")
# 
# dbExecute(conn, "CREATE TABLE Daily(       created_at TEXT,
#                                            avg_weighted_sentiment NUMERIC,
#                                            episodised_total_sentiment NUMERIC,
#                                            total_tweets NUMERIC,
#                                            negative NUMERIC,
#                                            neutral NUMERIC,
#                                            positive NUMERIC
#                                            )")
# 
# dbExecute(conn, "CREATE TABLE Contestant(  created_at TEXT,
#                                            contestant TEXT,
#                                            avg_sentiment NUMERIC,
#                                            avg_weighted_sentiment NUMERIC,
#                                            total_tweets NUMERIC,
#                                            negative NUMERIC,
#                                            positive NUMERIC,
#                                            neutral NUMERIC
#                                            )")
