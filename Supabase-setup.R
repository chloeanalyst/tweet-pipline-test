library(DBI)
library(RPostgres)

con <- dbConnect(RPostgres::Postgres(),dbname = 'postgres', 
                 host = 'db.oggqwqgajtiglcpuncqo.supabase.co', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                 port = 6543, # or any other port specified by your DBA
                 user = 'postgres',
                 password = 'XwW2Gd7Us63sbe9')


dbExecute(con, "CREATE TABLE LoveIslandTweets(
                                         created_at TEXT,
                                         text TEXT,
                                         favorite_count NUMERIC,
                                         retweet_count NUMERIC
                                         )")


dbExecute(con, "CREATE TABLE Daily(       created_at TEXT,
                                           avg_weighted_sentiment NUMERIC,
                                           episodised_total_sentiment NUMERIC,
                                           total_tweets NUMERIC,
                                           negative NUMERIC,
                                           neutral NUMERIC,
                                           positive NUMERIC
                                           )")

dbExecute(con, "CREATE TABLE Contestant(  created_at TEXT,
                                           contestant TEXT,
                                           avg_sentiment NUMERIC,
                                           avg_weighted_sentiment NUMERIC,
                                           total_tweets NUMERIC,
                                           negative NUMERIC,
                                           positive NUMERIC,
                                           neutral NUMERIC
                                           )")


localconn <- dbConnect(RSQLite::SQLite(), "LoveIslandIsBack.db")


table <- dbReadTable(localconn, "LoveIslandTweets")



dbWriteTable(con, "LoveIslandTweets", table, overwrite = T)





