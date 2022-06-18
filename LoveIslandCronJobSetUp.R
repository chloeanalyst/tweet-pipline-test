library("cronR")

cron_ls()

path = "/Users/chloe/Documents/GitHub/tweet-pipline-test"

cmd = cron_rscript(path)

cron_add(command = cmd,
         frequency = 'daily',
         at = "22:00", 
         days_of_week = c(1:7),
         id = 'LoveIslandPull',
         description = 'Run and analyse Love Island Tweets'
         )



#cron_rm( id = "binDay")
