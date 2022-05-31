#library(cronR)
#usl.script <- "C:/Users/admin/Dropbox (ADK Watershed)/Shiny Apps/USL Profiler/USL Data Cleanup.R"
#cmd <- cron_rscript(usl.script)
#cron_add(command = cmd, frequency = '*/5 * * * *', id = "usl_clean", description = "Cleans USL data", tags = 'usl')

#cmd1 <- cron_rscript(usl.tweet.script)
#usl.tweet.script <- "/Users/brendanwiltse/Dropbox (ADK Watershed)/Shiny Apps/USL Profiler/USL Twitter.R"
#cron_add(command = cmd1, at = "07:15", id = "usl_tweet_morning", description = "Tweets USL data", tags = 'usl')
#cron_add(command = cmd1, at = "17:15", id = "usl_tweet_evening", description = "Tweets USL data", tags = 'usl')

library(taskscheduleR)
usl.script <- "C:/Users/admin/Dropbox (ADK Watershed)/Shiny Apps/USL Profiler/USL Data Cleanup.R"
taskscheduler_create(taskname = "USLPlatformDataClean", 
                     rscript = usl.script,
                     schedule = "MINUTE",
                     startdate = "04/20/2022",
                     starttime = "12:00",
                     modifier = 5)

usl.tweet.script <- "C:/Users/admin/Dropbox (ADK Watershed)/Shiny Apps/USL Profiler/USL Twitter.R"
taskscheduler_create(taskname = "TweetMorning", 
                     rscript = usl.tweet.script,
                     schedule = "DAILY",
                     startdate = "04/20/2022",
                     starttime = "07:15")
taskscheduler_create(taskname = "TweetEvening", 
                     rscript = usl.tweet.script,
                     schedule = "DAILY",
                     startdate = "04/20/2022",
                     starttime = "17:15")
