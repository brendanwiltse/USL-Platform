library(cronR)
usl.script <- "/Users/brendanwiltse/Dropbox (ADK Watershed)/Shiny Apps/USL Profiler/USL Data Cleanup.R"
cmd <- cron_rscript(usl.script)
cron_add(command = cmd, frequency = '*/5 * * * *', id = "usl_clean", description = "Cleans USL data", tags = 'usl')

cmd1 <- cron_rscript(usl.tweet.script)
usl.tweet.script <- "/Users/brendanwiltse/Dropbox (ADK Watershed)/Shiny Apps/USL Profiler/USL Twitter.R"
cron_add(command = cmd1, at = "07:15", id = "usl_tweet_morning", description = "Tweets USL data", tags = 'usl')
cron_add(command = cmd1, at = "17:15", id = "usl_tweet_evening", description = "Tweets USL data", tags = 'usl')

