library(rtweet)
library(magick)
library(tidyverse)
library(lubridate)
library(rLakeAnalyzer)

#Load USL data
usl <- read_csv("https://dl.dropboxusercontent.com/s/mylhy2np4o8amrx/usl.csv?dl=0")

usl_surf <- read_csv("https://dl.dropboxusercontent.com/s/d1ylz95glszlglz/usl_surf.csv?dl=0")

usl_met_avg <- read_csv("https://dl.dropboxusercontent.com/s/khy8no6qcd5zs7g/usl_met_avg.csv?dl=0")

usl_wind <- read_csv("https://dl.dropboxusercontent.com/s/5gli9ype4err7td/usl_wind.csv?dl=0")

usl_windmax <- read_csv("https://dl.dropboxusercontent.com/s/prlph0kc8lxwncx/usl_windmax.csv?dl=0")

usl_rain <-read_csv("https://dl.dropboxusercontent.com/s/kdcsrzas7qgizkg/usl_rain.csv?dl=0")

#Load data frame of column names
usl_names <- read_csv("https://dl.dropboxusercontent.com/s/yionxh0g0hag9jj/parameter%20names.csv?dl=0")

#Subset data for most recent profile
usl_sub.profiles <- filter(usl, date.time == max(usl$date.time))

#Calculate metadepths for plotting
t.d <- thermo.depth(usl_sub.profiles$temp, usl_sub.profiles$depth, seasonal = T)

line1 <- paste("Conditions at ", format(as_datetime(last(usl_met_avg$date.time)), format = "%A, %B %d, %I:%M%p"), sep ="")
line2 <- paste("Air Temp: ", format(round((last(usl_met_avg$temp)*9/5)+32, 1), nsmall =1), "\u00B0F", sep ="")
line3 <- paste("Water Temp: ", format(round((last(usl_surf$temp)*9/5)+32, 1), nsmall =1), "\u00B0F", sep ="")
line4 <- paste("Wind: ", format(round(last(usl_wind$wspeed)*2.237, 1), nsmall =1), "mph out of the ", last(usl_wind$wdir.card), " gusting to ", format(round(last(usl_windmax$wmax)*2.237, 1), nsmall =1), "mph", sep ="")
line5 <- paste("Rain: ", round(sum(filter(usl_rain, (as_datetime(usl_rain$date.time) + 14400) >= as.POSIXlt(Sys.time(), tz = "UTC") - 86400)$tot)/25.4, digits = 2), "in over last 24 hrs", sep ="")
line6 <- paste("Thermocline Depth: ", round(t.d*3.281, digits = 1), "ft", sep ="")
line7 <- paste("@ADK_Watershed @paulsmiths #Adirondacks")
line8 <- paste("Live data -> adkwatershed.shinyapps.io/UpperSaranacLake")

status = paste(line1, line2, line3, line4, line5, line6, line7, line8, sep="\n")
nchar(status)

post_tweet(status,
           media = "/Users/brendanwiltse/Dropbox (ADK Watershed)/ADK Watershed Team Folder/Env Research Lab/USL_Profiler_Data/Web_Files/Pictures/USL Profiler Camera.jpg")
