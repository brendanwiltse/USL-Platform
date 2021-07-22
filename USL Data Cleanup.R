#This script will be used to clean USL profiler data and save cleaned data as an output DAT file of the same format as the originals.

library(tidyverse)
library(viridis)
library(lubridate)
library(metR)
library(dplyr)
library(RColorBrewer)
library(rLakeAnalyzer)
library(weathermetrics)

#usl_met_avg, usl_rain, usl_wind, & usl_windmax along with all additional calculations should be combined into one file. 

#Read in original data
usl <- read.delim(
  "https://dl.dropboxusercontent.com/s/dngkzr9774rln01/USL_Winch_IP_PFL_Step.dat?dl=0", 
  header=T, sep=",", 
  skip =3)
colnames(usl) <- c(
  "date.time", 
  "count", 
  "profile", 
  "station", 
  "depth", 
  "temp", 
  "cond", 
  "vertical.position", 
  "depth.m", 
  "ODO.percent", 
  "ODO", 
  "pH", 
  "pH.mV", 
  "turbidity", 
  "bga.pc.rfu", 
  "bga.pc.ugl", 
  "chl.rfu", 
  "chl.ugl", 
  "battery", 
  "cable.power")

usl_surf <- read.delim(
  "https://dl.dropboxusercontent.com/s/fwblnrnkwkj20g1/USL_Winch_IP_SondeHourly.dat?dl=0", 
  header=T, sep=",", 
  skip =3)
colnames(usl_surf) <- c(
  "date.time", 
  "count", 
  "temp", 
  "cond", 
  "vertical.position", 
  "depth.m", 
  "ODO.percent", 
  "ODO", 
  "pH", 
  "pH.mV", 
  "turbidity", 
  "bga.pc.rfu", 
  "bga.pc.ugl", 
  "chl.rfu", 
  "chl.ugl", 
  "battery", 
  "cable.power")

usl_met_avg <- read.delim(
  "https://dl.dropboxusercontent.com/s/6orv3ytktafas86/USL_MET_IP_TRB_Avg.dat?dl=0", 
  header=T, sep=",", 
  skip =3)

colnames(usl_met_avg) <- c(
  "date.time", 
  "count", 
  "temp",
  "hum",
  "baro",
  "rad",
  "empty",
  "start.time")

#Correct solar radiation
usl_met_avg$rad <- usl_met_avg$rad/-11.71/79.659866*79.84*-85.2

#Correct barometric pressure
usl_met_avg$baro.corr = usl_met_avg$baro*((1-((0.0065*479)/(usl_met_avg$temp+0.0065*479+273.15)))^-5.257)

usl_wind <- read.delim(
  "https://dl.dropboxusercontent.com/s/1ywgi3tuahczwl1/USL_MET_IP_WindCorrected.dat?dl=0", 
  header=T, sep=",", 
  skip =3)

colnames(usl_wind) <- c(
  "date.time", 
  "count", 
  "wdir",
  "wspeed"
)

usl_wind$wdir.card = cut(usl_wind$wdir, breaks = c(0,11.25,33.75,56.25,78.75,101.25, 123.75,146.25,168.75,191.25,213.75,236.25,258.75,281.25,303.75,326.25, 348.75, 365), labels = c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W","WNW","NW","NNW","N"))

usl_windmax <- read.delim(
  "https://dl.dropboxusercontent.com/s/fhpmqgtwsz2v71f/USL_MET_IP_WindMax.dat?dl=0",
  header = T, sep=",", skip = 3
)

colnames(usl_windmax) <-c(
  "date.time",
  "count",
  "wmax",
  "time"
)

usl_rain <-read.delim(
  "https://dl.dropboxusercontent.com/s/qkz9liajktqnfh2/USL_MET_IP_RainData.dat?dl=0",
  head = T, sep=",", skip=3
)

colnames(usl_rain) <-c(
  "date.time",
  "count",
  "mV",
  "new",
  "tot"
)

usl_sonde_swap <- read_csv("https://dl.dropboxusercontent.com/s/fqe50u8t80qz2dj/usl_sonde_swap.csv?dl=0")

#Round all met data to 15 minutes
usl_met_avg$date.time <- round_date(as_datetime(usl_met_avg$date.time), "15 minutes")
usl_rain$date.time <- round_date(as_datetime(usl_rain$date.time), "15 minutes")
usl_wind$date.time <- round_date(as_datetime(usl_wind$date.time), "15 minutes")
usl_windmax$date.time <- round_date(as_datetime(usl_windmax$date.time), "15 minutes")

#Filter 2021 data
usl <- filter(usl, date.time>=as_datetime("2021-04-14"))
usl_met_avg <- filter(usl_met_avg, date.time>=as_datetime("2021-04-14"))
usl_rain <- filter(usl_rain, date.time>=as_datetime("2021-04-14"))
usl_wind <- filter(usl_wind, date.time>=as_datetime("2021-04-14"))
usl_windmax <- filter(usl_windmax, date.time>=as_datetime("2021-04-14"))

#Clean USL data for plotting
#Round depth to whole numbers
usl$depth = round(usl$depth, 0)

#Replace date.time with date.time for start of profile
usl.days = levels(as.factor(floor_date(as_datetime(usl$date.time), unit = "day")))

for(n in 1:length(usl.days)){
  days = which(usl$date.time >= as_datetime(as.Date(usl.days[n])) & usl$date.time < as_datetime(as.Date(usl.days[n])+1))
  for(i in min(usl$profile[days]):max(usl$profile[days])){
    profile = which(usl$profile %in% i)
    usl$date.time[profile] = usl$date.time[profile[1]]
  }
}

#Remove duplicate depths in the event the profiler logs extra measurements on the winch return
usl <- distinct(usl, date.time, depth, .keep_all = T)

#Calculate dew point, wind chill, and heat index
usl_met_avg$dew <- humidity.to.dewpoint(usl_met_avg$hum, usl_met_avg$temp, temperature.metric = "celsius")
usl_met_avg$windchill <- 13.12 + 0.6215 * usl_met_avg$temp - 11.37*((usl_wind$wspeed*3.6)^0.16) + 0.3965 * (((usl_wind$wspeed*3.6)^0.16))
usl_met_avg$heatindex <- heat.index(usl_met_avg$temp, rh = usl_met_avg$hum, temperature.metric = "celsius", output.metric = "celsius", round = 3)

#Save dataframes as csv files
write.csv(usl, "/Users/brendanwiltse/Dropbox (ADK Watershed)/ADK Watershed Team Folder/Env Research Lab/USL_Profiler_Data/Web_Files/Cleaned Data/usl.csv", row.names = FALSE)
write.csv(usl_met_avg, "/Users/brendanwiltse/Dropbox (ADK Watershed)/ADK Watershed Team Folder/Env Research Lab/USL_Profiler_Data/Web_Files/Cleaned Data/usl_met_avg.csv", row.names = FALSE)
write.csv(usl_rain, "/Users/brendanwiltse/Dropbox (ADK Watershed)/ADK Watershed Team Folder/Env Research Lab/USL_Profiler_Data/Web_Files/Cleaned Data/usl_rain.csv", row.names = FALSE)
write.csv(usl_surf, "/Users/brendanwiltse/Dropbox (ADK Watershed)/ADK Watershed Team Folder/Env Research Lab/USL_Profiler_Data/Web_Files/Cleaned Data/usl_surf.csv", row.names = FALSE)
write.csv(usl_wind, "/Users/brendanwiltse/Dropbox (ADK Watershed)/ADK Watershed Team Folder/Env Research Lab/USL_Profiler_Data/Web_Files/Cleaned Data/usl_wind.csv", row.names = FALSE)
write.csv(usl_windmax, "/Users/brendanwiltse/Dropbox (ADK Watershed)/ADK Watershed Team Folder/Env Research Lab/USL_Profiler_Data/Web_Files/Cleaned Data/usl_windmax.csv", row.names = FALSE)

#Update data files with sonde correction. This will apply and cumulative correction which should be assessed in later versions.

usl.clean <- read_csv("https://dl.dropboxusercontent.com/s/mylhy2np4o8amrx/usl.csv?dl=0") #update USL to cleaned data 

usl.before <- filter(usl, as_datetime(date.time)<=as_datetime(last(usl_sonde_swap$date.time)))
usl.profile.before <- filter(usl.before, date.time == max(usl.before$date.time))

usl.after <- filter(usl, as_datetime(date.time)>=as_datetime(last(usl_sonde_swap$date.time)))
usl.profile.after <- filter(usl.after, date.time == min(usl.after$date.time))

usl.profile.before <- semi_join(usl.profile.before, usl.profile.after, by = "depth")
usl.profile.after <- semi_join(usl.profile.after, usl.profile.before, by = "depth")

temp.data <- data.frame('Before' = usl.profile.before$temp, 'After' = usl.profile.after$temp)
temp.fit <- glm(Before~After, data = temp.data)
temp.test <- t.test(usl.profile.before$temp, usl.profile.after$temp)

cond.data <- data.frame('Before' = usl.profile.before$cond, 'After' = usl.profile.after$cond)
cond.fit <- glm(Before~After, data = cond.data)
cond.test <- t.test(usl.profile.before$cond, usl.profile.after$cond)
usl.clean$cond[as_datetime(usl.clean$date.time) >= as_datetime(last(usl_sonde_swap$date.time))] = predict(cond.fit, newdata = data.frame("After" = usl.after$cond))

ODO.percent.data <- data.frame('Before' = usl.profile.before$ODO.percent, 'After' = usl.profile.after$ODO.percent)
ODO.percent.fit <- glm(Before~After, data = ODO.percent.data)
ODO.percent.test <- t.test(usl.profile.before$ODO.percent, usl.profile.after$ODO.percent)
usl.clean$ODO.percent[as_datetime(usl.clean$date.time) >= as_datetime(last(usl_sonde_swap$date.time))] = predict(ODO.percent.fit, newdata = data.frame("After" = usl.after$ODO.percent))

ODO.data <- data.frame('Before' = usl.profile.before$ODO, 'After' = usl.profile.after$ODO)
ODO.fit <- glm(Before~After, data = ODO.data)
ODO.test <- t.test(usl.profile.before$ODO, usl.profile.after$ODO)
usl.clean$ODO[as_datetime(usl.clean$date.time) >= as_datetime(last(usl_sonde_swap$date.time))] = predict(ODO.fit, newdata = data.frame("After" = usl.after$ODO))

pH.data <- data.frame('Before' = usl.profile.before$pH, 'After' = usl.profile.after$pH)
pH.fit <- glm(Before~After, data = pH.data)
pH.test <- t.test(usl.profile.before$pH, usl.profile.after$pH)
usl.clean$pH[as_datetime(usl.clean$date.time) >= as_datetime(last(usl_sonde_swap$date.time))] = predict(pH.fit, newdata = data.frame("After" = usl.after$pH))

pH.mV.fit <- glm(usl.profile.before$pH.mV~usl.profile.after$pH.mV)
pH.mV.test <- t.test(usl.profile.before$pH.mV, usl.profile.after$pH.mV)

turbidity.data <- data.frame('Before' = usl.profile.before$turbidity, 'After' = usl.profile.after$turbidity)
turbidity.fit <- glm(Before~After, data = turbidity.data)
turbidity.test <- t.test(usl.profile.before$turbidity, usl.profile.after$turbidity)
usl.clean$turbidity[as_datetime(usl.clean$date.time) >= as_datetime(last(usl_sonde_swap$date.time))] = predict(turbidity.fit, newdata = data.frame("After" = usl.after$turbidity))

bga.pc.rfu.data <- data.frame('Before' = usl.profile.before$bga.pc.rfu, 'After' = usl.profile.after$bga.pc.rfu)
bga.pc.rfu.fit <- glm(Before~After, data = bga.pc.rfu.data)
bga.pc.rfu.test <- t.test(usl.profile.before$bga.pc.rfu, usl.profile.after$bga.pc.rfu)
usl.clean$bga.pc.rfu[as_datetime(usl.clean$date.time) >= as_datetime(last(usl_sonde_swap$date.time))] = predict(bga.pc.rfu.fit, newdata = data.frame("After" = usl.after$bga.pc.rfu))

bga.pc.ugl.fit <- glm(usl.profile.before$bga.pc.ugl~usl.profile.after$bga.pc.ugl)
bga.pc.ugl.test <- t.test(usl.profile.before$bga.pc.ugl, usl.profile.after$bga.pc.ugl)
chl.rfu.fit <- glm(usl.profile.before$chl.rfu~usl.profile.after$chl.rfu)
chl.rfu.test <- t.test(usl.profile.before$chl.rfu, usl.profile.after$chl.rfu)
chl.ugl.fit <- glm(usl.profile.before$chl.ugl~usl.profile.after$chl.ugl)
chl.ugl.test <- t.test(usl.profile.before$chl.ugl, usl.profile.after$chl.ugl)

write.csv(usl.clean, "/Users/brendanwiltse/Dropbox (ADK Watershed)/ADK Watershed Team Folder/Env Research Lab/USL_Profiler_Data/Web_Files/Cleaned Data/usl.csv", row.names = FALSE)
