#IST 707 Data Munging

library(openxlsx)
library(lubridate)

myfile <- "/Users/fernandogranato/Documents/Mestrado/IST 707 - Data Analitics/Project/Data/Iquitos_Training_Data.csv"
iq.df <- read.csv(myfile, header = TRUE)

#Cleaning Variables we are not using
iq.df <- iq.df[,c(1:3,9)]
iq.df$week_start_date <- as.Date(iq.df$week_start_date)
dim(iq.df)

#Population
myfile <- "/Users/fernandogranato/Documents/Mestrado/IST 707 - Data Analitics/Project/Data/Iquitos_Population_Data.csv"
population <- read.csv(myfile, header = TRUE)

i <- 0
for (i in 1:dim(iq.df)[1]) {
  iq.df$population[i] <- population$Estimated_population[population$Year == year(iq.df$week_start_date)[i]]
}

#Max and Min air temperature
myfile <- "/Users/fernandogranato/Documents/Mestrado/IST 707 - Data Analitics/Project/Data/Iquitos.xlsx"
temp.df <- read.xlsx(myfile, sheet = 2)
temp.df <- temp.df[,c(1:8)]
temp.df$Date <- as.Date(paste(temp.df$Year,"-",temp.df$Month,"-",temp.df$Day, sep = ""))
temp.df <- temp.df[(temp.df$Date >= "2000-07-01") & (temp.df$Date <= "2009-07-01"),]

dweek <- 0
for (i in unique(year(iq.df$week_start_date))) {
  dvec <- temp.df$Date[which(temp.df$Year == i)]
  dweek <- c(dweek, max(dweek) + 1 + (as.numeric(dvec-dvec[1]) %/% 7))
  dweek[dweek == max(dweek)] <- ifelse(length(dweek[dweek == max(dweek)]) < 7 , max(dweek) -1, max(dweek))
}
dweek <- dweek[-1]

temp.df$Week <- dweek

iq.df$mintemp <- tapply(temp.df$minimum_air_temperature,temp.df$Week, FUN = min)
iq.df$maxtemp <- tapply(temp.df$maximum_air_temperature,temp.df$Week, FUN = max)
iq.df$avgtemp <- tapply(temp.df$TAVG, temp.df$Week, FUN = mean)

#Precipitation

iq.df$precipitation <- tapply(temp.df$precipitation_amount, temp.df$Week, FUN = sum)

#Humidity
myfile <- "/Users/fernandogranato/Documents/Mestrado/IST 707 - Data Analitics/Project/Data/Iquitos.xlsx"
temp.df <- read.xlsx(myfile, sheet = 1)
temp.df$Date <- as.Date(paste(temp.df$Year,"-",temp.df$Month,"-",temp.df$Day, sep = ""))
temp.df <- temp.df[(temp.df$Date >= "2000-07-01") & (temp.df$Date <= "2009-07-01"),]

dweek <- 0
for (i in unique(year(iq.df$week_start_date))) {
  dvec <- temp.df$Date[which(temp.df$Year == i)]
  dweek <- c(dweek, max(dweek) + 1 + (as.numeric(dvec-dvec[1]) %/% 7))
  dweek[dweek == max(dweek)] <- ifelse(length(dweek[dweek == max(dweek)]) < 7 , max(dweek) -1, max(dweek))
}
dweek <- dweek[-1]

temp.df$Week <- dweek

iq.df$air_temperature <- tapply(temp.df$air_temperature,temp.df$Week, FUN = mean)
iq.df$dew_point_temperature <- tapply(temp.df$dew_point_temperature,temp.df$Week, FUN = mean)
iq.df$relative_humidity <- tapply(temp.df$relative_humidity,temp.df$Week, FUN = mean)
iq.df$specific_humidity <- tapply(temp.df$specific_humidity,temp.df$Week, FUN = mean)

#Exporting CSV

write.csv(iq.df, file = "/Users/fernandogranato/Documents/Mestrado/IST 707 - Data Analitics/Project/Data/Iquistos_Clean.csv")

#2000 - 2009
#Precipitation - mm - Iquistos_precip.csv
# min air temperature - OK
# max air temperature (K) - OK
#dew point temperature
#Estimated Population - OK

#San Juan

myfile <- "/Users/fernandogranato/Documents/Mestrado/IST 707 - Data Analitics/Project/Data/San_Juan_Training_Data.csv"
sj.df <- read.csv(myfile, header = TRUE)

#Cleaning Variables we are not using
sj.df <- sj.df[521:988,c(1:3,10)]
sj.df$week_start_date <- as.Date(sj.df$week_start_date)

dim(sj.df)

#Population
myfile <- "/Users/fernandogranato/Documents/Mestrado/IST 707 - Data Analitics/Project/Data/San_Juan_Population_Data.csv"
population <- read.csv(myfile, header = TRUE)

i <- 0
for (i in 1:dim(sj.df)[1]) {
  sj.df$population[i] <- population$Estimated_population[population$Year == year(sj.df$week_start_date)[i]]
}

#Max and Min air temperature
myfile <- "/Users/fernandogranato/Documents/Mestrado/IST 707 - Data Analitics/Project/Data/SanJuan.xlsx"
temp.df <- read.xlsx(myfile, sheet = 2)
temp.df <- temp.df[,c(1:8)]
temp.df$Date <- as.Date(paste(temp.df$Year,"-",temp.df$Month,"-",temp.df$Day, sep = ""))
temp.df <- temp.df[(temp.df$Date >= "2000-07-01") & (temp.df$Date <= "2009-07-01"),]

dweek <- 0
for (i in unique(year(sj.df$week_start_date))) {
  dvec <- temp.df$Date[which(temp.df$Year == i)]
  dweek <- c(dweek, max(dweek) + 1 + (as.numeric(dvec-dvec[1]) %/% 7))
  dweek[dweek == max(dweek)] <- ifelse(length(dweek[dweek == max(dweek)]) < 7 , max(dweek) -1, max(dweek))
}
dweek <- dweek[-1]

temp.df$Week <- dweek

sj.df$mintemp <- tapply(temp.df$minimum_air_temperature,temp.df$Week, FUN = min)
sj.df$maxtemp <- tapply(temp.df$maximum_air_temperature,temp.df$Week, FUN = max)
sj.df$avgtemp <- tapply(temp.df$TAVG,temp.df$Week, FUN = mean)

#Precipitation

sj.df$precipitation <- tapply(temp.df$precipitation_amount, temp.df$Week, FUN = sum)

#Humidity
myfile <- "/Users/fernandogranato/Documents/Mestrado/IST 707 - Data Analitics/Project/Data/SanJuan.xlsx"
temp.df <- read.xlsx(myfile, sheet = 1)
temp.df$Date <- as.Date(paste(temp.df$Year,"-",temp.df$Month,"-",temp.df$Day, sep = ""))
temp.df <- temp.df[(temp.df$Date >= "2000-07-01") & (temp.df$Date <= "2009-07-01"),]

dweek <- 0
for (i in unique(year(iq.df$week_start_date))) {
  dvec <- temp.df$Date[which(temp.df$Year == i)]
  dweek <- c(dweek, max(dweek) + 1 + (as.numeric(dvec-dvec[1]) %/% 7))
  dweek[dweek == max(dweek)] <- ifelse(length(dweek[dweek == max(dweek)]) < 7 , max(dweek) -1, max(dweek))
}
dweek <- dweek[-1]

temp.df$Week <- dweek

sj.df$air_temperature <- tapply(temp.df$air_temperature,temp.df$Week, FUN = mean)
sj.df$dew_point_temperature <- tapply(temp.df$dew_point_temperature,temp.df$Week, FUN = mean)
sj.df$relative_humidity <- tapply(temp.df$relative_humidity,temp.df$Week, FUN = mean)
sj.df$specific_humidity <- tapply(temp.df$specific_humidity,temp.df$Week, FUN = mean)


#Exporting CSV

write.csv(sj.df, file = "/Users/fernandogranato/Documents/Mestrado/IST 707 - Data Analitics/Project/Data/SanJuan_Clean.csv")
