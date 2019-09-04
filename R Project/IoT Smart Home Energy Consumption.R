#### Load libraries & Setup ####
#install.packages("installr")
#suppressPackageStartupMessages(library(installr))
#library(installr)
#updateR()
install.packages("pacman")
library(pacman)

p_load(contrib.url, doParallel, here, readr, rstudioapi,        #parallel computing, relative path
       #caret, C50, caretEnsemble, mboost, mlr, Metrics, randomForest, party, MASS, 
       #ParamHelpers, hydroGOF #Classification and Regression
       #cluster, corrplot, fpc, e1071, recipes, Hmisc, #Clustering: Corr visulization, Clustering & SVM, 
       ggplot2, ggpubr, RColorBrewer, lattice, dygraphs, plotly, #Visualization
       ade4, inum, reshape,  #Cleaning, Preprocessing
       #FactoMineR, factoextra, #PCA, MCA
       plyr, dplyr, tidyr, tidyverse, textclean, 
       #arules, arulesViz,    # ASsociation Rules Mining: analyzing and visualize transactional data
       #markdown, shiny, tinytex, rmdformats, knitr #html docu, dashboard, Latex for PDF docu
       RMySQL, lubridate, # Time Series: MySQL, functions for DateTime
       #BBmisc, asbio
       imputeTS, padr, #interpolate missing values (Time Series)
       forecast, tseries, prophet #aTSA #Time Series
)

## Enable parallel computing
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

## Disable scientific notation
options(scipen = 999)

## Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

## File directory
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
getwd()
setwd("..")
setwd("Data")

#### 0. Loading data ####
##Test w/ Iris
# Use asterisk to specify all attributes for download
irisALL <- dbGetQuery(con, "SELECT * FROM iris")
# Use attribute names to specify specific attributes for download
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")
## Load time series
dbListTables(con)
dbListFields(con, "yr_2006")

#Id <- c("yr_2006", "yr_2007", "yr_2008", "yr_2009", "yr_2010")
#sqlcmd <- paste("SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM", Id, sep = " ")
#sqlcmd <- paste("SELECT * FROM", Id, sep = " ")

#list_all_raw <- lapply(sqlcmd, function(x) dbGetQuery(con, x))
#df_raw_all <- do.call(rbind, list_all_raw)   

#saveRDS(list_all_raw, file = "../Data/Smart_Home_rawList.rds")
#saveRDS(df_raw_all, file = "../Data/Smart_Home_rawDF.rds")

list_all_raw <- readRDS(file = "../Data/Smart_Home_rawList.rds")
df_raw_all <- readRDS(file = "../Data/Smart_Home_rawDF.rds")
df_y0710_raw <- bind_rows(list_all_raw[[2]], list_all_raw[[3]], list_all_raw[[4]], list_all_raw[[5]])

# Load and prepare energy price
ePrice_raw <- read.csv(file = "../Data/20190829_EnergyPrice_France0718.csv")


#### Investigate dfs ####
str(list_all_raw[[1]])
summary(list_all_raw[[1]]) 
head(list_all_raw[[5]]) #2006 12.16 - 12.31, complete: 2007, 2008, 2009, 2010:-11.26
tail(list_all_raw[[5]])

#### Pre-processing ####
## Creating DateTime attribute
# Combine Date and Time attribute values in a new attribute column
df_y0710_raw <- cbind(df_y0710_raw, paste(df_y0710_raw$Date, df_y0710_raw$Time), stringsAsFactors = FALSE)
colnames(df_y0710_raw)[ncol(df_y0710_raw)] <- "DateTime"

# Move the DateTime attribute within the dataset
df_y0710_raw <- df_y0710_raw[, c(ncol(df_y0710_raw), 1:(ncol(df_y0710_raw)-1))]

# Convert DateTime from POSIXlt to POSIXct 
Sys.timezone()
Sys.time()
#Sys.setenv(TZ = "Europe/Berlin")
df_y0710_raw$DateTime <- as.POSIXct(df_y0710_raw$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")

# Add the time zone (Remains UTC)
attr(df_y0710_raw$DateTime, "tzone") <- "GMT" 
tz(df_y0710_raw$DateTime)

# Descriptive statistics
summary(df_y0710_raw[,c(5, 9:11)])

# Inspect the data types
str(df_y0710_raw)

## Add missing values
df_y0710_flld <- pad(df_y0710_raw, break_above = 3)
df_y0710_NA0 <- anti_join(df_y0710_flld, df_y0710_raw, by = NULL)
df_y0710_flld <- na_interpolation(df_y0710_flld, option = "linear")
df_y0710_NA <- anti_join(df_y0710_flld, df_y0710_raw, by = NULL)

df_y0710_flld$Date <- date(df_y0710_flld$DateTime)

sum(is.na(df_y0710_flld)) # 25975 in time column

# Visualize of NAs by frequency and amount per weeks
df_y0710_NA0$Date <- date(df_y0710_NA0$DateTime)
df_y0710_NA0$year <- year(df_y0710_NA0$DateTime)
df_y0710_NA0$week <- week(df_y0710_NA0$DateTime)

NAs <- df_y0710_NA0 %>% group_by(year, week, id) %>% dplyr::summarise(count = n()) #NAs per Day

ggplot(NAs, aes(count)) +
  stat_bin(binwidth = 30, fill = "lightblue", color = "Black") +
  stat_bin(binwidth = 30, geom = "text", aes(label = ifelse(..count.. == 0, "", ..count..)), cex = 5,vjust = -0.5) +
  xlab("Amount of NAs") + ylab("Frequency") + ggtitle("NAs by year") + 
  facet_grid(~year)

ggplot(NAs, aes(x = week, y = count)) + geom_col() + 
  xlab("Week") + ylab("Total amount of NAs (mins)") + ggtitle("NAs by week and year") + 
  facet_grid(~year)


## Create variables for different time periods
# Create "year" attribute with lubridate
df_y0710_flld$year <- year(df_y0710_flld$DateTime)
df_y0710_flld$quart <- quarter(df_y0710_flld$DateTime)
df_y0710_flld$month <- month(df_y0710_flld$DateTime)
df_y0710_flld$week <- week(df_y0710_flld$DateTime)
df_y0710_flld$weekday <- weekdays(df_y0710_flld$DateTime)
df_y0710_flld$wday <- wday(df_y0710_flld$DateTime, week_start = 1)
df_y0710_flld$day <- day(df_y0710_flld$DateTime)
df_y0710_flld$hour <- hour(df_y0710_flld$DateTime)
df_y0710_flld$min <- minute(df_y0710_flld$DateTime)


## Harmonize units
# Calculate Global_power in kWh
df_y0710_flld$Global_power <- df_y0710_flld$Global_intensity*df_y0710_flld$Voltage/1000/60
df_y0710_flld$Global_active_power <- df_y0710_flld$Global_active_power/60
df_y0710_flld$Global_reactive_power <- df_y0710_flld$Global_reactive_power/60
df_y0710_flld$Sub_metering_1 <- df_y0710_flld$Sub_metering_1/1000
df_y0710_flld$Sub_metering_2 <- df_y0710_flld$Sub_metering_2/1000
df_y0710_flld$Sub_metering_3 <- df_y0710_flld$Sub_metering_3/1000

# Calculating Sub_rest energy [kWh] (consumption of electrical equipment not measured by sub-meters)
df_y0710_flld$Sub_rest <- (df_y0710_flld$Global_active_power-df_y0710_flld$Sub_metering_1-df_y0710_flld$Sub_metering_2-df_y0710_flld$Sub_metering_3)

# Include energy price
df_price <- left_join(df_y0710_flld, ePrice_raw, by = c("year", "hour"))
df_y0710_flld <- df_price

# Store dfs for further processing
df_y0709 <- df_y0710_flld[-c(which(year(df_y0710_flld$Date) == 2010)), ]
df_y10 <- df_y0710_flld[c(which(year(df_y0710_flld$Date) == 2010 & month(df_y0710_flld$Date) != 11)), ]
df_y0710 <- df_y0710_flld[-c(which(year(df_y0710_flld$Date) == 2010 & month(df_y0710_flld$Date) == 11)), ]


#### Initial visualization
#### Monthly ####
## Change granularity - Summarise
df_monthly <- df_y0710 %>% group_by(floor_date(df_y0710$DateTime, unit = "month", week_start = 1)) %>% 
  summarise(GAP = sum(Global_active_power), GRP = sum(Global_reactive_power), GP = sum(Global_power),
            kitchen = sum(Sub_metering_1), laundry = sum(Sub_metering_2), heatAC = sum(Sub_metering_3), others = sum(Sub_rest), price = mean(price))
colnames(df_monthly)[1] <- "Date"

## Visualization
ggplot(df_monthly, aes(x = floor)) +  
  geom_line(aes(y = GAP, colour = "a"), group = 1) +
  geom_line(aes(y = kitchen, colour = "b"), group = 1) + 
  geom_line(aes(y = laundry, colour = "c"), group = 1) + 
  geom_line(aes(y = heatAC, colour = "d"), group = 1) +
  geom_line(aes(y = others, colour = "e"), group = 1) +
  xlab("Year") + ylab("Electricity consumption (in kWh)") + 
  ggtitle("Energy consumption for 2007 - 2010") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(axis.text = element_text(size = 12)) + 
  theme(axis.title = element_text(size = 15)) + 
  theme(title = element_text(size = 18))  + 
  scale_color_manual(name = "Colour", 
                     values = c("a" = "black", "b" = "red", "c" = "blue", "d" = "green", "e" = "orange"),
                     labels = c("GAP", "kitchen", "laundry", "heatAC", "others"))

#### Weekly ####
## Change granularity - Summarise
df_weekly <- df_y0710 %>% group_by(floor_date(df_y0710$DateTime, unit = "week", week_start = 1)) %>% 
  summarise(GAP = sum(Global_active_power), GRP = sum(Global_reactive_power), GP = sum(Global_power), 
            kitchen = sum(Sub_metering_1), laundry = sum(Sub_metering_2), heatAC = sum(Sub_metering_3), others = sum(Sub_rest), price = mean(price))
colnames(df_weekly)[1] <- "Date"
df_weekly$DateTime <- as.POSIXct(df_weekly$Date, format = "%Y-%m-%d", tz = "GMT")
attr(df_weekly$Date, "tzone") <- "GMT" 


## Subsetting
# Subset the second week of 2008 - All Observations
houseWeek <- filter(df_weekly, year == 2009 & week == 52)
# Subset the second week of 2008 - Frequ = 20
houseWeek20 <- filter(df_weekly, year == 2009 & week == 52 & 
                        (min == 0 | min == 20 | min == 40))

## Visualization
ggplot(df_weekly_ts[c(1:nrow(df_weekly_ts)),], aes(x = Weekly)) +  
  geom_line(aes(y = GAP, colour = "a"), group = 1) +
  geom_line(aes(y = kitchen, colour = "b"), group = 1) + 
  geom_line(aes(y = laundry, colour = "c"), group = 1) + 
  geom_line(aes(y = heatAC, colour = "d"), group = 1) +
  xlab("Year") + ylab("Electricity consumption (in kWh)") + 
  ggtitle("Energy consumption for 2007 - 2010") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(axis.text = element_text(size = 12)) + 
  theme(axis.title = element_text(size = 15)) + 
  theme(title = element_text(size = 18))  + 
  scale_color_manual(name = "Colour", 
                     values = c("a" = "black", "b" = "red", "c" = "blue", "d" = "green"),
                     labels = c("GAP", "kitchen", "laundry", "heatAC"))

# Week - Plot sub-meter 1, 2 and 3
plot_ly(houseWeek, x = ~houseWeek$DateTime, y = ~houseWeek$Sub_metering_1, 
        name = 'kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~houseWeek$Sub_rest, name = 'Rest', mode = 'lines') %>%
  layout(title = "Power Consumption Xmes, 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (kWh)"))

# Week Frequ = 20 - Plot sub-meter 1, 2 and 3
plot_ly(houseWeek20, x = ~houseWeek20$DateTime, y = ~houseWeek20$Sub_metering_1, 
        name = 'kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek20$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek20$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~houseWeek20$Sub_rest, name = 'Rest', mode = 'lines') %>%
  layout(title = "Power Consumption Xmes, 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (kWh)"))


#### Daily ####
## Change granularity - Summarise
df_daily <- df_y0710 %>% group_by(Date) %>% 
  summarise(GAP = sum(Global_active_power), GRP = sum(Global_reactive_power), GP = sum(Global_power),
            kitchen = sum(Sub_metering_1), laundry = sum(Sub_metering_2), heatAC = sum(Sub_metering_3), others = sum(Sub_rest), price = mean(price))


## Subsetting
# Subset the 9th day of January 2008 - All observations
houseDay <- filter(df_daily1, year == 2008 & month == 1 & day == 12)

# Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(df_daily, year == 2008 & month == 1 & day == 9 & 
                       (min == 0 | min == 10 | min == 20 | min == 30 | min == 40 | min == 50))

## Visualization
ggplot(df_daily_ts[c(1:nrow(df_daily_ts)), ], aes(x = floor_daily)) +  
  geom_line(aes(y = GAP, colour = "a"), group = 1) +
  geom_line(aes(y = kitchen, colour = "b"), group = 1) + 
  geom_line(aes(y = laundry, colour = "c"), group = 1) + 
  geom_line(aes(y = heatAC, colour = "d"), group = 1) +
  xlab("Day") + ylab("Electricity consumption (in kWh)") + 
  ggtitle("Energy consumption for 2007 - 2010") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(axis.text = element_text(size = 12)) + 
  theme(axis.title = element_text(size = 15)) + 
  theme(title = element_text(size = 18))  + 
  scale_color_manual(name = "Colour", 
                     values = c("a" = "black", "b" = "red", "c" = "blue", "d" = "green"),
                     labels = c("GAP", "kitchen", "laundry", "heatAC"))

# Day - Plot sub-meter 1, 2 and 3
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, 
        name = 'kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_rest, name = 'Rest', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (kWh)"))

# Day Frequ = 10 - Plot sub-meter 1, 2 and 3
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'kitchen', 
        type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_rest, name = 'Rest', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (kWh)"))

# Calender headmap - Manually w/ ggplot
df_daily_ts$year <- as.numeric(year(df_daily_ts$floor_daily))
df_daily_ts$week <- week(df_daily_ts$floor_daily)
df_daily_ts$monthf <- factor(month(df_daily_ts$floor_daily), levels = as.character(1:12), 
                             labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"), 
                             ordered = TRUE)
df_daily_ts$weekday <- wday(df_daily_ts$floor_daily, week_start = 1)
df_daily_ts$weekdayf <- factor(df_daily_ts$weekday, levels = rev(1:7), labels = rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")), ordered=TRUE)
df_daily_ts$yearmonth <- as.yearmon(df_daily_ts$floor_daily)
df_daily_ts$yearmonthf <- factor(df_daily_ts$yearmonth)
test <- ddply(df_daily_ts,.(yearmonthf), transform, monthweek = 1 + week - min(week))
str(test)

ggplot(test, aes(x = monthweek, y = weekdayf, fill = GAP)) + 
  geom_tile(colour = "white") + 
  facet_grid(year~monthf) + 
  scale_fill_gradient(low = "green", high = "red") +
  xlab("Week of Month") + 
  ylab("") + 
  ggtitle("Time-Series Calendar Heatmap: GAP") + 
  labs(fill = "GAP")

# Creating a heatmap to plot the values - Using function
source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R")
calendarHeat(df_y0710$DateTime, df_y0710$Global_active_power, )

#### Hourly ####
## Change granularity - Summarise
df_hourly <- df_y0710 %>% group_by(floor_date(df_y0710$DateTime, unit = "hour")) %>% 
  summarise(GAP = sum(Global_active_power), GRP = sum(Global_reactive_power), GP = sum(Global_power),
            kitchen = sum(Sub_metering_1), laundry = sum(Sub_metering_2), heatAC = sum(Sub_metering_3), others = sum(Sub_rest), price = mean(price))
colnames(df_hourly)[1] <- "DateTime"
df_hourly$DateTime <- as.POSIXct(df_hourly$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
attr(df_hourly$DateTime, "tzone") <- "GMT" 


## Visualization
ggplot(df_hourly[c(1:168), ], aes(x = DateTime)) +  
  geom_line(aes(y = GAP, colour = "a"), group = 1) +
  geom_line(aes(y = kitchen, colour = "b"), group = 1) + 
  geom_line(aes(y = laundry, colour = "c"), group = 1) + 
  geom_line(aes(y = heatAC, colour = "d"), group = 1) +
  #geom_line(aes(y = heatAC, colour = "e"), group = 1) +
  xlab("hour") + ylab("Electricity consumption (in kWh)") + 
  ggtitle("Energy consumption for 2007 - 2010") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(axis.text = element_text(size = 12)) + 
  theme(axis.title = element_text(size = 15)) + 
  theme(title = element_text(size = 18))  + 
  scale_color_manual(name = "Colour", 
                     values = c("a" = "black", "b" = "red", "c" = "blue", "d" = "green", "e" = "orange"),
                     labels = c("GAP", "kitchen", "laundry", "heatAC", "others"))


#### Descriptive Analysis ####
## Average days - 2 ways tested
avg_wdays <- df_daily
avg_wdays$weekday <- weekdays(avg_wdays$Date)

avg_wdays1 <- aggregate(avg_wdays[, c(2:8)], list(avg_wdays$weekday), mean)

avg_wdays2 <- avg_wdays %>% 
  group_by(weekday) %>%
  summarise_at(vars(-Date), funs(mean(., na.rm=TRUE)))
avg_wdays2$weekday <- ordered(avg_wdays2$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                         "Friday", "Saturday", "Sunday"))

# Visualize
ggplot(avg_wdays2, aes(x = avg_wdays2$weekday, y = GAP)) + geom_col()


## Average hours
avg_hours <- df_hourly
avg_hours$hour <- hour(avg_hours$DateTime)
avg_hours <- avg_hours %>% 
  group_by(hour) %>%
  summarise_at(vars(-DateTime), funs(mean(., na.rm = TRUE)))

# Visualize
ggplot(avg_hours, aes(x = avg_hours$hour, y = laundry)) + geom_col()


## Average week - daily
avg_week_d <- df_daily
avg_week_d$week <- week(avg_week_d$Date)
avg_week_d <- avg_week_d %>% 
  group_by(week) %>%
  summarise_at(vars(-Date), funs(mean(., na.rm = TRUE)))

# Visualize
ggplot(avg_week_d, aes(x = avg_week_d$week, y = GAP)) + geom_col()


## Average week - hourly
avg_week_h <- df_hourly
avg_week_h$week <- week(avg_week_h$Date)
avg_week_h <- avg_week_h %>% 
  group_by(week) %>%
  summarise_at(vars(-Date), funs(mean(., na.rm = TRUE)))

# Visualize
ggplot(avg_week, aes(x = avg_week$week, y = GAP)) + geom_col()


## Avg day - Laundry used
# Identify days washing machine/tumble dryer is used
df_daily1 <- df_y0710
ggplot(df_daily[c(1:61),], aes(x = Date, y = laundry)) + geom_line()
df_days_laundry <- df_daily[which(df_daily$laundry >= 1),]
df_days_laundry$Date <- as.POSIXct(df_days_laundry$Date, format = "%Y-%m-%d", tz = "GMT")
attr(df_days_laundry$Date, "tzone") <- "GMT" 

# Save these days in new df and aggregate
df_y0710$Date <- as.POSIXct(df_y0710$Date, format = "%Y-%m-%d", tz = "GMT")
df_laundry_used_min <- df_y0710[which(df_y0710$Date %in% df_days_laundry$Date), ]

df_laundry_used_hourly <- df_laundry_used_min %>% group_by(floor_date(df_laundry_used_min$DateTime, unit = "hour")) %>% 
  summarise(GAP = sum(Global_active_power), GRP = sum(Global_reactive_power), GP = sum(Global_power),
            kitchen = sum(Sub_metering_1), laundry = sum(Sub_metering_2), heatAC = sum(Sub_metering_3), others = sum(Sub_rest), price = mean(price))
colnames(df_laundry_used_hourly)[1] <- "DateTime"

# Aggregate average day of laundry room used
avg_laundryday_hourly <- df_laundry_used_hourly %>% group_by(hour(df_laundry_used_hourly$DateTime)) %>% 
  summarise_at(vars(-DateTime), funs(mean(., na.rm = TRUE)))
colnames(avg_laundryday_hourly)[1] <- "hour"

# Visualize
plot_ly(avg_laundryday_hourly, x = ~avg_laundryday_hourly$hour, 
        y = ~avg_laundryday_hourly$kitchen, 
        name = 'kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~avg_laundryday_hourly$laundry, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~avg_laundryday_hourly$heatAC, name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~avg_laundryday_hourly$others, name = 'Rest', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (kWh)"))


#### Prepare for time series analysis - PoA ####
## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- filter(df_y0710, wday == 1 & hour == 20 & min == 1)

## Create TS object of submeters 1-3 weekly, 3 years
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency = 52, start = c(2007, 1))
tsSM1_070809weekly <- ts(house070809weekly$Sub_metering_1, frequency = 52, start = c(2007, 1))
tsSM2_070809weekly <- ts(house070809weekly$Sub_metering_2, frequency = 52, start = c(2007, 1))

## Create TS object houseDay
tsSM2_houseDay <- ts(houseDay$Sub_metering_2, frequency = 60 , start = c(0, 1))

## Plot 
plot.ts(tsSM3_070809weekly)
autoplot(tsSM3_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")
autoplot(tsSM1_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 1")
autoplot(tsSM2_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 2")
autoplot(tsSM2_houseDay, ts.colour = 'red', xlab = "Time", ylab = "kWh", main = "Sub-meter 2")


## Apply time series linear regression 
#to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 
summary(fitSM3)

#to day10 of sub-meter 2 ts object
fitSM2_houseDay <- tslm(tsSM2_houseDay ~ trend + season) 


## Create the forecast 
#for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM3 <- forecast(fitSM3, h = 20)

# Plot the forecast for sub-meter 3. 
plot(forecastfitSM3)

#for a day of sub-meter 2. Forecast ahead 20 time periods 
forecastfitSM2_houseDay <- forecast(fitSM2_houseDay, h = 240)

# Plot the forecast for a day of sub-meter 2.
plot(forecastfitSM2_houseDay)


## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h = 20, level = c(80, 90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, .03), ylab = "heatAC in kWh", xlab = "Time")

## One comparison chart showing the R2 and RMSE of each model you built


## Decompose Sub-meter 1-3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)
components070809SM1weekly <- decompose(tsSM1_070809weekly)
components070809SM2weekly <- decompose(tsSM2_070809weekly)

## Plot decomposed sub-meter 1-3
plot(components070809SM3weekly)
plot(components070809SM1weekly)
plot(components070809SM2weekly)

## Check summary statistics for decomposed sub-meter 3 
summary(components070809SM3weekly)

## One comparison chart showing the summary statistics for the seasonal, trend and remainder components from each decomposed object

## HoltWinters Forecasting
# Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
tsSM1_070809Adjusted <- tsSM1_070809weekly - components070809SM1weekly$seasonal
tsSM2_070809Adjusted <- tsSM2_070809weekly - components070809SM2weekly$seasonal
autoplot(tsSM3_070809Adjusted)
autoplot(tsSM1_070809Adjusted)
autoplot(tsSM2_070809Adjusted)

# Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted))
plot(decompose(tsSM1_070809Adjusted))
plot(decompose(tsSM2_070809Adjusted))

# Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta = FALSE, gamma = FALSE)
tsSM1_HW070809 <- HoltWinters(tsSM1_070809Adjusted, beta = FALSE, gamma = FALSE)
tsSM2_HW070809 <- HoltWinters(tsSM2_070809Adjusted, beta = FALSE, gamma = FALSE)

plot(tsSM3_HW070809, ylim = c(0, .03))
plot(tsSM1_HW070809, ylim = c(0, .03))
plot(tsSM2_HW070809, ylim = c(0, .03))

# HoltWinters forecast & plot
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h = 25)
tsSM1_HW070809for <- forecast(tsSM1_HW070809, h = 25)
tsSM2_HW070809for <- forecast(tsSM2_HW070809, h = 25)

plot(tsSM3_HW070809for, ylim = c(0, .03), ylab = "kWh", xlab = "Time - Sub-meter 3")
plot(tsSM1_HW070809for, ylim = c(0, .03), ylab = "kWh", xlab = "Time - Sub-meter 1")
plot(tsSM2_HW070809for, ylim = c(0, .03), ylab = "kWh", xlab = "Time - Sub-meter 2")

# Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h = 25, level = c(10,25))
## Plot only the forecasted area
plot(tsSM3_HW070809forC, ylim = c(0, .03), ylab = "kWh", xlab = "Time - Sub-meter 3", start(2010))

#### Time series analysis (TSA) - Pre-processing ####
## Create TS objects
ts_m <- lapply(df_monthly, function(x) ts(x, frequency = 12, start = c(2007, 1)))
ts_w <- lapply(df_weekly, function(x) ts(x, frequency = 52, start = c(2007, 1)))
ts_d <- lapply(df_daily, function(x) ts(x, frequency = 365, start = c(2007, 1)))


## Plot time series and compare granularity - Exemplary for GAP
#install.packages("cowplot")
#library(cowplot)

plot_GAP_m <- autoplot(ts_m$GAP, xlab = "Time in m", ylab = "kWh", main = "GAP monthly")
plot_GAP_w <- autoplot(ts_w$GAP, xlab = "Time in w", ylab = "kWh", main = "GAP weekly")
plot_GAP_d <- autoplot(ts_d$GAP, xlab = "Time in d", ylab = "kWh", main = "GAP daily")

library(cowplot)
plots_GAP <- plot_grid(plot_GAP_m, plot_GAP_w, plot_GAP_d)
plots_GAP


## Check for stationarity
l_stat_m <- lapply(ts_m, function(x) adf.test(x))
l_stat_w <- lapply(ts_w, function(x) adf.test(x))
l_stat_d <- lapply(ts_d, function(x) adf.test(x))


## Achieve stationarity - Option 1: Seasonal Differencing
# Seasonal Differencing
#Differencing a time series means, to subtract each data point in the series from its successor. 
#It is commonly used to make a time series stationary
nsdiffs(tsGAP_m)  # number for seasonal differencing needed; to be > 1
nsdiffs(tsGAP_w)
nsdiffs(tsGAP_d)

tsGAP_m_seasdiff <- diff(tsGAP_m, lag = frequency(tsGAP_m), differences = 1)
plot(tsGAP_m_seasdiff, type="l", main="Seasonally Differenced")  # still not stationary!

# Make it stationary
ndiffs(tsGAP_m_seasdiff)  # number of differences need to make it stationary; to be > 1 
stationaryTS <- diff(tsGAP_m_seasdiff, differences= 1)
plot(stationaryTS, type="l", main="Differenced and Stationary")  # appears to be stationary


## Achieve stationarity - Option 2: Transformation
# Requires retransformation after forecasting
Transformation
log # not to be used if there are zeros
box box()
yeo.johnson # box cox for negative values
xgboost


## Check autocorrelation
# Autocovariace and Autocorrelation
l_autocorr_m <- lapply(ts_m, function(x) acf(x, type = "correlation"))
l_autocov_m <- lapply(ts_m, function(x) acf(x, type = "covariance"))

l_autocorr_w <- lapply(ts_w, function(x) acf(x, type = "correlation"))
l_autocov_w <- lapply(ts_w, function(x) acf(x, type = "covariance"))

l_autocorr_d <- lapply(ts_d, function(x) acf(x, type = "correlation"))
l_autocov_d <- lapply(ts_d, function(x) acf(x, type = "covariance"))

# Partial autocorrelation
l_pAutocorr_m <- lapply(ts_m, function(x) pacf(x))
l_pAutocorr_w <- lapply(ts_w, function(x) pacf(x))
l_pAutocorr_d <- lapply(ts_d, function(x) pacf(x))


## Decomposing time series: stl(seasonal decomposition of Time Series by Loess)
l_stl_m <- lapply(ts_m, function(x) stl(x, s.window = "periodic"))
l_stl_w <- lapply(ts_w, function(x) stl(x, s.window = "periodic"))
l_stl_d <- lapply(ts_d, function(x) stl(x, s.window = "periodic"))

# Plot seasonality, trend, remainder
i <- NULL
for (i in c(2:length(l_stl_m))){
  print(plot(l_stl_m[[i]]))
}

# Safe stl components (seasonality, trend, remainder)
tsGAP_m_stl_components <- data.frame(tsGAP_m_stl$time.series)


## Compare percentage of randomness for diff. granularities
# Calculate percentage of remainder
perRand_m <- data.frame(map2(l_stl_m, ts_m, function(x,y) mean(abs(remainder(x)))/mean(y)*100))
perRand_w <- data.frame(map2(l_stl_w, ts_w, function(x,y) mean(abs(remainder(x)))/mean(y)*100))
perRand_d <- data.frame(map2(l_stl_d, ts_d, function(x,y) mean(abs(remainder(x)))/mean(y)*100))

# (Re)arrange data.frame
perRand <- rbind(perRand_m, perRand_w[, c(1:9)], perRand_d)
perRand$Granularity <- ordered(x = c("m", "w", "d"), levels = c("m", "w", "d"))
str(perRand)
perRand <- melt(perRand, id = c("Granularity"))
colnames(perRand) <- c("Granularity", "Meter", "Percentage")

# Plot percentages for each meter and diff. granularity
ggplot(perRand[-c(1:3) ,], aes(x = Granularity, y = Percentage)) + geom_col() +
  ylab("Percentage (remainder/data)") +
  ggtitle("Percentage of remainder per Meter for different Granularity") + 
  facet_grid(~Meter)


## Multi-Seasonal Time Series
# Create multi-seasonal TS
l_msts_m <- lapply(df_monthly, function(x) msts(x, seasonal.periods = c(12)))
l_msts_w <- lapply(df_weekly, function(x) msts(x, seasonal.periods = c(4.36, 52)))
l_msts_d <- lapply(df_daily, function(x) msts(x, seasonal.periods = c(7, 30, 365.25)))

# Multipe seasonal decomposition: mstl(multi-seasonal decomposition of Time Series by Loess)
l_multiDec_m <- lapply(l_msts_m, function(x) mstl(x, s.window = "periodic"))
l_multiDec_w <- lapply(l_msts_w, function(x) mstl(x, s.window = "periodic"))
l_multiDec_d <- lapply(l_msts_d, function(x) mstl(x, s.window = "periodic"))

# Plot mstl
i <- NULL
for (i in c(2:length(l_multiDec_d))){
  print(plot(l_multiDec_d[[i]]))
}


## De-seasonalize
# Monthly
tsGAP_m_sa <- seasadj(tsGAP_m_stl)  # de-seasonalize
plot(tsGAP_m, type = "l")  # original series
plot(tsGAP_m_sa, type = "l")  # seasonal adjusted
seasonplot(tsGAP_m_sa, 12, col = rainbow(12), year.labels = TRUE, main = "Seasonal plot: GAP_monthly") # seasonal frequency set as 12 for monthly data.

# Weekly
tsGAP_w_sa <- seasadj(tsGAP_w_stl)  # de-seasonalize
plot(tsGAP_w, type = "l")  # original series
plot(tsGAP_w_sa, type = "l")  # seasonal adjusted
seasonplot(tsGAP_w_sa, 52, col = rainbow(12), year.labels = TRUE, main = "Seasonal plot: GAP_weekly") # seasonal frequency set as 12 for monthly data.


## Data splitting
# Monthly, weekly, daily
l_train_m <- lapply(ts_m, function(x) window(x, start = c(2007, 1), end = c(2009, 12)))
l_train_w <- lapply(ts_w, function(x) window(x, start = c(2007, 1), end = c(2009, 52)))
l_train_d <- lapply(ts_d, function(x) window(x, start = c(2007, 1), end = c(2009, 365)))

l_train_msts_m <- lapply(l_msts_m, function(x) window(x, start = c(1, 1), end = c(3, 12)))
l_train_msts_w <- lapply(l_msts_w, function(x) window(x, start = c(1, 1), end = c(3, 52)))
l_train_msts_d <- lapply(l_msts_d, function(x) window(x, start = c(1, 1), end = c(3, 365)))


#### TS-Analysis - Models (Train and forecast) ####
## Model 1: ARIMA
# Train model
l_aA_m <- lapply(l_train_m, function(x) auto.arima(x))
l_aA_w <- lapply(l_train_w, function(x) auto.arima(x))
#l_aA_d <- lapply(l_train_d, function(x) auto.arima(x)) # takes to long

# Cross-Validation
# Problem: How to extract variables??
tsCV()

# Forecast
l_fc_aA_m <- lapply(l_aA_m, function(x) forecast(x, h = 10))
l_fc_aA_w <- lapply(l_aA_w, function(x) forecast(x, h = 43))

# Plot forecast
i <- NULL
for (i in c(2:length(l_fc_aA_m))){
  print(plot(l_fc_aA_m[[i]]))
}

i <- NULL
for (i in c(2:length(l_fc_aA_m))){
  print(autoplot(ts_m[[i]]) + autolayer(l_fc_aA_m[[i]], series = "Arima", PI = FALSE))
}

# Calculate performance metrics
l_aA_acc_m <- map2(l_fc_aA_m, ts_m, function(x, y) accuracy(x, y))
l_aA_acc_w <- map2(l_fc_aA_w, ts_w, function(x, y) accuracy(x, y))

# Plot residuals

plot.ts(l_fc_aA_m$GAP$residuals)

plot_fcErrors <- function(fcerrors) {
  # create hist of fcerrors
  v_binsize <- IQR(fcerrors, na.rm = TRUE)/4
  v_sd <- sd(fcerrors, na.rm = TRUE)
  v_min <- min(fcerrors, na.rm = TRUE) - v_sd*5
  v_max <- max(fcerrors, na.rm = TRUE) + v_sd*3
  # generate normal distribution (mean = 0, standard deviation = sd)
  v_norm <- rnorm(10000, mean = 0, sd = v_sd)
  v_min2 <- min(v_norm)
  v_max2 <- max(v_norm)
  if (v_min2 < v_min) { v_min <- v_min2 }
  if (v_max2 > v_max) { v_max <- v_max2 }
  # create hist of fc errors (red) w/ normally distributed data overlaid:
  v_bins <- seq(v_min, v_max, v_binsize)
  hist(fcerrors, col = "red", freq = FALSE, breaks = v_bins) # freq = F: area under the hist = 1
  # create hist of normal distributed data
  v_hist <- hist(v_norm, plot = FALSE, breaks = v_bins)
  # plot normal curve (blue line)
  points(v_hist$mids, v_hist$density, type = "l", col = "blue", lwd = 2)
}
plot_fcErrors(l_fc_aA_m$GAP$residuals)


## Model 2a: HoltWinters
# Train model (w/ Exponential Smoothing)
l_hw_m <- lapply(l_train_m, function(x) HoltWinters(x, beta = FALSE, gamma = FALSE ))
l_hw_w <- lapply(l_train_w, function(x) HoltWinters(x, beta = FALSE, gamma = FALSE ))
l_hw_d <- lapply(l_train_d, function(x) HoltWinters(x, beta = FALSE, gamma = FALSE ))

#test w/o smoothing and using msts --> w/o even stranger; w/ msts same than above
test <- lapply(l_train_m, function(x) HoltWinters(x, gamma = FALSE ))
test_msts <- lapply(l_train_msts_m, function(x) HoltWinters(x, beta = FALSE, gamma = FALSE ))
  # beta = FALSE: applies exponential smoothing, gamma = FALSE: fits a non-seasonal model
plot(test_msts$GAP, ylim = c(0, 1000))
plot(l_hw_m$GAP, ylim = c(0, 1000))

# Cross-Validation

# Forecast
l_fc_hw_m <- lapply(l_hw_m, function(x) forecast(x, h = 10))
l_fc_hw_w <- lapply(l_hw_w, function(x) forecast(x, h = 43))
l_fc_hw_d <- lapply(l_hw_d, function(x) forecast(x, h = 301))

test2 <- lapply(test, function(x) forecast(x, h = 10))
l_fc_test_msts <- lapply(test_msts, function(x) forecast(x, h = 10))

# Plot forecast
plot(l_fc_hw_d$heatAC, ylim = c(0, 1000), ylab = "kWh", xlab = "Time - GAP")

plot(test2$GAP, ylim = c(0, 1000), ylab = "kWh", xlab = "Time - GAP")
plot(l_fc_test_msts$GAP, ylim = c(0, 1000), ylab = "kWh", xlab = "Time - GAP")

autoplot(ts_m$GAP) + autolayer(l_fc_hw_m$GAP, series = "Arima", PI = FALSE)

# Calculate performance metrics
l_hw_acc_m <- map2(l_fc_hw_m, ts_m, function(x, y) accuracy(x, y))
l_hw_acc_w <- map2(l_fc_hw_w, ts_w, function(x, y) accuracy(x, y))
l_hw_acc_d <- map2(l_fc_hw_d, ts_d, function(x, y) accuracy(x, y))


## Model 2b: HoltWinters (seasonal adjusted)
# Seasonal adjusting (see above)
autoplot(tsGAP_m)
autoplot(tsGAP_m_sa)

# Test Seasonal Adjustment by running Decompose again --> very, very small scale for Seasonal
plot(decompose(tsGAP_m))
plot(decompose(tsGAP_m_sa)) # small??

# Holt Winters Exponential Smoothing (training) & Plot
HW_GAP_m <- HoltWinters(tsGAP_m_sa, beta = FALSE, gamma = FALSE)
plot(HW_GAP_m, ylim = c(0, 1000))

# HoltWinters forecast & plot
fc_HW_GAP_m <- forecast(HW_GAP_m, h = 9)
plot(fc_HW_GAP_m, ylim = c(0, 1000), ylab = "kWh", xlab = "Time - GAP")
autoplot(tsGAP_w) + autolayer(fc_HW_GAP_m, series = "Arima", PI = FALSE)


## Model 3: Tbats
# Train model
l_tbats_m <- lapply(l_train_msts_m, function(x) tbats(x))
l_tbats_w <- lapply(l_train_msts_w, function(x) tbats(x))
# l_tbats_d <- lapply(l_train_msts_d, function(x) tbats(x)) # Takes too long

# Cross-Validation
#test <- tsCV(ts_m$GAP, tbats, h = 9) # ????

# Forecast
l_fc_tbats_m <- lapply(l_tbats_m, function(x) forecast(x, h = 10))
l_fc_tbats_w <- lapply(l_tbats_w, function(x) forecast(x, h = 43))

# Plot forecast
i <- NULL
for (i in c(2:length(l_fc_tbats_m))){
  print(plot(l_fc_tbats_m[[i]]))
}

# Calculate performance metrics
l_tbats_acc_m <- map2(l_fc_tbats_m, l_msts_m, function(x, y) accuracy(x, y))
l_tbats_acc_w <- map2(l_fc_tbats_w, l_msts_w, function(x, y) accuracy(x, y))

## Model 4: TSLM
# Train model
l_tslm_m <- lapply(l_train_m, function(x) tslm(x ~ trend + season))
l_tslm_w <- lapply(l_train_w, function(x) tslm(x ~ trend + season))
l_tslm_d <- lapply(l_train_d, function(x) tslm(x ~ trend + season))

# Cross-Validation
#test <- tsCV(ts_m$GAP, tbats, h = 9) # ????

# Forecast
l_fc_tslm_m <- lapply(l_tslm_m, function(x) forecast(x, h = 10))
l_fc_tslm_w <- lapply(l_tslm_w, function(x) forecast(x, h = 43))
l_fc_tslm_d <- lapply(l_tslm_d, function(x) forecast(x, h = 301))
summary(l_fc_tslm_m$GAP)

# Plot forecast
i <- NULL
for (i in c(2:length(l_fc_tslm_m))){
  print(plot(l_fc_tslm_m[[i]]))
}

# Calculate performance metrics
l_tslm_acc_m <- map2(l_fc_tslm_m, ts_m, function(x, y) accuracy(x, y))
l_tslm_acc_w <- map2(l_fc_tslm_w, ts_w, function(x, y) accuracy(x, y))
l_tslm_acc_d <- map2(l_fc_tslm_d, ts_d, function(x, y) accuracy(x, y))


#### TS-Analysis - Prophet ####
## Prophet - Single variable
# Split, rename, train
df_proph_d <- df_daily[c(1:821), c(1:2)]
colnames(df_proph_d) <- c("ds", "y")
Proph_GAP_d <- prophet(df_proph_d, daily.seasonality = TRUE)

# Forecast
future <- make_future_dataframe(Proph_GAP_d, periods = 274)
forecast <- predict(Proph_GAP_d, future)

# Visualize Forecast
dyplot.prophet(Proph_GAP_d, forecast)
plot(Proph_GAP_d, forecast)

# Evaluation
proph_CV_d <- cross_validation(Proph_GAP_d, 
                               initial = 100, 
                               horizon = 180, units = "days") 
proph_Acc_d <- performance_metrics(proph_CV_d)
plot_cross_validation_metric(proph_CV_d, metric = "mape")


## Prophet - All_daily
# Split, rename, train
df_proph_d <- df_daily[c(1:1096),]
x <- colnames(df_proph_d)
l_proph_d <- NULL
l_proph_d <- list()
i <- NULL
for (i in c(1:(ncol(df_proph_d)-1))) {
  ds <- df_proph_d$Date
  y <- df_proph_d[, i+1]
  z <- cbind(ds, y)
  colnames(z)[2] <- "y"
  l_proph_d[[i]] <- data.frame(z)
  i <- i+1
}

l_prophet_d <- lapply(l_proph_d, function(x) prophet(x, daily.seasonality = TRUE))

# Forecast
l_futDF_Proph_d <- lapply(l_prophet_d, function(x) make_future_dataframe(x, periods = 304))
l_pred_proph_d <- map2(l_prophet_d, l_futDF_Proph_d, function(x, y) predict(x, y))

# Visualize forecast
i <- NULL
for (i in c(1:length(l_prophet_d))){
  print(plot(l_prophet_d[[i]], l_pred_proph_d[[i]]))
}

# Calculate performance metrics 
l_prophCV_d <- lapply(l_prophet_d, function(x) cross_validation(x, initial = 900,   horizon = 62, units = "days"))
l_proph_Acc_d <- lapply(l_prophCV_d, function(x) performance_metrics(x))

i <- NULL
for (i in c(2:length(l_prophCV_d))){
  print(plot_cross_validation_metric(l_prophCV_d[[i]], metric = "mape"))
}


## Prophet - All_weekly
# Split, rename, train
df_proph_w <- df_weekly[c(1:157),]
x <- colnames(df_proph_w)
l_proph_w <- NULL
l_proph_w <- list()
i <- NULL
for (i in c(1:(ncol(df_proph_w)-1))) {
  ds <- df_proph_w$Date
  y <- df_proph_w[, i+1]
  z <- cbind(ds, y)
  colnames(z)[2] <- "y"
  l_proph_w[[i]] <- data.frame(z)
  ds <- NULL
  y <- NULL
  z <- NULL
  i <- i + 1
}

l_prophet_w <- lapply(l_proph_w, function(x) prophet(x, weekly.seasonality = TRUE, daily.seasonality = FALSE))

# Forecast
l_fcProph_w <- lapply(l_prophet_w, function(x) make_future_dataframe(x, periods = 43, freq = "week"))
l_pred_proph_w <- map2(l_prophet_w, l_fcProph_w, function(x, y) predict(x, y))

# Visualize forecast
i <- NULL
for (i in c(1:length(l_prophet_w))){
  print(plot(l_prophet_w[[i]], l_pred_proph_w[[i]]))
}

# Calculate performance metrics 
l_prophCV_w <- lapply(l_prophet_w, function(x) cross_validation(x, horizon = 9, units = "weeks", initial = 130))
l_proph_acc_w <- lapply(l_prophCV_w, function(x) performance_metrics(x))


## Prophet - All_monthly
# Split, rename, train
df_proph_m <- df_monthly[c(1:36),]
x <- colnames(df_proph_m)
l_proph_m <- NULL
l_proph_m <- list()
i <- NULL
for (i in c(1:(ncol(df_proph_m)-1))) {
  ds <- df_proph_m$Date
  y <- df_proph_m[, i+1] # Date column not included
  z <- cbind(ds, y)
  colnames(z)[2] <- "y"
  l_proph_m[[i]] <- data.frame(z)
  ds <- NULL
  y <- NULL
  z <- NULL
  i <- i + 1
}

l_prophet_m <- lapply(l_proph_m, function(x) prophet(x, yearly.seasonality = TRUE,  weekly.seasonality = FALSE, daily.seasonality = FALSE))

# Forecast
l_fcProph_m <- lapply(l_prophet_m, function(x) make_future_dataframe(x, periods = 10, freq = "month"))
l_pred_proph_m[[2]] <- map2(l_prophet_m, l_fcProph_m, function(x, y) predict(x, y))

dyplot.prophet(l_prophet_m[[2]], l_pred_proph_m[[2]])

map2(prophetmodel$daily, prophetpredictions$daily, dyplot.prophet)

# Visualize forecast
i <- NULL
for (i in c(1:length(l_prophet_m))){
  print(plot(l_prophet_m[[i]], l_pred_proph_m[[i]]))
}

# Calculate performance metrics 
l_prophCV_m <- lapply(l_prophet_m, function(x) cross_validation(x, horizon = 9, units = "weeks", initial = 130))
l_proph_acc_m <- lapply(l_prophCV_m, function(x) performance_metrics(x))

df_proph_acc_mean_m <- data.frame()
for (b in c(1:length(l_proph_acc_m))){
  df_proph_acc_mean_m <- rbind(df_proph_acc_mean_m, colMeans(l_proph_acc_m[[1]][,c(2:5)]))
}
colnames(df_proph_acc_mean_m) <- c("MSE", "RMSE", "MAE", "MAPE")
df_proph_acc_mean_m$Model <- rep("proph", 7)
x <- names(l_aA_acc_m)
df_proph_acc_mean_m$Meter <- x[c(2:8)]


#### TS-Analysis - Evaluation of Models ####
## Create date.frames
# Monthly
df_aA_acc_m <- NULL
df_hw_acc_m <- NULL
df_tbats_acc_m <- NULL
df_tslm_acc_m <- NULL

for (a in c(2:length(l_aA_acc_m))) {
  df_aA_acc_m <- data.frame(rbind(df_aA_acc_m, l_aA_acc_m[[a]][2,]))
  df_hw_acc_m <- data.frame(rbind(df_hw_acc_m, l_hw_acc_m[[a]][2,]))
  df_tbats_acc_m <- data.frame(rbind(df_tbats_acc_m, l_tbats_acc_m[[a]][2,]))
  df_tslm_acc_m <- data.frame(rbind(df_tslm_acc_m, l_tslm_acc_m[[a]][2,]))
}

df_aA_acc_m$Model <- rep("aA", 7)
df_aA_acc_m$Meter <- x[c(2:8)]
df_hw_acc_m$Model <- rep("hw", 7)
df_hw_acc_m$Meter <- x[c(2:8)]
df_tbats_acc_m$Model <- rep("tbats", 7)
df_tbats_acc_m$Meter <- x[c(2:8)]
df_tslm_acc_m$Model <- rep("tslm", 7)
df_tslm_acc_m$Meter <- x[c(2:8)]

# Weekly
df_aA_acc_w <- NULL
df_hw_acc_w <- NULL
df_tbats_acc_w <- NULL
df_tslm_acc_w <- NULL

for (a in c(2:length(l_aA_acc_w))) {
  df_aA_acc_w <- data.frame(rbind(df_aA_acc_w, l_aA_acc_w[[a]][2,]))
  df_hw_acc_w <- data.frame(rbind(df_hw_acc_w, l_hw_acc_w[[a]][2,]))
  df_tbats_acc_w <- data.frame(rbind(df_tbats_acc_w, l_tbats_acc_w[[a]][2,]))
  df_tslm_acc_w <- data.frame(rbind(df_tslm_acc_w, l_tslm_acc_w[[a]][2,]))
}

df_aA_acc_w$Model <- rep("aA", 7)
df_aA_acc_w$Meter <- x[c(2:8)]
df_hw_acc_w$Model <- rep("hw", 7)
df_hw_acc_w$Meter <- x[c(2:8)]
df_tbats_acc_w$Model <- rep("tbats", 7)
df_tbats_acc_w$Meter <- x[c(2:8)]
df_tslm_acc_w$Model <- rep("tslm", 7)
df_tslm_acc_w$Meter <- x[c(2:8)]

# Daily
#df_aA_acc_d <- NULL
df_hw_acc_d <- NULL
#df_tbats_acc_d <- NULL
df_tslm_acc_d <- NULL

for (a in c(2:length(l_hw_acc_d))) {
  #df_aA_acc_w <- data.frame(rbind(df_aA_acc_d, l_aA_acc_w[[a]][2,]))
  df_hw_acc_d <- data.frame(rbind(df_hw_acc_d, l_hw_acc_d[[a]][2,]))
  #df_tbats_acc_w <- data.frame(rbind(df_tbats_acc_d, l_tbats_acc_w[[a]][2,]))
  df_tslm_acc_d <- data.frame(rbind(df_tslm_acc_d, l_tslm_acc_d[[a]][2,]))
}

#df_aA_acc_w$Model <- rep("aA", 7)
df_hw_acc_d$Model <- rep("hw", 7)
df_hw_acc_d$Meter <- x[c(2:8)]
#df_tbats_acc_w$Model <- rep("tbats", 7)
df_tslm_acc_d$Model <- rep("tslm", 7)
df_tslm_acc_d$Meter <- x[c(2:8)]

# Merge data.frames of all perf. metrics of each model and granularity
df_acc_m <- rbind(df_aA_acc_m, df_hw_acc_m, df_tbats_acc_m, df_tslm_acc_m)
#df_acc_m$Granularity <- rep("m", 28)
df_acc_m <- rbind(df_acc_m[, c(2, 3, 5, 9, 10)], df_proph_acc_mean_m[, c(2:6)])
df_acc_m_melt <- melt(df_acc_m, id = c("Model", "Meter"))
colnames(df_acc_m_melt) <- c("Model", "Meter", "Metric", "Value")
df_acc_m_melt$Meter <- ordered(x = df_acc_m_melt$Meter , levels = x[c(2:8)])

df_acc_w <- rbind(df_aA_acc_w, df_hw_acc_w, df_tbats_acc_w, df_tslm_acc_w)
#df_acc_w$Granularity <- rep("w", 28)
df_acc_w_melt <- melt(df_acc_w, id = c("Model", "Meter"))
colnames(df_acc_w_melt) <- c("Model", "Meter", "Metric", "Value")

df_acc_d <- rbind(df_hw_acc_d, df_tslm_acc_d)
#df_acc_d$Granularity <- rep("d", 14)
df_acc_d_melt <- melt(df_acc_d, id = c("Model", "Meter"))
colnames(df_acc_d_melt) <- c("Model", "Meter", "Metric", "Value")


## Compare models - performance metrics
# Plot performance metrics for each model for each meter
ggplot(df_acc_m_melt, aes(x = Metric, y = Value)) + geom_col() +
  ylab("Value of perf. metric") + ylim(0, 300) +
  ggtitle("Comparison of models for monthly granularity") + 
  facet_grid(Meter~Model)

ggplot(df_acc_w_melt, aes(x = Metric, y = Value)) + geom_col() +
  ylab("Value of perf. metric") + 
  ggtitle("Comparison of models for weekly granularity") + 
  facet_grid(Meter~Model)

ggplot(df_acc_d_melt, aes(x = Metric, y = Value)) + geom_col() +
  ylab("Value of perf. metric") +
  ggtitle("Comparison of models for daily granularity") + 
  facet_grid(Meter~Model)

#compare reality and the forecast for 2010 - SNaive & Hybridnormal are the most accurace
autoplot(ts_m$GAP) + autolayer(l_fc_aA_m$GAP, series = "Arima", PI = FALSE)

autoplot(ts_m$GAP, cex = 2) +
  autolayer(l_fc_aA_m$GAP, series = "aA", PI = FALSE, cex = 1) +
  autolayer(l_fc_hw_m$GAP, series = "hw", PI = FALSE, cex = 1) + 
  #autolayer(l_fc_tbats_m$GAP, series = "tbats", PI = FALSE, cex = 1) +
  autolayer(l_fc_tslm_m$GAP, series = "tslm", PI = FALSE, cex = 1) + 
  xlab("Time [y]") + ggtitle("Forecaste 2010 using diff. models - GAP")


## Check residulas 
#checkresiduals(object, lag, df = NULL, test, plot = TRUE, ...)



#### Dashboard Exports ####
## Descriptive history - Consumption
monthly_consumption <- data.frame(df_monthly[,c(1:3, 5:8)])
monthly_consumption <- melt(monthly_consumption, id = c("Date"))
monthly_consumption$year <- year(monthly_consumption$Date)
monthly_consumption$month <- month(monthly_consumption$Date)
write.csv(monthly_consumption, file = "monthly_consumption.csv" )

daily_consumption <- data.frame(df_daily[,c(1:3, 5:8)])
daily_consumption <- melt(daily_consumption, id = c("Date"))
write.csv(daily_consumption, file = "daily_consumption.csv" )


## Energy price
monthly_cost <- data.frame(df_monthly)
c <- NULL
for (c in c(1:7)){
  monthly_cost[9+c] <- monthly_cost[1+c] * monthly_cost[9]/100
}
colnames(monthly_cost)[c(10:16)] <- c("GAP_cost", "GRP_cost", "GP.1", "kitchen_cost", "laundry_cost", "heatAC_cost", "others_cost" )
monthly_cost <- monthly_cost[, c(1:3, 5:11, 13:16)]
monthly_cost <- monthly_cost[, c(1, 10:14)]
monthly_cost <- melt(monthly_cost, id = c("Date"))


## Saving potential
# Create consumption lag
lag_matrix <- rbind(avg_laundryday_hourly, avg_laundryday_hourly)
avg_laundryday_hourly$laundry_lag <- lag_matrix$laundry[c(15:38)]

plot_ly(avg_laundryday_hourly, x = ~avg_laundryday_hourly$hour, 
        y = ~avg_laundryday_hourly$kitchen, 
        name = 'kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~avg_laundryday_hourly$laundry, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~avg_laundryday_hourly$heatAC, name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~avg_laundryday_hourly$others, name = 'Rest', mode = 'lines') %>%
  add_trace(y = ~avg_laundryday_hourly$laundry_lag, name = 'laundry_optimized', mode = 'lines') %>%
  layout(title = "Average day using laundry room & savings potential",
         xaxis = list(title = "Time [d]"),
         yaxis = list (title = "Power [kWh]"))



# Calculate savings per year
savings <- NULL

for (yl in c(2007:2010)){
  savings[yl-2006] <- (sum(avg_laundryday_hourly$laundry*avg_laundryday_hourly$price)-
                sum(avg_laundryday_hourly$laundry_lag*avg_laundryday_hourly$price))/100*
    length(which(year(df_days_laundry$Date) == yl))
}

year <- c(2007:2010)
df_savings <- cbind(year, savings)

# Saving CSV files
write.csv(df_savings, file = "df_savings.csv")

avg_laundryday_hourly <- avg_laundryday_hourly[, c(1:3, 5:8, 10)]
avg_laundryday_hourly <- melt(data.frame(avg_laundryday_hourly), id = c("hour"))
write.csv(avg_laundryday_hourly, file = "avg_laundryday_hourly.csv")


## Forecast
monthly_fc_proph <- monthly_consumption[which(year(monthly_consumption$Date) != 2010) ,c(1:3)]
colnames(monthly_fc_proph) <- c("ds", "variable", "y")
write.csv(monthly_fc_proph, file = "monthly_fc_proph.csv")


#### End ####
##ToDO
# Try Hybrid model
# Compare/plot models and prophet
