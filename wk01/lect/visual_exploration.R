# Content from: David Huang's Data Vizualization kaggle kernel: https://www.kaggle.com/h19881812/data-vizualization/code

# install.packages('ggplot2')
# install.packages('lubridate')
# install.packages('ddply')
# install.packages('data.table')

rm(list = ls())

library(ggplot2)
library(lubridate)
library(plyr)
library(data.table)

data_localfile <- "wk01/lect/data/bike_rental_train.csv"
train <- fread(data_localfile)

train$season  <- factor(train$season, labels = c("Q1", "Q2", "Q3", "Q4"))
train$weather <- factor(train$weather, labels = c("Good", "Normal", "Bad", "Very Bad"))
train$hour    <- factor(hour(ymd_hms(train$datetime)))
train$times   <- as.POSIXct(strftime(ymd_hms(train$datetime), format="%H:%M:%S"), format="%H:%M:%S")
train$Weekday <- wday(ymd_hms(train$datetime), label=TRUE)

train$prediction <- 100
train[season == "Spring", prediction := 10 ]

error <- sum((train$prediction - train$count) ^  2)/length(train$prediction)
error

## Few example plots
ggplot(train, aes(x=count))+geom_density() 
ggplot(train, aes(x=season, y=windspeed))+geom_boxplot() # easy to understand
ggplot(train, aes(x=windspeed)) + geom_density() + facet_grid(. ~ season) # difficult to understand

ggplot(train, aes(x=temp)) + geom_density() + facet_grid(. ~ season)

traindt = data.table(train)
season_summary <- train[, .(count = mean(count)), by=.(season, hour)]
weather_summary <- train[, .(count = mean(count)), by=.(weather, hour)]

ggplot(train, aes(x = hour, y = count, colour = season)) +
  geom_point(data = season_summary, aes(group = season)) +
  geom_line(data = season_summary, aes(group = season)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("People rent bikes more in Fall, and much less in Spring.\n") + 
  theme(plot.title=element_text(size=18))

ggplot(train, aes(x = hour, y = count, colour = weather)) +
  geom_point(data = weather_summary, aes(group = weather)) +
  geom_line(data = weather_summary, aes(group = weather)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("People rent bikes more when the weather is Good.\n") + 
  theme(plot.title=element_text(size=18))