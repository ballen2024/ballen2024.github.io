## ----warning=FALSE-------------------------------------------------------
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(tidyverse))
# suppressMessages(install.packages("devtools"))
# suppressMessages(devtools::install_github("stefanedwards/lemon"))
suppressMessages(library(lemon))
suppressMessages(library(scales))
knit_print.data.frame <- lemon_print

## ----warning=FALSE-------------------------------------------------------
raw <- suppressMessages(read_csv("https://query.data.world/s/jo4zezsyrfp3rnutrsmjk7nzw75npd"))

game_data <- tibble(date=raw$date, 
                    day=raw$day_of_week, 
                    day_night=raw$day_night, 
                    v_name=raw$v_name, 
                    h_name=raw$h_name,
                    park_id=raw$park_id,
                    attendance=raw$attendance,
                    duration=raw$length_minutes,
                    v_score=raw$v_score,
                    h_score=raw$h_score,
                    v_hits=raw$v_hits,
                    h_hits=raw$h_hits,
                    v_homeruns=raw$v_homeruns,
                    h_homeruns=raw$h_homeruns)

game_data <- game_data %>%
  na.omit()

## ----render=lemon_print, echo=FALSE--------------------------------------
head(game_data)
df <- summary(game_data[7:14])

## ----render=lemon_print, echo=FALSE--------------------------------------
head(df)

## ---- echo=FALSE, render=lemon_print-------------------------------------
t <- table(game_data$day_night)
day_night_data <- as.data.frame(t)
colnames(day_night_data) <- c("Game Time", "Number of Games")
day_night_data$`Game Time` <- c("Day","Night") 
head(day_night_data)
# p <- ggplot(day_night_data, aes(x=Var1,y=Freq)) + geom_bar(stat = "identity", fill="steelblue3") + geom_text(aes(label= paste("Count = ", day_night_data$Freq)), vjust=1.6, color="white", size=4) + labs(x = "Game Time (Day/Night)",y="Number of Games",title="Frequency of Day/Night Games from 1871-2016")

## ----echo = FALSE--------------------------------------------------------
require(scales)
date <- as.character(game_data$date)
year <- substr(date, 1, 4)
p <- ggplot(game_data, aes(x = year, y = game_data$attendance)) + geom_bar(stat="identity", fill="steelblue3") + theme(axis.text.x = element_text(angle=45, hjust=1)) + labs(y="Total Attendance",x="Year") + scale_x_discrete(breaks =  seq(1871, 2016, 5)) + scale_y_continuous(labels = scales::comma)
p

