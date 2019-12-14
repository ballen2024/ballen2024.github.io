## ----warning=FALSE------------------------------------------------------------
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("devtools")
# devtools::install_github("stefanedwards/lemon")
# install.packages("scales")
# install.packages("tidyverse")
# install.packages("knitr")
# install.packages("stringr")
# install.packages("caret")

suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(lemon))
suppressMessages(library(scales))
suppressMessages(library(tidyverse))
suppressMessages(library(knitr))
suppressMessages(library(stringr))
suppressMessages(library(caret))
suppressMessages(library(baseballr))
knit_print.data.frame <- lemon_print


## ----message=FALSE, error=FALSE, warning=FALSE, results='hide'----------------
purl("deliverable1.Rmd", output = "deliverable1.r")
source("deliverable1.r")


## ---- warning=FALSE-----------------------------------------------------------
foul_balls <- suppressMessages(read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/foul-balls/foul-balls.csv"))
launch_data <- suppressMessages(read_csv("./savant_data.csv"))
standings <- suppressMessages(read_csv("./standings.csv"))



## ---- warning=FALSE-----------------------------------------------------------
game_data <- game_data %>%
  filter(date >= 19900101) %>%
  rename(away=v_name, home=h_name)

foul_balls <- foul_balls %>%
  na.omit() %>%
  rename(date = game_date, hit_type = type_of_hit, velocity = exit_velocity)

launch_data <- launch_data %>%
  select(c(game_date, player_name, home_team, away_team, launch_speed, launch_angle)) %>%
  filter(launch_speed != "null", launch_angle != "null") %>%
  rename(date=game_date, field=player_name, home=home_team, away=away_team, velocty=launch_speed, angle=launch_angle)

standings <- standings %>%
  select(c(Rk, Tm, G, W, L, W_L_per, Rdiff)) %>%
  rename(rank=Rk, team=Tm, games=G, wins=W, losses=L, win_loss_percentage= W_L_per, run_diff=Rdiff) %>%
  na.omit()


## ----warning=FALSE------------------------------------------------------------
game_data$date <- as.character(game_data$date)
game_data$date <- as.Date(game_data$date, format="%Y%m%d")

foul_balls$predicted_zone <- as.factor(foul_balls$predicted_zone)
foul_balls$camera_zone <- as.factor(foul_balls$camera_zone)
foul_balls$used_zone <- as.factor(foul_balls$used_zone)

launch_data$velocty <- as.double(launch_data$velocty)
launch_data$angle <- as.double(launch_data$angle)



## ----warning=FALSE------------------------------------------------------------
teams <- suppressMessages(read_csv("./teams.csv"))


## ----warning=FALSE------------------------------------------------------------
split <- str_split(foul_balls$matchup, "(vs|VS)")
mat <- matrix(unlist(split), ncol=2, byrow=TRUE)
matchup <- as.data.frame(mat)
foul_balls$home <- as.character(matchup$V2)
foul_balls$away <- as.character(matchup$V1)

teams$space_front <- paste("", teams$full)
teams$space_back <- paste(teams$full, "")
foul_balls$home <- teams[match(foul_balls$home, teams$space_front), "abb"]
foul_balls$away <- teams[match(foul_balls$away, teams$space_back), "abb"]


## ---- echo=FALSE, render=lemon_print------------------------------------------
head(game_data)


## ---- echo=FALSE, render=lemon_print------------------------------------------
head(foul_balls)


## ---- echo=FALSE, render=lemon_print------------------------------------------
head(launch_data)


## ---- echo=FALSE, render=lemon_print------------------------------------------
head(standings)


## ---- echo=FALSE, render=lemon_print------------------------------------------
head(teams)


## -----------------------------------------------------------------------------
run_differential <- tibble(date=game_data$date, 
                           home=game_data$home, 
                           away=game_data$away, 
                           h_score=game_data$h_score, 
                           v_score=game_data$v_score)
run_differential <- run_differential %>%
  filter(date >= "2016-04-03") %>%
  filter(date <= "2016-05-03")

run_differential <- run_differential %>%
  mutate(h_run_diff=(h_score-v_score), v_run_diff=(v_score-h_score))

# I had to use the following 2 lines to get the win-loss percentage as a vector rather than a dataframe.
# It seems when I extracted the match it stored it as a datafrane even though it was a list, which made
# accessing it a pain, so I decided to strip it down to a vector first
win_loss <- standings[match(teams$abb, standings$team), "win_loss_percentage"]
win_loss <- as.vector(win_loss$win_loss_percentage)

model <- tibble(team=teams$abb,
                run_diff=0,
                win_loss_percentage=win_loss
                )

# I realize that this is a very inefficient algorithm, but for my purpose of only analyzing less 
# than 400 x 30 observations, I deemed it okay. I'm sure there is a way to leverage the vectorization
# of R to do this much more efficiently.
for (x in 1:length(run_differential$date)) {
  for (t in 1: length(model$team)) {
    if (model$team[t] == run_differential$away[x]) {
      model$run_diff[t] <- model$run_diff[t] + run_differential$v_run_diff[x]
    }
    if (model$team[t] == run_differential$home[x]) {
      model$run_diff[t] <- model$run_diff[t] + run_differential$h_run_diff[x]
    }
  }
}


## -----------------------------------------------------------------------------
set.seed(1) # set the seed to yield consistent values in the data partition
selection <- createDataPartition(model$win_loss_percentage, p=0.75, list=FALSE)

train <- model[selection, ]
test <- model[-selection, ]


train_model <- lm(data = train, formula=win_loss_percentage~run_diff)

predictions <- train_model %>% 
  predict(test)


## -----------------------------------------------------------------------------
summary(train_model)
summary(predictions)


## -----------------------------------------------------------------------------
ggplot(test, aes(x=predictions, y=win_loss_percentage)) + geom_point() + labs(x="Predicted Win-Loss Percentage",y="Actual Win-Loss Percentage",title="Model Predictions vs. Actual Values") + geom_abline()


## -----------------------------------------------------------------------------
homeruns <- tibble(date=game_data$date,
                   home=game_data$home,
                   away=game_data$away,
                   h_homeruns=game_data$h_homeruns,
                   v_homeruns=game_data$v_homeruns)


wins <- standings[match(teams$abb, standings$team), "wins"]
wins <- as.vector(wins$wins)

homeruns_model <- tibble(team=teams$abb,
                homeruns=0,
                wins=wins
                )

for (x in 1:length(homeruns$date)) {
  for (t in 1: length(homeruns_model$team)) {
    if (homeruns_model$team[t] == homeruns$away[x]) {
      homeruns_model$homeruns[t] <- homeruns_model$homeruns[t] + homeruns$v_homeruns[x]
    }
    if (homeruns_model$team[t] == homeruns$home[x]) {
      homeruns_model$homeruns[t] <- homeruns_model$homeruns[t] + homeruns$v_homeruns[x]
    }
  }
}


## -----------------------------------------------------------------------------
set.seed(1) # set the seed to yield consistent values in the data partition
h_model_selection <- createDataPartition(homeruns_model$wins, p=0.75, list = FALSE)

h_train <- homeruns_model[h_model_selection, ]
h_test <- homeruns_model[-h_model_selection, ]


h_train_model <- lm(data = h_train, formula=wins~homeruns)

h_predictions <- h_train_model %>% 
  predict(h_test)


## -----------------------------------------------------------------------------
summary(h_train_model)
summary(h_predictions)


## -----------------------------------------------------------------------------
ggplot(h_test, aes(x=h_predictions, y=wins)) + geom_point() + labs(x="Predicted Wins",y="Actual Wins",title="Model Predictions vs. Actual Values") + geom_abline()


## ---- render=lemon_print------------------------------------------------------
team_h_max <- homeruns_model[match(max(homeruns_model$homeruns), homeruns_model$homeruns), "team"]

standings[match(team_h_max, standings$team), c("rank", "team", "wins")]

