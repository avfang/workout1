rm(list=ls())
library(pacman)
library(readr)
library(ggplot2)
library(dplyr)
library(knitr)

# Data Preparation
# Description: Preparing 2018 NBA team data from 2018 NBA individual data
# inputs: nba2018.csv
# outputs: nba2018-teams.csv, efficiency-summary-txt, teams-summary.txt

#load data
df <- read.csv("../data/nba2018.csv", stringsAsFactors = FALSE)

#"experience" R needs to be 0, then convert to integers
df$experience <- ifelse(df$experience=="R", 0, df$experience) %>% as.integer()
#"salary" needs to be in millions (currently in dollars)
df$salary <- df$salary/1000000
#"position" needs to be factor with 5 levels 
df$position <- factor(df$position)
levels(df$position) <- list(
  center="C", power_fwd="PF", point_guard="PG", small_fwd="SF", shoot_guard="SG")

#add missed field goals, missed free throws, total rebounds (already in data set), efficiency 
df <- mutate(df, 
             missed_fg=field_goals_atts-field_goals,
             missed_ft=points1_atts-points1,
             efficiency=(points+total_rebounds+assists+steals+blocks-missed_fg-missed_ft-turnovers)/games)

#sink summary of efficiency to a text file ("efficiency-summary.txt")
summary(df$efficiency)

#create team summary then sink
df1 <- summarize(group_by(df,team),
          experience=round(sum(experience),2),
          salary=round(sum(salary),2),
          points3=sum(points3),
          points2=sum(points2),
          points1=sum(points1),
          points=sum(points),
          off_rebounds=sum(off_rebounds),
          def_rebounds=sum(def_rebounds),
          assists=sum(assists),
          steals=sum(steals),
          blocks=sum(blocks),
          turnovers=sum(turnovers),
          fouls=sum(fouls),
          efficiency=sum(efficiency))

#also write csv
write.csv(df1, "../data/nba2018-teams.csv")
