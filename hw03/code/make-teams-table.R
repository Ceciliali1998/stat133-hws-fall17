# ===================================================================
# Title: make-teams-table
# Description:
#   This script contains the R code to complete the data preparation 
#   stage that will contain the required variables to be used in 
#   the ranking analysis.
# Input(s): data file 'nba2017-stats.csv
# Output(s): data file 'nba2017-teams.csv'
# Author: Cecilia Li
# Date: 10-14-2017
# ===================================================================

# Raw Data and Dictionaries 

roster <- "https://raw.githubusercontent.com/ucb-stat133/stat133-fall-2017/master/data/nba2017-roster.csv"
destination <- "nba2017-roster.csv"
download.file(url=roster,destfile = destination)
roster <- read.csv("nba2017-roster.csv",stringsAsFactors=FALSE)

stats <- "https://raw.githubusercontent.com/ucb-stat133/stat133-fall-2017/master/data/nba2017-stats.csv"
download.file(url=stats,destfile = "nba2017-stats.csv")
stats <- read.csv("nba2017-stats.csv",stringsAsFactors = FALSE)

write.csv(stats, "/Users/cecilia1998/stat133/stat133-hws-fall17/hw03/data/nba2017-stats.csv", row.names=FALSE)
write.csv(stats, "/Users/cecilia1998/stat133/stat133-hws-fall17/hw03/data/nba2017-roster.csv", row.names=FALSE)

library(readr)
library(dplyr)
library(ggplot2)

# Adding new variables
stat1 <-
  mutate(
    stats,
    missed_fg = stats$field_goals_atts - stats$field_goals_made,
    missed_ft = stats$points1_atts - stats$points1_made,
    points = stats$points1_made + 2 * stats$points2_made + 3 * stats$points3_made,
    rebounds = stats$off_rebounds + stats$def_rebounds,
  )
stat1

stat2 <-
  mutate(
    stat1,
    efficiency = (
      stat1$points + stat1$rebounds + stat1$assists +
        stat1$steals + stat1$blocks - stat1$missed_fg -
        stat1$missed_ft - stat1$turnovers
    ) / stat1$games_played
  )
stat2

sink(file = './output/efficiency-summary.txt')
summary(stat2, 'efficiency')
sink()

# Merging tables
data <- merge(stat2, roster)
data

# Creating nba2017-teams.csv

teams <-
  data %>% group_by(team) %>% summarise(
    experience = round(sum(experience), 2),
    salary = sum(round(salary /
                         1000000, 2)),
    points3 = sum(points3_made),
    points2 = sum(points2_made),
    free_throws = sum(points1_made),
    points = sum(point3 + point2 + free_throws),
    off_rebounds = sum(off_rebounds),
    def_rebounds = sum(def_rebounds),
    assists = sum(assists),
    steals = sum(steals),
    blocks = sum(blocks),
    turnovers = sum(turnovers),
    fouls = sum(fouls),
    efficiency = sum(efficiency)
  )
teams
summary(teams)

sink(file = './data/teams-summary.txt')
summary(teams)
sink()

write.csv(summary(teams),'./data/teams-summary.csv', row.names=FALSE )

# Some graphics

pdf(file = './image/teams_star_plot.pdf')
stars(teams[, -1], labels = teams$team)
dev.off()

pdf(file = "experience_salary.pdf")
ggplot(data=teams, aes(x=experience, y=salary)) + geom_point(aes(color=team)) + geom_text(aes(label=team))
dev.off()

