#### Global cached ------------------------
# Here is where all the data and functions gets loaded at the start of a session
# Cached to speed up start up. global.R contains the original calls
# Alberto Sanchez Rodelgo
#### --------------------------------
library(flexdashboard)
library(tidyverse)
library(DT)
library(shinyBS)
library(shinyjs)
library(caret)
library(neuralnet) # neural network for regression
library(rlist) # write list as file
#library(shinydashboard)

# variables set up
thisYear <- substr(Sys.Date(),1,4)
if (substr(Sys.Date(),6,7) > '08'){
  seasonOffset <- 0
} else {
  seasonOffset <- 1
}
thisSeason <- paste0(as.numeric(substr(Sys.Date(),1,4))-seasonOffset,"-",as.numeric(substr(Sys.Date(),1,4))-seasonOffset+1) 

# helper functions
source("helpers.R", local=TRUE)$value
source("shiny_server_helpers.R", local=TRUE)$value
# read data
team_stats <- read.csv("data/teamStats.csv") # from write_TeamStats.R
franchises <- read.csv("data/franchisesHistory.csv",stringsAsFactors = FALSE) 
team_stats <- merge(team_stats,franchises,by.x="Team",by.y="Franchise",all.x=TRUE)
#
playersHist <- read.csv("data/playersHist.csv", stringsAsFactors = FALSE) # from write_playersHist.R
playersHist <- .rename_PlayerDuplicates(playersHist) # differentiate players with the same name

playersPredictedStats_adjMin <- read.csv("data/playersNewPredicted_Final_adjMin.csv", stringsAsFactors = FALSE) %>%
  distinct(Player, .keep_all = TRUE)
playersPredictedStats_adjPer <- read.csv("data/playersNewPredicted_Final_adjPer.csv", stringsAsFactors = FALSE) %>%
  distinct(Player, .keep_all = TRUE)

# load neuralnet models
load("data/modelNeuralnet19_PTS.Rdata")
nn_Offense <- model$finalModel
load("data/modelNeuralnet19_PTSA.Rdata")
nn_Defense <- model$finalModel
# load limits for scaled data. Each trade will trigger a predict() from the selected NNet model
# But scale limits must be kept as originally trained in the model for consistency
team_stats_Off <- read.csv("cache_global/team_stats_Off.csv", stringsAsFactors = FALSE)
team_stats_Def <- read.csv("cache_global/team_stats_Def.csv", stringsAsFactors = FALSE)
scaleMaxMin_Off <- read.csv("cache_global/scaleMaxMin_Off.csv", stringsAsFactors = FALSE)
scaleMaxMin_Def <- read.csv("cache_global/scaleMaxMin_Def.csv", stringsAsFactors = FALSE)
maxs_Off <- scaleMaxMin_Off$maxs
mins_Off <- scaleMaxMin_Off$mins
maxs_Def <- scaleMaxMin_Def$maxs
mins_Def <- scaleMaxMin_Def$mins
maxs_vector_input <- cbind(maxs_Off,maxs_Def)
mins_vector_input <- cbind(mins_Off,mins_Def)

# compute teams and players Offense and Defense
source("code_chunks/source_computeOffenseDefense.R",local=TRUE)
playersNewPredicted_Final_adjMinPer2 <- select(playersPredictedStats_adjPer, -contains("Per"), -effFG, -effFGA, -effPTS, -effTRB)
playersNewPredicted_OffDef <- mutate(playersNewPredicted_Final_adjMinPer2, Tm = Player, effMin = 1)
playersPredicted <- read.csv("cache_global/playersPredicted.csv", stringsAsFactors = FALSE)
playersPredicted2 <- merge(playersPredicted,playersPredictedStats_adjMin[,c("Player","Exp","Age","Tm","effMin")], by = "Player") %>%
  mutate(adjPlusMinus = plusMinus*effMin*100) %>%
  group_by(Tm) %>%
  mutate(teamPlusMinus = sum(adjPlusMinus,na.rm=TRUE)) %>%
  distinct(Player, .keep_all = TRUE) %>%
  ungroup()

playerDashboard <- read.csv("cache_global/playerDashboard.csv", stringsAsFactors = FALSE) %>%
  distinct(Player, .keep_all = TRUE)
playerRanks <- read.csv("cache_global/playerRanks.csv", stringsAsFactors = FALSE) %>%
  distinct(Player, .keep_all = TRUE)
playerMax <- summarise_if(playerDashboard, is.numeric, max)
playerMin <- summarise_if(playerDashboard, is.numeric, min)

# tSNE for similiarity and historical evolution of players
tsne_ready_hist <- read.csv("data/tsne_ready_hist.csv", stringsAsFactors = FALSE)
# tSNE for predicted stats in new season
tsne_ready <- read.csv("data/tsne_ready_newSeason.csv", stringsAsFactors = FALSE)
# intialize lists historical t-SNE
teams_list <- sort(unique(tsne_ready_hist$Tm))
ages_list <- sort(unique(tsne_ready_hist$Age))
seasons_list <- sort(unique(tsne_ready_hist$Season))
players_list <- sort(unique(tsne_ready_hist$Player))
skills_list <- names(tsne_ready_hist)[6:(ncol(tsne_ready_hist)-2)]

# Average player
averagePlayer <- read.csv("cache_global/averagePlayer.csv", stringsAsFactors = FALSE)
############################
# Team loads
# Actual Season schedule
realSeasonSchedule <- read.csv("data/realSeasonSchedule.csv",stringsAsFactors = FALSE) # from write_seasonSchedule.R
datesRange <- unique(realSeasonSchedule$Date)
### Global hyperparameters for Normal distributions
# get game scores from past 7 seasons (since 2009-2010)
gameScores <- read.csv("data/gameScores.csv", stringsAsFactors = FALSE) # write_scoreDifferentials.R 
#sigmaHome <- sd(as.numeric(gameScores$pts_home), na.rm = TRUE)
#sigmaAway <- sd(as.numeric(gameScores$pts_away), na.rm = TRUE)
sigma <- sd(c(as.numeric(gameScores$pts_home),as.numeric(gameScores$pts_away)), na.rm = TRUE)
avgHome <- mean(as.numeric(gameScores$pts_home), na.rm = TRUE)
avgAway <- mean(as.numeric(gameScores$pts_away), na.rm = TRUE)
global_mean <- mean(c(as.numeric(gameScores$pts_home),as.numeric(gameScores$pts_away)), na.rm = TRUE)
home_away_factor <- avgHome - avgAway # how many extra points does a team score on average when playing home
# Teams Predicted powers and wins
teamsPredicted <- read.csv("cache_global/teamsPredicted.csv", stringsAsFactors = FALSE)
win_predictions <- read.csv("cache_global/win_predictions.csv", stringsAsFactors = FALSE)
teamDashboard <- read.csv("cache_global/teamDashboard.csv", stringsAsFactors = FALSE)
teamRanks <- read.csv("cache_global/teamRanks.csv", stringsAsFactors = FALSE)
teamStats <- read.csv("cache_global/teamStats.csv", stringsAsFactors = FALSE)
teamStatRanks <- mutate_if(teamStats, is.numeric, function(x) row_number(desc(x)))
teamMax <- summarise_if(teamStats, is.numeric, max)
# t-SNE for teams
tsne_ready_teams <- read.csv("data/tsne_ready_teams.csv", stringsAsFactors = FALSE)

##########################
# Regular Season schedule
conferences <- read.csv("data/nba_conferences.csv", stringsAsFactors = FALSE) # Same as franchises 
realSeasonSchedule <- read.csv("data/realSeasonSchedule.csv",stringsAsFactors = FALSE) # from write_seasonSchedule.R
datesRange <- unique(realSeasonSchedule$Date)
standings <- list.load("cache_global/standings.rds")
games <- read.csv("cache_global/games.csv", stringsAsFactors = FALSE)

