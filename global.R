#### Global utils ------------------------
# Here is where all the data and functions gets loaded at the start of a session
#
# Alberto Sanchez Rodelgo
#### --------------------------------
library(flexdashboard)
library(tidyverse)
library(DT)
library(shinyBS)
library(shinyjs)

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

playersPredictedStats_adjMin <- read.csv("data/playersNewPredicted_Final_adjMin.csv", stringsAsFactors = FALSE)
playersPredictedStats_adjPer <- read.csv("data/playersNewPredicted_Final_adjPer.csv", stringsAsFactors = FALSE)

# load neuralnet models
load("data/modelNeuralnet5_PTS.Rdata")
nn_Offense <- model$finalModel
load("data/modelNeuralnet5_PTSA.Rdata")
nn_Defense <- model$finalModel
# load limits for scaled data. Each trade will trigger a predict() from the selected NNet model
# But scale limits must be kept as originally trained in the model for consistency
team_stats_Off <- .prepareModel("PTS")
team_stats_Def <- .prepareModel("PTSA")
scaleMaxMin_Off <- .getScaleLimits("PTS", team_stats_Off)
scaleMaxMin_Def <- .getScaleLimits("PTSA", team_stats_Def)
scaleMaxMin_Off <- scaleMaxMin_Off[!(row.names(scaleMaxMin_Off) %in%       
                                       c("FGPer","FG3Per","FG2Per","effFGPer","FTPer","effFG","effFGA","effTRB","effPTS")),]
scaleMaxMin_Def <- scaleMaxMin_Def[!(row.names(scaleMaxMin_Def) %in%       
                                       c("FGPer","FG3Per","FG2Per","effFGPer","FTPer","effFG","effFGA","effTRB","effPTS")),]
maxs_Off <- scaleMaxMin_Off$maxs
mins_Off <- scaleMaxMin_Off$mins
maxs_Def <- scaleMaxMin_Def$maxs
mins_Def <- scaleMaxMin_Def$mins
maxs_vector_input <- cbind(maxs_Off,maxs_Def)
mins_vector_input <- cbind(mins_Off,mins_Def)

# compute teams and players Offense and Defense
source("code_chunks/source_computeOffenseDefense.R",local=TRUE)
playersPredicted2 <- merge(playersPredicted,playersPredictedStats_adjMin[,c("Player","Exp","Age","Tm","effMin")], by = "Player") %>%
  mutate(adjPlusMinus = plusMinus*effMin*100) %>%
  group_by(Tm) %>%
  mutate(teamPlusMinus = sum(adjPlusMinus,na.rm=TRUE)) %>%
  ungroup()

playerDashboard <- merge(playersPredictedStats_adjPer,select(playersPredicted2, -c(Age,Tm,effMin)), by = "Player")
playerRanks <- mutate_if(playerDashboard, is.numeric, function(x) row_number(desc(x)))

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
avg_Exp <- mean(as.numeric(gsub("R","0",playersPredictedStats_adjMin$Exp)), na.rm=TRUE)
averagePlayer <- .calculate_AvgPlayer(playersPredictedStats_adjMin) %>% mutate(Exp = as.character(floor(avg_Exp))) %>%
  select(Player, Pos, Season, Age, Tm, Exp, everything()) %>%
  mutate(Age = round(Age,0)) %>%
  as.data.frame()
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
source("code_chunks/source_predictedPowersWins.R",local=TRUE)
##########################
# Regular Season schedule
conferences <- read.csv("data/nba_conferences.csv", stringsAsFactors = FALSE) # Same as franchises 
realSeasonSchedule <- read.csv("data/realSeasonSchedule.csv",stringsAsFactors = FALSE) # from write_seasonSchedule.R
datesRange <- unique(realSeasonSchedule$Date)
regSeasonOutcome <- .standings(real=TRUE)


