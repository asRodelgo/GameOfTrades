### Follow this script at the beginning of a new season or at any point in time
### during the regular season to update rosters or players skills

##### INITIAL LOADS ------------------------------------
# Load packages, global variables, functions

library(flexdashboard)
library(tidyverse)
library(DT)
library(shinyBS)
library(shinyjs)
library(caret)
library(neuralnet) # neural network for regression
library(rlist) # write list as file

# variables set up
thisYear <- substr(Sys.Date(),1,4)
if (substr(Sys.Date(),6,7) > '08'){
  seasonOffset <- 0
} else {
  seasonOffset <- 1
}
thisSeason <- paste0(as.numeric(substr(Sys.Date(),1,4))-seasonOffset,"-",as.numeric(substr(Sys.Date(),1,4))-seasonOffset+1)

# helper functions
source("helpers.R", local=TRUE)
source("shiny_server_helpers.R", local=TRUE)
source("writers.R", local=TRUE)

##### UPDATE DATASETS ----------------
# update historical player data.
# returns: playersHist.csv
write_playersHist()
playersHist <- read.csv("data/playersHist.csv", stringsAsFactors = FALSE) # from write_playersHist.R
# differentiate players with the same name
playersHist <- .rename_PlayerDuplicates(playersHist)
#
# update teams historical stats
# returns: teamStats.csv
write_teamStats(what_season = thisYear)
team_stats <- read.csv("data/teamStats.csv", stringsAsFactors = FALSE) # from write_TeamStats.R
#
# These data may not change from season to season. Only if there are name changes, expansion teams, etc
franchises <- read.csv("data/franchisesHistory.csv",stringsAsFactors = FALSE) %>%
  distinct(Franchise, .keep_all = TRUE) # Charlotte Hornets have 2 team codes for the same franchise name
team_stats <- left_join(team_stats, franchises, by = c("Team"="Franchise"))
#
# Pre-compute tsne_points for all ages to save time as these computations don't really
# depend on the player selected.
library(tsne)
write_tsneBlocks()
# read the tsne blocks
tsneBlock <- list()
for (a in 18:41){
  tsneBlock[[a]] <- read.csv(paste0("data/tsneBlock","_",a,".csv"))
}
#
# Actual Season schedule
write_realSeasonSchedule()
realSeasonSchedule <- read.csv("data/realSeasonSchedule.csv",stringsAsFactors = FALSE) # from write_seasonSchedule.R
datesRange <- unique(realSeasonSchedule$Date)
#
# Save Neural Network model to be loaded at the start
library(rlist) # write list as file
### STEP 1: Test different models using .computeModel_neuralnet()
### STEP 2: Once the right model is selected plug it as parameters (split train-test percentage & hidden layers)
# to the .selectedModel() function and run:
.computeModel_neuralnet("PTS")
.computeModel_neuralnet("PTSA")

#nn_Offense <- .selectedModel("PTS", perc_train_sample = .8, hidden_layers = c(6,4,2))
#nn_Defense <- .selectedModel("PTSA", perc_train_sample = .8, hidden_layers = c(6,4,2))
# save models
#list.save(nn_Offense, "data/nn_Offense.rds")
#list.save(nn_Defense, "data/nn_Defense.rds")
#
# load neuralnet models
load("data/modelNeuralnet19_PTS.Rdata")
nn_Offense <- model$finalModel
load("data/modelNeuralnet19_PTSA.Rdata")
nn_Defense <- model$finalModel
# neuralnet default parameters:
#   neuralnet(formula, data, hidden = 1, threshold = 0.01,
#             stepmax = 1e+05, rep = 1, startweights = NULL,
#             learningrate.limit = NULL,
#             learningrate.factor = list(minus = 0.5, plus = 1.2),
#             learningrate=NULL, lifesign = "none",
#             lifesign.step = 1000, algorithm = "rprop+",
#             err.fct = "sse", act.fct = "logistic",
#             linear.output = TRUE, exclude = NULL,
#             constant.weights = NULL, likelihood = FALSE)
#
# Adjusting players skills
# Returning NBA players (played at least 1 minute in the NBA ever)
write_playersNewPredicted()
playersNewPredicted <- read.csv("data/playersNewPredicted_Sep8_18.csv", stringsAsFactors = FALSE) %>% # from .computePredictedPlayerStats() in write_teams_predicted_stats_new_season.R
  distinct(Player, .keep_all=TRUE) %>% select(-c(Pos,Season,Age))
# make sure no shooting percentages are > 1
playersNewPredicted <- mutate_at(playersNewPredicted, vars(contains("Per")), function(x) ifelse(x >=1, quantile(x,.99), x))
#
# Handle rookies
# History of drafts.
# writes: rookiesDraftHist.csv
write_rookiesDraftHist()
# write all rookies (all draft rounds and non drafted)
# writes: rookies.csv
write_rookies()
# write college players stats from last season
# writes: collegePlayers.csv
write_collegePlayers()
# read rookies and college players (to grab their stats) and merge them. There will be differences in 
# names so I have to manually change those. For now, I won't do anything manually
# If no manual changes required, simply run this:
# writes: rookieStats.csv, europePlayers.csv
write_rookieStats_europePlayers()
rookieStats <- read.csv("data/rookieStats.csv", stringsAsFactors = FALSE)
# Once per game stats have been compiled for all rookies (college and international) compute their
# per-minute stats. Writes: rookieEfficientStats.csv
write_rookieEfficientStats()
rookieEffStats <- read.csv("data/rookieEfficientStats.csv", stringsAsFactors = FALSE) %>% # write_Rookies_efficientStats in write_rookiesDraft.R
  select(-c(Season,Age)) %>% mutate(effFGPer = ifelse(effFGPer <= 0.001,FGPer,effFGPer)) # international players don't have this calculated so their per = 0
# 
# next step current_rosters
# writes: currentRosters.csv
write_currentRosters_rostersLastSeason()
current_rosters <- read.csv("data/currentRosters.csv", stringsAsFactors = FALSE) %>% 
  distinct(Player, .keep_all=TRUE)
# merge current rosters with players calculated efficient stats
playerSet_a <- merge(playersNewPredicted,current_rosters, by = "Player") %>% distinct(Player,.keep_all=TRUE)
playerSet_c <- merge(rookieEffStats,current_rosters, by = "Player") %>% distinct(Player,.keep_all=TRUE)
playerSet_a_c <- merge(playerSet_a,playerSet_c, by = "Player") # this should be empty
playerSet_aPlusc <- bind_rows(playerSet_a,playerSet_c)  %>% distinct(Player,.keep_all=TRUE)
# players whose names didn't match come from merging playerSetaPlusc and current_rosters
playerSet_b <- merge(current_rosters, playerSet_aPlusc, by = "Player", all.x = TRUE)  %>% distinct(Player,.keep_all=TRUE) %>%
  filter(is.na(Tm.y)) %>% select(Player, Age.x)

# This is the final list of unmatched players who actually played last season:
# May be more (check out playerSet_b file)
current_rosters[which(current_rosters$Player == "Gary Payton II"),]$Player <- "Gary Payton 2"
current_rosters[which(current_rosters$Player == "Glenn Robinson III"),]$Player <- "Glenn Robinson 2"
current_rosters[which(current_rosters$Player == "Kelly Oubre Jr."),]$Player <- "Kelly Oubre"
current_rosters[which(current_rosters$Player == "Larry Nance Jr."),]$Player <- "Larry Nance 2"
current_rosters[which(current_rosters$Player == "Nene"),]$Player <- "Nene Hilario"
current_rosters[which(current_rosters$Player == "Taurean Prince"),]$Player <- "Taurean Waller-Prince"
current_rosters[which(current_rosters$Player == "Tim Hardaway Jr."),]$Player <- "Tim Hardaway 2"
current_rosters[which(current_rosters$Player == "Dennis Smith Jr."),]$Player <- "Dennis Smith"
current_rosters[which(current_rosters$Player == "Derrick Jones Jr."),]$Player <- "Derrick Jones"
current_rosters[which(current_rosters$Player == "Frank Mason III"),]$Player <- "Frank Mason"
# run again sets a to c
playerSet_a <- merge(playersNewPredicted,current_rosters, by = "Player") %>% distinct(Player,.keep_all=TRUE)
playerSet_c <- merge(rookieEffStats,current_rosters, by = "Player") %>% distinct(Player,.keep_all=TRUE)
playerSet_a_c <- merge(playerSet_a,playerSet_c, by = "Player") # this should be empty
playerSet_aPlusc <- bind_rows(playerSet_a,playerSet_c)  %>% distinct(Player,.keep_all=TRUE) %>%
  select(Player,Pos,Season, Age,Tm=Tm.y,Exp,contains("Per"),contains("eff"))
# players whose names didn't match come from merging playerSetaPlusc and current_rosters
playerSet_b <- merge(current_rosters, playerSet_aPlusc, by = "Player", all.x = TRUE)  %>% distinct(Player,.keep_all=TRUE) %>%
  filter(is.na(Tm.y)) %>% select(Player, Pos = Pos.x, Age = Age.x, Tm = Tm.x, Exp = Exp.x)
# now this final set of non matched players correspond to those with a history in the NBA but didn't play in last season
# Next step is to calculate their predicted stats and add them to the final set
playerSet_Leftover <- .computePredictedPlayerStats_Leftovers(playerSet_b) # from compute_PredictedLeftovers.R
write.csv(playerSet_Leftover,"data/playerPredicted_Leftover.csv", row.names = FALSE)
playerSet_Leftover <- read.csv("data/playerPredicted_Leftover.csv", stringsAsFactors = FALSE)
playerSet_Leftover <- mutate(playerSet_Leftover, Season = current_rosters$Season[1]) %>%
  mutate_at(vars(contains("Per")), function(x) ifelse(x >= 1, mean(x, na.rm=TRUE), x)) # to avoid players with 100% shot accuracy (because they may have taken just very few shots and converted all)
# Now append together playerSetaPlusc and playerSet_Leftover for the final players stats predicted for new season
playersNewPredicted_Final <- bind_rows(playerSet_aPlusc,playerSet_Leftover)
# check this file has same rows as current_rosters
checkFinalRosters <- merge(playersNewPredicted_Final,current_rosters, by="Player", all.x = TRUE)
# Write final file:
write.csv(playersNewPredicted_Final, "data/playersNewPredicted_FINAL.csv",row.names = FALSE)





