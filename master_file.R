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

write_playersNewPredicted()



