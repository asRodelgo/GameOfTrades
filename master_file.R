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
library(httr)
library(rvest)

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
#
#
################ Compute powers ################# 
#
#
# write college players History
write_collegePlayersHist(col_G = 15,num_pages = 30,firstDraft = 1994,lastDraft=as.numeric(thisYear))
collegePlayersHist <- read.csv("data/collegePlayersHist.csv", stringsAsFactors = FALSE)
rookiesDraftHist <- read.csv("data/rookiesDraftHist.csv", stringsAsFactors = FALSE)
#
# There's a bunch of calculations that I use to estimate certain parameters. 
# They don't need to be run everytime this script is run, just once every year
# compute % minutes played per player:
collegeMinutes <- merge(collegePlayersHist,rookiesDraftHist[,c("Player","Pick","Year")], by = "Player") %>%
  mutate(perMin = MP/(G*40)) %>%
  group_by(Player) %>%
  mutate(perMin = mean(perMin, na.rm=TRUE)) %>% # average minutes played per game in ther college years
  distinct(Player, .keep_all=TRUE) %>%
  filter(!is.na(perMin))
# now do the same for NBA players by experience year
nbaMinutes <- select(playersHist, Player,Season,Age,Tm,G,MP) %>%
  filter(!(Tm == "TOT")) %>%
  group_by(Player) %>%
  filter(Age == min(Age)) %>% # keep players when they were younger (rookie year)
  group_by(Player,Season) %>%
  filter(G >= 30) %>% # played at least 30 games
  mutate(perMin = mean(MP, na.rm=TRUE)/48) %>%
  distinct(Player, .keep_all=TRUE)
# put them together
college2nbaMinutes <- merge(nbaMinutes,collegeMinutes, by = "Player", all.x = TRUE) %>%
  mutate(minDiff = perMin.y-perMin.x) %>%
  filter(!is.na(minDiff))
# Although Rank is different from Draft pick. Let's see this by draft pick for the top 30 picks:
plot(college2nbaMinutes$Pick,college2nbaMinutes$minDiff)
draftMinutesInNBA <- arrange(college2nbaMinutes, desc(Pick)) %>%
  group_by(Pick) %>%
  summarise(mean(minDiff)) %>%
  mutate(Pick = as.numeric(Pick)) %>%
  arrange(Pick)
barplot(draftMinutesInNBA$`mean(minDiff)`)
#
# add Pick round to rookie players:
rookiesDraft <- filter(rookiesDraftHist, Year >= as.numeric(thisYear)-2) # get last 3 drafts to include rookies like Ben Simmons who didn't play any minute last season
playersNewPredicted_Final <- merge(playersNewPredicted_Final, rookiesDraft[,c("Player","Pick")], by="Player",all.x=TRUE)
# For those not in the draft (international players or non-drafted players), average by the
# average percentage for the tail of the 2nd round picks (51-60)
col2nbaMinDiff <- mean(draftMinutesInNBA$`mean(minDiff)`[51:nrow(draftMinutesInNBA)]) 
playersNewPredicted_Final_adjMin <- mutate(playersNewPredicted_Final,Exp = ifelse(is.na(Exp),"1",Exp),
                                           Pick = ifelse(is.na(Pick),0,as.numeric(Pick))) %>% # in case Exp is NA for instance returning NBA players
  group_by(Player) %>%
  mutate(effMin = ifelse(Pick > 0 & Exp == "R",effMin*(1-draftMinutesInNBA$`mean(minDiff)`[Pick]),
                         ifelse(Exp == "R",effMin*(1-col2nbaMinDiff),effMin)))

write.csv(playersNewPredicted_Final_adjMin, "data/playersNewPredicted_Final_adjMin.csv", row.names = FALSE)
# 3. adjust percent of play time -----------------------------------
# Based on historical data for the last 5 seasons:
topMinShare <- .minutes_density(playersHist,5)
averageShare <- group_by(topMinShare,Season) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  summarise_if(is.numeric, mean)
incrementShare <- gather(averageShare,top_n,percent)
plot(seq(18,1,-1),incrementShare$percent)
# top7 seems to be the turning point at 60%, after it, the scale of time every new player adds goes down (slope). 
# I will use this as estimate. Although the trend is going down, in last 5 seasons, % is 59%
# because rosters are getting bigger thus utilize more players. Usually top 1 % revolves around 10% 
playersNewPredicted_Final_adjMin2 <- .redistributeMinutes(playersNewPredicted_Final_adjMin, topHeavy = 7, topMinShare = .6, min_share_top1 = .105)
# The reason to go top_1 = 1.1 is to give more prominence to star players which adjust better when simulating
# wins in the regular season
# make sure Season column shows the new season to come and remove Pick column as i don't need it anymore
playersNewPredicted_Final_adjMin2 <- mutate(playersNewPredicted_Final_adjMin2, 
                                            Season = paste0(thisYear,"-",as.numeric(thisYear)+1)) %>%
  select(-Pick, -Exp) %>%
  as.data.frame()
# 4. compute team powers ---------------------------
# See teams_power.R for details. See if actual effMin matter (double check weighted means)
# playersNewPredicted_pumped <- mutate(playersNewPredicted_Final_adjMin2, effMin = ifelse(Tm == "UTA",effMin - .002,effMin))
# teamsPredicted_pumped <- .teamsPredictedPower(data = playersNewPredicted_pumped,actualOrPred="predicted")
# confrmed: effMin volume matters, I will transform in percentages
playersNewPredicted_Final_adjMinPer <- group_by(playersNewPredicted_Final_adjMin2, Tm) %>%
  mutate(effMin = effMin/sum(effMin,na.rm=TRUE)) %>%
  as.data.frame()
write.csv(playersNewPredicted_Final_adjMinPer, "data/playersNewPredicted_Final_adjPer.csv", row.names = FALSE)
#
###########################################################################################
####### THE 2 FILES THAT ULTIMATELY CONTAIN ALL THE INFORMATION:
## playersNewPredicted_Final_adjMin
## playersNewPredicted_Final_adjMinPer
###########################################################################################






