### Follow this script at the beginning of a new season or at any point in time
### during the regular season to update rosters or players skills

##### ACTIVATE WRITERS ?? ------------------------------
activeWriters <- FALSE

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
if (substr(Sys.Date(),6,7) > '08'){ # in case we're on the 2nd year of a season
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
if (activeWriters) write_playersHist(fromYear = thisYear-seasonOffset-1, toYear = thisYear-seasonOffset)
playersHist <- read.csv("data/playersHist.csv", stringsAsFactors = FALSE) # from write_playersHist.R
# differentiate players with the same name
playersHist <- .rename_PlayerDuplicates(playersHist)
#
# update teams historical stats
# returns: teamStats.csv
if (activeWriters) write_teamStats(what_season = thisYear)
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
if (activeWriters) write_tsneBlocks()
# read the tsne blocks
tsneBlock <- list()
for (a in 18:41){
  tsneBlock[[a]] <- read.csv(paste0("data/tsneBlock","_",a,".csv"))
}
#
# Actual Season schedule
if (activeWriters) write_realSeasonSchedule()
realSeasonSchedule <- read.csv("data/realSeasonSchedule.csv",stringsAsFactors = FALSE) # from write_seasonSchedule.R
datesRange <- unique(realSeasonSchedule$Date)
# filter by the exact dates
#datesRange <- datesRange[which(datesRange>"2018-10-12")]
#
# Offense and Defense models:
library(rlist) # write list as file
### Offense: Compute a linear regression model using all scorebox game totals since 1985:
##### Step 1: Include only field goal and free throw percentages (efficiency) and AST, ORB, TOV and PF
##### Step 2: Compute points per minute: 3*3PM + 2*2PM + FTM
##### Step 3: Offense power = (2*points per minute + Efficient Offense)/3
### Defense: Compute a linear regression model using all scorebox game totals since 1985:
##### Step 1: Include only DRB, ORB, TOV, STL, BLK and PF
#
if (activeWriters) {
  load("data/box_scores.Rdata") # load historical box scores
  write_update_boxscores(latestSeason = max(box_scores_all$Year)) #write new box scores
  ### 1. Prepare the data: 
  box_scores <- read.csv("data/box_scores.csv", stringsAsFactors = FALSE) %>% # read new box_scores
    mutate_at(vars(-matches("Player|Starter|Tm")), as.numeric)
  box_scores_all <- bind_rows(box_scores_all,box_scores) %>%
    mutate_at(vars(-matches("Player|Starter|Tm")), as.numeric)
  #save(box_scores_all, file = paste0("data/box_scores.Rdata"))
  
  box_scores_totals <- filter(box_scores_all, grepl("Total",Player) | grepl("Total",Starters)) %>%
    filter(Year >= 1985) # data before 85 has holes
  box_scores_totals <- cbind(box_scores_totals,homeOrAway = rep(c('H','A'),nrow(box_scores_totals)/2))
  # Try simple regression model
  dataToModel <- box_scores_totals %>%
    #group_by(box_scores_totals,Year,Month,Day) %>%
    mutate(leadPTS = lead(PTS),lagPTS = lag(PTS), leadTm = lead(Tm), lagTm = lag(Tm)) %>%
    mutate(PTSA = ifelse(homeOrAway=="H",leadPTS,lagPTS))
  
  dataToModel$leadTm[nrow(dataToModel)] <- dataToModel$lagTm[nrow(dataToModel)]
  
  dataToModel <- mutate(dataToModel, TmA_Date_TmB = paste0(Tm,"_",Month,Day,Year,leadTm)) %>%
    mutate(X2P = FG-X3P, X2PA = FGA-X3PA) %>%
    select(TmA_Date_TmB,Year,MP,X2P,X2PA,X3P,X3PA,FT,FTA,ORB,DRB,AST,STL,BLK,TOV,PF,PTS,PTSA) %>%
    #select(TmA_Date_TmB,Year,MP,X2PA,X3PA,FTA,ORB,DRB,AST,STL,BLK,TOV,PF,PTS,PTSA) %>%
    distinct(TmA_Date_TmB,.keep_all = TRUE)
  
  dataToModel2 <- dataToModel %>%
    filter(Year >= 1985) %>%
    select(-Year) %>%
    group_by(TmA_Date_TmB,MP) %>%
    gather(skill,value, -c(TmA_Date_TmB,MP,PTS,PTSA)) %>%
    mutate(value = value/MP) %>%
    spread(skill,value) %>%
    mutate(PTS = PTS*240/MP,PTSA = PTSA*240/MP,score_diff = PTS-PTSA) %>%
    ungroup() %>%
    as.data.frame()
  
  ### 2. Model Efficient Offense linear regression
  library(caret)
  dataToModel3 <- select(dataToModel2, -MP, -score_diff, -PTSA)
  dataToModel3 <- mutate(dataToModel3, FG2Per = ifelse(X2PA==0,0,X2P/X2PA),
                         FG3Per = ifelse(X3PA==0,0,X3P/X3PA), FTPer = ifelse(FTA==0,0,FT/FTA),
                         FG2A = X2PA, FG3A = X3PA)
  #dataToModel3 <- select(dataToModel3, -BLK,-DRB,-FT,-FTA,-STL,-starts_with("X"))
  dataToModel3 <- select(dataToModel3, -FT,-starts_with("X"))
  
  set.seed(998)
  perc <- 0.8
  train_split <- round(perc*nrow(dataToModel3))
  
  teams_train <- sample(dataToModel3$TmA_Date_TmB,train_split)
  teams_test <- filter(dataToModel3, !(TmA_Date_TmB %in% teams_train))$TmA_Date_TmB
  training <- filter(dataToModel3, TmA_Date_TmB %in% teams_train)
  testing <- filter(dataToModel3, TmA_Date_TmB %in% teams_test)
  
  # remove non-numeric variables
  train_teamSeasonCodes <- training$TmA_Date_TmB
  test_teamSeasonCodes <- testing$TmA_Date_TmB
  training <- training[,-1]
  testing <- testing[,-1]
  
  fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10,
    ## repeated ten times
    repeats = 10)
  
  set.seed(825)
  glmFit <- train(PTS ~ ., data = training,
                  method = "glm",
                  trControl = fitControl)
  
  model <- glmFit # pick the model
  # save model
  list.save(model, "data/model_Offense_2020.Rdata")
  #
  ### 3. Model Defense linear regression
  
  dataToModel3 <- select(dataToModel2, -MP, -PTS, -score_diff)
  dataToModel3 <- mutate(dataToModel3, FG2Per = ifelse(X2PA==0,0,X2P/X2PA),
                         FG3Per = ifelse(X3PA==0,0,X3P/X3PA), FTPer = ifelse(FTA==0,0,FT/FTA),
                         FG2A = X2PA, FG3A = X3PA)
  dataToModel3 <- select(dataToModel3, -FT,-starts_with("X"))
  
  set.seed(998)
  perc <- 0.8
  train_split <- round(perc*nrow(dataToModel3))
  
  teams_train <- sample(dataToModel3$TmA_Date_TmB,train_split)
  teams_test <- filter(dataToModel3, !(TmA_Date_TmB %in% teams_train))$TmA_Date_TmB
  training <- filter(dataToModel3, TmA_Date_TmB %in% teams_train)
  testing <- filter(dataToModel3, TmA_Date_TmB %in% teams_test)
  
  # remove non-numeric variables
  train_teamSeasonCodes <- training$TmA_Date_TmB
  test_teamSeasonCodes <- testing$TmA_Date_TmB
  training <- training[,-1]
  testing <- testing[,-1]
  
  fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10,
    ## repeated ten times
    repeats = 10)
  
  # Linear Regression
  set.seed(825)
  glmFit <- train(PTSA ~ ., data = training,
                  method = "glm",
                  trControl = fitControl)
  
  model <- glmFit # pick the model
  # save model
  list.save(model, "data/model_Defense_2020.Rdata")
}
# load models
load("data/model_Offense_2020.Rdata")
nn_Offense <- x$finalModel
load("data/model_Defense_2020.Rdata")
nn_Defense <- x$finalModel
#
# Adjusting players skills
# Returning NBA players (played at least 1 minute in the NBA ever)
if (activeWriters) write_playersNewPredicted() # THIS IS WHERE ALL THE MAGIC HAPPENS, MOSTLY IN .predictPlayerWeighted() helper function
playersNewPredicted <- read.csv("data/playersNewPredicted_2020.csv", stringsAsFactors = FALSE) %>% # from .computePredictedPlayerStats() in write_teams_predicted_stats_new_season.R
  distinct(Player, .keep_all=TRUE) %>% select(-c(Pos,Season,Age))
# make sure no shooting percentages are > 1
playersNewPredicted <- mutate_at(playersNewPredicted, vars(contains("Per")), function(x) ifelse(x >=1, quantile(x,.99), x))
# make sure percentages are correct (top10_var computes variation for all absolute and percentage so percentages may not be correct)
playersNewPredicted <- group_by(playersNewPredicted, Player) %>%
  mutate(FGPer = effFG/effFGA, FG3Per = eff3PM/eff3PA, FG2Per = eff2PM/eff2PA,
         effFGPer = (effFG + .5*eff3PM)/effFGA, FTPer = effFTM/effFTA,
         effTRB = effDRB + effORB, effPTS = effFTM + 2*eff2PM + 3*eff3PM) %>%
  mutate_all(function(x) ifelse(is.na(x),0,x)) %>%
  ungroup()

playersNewPredicted_Final <- group_by(playersNewPredicted, Player) %>%
  mutate(Exp = .getPlayerExperience(Player),
         Age = .getPlayerAge(Player),
         Pos = .getPlayerPosition(Player),
         Season = paste0(as.numeric(thisYear)-seasonOffset,"-",as.numeric(thisYear)-seasonOffset+1)) %>%
  select(Player, Pos, Season, Age, Tm, Exp, everything())
#
################ Compute powers #################
#
### Make sure no percentages are off (FT%, FG%, etc)
playersNewPredicted_Final_adjMin <- ungroup(playersNewPredicted_Final) %>%
  mutate_at(vars(contains("Per")), function(x) ifelse(x >=.95, quantile(x,.95), x))

###
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
# Make sure there are no negative udage rate
playersNewPredicted_Final_adjMin2 <- mutate(playersNewPredicted_Final_adjMin2, effMin = ifelse(effMin < 0, .000001, effMin))
# The reason to go top_1 = 1.1 is to give more prominence to star players which adjust better when simulating
# wins in the regular season

### 4. compute team powers ---------------------------
# See teams_power.R for details. See if actual effMin matter (double check weighted means)
# playersNewPredicted_pumped <- mutate(playersNewPredicted_Final_adjMin2, effMin = ifelse(Tm == "UTA",effMin - .002,effMin))
# teamsPredicted_pumped <- .teamsPredictedPower(data = playersNewPredicted_pumped,actualOrPred="predicted")
# confrmed: effMin volume matters, I will transform in percentages
playersNewPredicted_Final_adjMinPer <- group_by(playersNewPredicted_Final_adjMin2, Tm) %>%
  mutate(effMin = effMin/sum(effMin,na.rm=TRUE)) %>%
  distinct(Player, .keep_all = TRUE) %>%
  as.data.frame()
write.csv(playersNewPredicted_Final_adjMinPer, "data/playersNewPredicted_Final_adjPer.csv", row.names = FALSE)
#
###########################################################################################
####### THE 2 FILES THAT ULTIMATELY CONTAIN ALL THE INFORMATION:
## playersNewPredicted_Final_adjMin
## playersNewPredicted_Final_adjMinPer
playersPredictedStats_adjMin <- read.csv("data/playersNewPredicted_Final_adjMin.csv",stringsAsFactors = FALSE)
playersPredictedStats_adjPer <- read.csv("data/playersNewPredicted_Final_adjPer.csv",stringsAsFactors = FALSE)
###########################################################################################
#
# Calculate tsne files for later use in the app. SOme may take forever. Check this
if (activeWriters) {
    data_tsne <- .tSNE_prepare_All() # for tSNE visualization from similarityFunctions.R
    write_tsne_points_All()
    write_tsne_ready_hist()
    write_tsne_points_newSeason()
    write_tsne_ready_newSeason()
    write_tsne_ready_teams()
    # get game scores from past 7 seasons (since 2009-2010)
    write_gameScores()
}
###########################################################################################
# Caching some files to speed up application start-up (folder: /cache_global/)
# Basically all this would go inside the global.R file
#
# compute teams and players Offense and Defense
#
### Offense
load("data/model_Offense_2020.Rdata")
model <- x
#
predict_data_Off <- playersPredictedStats_adjPer %>%
  #filter(playersPredictedStats_adjPer, grepl("lebron|durant|westbro|curry|anthony dav|giann", tolower(Player))) %>%
  select(-contains("Per"),-Pos,-Season,-Age,-Tm) %>%
  rename_all(funs(gsub("eff","",.))) %>%
  select(-Min,-FG,-FGA,-TRB,-PTS) %>%
  rename_all(funs(gsub("2","X2",.))) %>%
  rename_all(funs(gsub("3","X3",.))) %>%
  rename_all(funs(gsub("M","",.))) %>%
  mutate(FG2Per = ifelse(X2PA==0,0,X2P/X2PA),
         FG3Per = ifelse(X3PA==0,0,X3P/X3PA),
         FTPer = ifelse(FTA==0,0,FT/FTA),FG2A = X2PA, FG3A = X3PA) %>%
  select(-FT,-starts_with("X"))

efficient_Off <- predict(model, newdata = predict_data_Off)

points_data_Off <- playersPredictedStats_adjPer %>%
  mutate(points = 40*5*(effFTM + 2*eff2PM + 3*eff3PM)) %>%
  select(Player, points)

predicted_Offense <- data.frame(Player = predict_data_Off$Player,efficient_Off = as.numeric(efficient_Off))
predicted_Offense <- merge(predicted_Offense,points_data_Off, by = "Player") %>%
  mutate(Offense = (2*points + efficient_Off)/3)
#
### Defense
load("data/model_Defense_2020.Rdata")
model <- x
#
predict_data_Def <- playersPredictedStats_adjPer %>%
  #filter(playersPredictedStats_adjPer, grepl("lebron|durant|westbro|curry|anthony dav|giann", tolower(Player))) %>%
  select(-contains("Per"),-Pos,-Season,-Age,-Tm) %>%
  rename_all(funs(gsub("eff","",.))) %>%
  select(-Min,-FG,-FGA,-TRB,-PTS) %>%
  rename_all(funs(gsub("2","X2",.))) %>%
  rename_all(funs(gsub("3","X3",.))) %>%
  rename_all(funs(gsub("M","",.))) %>%
  mutate(FG2Per = ifelse(X2PA==0,0,X2P/X2PA),
         FG3Per = ifelse(X3PA==0,0,X3P/X3PA),
         FTPer = ifelse(FTA==0,0,FT/FTA),FG2A = X2PA, FG3A = X3PA) %>%
  select(-FT,-starts_with("X"))

predicted_Def <- predict(model, newdata = predict_data_Def)
predicted_Defense <- data.frame(Player = predict_data_Def$Player,Defense = as.numeric(predicted_Def))

### Offense and Defense together
predicted_Off_Def <- merge(predicted_Offense, predicted_Defense, by = "Player") %>%
  select(Player, Offense, Defense)

write.csv(predicted_Off_Def,"cache_global/playersPredicted_Off_Def.csv", row.names=FALSE)
##
##
playersPredicted2 <- merge(predicted_Off_Def,playersPredictedStats_adjMin[,c("Player","Exp","Age","Tm","effMin")], by = "Player") %>%
  mutate(plusMinus = Offense - Defense,adjPlusMinus = plusMinus*effMin*100) %>%
  group_by(Tm) %>%
  mutate(teamPlusMinus = sum(adjPlusMinus,na.rm=TRUE)) %>%
  ungroup()

# Average player
avg_Exp <- mean(as.numeric(gsub("R","0",playersPredictedStats_adjMin$Exp)), na.rm=TRUE)
averagePlayer <- .calculate_AvgPlayer(playersPredictedStats_adjMin) %>% mutate(Exp = as.character(floor(avg_Exp))) %>%
  select(Player, Pos, Season, Age, Tm, Exp, everything()) %>%
  mutate(Age = round(Age,0)) %>%
  as.data.frame()
write.csv(averagePlayer, "cache_global/averagePlayer.csv", row.names=FALSE)
#
playerDashboard <- merge(playersPredictedStats_adjPer,select(playersPredicted2, -c(Age,Tm,effMin)), by = "Player")
playerRanks <- mutate_if(playerDashboard, is.numeric, function(x) row_number(desc(x)))
write.csv(playerDashboard, "cache_global/playerDashboard.csv", row.names=FALSE)
write.csv(playerRanks, "cache_global/playerRanks.csv", row.names=FALSE)
#
# Regular Season schedule
conferences <- read.csv("data/nba_conferences.csv", stringsAsFactors = FALSE) # Same as franchises
realSeasonSchedule <- read.csv("data/realSeasonSchedule.csv",stringsAsFactors = FALSE)
gameScores <- read.csv("data/gameScores.csv", stringsAsFactors = FALSE)
datesRange <- unique(realSeasonSchedule$Date)
sigma <- sd(c(as.numeric(gameScores$pts_home),as.numeric(gameScores$pts_away)), na.rm = TRUE)
avgHome <- mean(as.numeric(gameScores$pts_home), na.rm = TRUE)
avgAway <- mean(as.numeric(gameScores$pts_away), na.rm = TRUE)
global_mean <- mean(c(as.numeric(gameScores$pts_home),as.numeric(gameScores$pts_away)), na.rm = TRUE)
home_away_factor <- avgHome - avgAway # how many extra points does a team score on average when playing home
# Teams Predicted powers and wins
source("code_chunks/source_predictedPowersWins_2018.R",local=TRUE)
write.csv(teamsPredicted, "cache_global/teamsPredicted.csv", row.names=FALSE)
write.csv(win_predictions, "cache_global/win_predictions.csv", row.names=FALSE)
write.csv(teamDashboard, "cache_global/teamDashboard.csv", row.names=FALSE)
write.csv(teamRanks, "cache_global/teamRanks.csv", row.names=FALSE)
write.csv(teamStats, "cache_global/teamStats.csv", row.names=FALSE)
#
regSeasonOutcome <- .standings(real=TRUE)
standings <- regSeasonOutcome[[1]]
games <- regSeasonOutcome[[2]]
write.csv(games, "cache_global/games.csv", row.names = FALSE)
list.save(standings, "cache_global/standings.rds")

