### Scraping box scores from individual games
library(tidyverse)
library(rvest)
library(httr)
thisSeason = substr(Sys.Date(),1,4)
box_scores <- data.frame()
thisSeason <- as.numeric(thisSeason)

franchises <- read.csv("data/franchisesHistory.csv",stringsAsFactors = FALSE) %>%
  distinct(Franchise, .keep_all = TRUE)

months <- c("01","02","03","04","05","10","11","12")
days <- c(1:31)
for (thisYear in c(1996:thisSeason)) {
  for (thisTeam in franchises$teamCode){
    for (thisMonth in months) {
      for (thisDay in days) {
        url <- paste0("https://www.basketball-reference.com/boxscores/",
                      thisYear,thisMonth,ifelse(nchar(thisDay)>1,thisDay,paste0("0",thisDay)),"0",thisTeam,
                      ".html")
        if (status_code(GET(url)) == 200) { # successful response

          getOtherTeam <- url %>%
            read_html() %>%
            #RCurl::getURL(ssl.verifyhost = 0L, ssl.verifypeer = 0L) %>%
            html_nodes(xpath ='//*[@id="content"]/div[2]/div[1]/div[1]/strong[1]/a[1]') %>%
            html_attr("href") %>%
            substr(8,10)
          print(paste(thisYear,"-",thisMonth,"-",thisDay,"-",thisTeam,"-",getOtherTeam))
          
          getBoxScoreTeam <- url %>%
            read_html() %>%
            html_nodes(xpath = paste0('//*[@id="box_',tolower(thisTeam),'_basic"]')) %>%
            html_table(fill = TRUE)
          getBoxScoreTeam <- getBoxScoreTeam[[1]]
          names(getBoxScoreTeam) <- getBoxScoreTeam[1,]
          getBoxScoreTeam <- getBoxScoreTeam[-1,]
          getBoxScoreTeam <- mutate(getBoxScoreTeam, Tm = thisTeam, Year = thisYear, Month = thisMonth, Day = thisDay)
          
          getBoxScoreOtherTeam <- url %>%
            read_html() %>%
            html_nodes(xpath = paste0('//*[@id="box_',tolower(getOtherTeam),'_basic"]')) %>%
            html_table(fill = TRUE)
          getBoxScoreOtherTeam <- getBoxScoreOtherTeam[[1]]
          names(getBoxScoreOtherTeam) <- getBoxScoreOtherTeam[1,]
          getBoxScoreOtherTeam <- getBoxScoreOtherTeam[-1,]
          getBoxScoreOtherTeam <- mutate(getBoxScoreOtherTeam, Tm = getOtherTeam ,Year = thisYear, Month = thisMonth, Day = thisDay)
          
          if (nrow(box_scores)>0) {
            box_scores <- bind_rows(box_scores,getBoxScoreTeam,getBoxScoreOtherTeam)
          } else {
            box_scores <- bind_rows(getBoxScoreTeam,getBoxScoreOtherTeam)
          }
        }
      }
    }
  }
}

# final dataset
box_scores1 <- read.csv("data/box_scores_1980_1995.csv", stringsAsFactors = FALSE)
box_scores2 <- read.csv("data/box_scores_1996_2018.csv", stringsAsFactors = FALSE)
box_scores_all <- bind_rows(box_scores1,box_scores2) %>%
  mutate_at(vars(-matches("Player|Starter|Tm")), as.numeric)

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

# NBA has changed especially due to the 3-point. Big guys who can't shoot the 3 point will get their
# power boosted if I include games prior to the 2000s in the regression. 2 options:
# 1. Reduce the input data to include only post 2000 or post 2005
# 2. Sample the input data to give more prominence to recent years. Ex: 2017: 100%, 2016: 98%, 2015:96%,
# ... 1985: 38%

### Let's model a liner regression
library(tidyverse)
library(caret)

############# Offense (PTS) or Defense (PTSA) model
Off_or_Def <- "PTSA"
Off_or_Def <- "PTS"

#dataToModel2 <- select(dataToModel2, -MP, -eval(parse(text=Off_or_Def)))
# Defensive model
dataToModel3 <- select(dataToModel2, -MP, -PTS, -score_diff)
dataToModel3 <- select(dataToModel3, -AST,-FT,-FTA,-starts_with("X"))
# Offensive model
dataToModel3 <- select(dataToModel2, -MP, -score_diff, -PTSA)
dataToModel3 <- mutate(dataToModel3, FG2Per = ifelse(X2PA==0,0,X2P/X2PA), 
                       FG3Per = ifelse(X3PA==0,0,X3P/X3PA), FTPer = ifelse(FTA==0,0,FT/FTA))
dataToModel3 <- select(dataToModel3, -BLK,-DRB,-FT,-FTA,-STL,-starts_with("X"))
  
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

###### ---------------------------------
###### Linear Regression ---------------------------------
###### ---------------------------------
#
set.seed(825)
glmFit <- train(PTSA ~ ., data = training,
                method = "glm",
                trControl = fitControl)

###########
# library(randomForest)
# set.seed(825)
# rfFit <- train(PTSA ~ ., data = training,
#                method = "rf",
#                ntree = 10,
#                trControl = fitControl)
# #trControl = trainControl(method = "oob"))
# 
# rfFit$finalModel
# varImpPlot(rfFit$finalModel)
###########

# Check predictions
model <- glmFit # pick the model
#
predict_data <- training # in sample
#predict_data <- testing # out of sample
# predict a particular player
predict_data <- playersPredictedStats_adjPer %>%
  #filter(playersPredictedStats_adjPer, grepl("lebron|durant|westbro|curry|anthony dav|giann", tolower(Player))) %>%
  select(-contains("Per"),-Pos,-Season,-Age,-Tm) %>%
  rename_all(funs(gsub("eff","",.))) %>% 
  select(-Min,-FG,-FGA,-TRB,-PTS) %>%
  rename_all(funs(gsub("2","X2",.))) %>%
  rename_all(funs(gsub("3","X3",.))) %>%
  rename_all(funs(gsub("M","",.))) %>%
  mutate(FG2Per = ifelse(X2PA==0,0,X2P/X2PA), 
         FG3Per = ifelse(X3PA==0,0,X3P/X3PA), 
         FTPer = ifelse(FTA==0,0,FT/FTA)) %>%
  #select(-AST,-FT,-FTA,-starts_with("X"))
  select(-BLK,-DRB,-FT,-FTA,-STL,-starts_with("X"))

predict_data_Def <- playersPredictedStats_adjPer %>%
  #filter(playersPredictedStats_adjPer, grepl("lebron|durant|westbro|curry|anthony dav|giann", tolower(Player))) %>%
  select(-contains("Per"),-Pos,-Season,-Age,-Tm) %>%
  rename_all(funs(gsub("eff","",.))) %>% 
  select(-Min,-FG,-FGA,-TRB,-PTS) %>%
  rename_all(funs(gsub("2","X2",.))) %>%
  rename_all(funs(gsub("3","X3",.))) %>%
  rename_all(funs(gsub("M","",.))) %>%
  select(-AST,-FT,-FTA,-starts_with("X"))

points_data <- playersPredictedStats_adjPer %>%
  mutate(points = 40*5*(effFTM + 2*eff2PM + 3*eff3PM)) %>%
  select(Player, points)
  
predicted <- predict(model, newdata = predict_data)
predicted_ifPlayers <- data.frame(Player = predict_data$Player,predicted = as.numeric(predicted))
predicted_ifPlayers <- merge(predicted_ifPlayers,points_data, by = "Player") %>%
  mutate(Off_avg = (points + predicted)/2, Off_avg2_1 = (2*points + predicted)/3)

predicted_Def <- predict(model, newdata = predict_data_Def)
predicted_ifPlayers_Def <- data.frame(Player = predict_data_Def$Player,Def_predicted = as.numeric(predicted_Def))

predicted_ifPlayers <- merge(predicted_ifPlayers,predicted_ifPlayers_Def, by = "Player") %>%
  mutate(diff = Off_avg2_1 - Def_predicted)


#############
predictions <- data.frame(actual_PTSA = predict_data$PTSA, predicted_PTSA = predicted) %>%
  mutate(pointwise_error = (actual_PTSA-predicted_PTSA)^2)
#predictions <- data.frame(actual_Diff = predict_data$score_diff, predicted_Diff = predicted) %>%
#  mutate(pointwise_error = (actual_Diff-predicted_Diff)^2)
fit_error <- summarise(predictions, mean(pointwise_error))
plot(predictions$actual_PTSA,predictions$predicted_PTSA)
#plot(predictions$actual_Diff,predictions$predicted_Diff)
#
###################################
###################################
#### Neural Network
#
#dataToModel2 <- sample_frac(dataToModel2, .3)
dataToModel2 <- select(dataToModel2, -MP, -PTS, -score_diff)

maxs <- apply(dataToModel2[,-1], 2, max) 
mins <- apply(dataToModel2[,-1], 2, min)
scaleMaxMin <- data.frame(maxs,mins)
# scale the data [0,1] for easier convergence of backpropagation algorithm
maxs <- scaleMaxMin$maxs 
mins <- scaleMaxMin$mins

team_season <- dataToModel2[,1]
scaled <- as.data.frame(scale(dataToModel2[,-1], center = mins, scale = maxs - mins))
scaled <- cbind(team_season,scaled)

###
set.seed(998)
perc <- 0.8
train_split <- round(perc*nrow(dataToModel2))

teams_train <- sample(dataToModel2$TmA_Date_TmB,train_split)
teams_test <- filter(dataToModel2, !(TmA_Date_TmB %in% teams_train))$TmA_Date_TmB
training <- filter(scaled, team_season %in% teams_train)
testing <- filter(scaled, team_season %in% teams_test)

# remove non-numeric variables
train_teamSeasonCodes <- training$team_season
test_teamSeasonCodes <- testing$team_season
training <- training[,-1]
testing <- testing[,-1]

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

nnetGrid <-  expand.grid(layer1 = c(4,5,6), 
                         layer2 = c(3,4), 
                         layer3 = c(2,3)
)

library(neuralnet)
library(caret)
#library(tidyverse)

set.seed(825)
# uses rmse for regression and softmax for classification by default (corresponds to parameter out_activation)
nnetFit <- train(PTSA ~ ., data = training, 
                 method = "neuralnet", 
                 trControl = fitControl, 
                 tuneGrid = nnetGrid)

model <- nnetFit
model
#save(model, file = paste0("data/model_","nnetFit","_",Sys.Date(),".Rdata"))
predict_data <- training
predicted <- predict(model, newdata = predict_data)
#save(model, file = paste0("data/modelNeuralnet19_",Off_or_Def,".Rdata"))
predictions <- data.frame(actual_PTSA = predict_data$PTSA, predicted_PTSA = predicted)
plot(predictions)



###################################
### Read players stats
playersPredictedStats_adjPer <- read.csv("data/playersNewPredicted_Final_adjPer.csv",stringsAsFactors = FALSE) %>%
  distinct(Player, .keep_all = TRUE)
modelPlayers <- mutate(playersPredictedStats_adjPer, PTS_game = (2*eff2PM + 3*eff3PM + effFTM)*48*8*effMin)







