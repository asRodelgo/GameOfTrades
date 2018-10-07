### Testing different models to team powers --------------
#
# Load packages and functions
library(tidyverse)
library(caret)
source("helpers.R", local = TRUE)

############# Offense (PTS) or Defense (PTSA) model
Off_or_Def <- "PTSA"
Off_or_Def <- "PTS"
#############

playersSumm <- .prepareModel(Off_or_Def)
## Strip linearly relationed columns: FG, FGA, FG%,3P%,2P%,FT%,effFG%, effPTS
playersSumm <- select(playersSumm, -contains("Per"), -effFG, -effFGA, -effPTS, -effTRB)
## End of Strip
#scaleMaxMin <- .getScaleLimits(Off_or_Def, data = playersSumm)
# scale the data [0,1] for easier convergence of backpropagation algorithm
#maxs <- scaleMaxMin$maxs
#mins <- scaleMaxMin$mins

team_season <- playersSumm[,1]
#scaled <- as.data.frame(scale(playersSumm[,-1], center = mins, scale = maxs - mins))
#scaled <- cbind(team_season,scaled)

###
set.seed(998)
perc <- 0.8
train_split <- round(perc*nrow(playersSumm))

teams_train <- sample(playersSumm$team_season,train_split)
teams_test <- filter(playersSumm, !(team_season %in% teams_train))$team_season
# training and testing with scaling
#training <- filter(scaled, team_season %in% teams_train)
#testing <- filter(scaled, team_season %in% teams_test)
# training and testing without scaling (works better for linear models)
training <- filter(playersSumm, team_season %in% teams_train)
testing <- filter(playersSumm, team_season %in% teams_test)

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

###### ---------------------------------
###### Linear Regression ---------------------------------
###### ---------------------------------
#
set.seed(825)
glmFit <- train(PTS ~ ., data = training,
                 method = "glm",
                 trControl = fitControl)

###### ---------------------------------
###### Random Forest ---------------------------------
###### ---------------------------------

library(randomForest)
set.seed(825)
rfFit <- train(PTS ~ ., data = training,
               method = "rf",
               ntree = 20,
               trControl = fitControl)
               #trControl = trainControl(method = "oob"))

rfFit$finalModel
varImpPlot(rfFit$finalModel)

########################################################################################################
########################################################################################################

# check predictions
model <- glmFit # pick the model
#model <- rfFit # pick the model
#
predict_data <- training # in sample
predict_data <- testing # out of sample
predicted <- predict(model, newdata = predict_data)
#save(model, file = paste0("data/modelNeuralnet19_",Off_or_Def,".Rdata"))
predictions <- data.frame(actual_PTS = predict_data$PTS, predicted_PTS = predicted) %>%
  mutate(pointwise_error = (actual_PTS-predicted_PTS)^2)
fit_error <- summarise(predictions, mean(pointwise_error))
plot(predictions$actual_PTS,predictions$predicted_PTS)

########################################################################################################
########################################################################################################
# save models
save(model, file = paste0("data/modelGLM_",Off_or_Def,".Rdata"))

########################################################################################################
########################################################################################################
# calculate team powers
# Load models:
load("data/modelGLMPTS.Rdata")
nn_Offense <- model$finalModel
load("data/modelGLMPTSA.Rdata")
nn_Defense <- model$finalModel

# Uses: .computePower_NoScale()
# Read predicted players skills:
playersPredictedStats_adjPer <- read.csv("data/playersNewPredicted_Final_adjPer.csv",stringsAsFactors = FALSE) %>%
  distinct(Player, .keep_all = TRUE)
# Code from source_computeOffenseDefense.R:
playersNewPredicted_Final_adjMinPer2 <- select(playersPredictedStats_adjPer, -contains("Per"), -effFG, -effFGA, -effPTS, -effTRB)
playersNewPredicted_OffDef <- mutate(playersNewPredicted_Final_adjMinPer2, Tm = Player, effMin = 1)
playersPredicted <- .teamsPredictedPower(data = playersNewPredicted_OffDef,actualOrPred="predicted",
                                         scaled_data = FALSE, off_model = nn_Offense, def_model = nn_Defense) %>%
  mutate(Player = substr(team_season,1,regexpr("_",team_season)-1),plusMinus = TEAM_PTS-TEAM_PTSAG) %>%
  select(Player,Offense = TEAM_PTS, Defense = TEAM_PTSAG, plusMinus) %>%
  as.data.frame()




