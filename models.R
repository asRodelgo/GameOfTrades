### Testing different models to team powers --------------
#
# Load packages and functions
library(tidyverse)
library(caret)
source("helpers.R", local = TRUE)

############# Offense (PTS) or Defense (PTSA) model
Off_or_Def <- "PTSA"
#############

playersSumm <- .prepareModel(Off_or_Def)
## Strip linearly relationed columns: FG, FGA, FG%,3P%,2P%,FT%,effFG%, effPTS
playersSumm <- select(playersSumm, -contains("Per"), -effFG, -effFGA, -effPTS, -effTRB)
## End of Strip
scaleMaxMin <- .getScaleLimits(Off_or_Def, data = playersSumm)
# scale the data [0,1] for easier convergence of backpropagation algorithm
maxs <- scaleMaxMin$maxs
mins <- scaleMaxMin$mins

team_season <- playersSumm[,1]
scaled <- as.data.frame(scale(playersSumm[,-1], center = mins, scale = maxs - mins))
scaled <- cbind(team_season,scaled)

###
set.seed(998)
perc <- 0.8
train_split <- round(perc*nrow(playersSumm))

teams_train <- sample(playersSumm$team_season,train_split)
teams_test <- filter(playersSumm, !(team_season %in% teams_train))$team_season
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
               ntree = 200,
               trControl = fitControl)
               #trControl = trainControl(method = "oob"))

rfFit$finalModel
varImpPlot(rfFit$finalModel)

########################################################################################################
########################################################################################################

# check predictions
model <- glmFit # pick the model
model <- rfFit # pick the model
#
predict_data <- training # in sample
predict_data <- testing # out of sample
predicted <- predict(model, newdata = predict_data)
#save(model, file = paste0("data/modelNeuralnet19_",Off_or_Def,".Rdata"))
predictions <- data.frame(actual_PTS = predict_data$PTS, predicted_PTS = predicted) %>%
  mutate(pointwise_error = (actual_PTS-predicted_PTS)^2)
fit_error <- summarise(predictions, mean(pointwise_error))
plot(predictions$actual_PTS,predictions$predicted_PTS)



