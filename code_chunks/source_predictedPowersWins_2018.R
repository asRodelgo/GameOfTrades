# Teams Predicted powers and wins
#
# Team weighted final skills
prepareTeamData <- .prepareModelOncePredicted(playersNewPredicted_Final_adjMinPer2,"All")

# Compute team powers using models
# Team Offense
    predict_Team_Off <- prepareTeamData %>%
      select(-Age) %>%
      rename_all(funs(gsub("eff","",.))) %>%
      rename_all(funs(gsub("2","X2",.))) %>%
      rename_all(funs(gsub("3","X3",.))) %>%
      rename_all(funs(gsub("M","",.))) %>%
      mutate(FG2Per = ifelse(X2PA==0,0,X2P/X2PA),
             FG3Per = ifelse(X3PA==0,0,X3P/X3PA),
             FTPer = ifelse(FTA==0,0,FT/FTA)) %>%
      select(-BLK,-DRB,-FT,-FTA,-STL,-starts_with("X"))


    efficient_Team_Off <- predict(nn_Offense, newdata = predict_Team_Off)

    points_Team_Off <- prepareTeamData %>%
      mutate(points = 48*5*(effFTM + 2*eff2PM + 3*eff3PM)) %>%
      select(team_season, points)

    predicted_Team_Offense <- data.frame(team_season = predict_Team_Off$team_season,efficient_Off = as.numeric(efficient_Team_Off))
    predicted_Team_Offense <- merge(predicted_Team_Offense,points_Team_Off, by = "team_season") %>%
      mutate(Offense = (2*points + efficient_Team_Off)/3)

# Team Defense

    predict_Team_Def <- prepareTeamData %>%
      select(-Age) %>%
      rename_all(funs(gsub("eff","",.))) %>%
      rename_all(funs(gsub("2","X2",.))) %>%
      rename_all(funs(gsub("3","X3",.))) %>%
      rename_all(funs(gsub("M","",.))) %>%
      select(-AST,-FT,-FTA,-starts_with("X"))

    predicted_Team_Def <- predict(nn_Defense, newdata = predict_Team_Def)
    predicted_Team_Defense <- data.frame(team_season = predict_Team_Def$team_season,Defense = as.numeric(predicted_Team_Def))


#####
team_power <- merge(predicted_Team_Offense,predicted_Team_Defense,by="team_season")

team_power <- team_power %>%
  mutate(teamCode = substr(team_season,1,3),
         Season = substr(team_season, 5,13)) %>%
  select(team_season, TEAM_PTS = Offense, TEAM_PTSAG = Defense, TeamCode = teamCode, Season) %>%
  as.data.frame()

team_power$TEAM_PTS <- as.numeric(as.character(team_power$TEAM_PTS))
team_power$TEAM_PTSAG <- as.numeric(as.character(team_power$TEAM_PTSAG))
teamsPredicted <- team_power
#
#
win_predictions <- .teamsPredictedWins(data = teamsPredicted)

teamDashboard <- merge(teamsPredicted, win_predictions, by.x = "TeamCode", by.y = "team") %>%
  select(Tm = TeamCode, TeamOffense = TEAM_PTS, TeamDefense = TEAM_PTSAG, Season, wins)
teamRanks <- mutate_if(teamDashboard, is.numeric, function(x) row_number(desc(x)))
teamStats <- .computeTeamStats(data = playersPredictedStats_adjPer)
teamStatRanks <- mutate_if(teamStats, is.numeric, function(x) row_number(desc(x)))
teamRanks <- merge(teamStatRanks, select(teamRanks,-Season),by="Tm")
teamMax <- summarise_if(teamStats, is.numeric, max)

# t-SNE for teams
tsne_ready_teams <- read.csv("data/tsne_ready_teams.csv", stringsAsFactors = FALSE)
