# Teams Predicted powers and wins
teamsPredicted <- .teamsPredictedPower(data = playersNewPredicted_Final_adjMinPer2,actualOrPred="predicted",maxs_vector = maxs_vector_input,
                                       mins_vector = mins_vector_input)
win_predictions <- .teamsPredictedWins(data = teamsPredicted) 

teamDashboard <- merge(teamsPredicted, win_predictions, by.x = "TeamCode", by.y = "team") %>%
  select(Tm = TeamCode, TeamOffense = TEAM_PTS, TeamDefense = TEAM_PTSAG, Season, wins)
teamRanks <- mutate_if(teamDashboard, is.numeric, function(x) row_number(desc(x)))
teamStats <- .computeTeamStats(data = playersPredictedStats_adjPer)
teamStatRanks <- mutate_if(teamStats, is.numeric, function(x) row_number(desc(x)))
teamRanks <- merge(teamStatRanks, select(teamRanks,-Season),by="Tm")
teamMax <- summarise_if(teamStats, is.numeric, max)
#write.csv(teamsPredicted, "cache_global/teamsPredicted.csv", row.names=FALSE)
#write.csv(win_predictions, "cache_global/win_predictions.csv", row.names=FALSE)
#write.csv(teamDashboard, "cache_global/teamDashboard.csv", row.names=FALSE)
#write.csv(teamRanks, "cache_global/teamRanks.csv", row.names=FALSE)
#write.csv(teamStats, "cache_global/teamStats.csv", row.names=FALSE)

# t-SNE for teams
tsne_ready_teams <- read.csv("data/tsne_ready_teams.csv", stringsAsFactors = FALSE)
