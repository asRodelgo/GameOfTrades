# Teams Predicted powers and wins
#
# Team weighted final skills
playersNewPredicted_Final_adjMinPer2 <- select(playersPredictedStats_adjPer, -contains("Per"), -effFG, -effFGA, -effPTS, -effTRB)
teamsPredicted <- .teamsPredictedPower_2018(playersNewPredicted_Final_adjMinPer2, nn_Offense, nn_Defense)
# Scale Off and Def to reflect reality (taking data from last year)
avgOff <- filter(team_stats, Season == max(as.character(Season))) %>%
  distinct(teamCode, .keep_all = TRUE) %>%
  summarise(mean(PTS)) %>% as.numeric()
#
avgPredOff <- summarise(teamsPredicted,mean(TEAM_PTS)) %>% as.numeric()
avgPredDef <- summarise(teamsPredicted,mean(TEAM_PTSAG)) %>% as.numeric()

teamsPredicted <- mutate(teamsPredicted, TEAM_PTS = round(TEAM_PTS + (avgOff-avgPredOff),1),
                  TEAM_PTSAG = round(TEAM_PTSAG + (avgOff-avgPredDef),1))

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
