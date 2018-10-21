# compute teams and players Offense and Defense
playersNewPredicted_Final_adjMinPer2 <- select(playersPredictedStats_adjPer, -contains("Per"), -effFG, -effFGA, -effPTS, -effTRB)
playersNewPredicted_OffDef <- mutate(playersNewPredicted_Final_adjMinPer2, Tm = Player, effMin = 1)
playersPredicted <- .teamsPredictedPower_2018(data = playersNewPredicted_OffDef) %>%
  mutate(Player = substr(team_season,1,regexpr("_",team_season)-1),plusMinus = TEAM_PTS-TEAM_PTSAG) %>%
  select(Player,Offense = TEAM_PTS, Defense = TEAM_PTSAG, plusMinus) %>%
  as.data.frame()
write.csv(playersPredicted, "cache_global/playersPredicted.csv", row.names = FALSE)
