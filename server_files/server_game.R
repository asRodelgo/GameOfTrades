## server_game.R --------------------
library(radarchart)
## Logos
output$teamALogo <- renderImage({
  filename <- normalizePath(file.path('./images',
                                      paste('ATL', '.svg', sep='')))

  # Return a list containing the filename and alt text
  list(src = filename,
       width = 150,
       height = 150,
       alt = paste("Team: ", "ATL"))

}, deleteFile = FALSE)

output$teamBLogo <- renderImage({
  filename <- normalizePath(file.path('./images',
                                      paste('GSW', '.svg', sep='')))

  # Return a list containing the filename and alt text
  list(src = filename,
       width = 150,
       height = 150,
       alt = paste("Team: ", "GSW"))

}, deleteFile = FALSE)

# Team Powers
output$teamAOffense <- renderText({
  data <- filter(values$teamsDatabase, Tm == "ATL") %>%
    select(TeamOffense) %>% as.numeric()
  return(data)
})

output$teamBOffense <- renderText({
  data <- filter(values$teamsDatabase, Tm == "GSW") %>%
    select(TeamOffense) %>% as.numeric()
  return(data)
})

# Team Radars
output$radarTeamAStats <- renderChartJSRadar({

  data <- filter(values$teamsDatabase, Tm == "ATL") %>%
    select(contains("eff"), contains("Per")) %>%
    gather(Stat, Value)

  ranks <- filter(values$teamsRanks, Tm == "ATL") %>%
    #ranks <- filter(playerRanks, Player == "Kevin Durant") %>%
    select(contains("eff"), contains("Per")) %>%
    gather(Stat, Value) %>%
    mutate(Value = (nrow(teamRanks)-Value + 1)/nrow(teamRanks)) %>%
    spread(Stat, Value) %>%
    mutate(Scoring = effPTS,
           #(3*eff3PM + 2*eff2PM + effFTM)/6,
           Accuracy = effFGPer,
           Assists = effAST,
           Rebounds = effTRB,
           Blocks = effBLK,
           Three_Point = (2*eff3PM + eff3PA)/3) %>%
    select(-contains("eff"), -contains("Per")) %>%
    gather(Stat, Value)

  stats <- ranks$Stat

  scores <- list(
    "ATL" = ranks$Value
  )

  chartJSRadar(scores = scores, labs = stats, maxScale = 1, polyAlpha = 0)
})

output$radarTeamAStats <- renderChartJSRadar({

  data <- filter(values$teamsDatabase, Tm == "GSW") %>%
    select(contains("eff"), contains("Per")) %>%
    gather(Stat, Value)

  ranks <- filter(values$teamsRanks, Tm == "GSW") %>%
    #ranks <- filter(playerRanks, Player == "Kevin Durant") %>%
    select(contains("eff"), contains("Per")) %>%
    gather(Stat, Value) %>%
    mutate(Value = (nrow(teamRanks)-Value + 1)/nrow(teamRanks)) %>%
    spread(Stat, Value) %>%
    mutate(Scoring = effPTS,
           #(3*eff3PM + 2*eff2PM + effFTM)/6,
           Accuracy = effFGPer,
           Assists = effAST,
           Rebounds = effTRB,
           Blocks = effBLK,
           Three_Point = (2*eff3PM + eff3PA)/3) %>%
    select(-contains("eff"), -contains("Per")) %>%
    gather(Stat, Value)

  stats <- ranks$Stat

  scores <- list(
    "GSW" = ranks$Value
  )

  chartJSRadar(scores = scores, labs = stats, maxScale = 1, polyAlpha = 0)
})


