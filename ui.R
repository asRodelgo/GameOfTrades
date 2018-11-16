# ui.R
source("global_cache.R", local = TRUE)

fluidPage(title = "Players",
  fluidRow(
    column(2,br()),
    column(4,
      imageOutput('playerLogo',width = '200px',height='150px'),
      tags$style(type='text/css', ".selectize-input { font-size: 80%; line-height: 10px; width: 95%; min-height: 25px; max-height: 30px} .selectize-dropdown { font-size: 80%; line-height: 15px; }"),
      selectizeInput("playerName", "Select Player", choices = playerDashboard$Player, selected = playerDashboard$Player[1]),
      chartJSRadarOutput('radarPlayerStats')
    ),
    column(4,
           imageOutput('playerLogo2',width = '200px',height='150px'),
           tags$style(type='text/css', ".selectize-input { font-size: 80%; line-height: 10px; width: 95%; min-height: 25px; max-height: 30px} .selectize-dropdown { font-size: 80%; line-height: 15px; }"),
           selectizeInput("playerName2", "Select Player", choices = playerDashboard$Player, selected = playerDashboard$Player[2]),
           chartJSRadarOutput('radarPlayerStats2')
    ),
    column(2,br())
    # column(6,
    #        plotOutput('barplotstats_p'),
    #        chartJSRadarOutput('radarPlayerStats')
    #        )
  ),
  source("ui_files/ui_game.R", local = TRUE)$value
)
