# ui.R
fluidPage(title = "Players",
  fluidRow(
    column(6,
      imageOutput('playerLogo',width = '200px',height='150px'),
      tags$style(type='text/css', ".selectize-input { font-size: 80%; line-height: 10px; width: 95%; min-height: 25px; max-height: 30px} .selectize-dropdown { font-size: 80%; line-height: 15px; }"),
      selectizeInput("playerName", "Select Player", choices = playerDashboard$Player, selected = playerDashboard$Player[1])
    ),
    column(6,
           plotOutput('barplotstats_p'),
           chartJSRadarOutput('radarPlayerStats')
           )
  )
)
