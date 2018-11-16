## ui_game.R
fluidRow(
  # Team A:
  column(6,
         fluidRow(
           imageOutput('teamALogo',width = '150px',height='150px'),
           textOutput('teamAName')
         ),
         fluidRow(
           textOutput('teamAOffense'),
           textOutput('teamADefense'),
           chartJSRadarOutput('radarTeamAStats')
         )

         ),
  # Team B:
  column(6,
         imageOutput('teamBLogo',width = '150px',height='150px')


         )


)

