# ui.R
dashboardPage(
  dashboardHeader(title = "Players"),
  dashboardSidebar(
    imageOutput('playerLogo',width = '200px',height='150px'),
    tags$style(type='text/css', ".selectize-input { font-size: 80%; line-height: 10px; width: 95%; min-height: 25px; max-height: 30px} .selectize-dropdown { font-size: 80%; line-height: 15px; }"),
    selectizeInput("playerName", "Select Player", choices = playerDashboard$Player, selected = playerDashboard$Player[1]),
    sliderInput("player_num_clusters", "Number of clusters", min = 2, max = 100, value = 10),
    selectizeInput("player_tsne_color", "Color by", choices = c("K_means","Offense","Defense"), selected = "K_means"),
    sidebarMenu(
      menuItem("Players", tabName = "players")
      #menuItem("Raw data", tabName = "rawdata")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("players",
              fluidRow(
                shinydashboard::valueBoxOutput("offBox_p"),
                shinydashboard::valueBoxOutput("defBox_p"),
                shinydashboard::valueBoxOutput("useBox_p"),
                shinydashboard::valueBoxOutput("expBox_p"),
                shinydashboard::valueBoxOutput("ageBox_p"),
                shinydashboard::valueBoxOutput("tmBox_p")
              ),
              fluidRow(
                box(
                  width = 12, status = "info", solidHeader = TRUE,
                  title = "Player stats",
                  plotOutput("barplotstats_p", width = "100%", height = 300)
                )
                ),
              fluidRow(
                box(
                  width = 12, status = "info",
                  title = "Player map",
                  plotOutput("tsneHist_p",width = "100%", height = 300)
                  )
                )
              )
      # ),
      # tabItem("playerTables",
      #         fluidRow(
      #           box(
      #             width = 4, status = "info", solidHeader = TRUE,
      #             title = "Player stats",
      #             dataTableOutput("similar_p", width = "100%", height = 600)
      #           ),
      #           box(
      #             width = 8, status = "info", solidHeader = TRUE,
      #             title = "Player stats",
      #             dataTableOutput("similarHist_p", width = "100%", height = 600)
      #           )
      #         )
      # )
    )
  )
)



