### College and International players analysis -------------
### College
library(tidyverse)
library(httr)
library(rvest)
library(shiny)
library(DT)
write_collegePlayersHist()
# Read data
# historical college players
collegePlayersHist <- read.csv("data/collegePlayersHist2018.csv", stringsAsFactors = FALSE)
# most recent college players
collegePlayers <- read.csv("data/collegePlayers2018.csv", stringsAsFactors = FALSE)
# most recent college players efficient stats
collegeEffStats <- read.csv("data/collegeEffStats.csv", stringsAsFactors = FALSE)

# offensive and defensive rating for each player
load("data/modelNeuralnet5_PTS.Rdata")
nn_Offense <- model$finalModel
load("data/modelNeuralnet5_PTSA.Rdata")
nn_Defense <- model$finalModel

# load limits for scaled data. Each trade will trigger a predict() from the selected NNet model
# But scale limits must be kept as originally trained in the model for consistency
team_stats_Off <- .prepareModel("PTS")
#write.csv(team_stats_Off, "cache_global/team_stats_Off.csv", row.names = FALSE)
team_stats_Def <- .prepareModel("PTSA")
#write.csv(team_stats_Def, "cache_global/team_stats_Def.csv", row.names = FALSE)
scaleMaxMin_Off <- .getScaleLimits("PTS", team_stats_Off)
scaleMaxMin_Def <- .getScaleLimits("PTSA", team_stats_Def)
scaleMaxMin_Off <- scaleMaxMin_Off[!(row.names(scaleMaxMin_Off) %in%
                                       c("FGPer","FG3Per","FG2Per","effFGPer","FTPer","effFG","effFGA","effTRB","effPTS")),]
scaleMaxMin_Def <- scaleMaxMin_Def[!(row.names(scaleMaxMin_Def) %in%
                                       c("FGPer","FG3Per","FG2Per","effFGPer","FTPer","effFG","effFGA","effTRB","effPTS")),]
#write.csv(scaleMaxMin_Off, "cache_global/scaleMaxMin_Off.csv", row.names = FALSE)
#write.csv(scaleMaxMin_Def, "cache_global/scaleMaxMin_Def.csv", row.names = FALSE)
maxs_Off <- scaleMaxMin_Off$maxs
mins_Off <- scaleMaxMin_Off$mins
maxs_Def <- scaleMaxMin_Def$maxs
mins_Def <- scaleMaxMin_Def$mins
maxs_vector_input <- cbind(maxs_Off,maxs_Def)
mins_vector_input <- cbind(mins_Off,mins_Def)

collegeEffStats_pred <- select(collegeEffStats, -contains("Per"), -effFG, -effFGA, -effPTS, -effTRB)
collegeEffStats_pred_OffDef <- mutate(collegeEffStats_pred, Tm = Player, effMin = 1, Season = "2018-2019")
collegePlayersPredicted <- .teamsPredictedPower(data = collegeEffStats_pred_OffDef,actualOrPred="predicted",maxs_vector = maxs_vector_input,
                                         mins_vector = mins_vector_input) %>%
  mutate(Player = substr(team_season,1,regexpr("_",team_season)-1),plusMinus = TEAM_PTS-TEAM_PTSAG) %>%
  select(Player,Offense = TEAM_PTS, Defense = TEAM_PTSAG, plusMinus) %>%
  as.data.frame()

collegeEffStats2 <- merge(collegeEffStats, collegePlayersPredicted, by = "Player")
collegePlayerRanks <- mutate_if(collegeEffStats2, is.numeric, function(x) row_number(desc(x)))
names(collegePlayerRanks) <- c("Player",paste0("rank_",names(collegePlayerRanks)[-1]))
collegeDashboard <- merge(collegeEffStats2,collegePlayerRanks, by = "Player")

# Define the UI
ui <- fluidRow(
  #column(3,
  #       selectizeInput('season', 'Season', choices = collegePlayersHist$Season)
         #selectizeInput('player', 'Player name', choices = collegePlayersHist$Player)
  #       ),
  column(12,DT::dataTableOutput('collegePlayersTable') %>%
           formatStyle(c(1:ncol(collegeDashboard)),fontSize = '50%', lineHeight = '60%'))
)


# Define the server code
server <- function(input, output) {
  #updateSelectizeInput('player','Player name', choices = filter(collegePlayersHist, Season == input$season)$Player)
  output$collegePlayersTable <- renderDataTable({
    #filter(collegePlayersHist, Season == input$season)
    datatable(collegeDashboard, extensions = c('Scroller','FixedColumns'), options = list(dom="ft",pageLength = 60,
                                                                               autoWidth = TRUE,
                                                                               deferRender = TRUE,
                                                                               scrollY = 650,
                                                                               scrollX = TRUE,
                                                                               fixedColumns = list(leftColumns = 2),
                                                                               scroller = TRUE,
                                                                               initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#fff', 'color': '#575859', 'font-size': '10px', 'font-weight': 'bold'});","}"),
                                                                               columnDefs = list(list(visible=FALSE, targets=c(24:46)))),
              rownames = FALSE,escape = FALSE) %>%
      formatStyle(c('effTOV','effPF','Defense'), valueColumns=c('rank_effTOV','rank_effPF','rank_Defense'), color = JS(paste0("value < ",nrow(collegePlayerRanks)/3," ? 'red' : value < ",nrow(collegePlayerRanks)*2/3," ? 'lightblue' : 'green'")), fontWeight = 'bold') %>%
      formatStyle(c(2:24), valueColumns=c(25:47), color = JS(paste0("value < ",nrow(collegePlayerRanks)/3," ? 'green' : value < ",nrow(collegePlayerRanks)*2/3," ? 'lightblue' : 'red'")), fontWeight = 'bold') %>%
      formatRound(c(2:24), 3) %>%
      #formatStyle(c('Exp','Usage'), textAlign = 'right') %>%
      formatStyle(0, target = 'row', fontSize = '70%', lineHeight = '70%')
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)
