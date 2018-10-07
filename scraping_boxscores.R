### Scraping box scores from individual games
library(tidyverse)
library(rvest)
library(httr)
thisSeason = substr(Sys.Date(),1,4)
box_scores <- data.frame()
thisSeason <- as.numeric(thisSeason)

franchises <- read.csv("data/franchisesHistory.csv",stringsAsFactors = FALSE) %>%
  distinct(Franchise, .keep_all = TRUE)

months <- c("01","02","03","04","05","10","11","12")
days <- c(1:31)
for (thisYear in c(1980:thisSeason) {
  for (thisTeam in franchises$teamCode){
    for (thisMonth in months) {
      for (thisDay in days) {
        url <- paste0("https://www.basketball-reference.com/boxscores/",
                      thisYear,thisMonth,ifelse(nchar(thisDay)>1,thisDay,paste0("0",thisDay)),"0",thisTeam,
                      ".html")
        if (status_code(GET(url)) == 200){ # successful response

          getTeamsLineScore <- url %>%
            read_html() %>%
            html_nodes(xpath='//*[@id="content"]/h1') %>%
            html_text()
          
          }


      }
    }



    url <- paste0("https://www.basketball-reference.com/teams/",thisTeam,"/",thisYear,"_games.html")
    if (status_code(GET(url)) == 200){ # successful response
      getBoxscoreLinks <- url %>%
        read_html() %>%
        html_nodes(xpath='//*[@id="games"]') %>%
        #html_nodes(xpath='//*[@id="games"]/tbody/tr[1]/td[4]/a') %>%
        html_table(fill = TRUE)
      thisBoxScoreList <- getBoxscoreLinks[[1]]
      # home_team, away_team, date,
      thisBoxScoreKeys <- select(thisBoxScoreList, Date,)

      thisRoster <- getRoster[[1]] %>% select(-`No.`)
      names(thisRoster)[which(names(thisRoster)=='')] <- "Nationality"
      thisRoster <- mutate(thisRoster, Tm = thisTeam, Exp = as.character(Exp))
      if (nrow(current_rosters)>0){
        current_rosters <- bind_rows(current_rosters,thisRoster)
      } else{
        current_rosters <- thisRoster
      }
    }
  }
  names(current_rosters) <- gsub(" ","_",names(current_rosters))



