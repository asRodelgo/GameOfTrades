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
for (thisYear in c(1996:thisSeason)) {
  for (thisTeam in franchises$teamCode){
    for (thisMonth in months) {
      for (thisDay in days) {
        url <- paste0("https://www.basketball-reference.com/boxscores/",
                      thisYear,thisMonth,ifelse(nchar(thisDay)>1,thisDay,paste0("0",thisDay)),"0",thisTeam,
                      ".html")
        if (status_code(GET(url)) == 200) { # successful response

          getOtherTeam <- url %>%
            read_html() %>%
            #RCurl::getURL(ssl.verifyhost = 0L, ssl.verifypeer = 0L) %>%
            html_nodes(xpath ='//*[@id="content"]/div[2]/div[1]/div[1]/strong[1]/a[1]') %>%
            html_attr("href") %>%
            substr(8,10)
          print(paste(thisYear,"-",thisMonth,"-",thisDay,"-",thisTeam,"-",getOtherTeam))
          
          getBoxScoreTeam <- url %>%
            read_html() %>%
            html_nodes(xpath = paste0('//*[@id="box_',tolower(thisTeam),'_basic"]')) %>%
            html_table(fill = TRUE)
          getBoxScoreTeam <- getBoxScoreTeam[[1]]
          names(getBoxScoreTeam) <- getBoxScoreTeam[1,]
          getBoxScoreTeam <- getBoxScoreTeam[-1,]
          getBoxScoreTeam <- mutate(getBoxScoreTeam, Tm = thisTeam, Year = thisYear, Month = thisMonth, Day = thisDay)
          
          getBoxScoreOtherTeam <- url %>%
            read_html() %>%
            html_nodes(xpath = paste0('//*[@id="box_',tolower(getOtherTeam),'_basic"]')) %>%
            html_table(fill = TRUE)
          getBoxScoreOtherTeam <- getBoxScoreOtherTeam[[1]]
          names(getBoxScoreOtherTeam) <- getBoxScoreOtherTeam[1,]
          getBoxScoreOtherTeam <- getBoxScoreOtherTeam[-1,]
          getBoxScoreOtherTeam <- mutate(getBoxScoreOtherTeam, Tm = getOtherTeam ,Year = thisYear, Month = thisMonth, Day = thisDay)
          
          if (nrow(box_scores)>0) {
            box_scores <- bind_rows(box_scores,getBoxScoreTeam,getBoxScoreOtherTeam)
          } else {
            box_scores <- bind_rows(getBoxScoreTeam,getBoxScoreOtherTeam)
          }
        }
      }
    }
  }
}

# final dataset
box_scores1 <- read.csv("data/box_scores_1980_1995.csv", stringsAsFactors = FALSE)
box_scores2 <- read.csv("data/box_scores_1996_2018.csv", stringsAsFactors = FALSE)
box_scores_all <- bind_rows(box_scores1,box_scores2) %>%
  mutate_at(vars(-matches("Player|Starter|Tm")), as.numeric)

box_scores_totals <- filter(box_scores_all, grepl("Total",Player) | grepl("Total",Starters)) %>%
  filter(Year >= 1985) # data before 85 has holes
box_scores_totals <- cbind(box_scores_totals,homeOrAway = rep(c('H','A'),nrow(box_scores_totals)/2))
# Try simple regression model
dataToModel <- box_scores_totals %>%
  #group_by(box_scores_totals,Year,Month,Day) %>%
  mutate(leadPTS = lead(PTS),lagPTS = lag(PTS), leadTm = lead(Tm), lagTm = lag(Tm)) %>%
  mutate(PTSA = ifelse(homeOrAway=="H",leadPTS,lagPTS))
  
dataToModel$leadTm[nrow(dataToModel)] <- dataToModel$lagTm[nrow(dataToModel)]  

dataToModel <- mutate(dataToModel, TmA_Date_TmB = paste0(Tm,"_",Month,Day,Year,leadTm)) %>%
  select(TmA_Date_TmB,FG,FGA,X3P,X3PA,FT,FTA,ORB,DRB,AST,STL,BLK,TOV,PF,PTS,PTSA)




