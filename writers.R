#### Writers ------------------------
# At the beginning of a new season, use this script to update all necessary data
#
# Alberto Sanchez Rodelgo
#### --------------------------------

########## PLAYERS ###########

## Update the historical players database: playersHist
write_playersHist <- function() {
  # Look in basketballreference.com and loop for all seasons
  # Example: http://www.basketball-reference.com/leagues/NBA_2017_per_game.html
  require(httr)
  require(tidyverse)
  library(rvest)
  thisYear <- substr(Sys.Date(),1,4)
  
  ##### ALL SEASONS ########
  firstYear <- 1980
  
  ##### NEW SEASON ########
  firstYear <- thisYear
  
  playersHist <- data.frame()
  for (year in firstYear:thisYear){
    url <- paste0("http://www.basketball-reference.com/leagues/NBA_",year,"_per_game.html")
    thisSeasonStats <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="per_game_stats"]') %>%
      html_table(fill = TRUE)
    thisSeasonStats <- thisSeasonStats[[1]] %>% filter(!(Player == "Player")) %>% 
      mutate(Season = paste0(year-1,"-",year)) %>%
      select(-Rk)
    
    if (nrow(playersHist)>0) playersHist <- bind_rows(playersHist,thisSeasonStats) 
    else playersHist <- thisSeasonStats
  }
  
  playersHist <- mutate_at(playersHist, vars(c(3,5:(ncol(playersHist)-1))), funs(as.numeric))
  playersHist[is.na(playersHist)] <- 0
  names(playersHist) <- gsub("%",".",names(playersHist),fixed=TRUE)
  names(playersHist) <- gsub("2","X2",names(playersHist))
  names(playersHist) <- gsub("3","X3",names(playersHist))
  names(playersHist) <- gsub("PS/G","PTS",names(playersHist),fixed=TRUE)
  
  if (firstYear==thisYear) {
    playersHistOLD <- read.csv("data/playersHist.csv", stringsAsFactors = FALSE)
    playersHist <- bind_rows(playersHistOLD,playersHist)
  } 
  playersHist<- filter(playersHist, Season > "1978-1979")
  write.csv(playersHist, "data/playersHist.csv",row.names = FALSE)  
}

# Pre-compute tsne_points for all ages to save time as these computations don't really
# depend on the player selected. 
write_tsneBlocks <- function(){
  
  tsneBlock <- list()
  num_iter <- 300
  max_num_neighbors <- 20
  for (a in 18:41){ # ages 18 to 41
    tsneBlock[[a]] <- .tSNE_compute(num_iter, max_num_neighbors, a)
    write.csv(tsneBlock[[a]],paste0("data/tsneBlock","_",a,".csv"),row.names = FALSE)
  }
}

# write current or previous season rosters
write_currentRosters_rostersLastSeason <- function(previousSeason = FALSE){
  
  thisSeason = substr(Sys.Date(),1,4)
  library(httr)
  new_rosters <- data.frame()
  thisSeason <- as.numeric(thisSeason) + 1
  
  current_rosters <- data.frame()
  playersNew <- playersHist %>% # keep only players last season
    filter(Season == max(as.character(Season))) %>%
    mutate(Season = as.factor(paste0(as.numeric(substr(Season,1,4))+1,"-",as.numeric(substr(Season,1,4))+2)))
  playersNew <- filter(playersNew,!(Tm == "TOT"))
  for (thisTeam in unique(playersNew$Tm)){
    
    url <- paste0("https://www.basketball-reference.com/teams/",thisTeam,"/",thisSeason,".html")
    if (status_code(GET(url)) == 200){ # successful response
      getRoster <- url %>%
        read_html() %>%
        html_nodes(xpath='//*[@id="roster"]') %>%
        html_table(fill = TRUE)
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
  #names(current_rosters) <- c(names(current_rosters)[1:5],"Birth_Date","Nationality","Experience","College","Team")
  # I need to compute their current ages for the prediction model is based on their age
  current_rosters <- mutate(current_rosters, Age = thisSeason - as.numeric(substr(Birth_Date,nchar(Birth_Date)-3,nchar(Birth_Date))),
                            Season = paste0(thisSeason-1,"-",thisSeason))
  
  # write current_rosters or rostersLastSeason depending on value of thisSeason
  if (previousSeason) {
    write.csv(current_rosters, "data/rostersLastSeason.csv",row.names = FALSE)
  } else {
    write.csv(current_rosters, "data/currentRosters.csv",row.names = FALSE)
  }
}

# write players predicted stats for an upcoming season
# imports: playersHist.csv
# calls several helpers
write_playersNewPredicted <- function() {
  
  # update currentRosters, europePlayers and College players from write_rookiesDraft.R
  current_rosters <- read.csv("data/currentRosters.csv", stringsAsFactors = FALSE)
  rookies <- read.csv("data/rookies.csv",stringsAsFactors = FALSE)
  collegePlayers <- read.csv("data/collegePlayers.csv", stringsAsFactors = FALSE)
  rookieStats <- read.csv("data/rookieStats.csv", stringsAsFactors = FALSE)
  europePlayers <- read.csv("data/europePlayers.csv", stringsAsFactors = FALSE)
  playersNew <- playersHist %>%
    filter(Season == max(as.character(Season))) %>%
    mutate(Season = as.factor(paste0(as.numeric(substr(Season,1,4))+1,"-",as.numeric(substr(Season,1,4))+2)))
  
  playersNewPredicted <- data.frame()
  for (team in unique(playersNew$Tm)){
    
    thisTeam <- filter(playersNew, Tm == team)
    thisTeamStats <- data.frame()
    counter <- 0 # keep count
    for (player in thisTeam$Player){
      counter <- counter + 1
      #if (!(player %in% playersNewPredicted$Player)){ # skip running all. Start over where it failed
      thisPlayer <- filter(thisTeam, Player == player)
      #thisPlayer <- filter(playersNew, Player == player)
      print(paste0("Team: ", team,": Processing ",thisPlayer$Player, " (",round(counter*100/nrow(playersNew),1),"%)"))
      if (thisPlayer$Age < 20) { # not enough players to compare to at age 19 or younger
        thisPlayer$Age <- 20
      }
      if (thisPlayer$Age > 39) { # not enough players to compare to at age 41 or older
        thisPlayer$Age <- 39
      }
      thisPlayerStats <- .predictPlayer(thisPlayer$Player,20,thisPlayer$Age,10) %>% 
        select(Player,Pos,Season,Age,everything())
      
      if (nrow(thisPlayerStats)>0){ # in case thisPlayerStats return an empty data.frame
        if (!is.na(thisPlayerStats$effPTS)){ # rosters not yet updated so include R (last season rookies)
          #if (thisPlayer$Exp %in% c(seq(1,25,1),"R")){ # rosters not yet updated so include R (last season rookies)
          print("NBA player: OK!")
          print(thisPlayerStats)
        } else if (player %in% playersNew$Player) { # NBA player that didn't play enough minutes so I use his numbers from last season as prediction
          thisPlayerStats <- .team_preparePredict(filter(playersNew, Player == player),team)  %>%
            mutate(Age = Age + 1) %>%
            select(Player,Pos,Season,Age,everything())
          
          thisMin <- thisTeam %>% mutate(effMin = MP*G/(5*15*3936)) # Use 15 as an approximate roster size to account for effective minutes played for players with low total minutes
          teamMinutes <- sum(thisMin$effMin)
          thisMin <- #mutate(thisMin, effMin = effMin) %>%
            filter(thisMin,Player == player) %>%
            distinct(effMin) %>%
            as.numeric()
          thisPlayerStats <- mutate(thisPlayerStats, effMin = thisMin)
          
          print("NBA player: Empty predicted stats!")
          print(thisPlayerStats)
        } else { # Rookie player or returns NA stats
          # compute rookie player average stats for this player
          thisPlayerStats <- .calculate_AvgPlayer(playersNew, thisPlayer$Age + 1) %>%
            mutate(Player = as.character(thisPlayer$Player), Pos = as.character(thisPlayer$Pos), 
                   G = as.numeric(thisPlayer$G), GS = as.numeric(thisPlayer$GS), Tm = team) 
          thisPlayerStats <- .team_preparePredict(data = thisPlayerStats, thisTeam = as.character(thisPlayer$Tm),singlePlayer = TRUE)
          print("Average player: OK!")
          print(thisPlayerStats)
          #}
        }
      } else if (player %in% playersNew$Player) { # NBA player that didn't play enough minutes so I use his numbers from last season as prediction
        thisPlayerStats <- .team_preparePredict(filter(playersNew, Player == player),team)  %>%
          mutate(Age = Age + 1) %>%
          select(Player,Pos,Season,Age,everything())
        print("NBA player: Short minutes!")
        print(thisPlayerStats)
      } else {
        thisPlayerStats <- .calculate_AvgPlayer(playersNew, thisPlayer$Age + 1) %>%
          mutate(Player = as.character(thisPlayer$Player), Pos = as.character(thisPlayer$Pos), 
                 G = as.numeric(thisPlayer$G), GS = as.numeric(thisPlayer$GS), Tm = team, Age) 
        thisPlayerStats <- .team_preparePredict(data = thisPlayerStats, thisTeam = as.character(thisPlayer$Tm),singlePlayer = TRUE)
        print("Average player: OK!")
        print(thisPlayerStats)
      }  
      if (nrow(thisTeamStats)>0){
        thisTeamStats <- bind_rows(thisTeamStats,thisPlayerStats)
      } else{
        thisTeamStats <- thisPlayerStats
      }
    }
    if (nrow(thisTeamStats) > 0) {
      thisTeamStats <- mutate(thisTeamStats, Tm = team)
      if (nrow(playersNewPredicted)>0){
        playersNewPredicted <- bind_rows(playersNewPredicted,thisTeamStats)
      } else{
        playersNewPredicted <- thisTeamStats
      }
    }
  }
  playersNewPredicted <- distinct(playersNewPredicted, Player, Tm, .keep_all=TRUE)
  limitMinutes <- 2*quantile(playersNewPredicted$effMin,.95) # control for possible outliers
  defaultMinutes <- quantile(playersNewPredicted$effMin,.1) # assign low minutes to outliers as they most likely belong to players with very little playing time
  playersNewPredicted2 <- mutate(playersNewPredicted,effMin = ifelse(effMin > limitMinutes, defaultMinutes,effMin))
  write.csv(playersNewPredicted2, "data/playersNewPredicted_Oct20.csv", row.names = FALSE)
}  

# pre-calculate tsne points for all players and seasons
write_tsne_points_All <- function(){
  
  # data_tsne contains the input data for tSNE filtered and cleaned up
  # from: data_tsne <- .tSNE_prepare_All() # for tSNE visualization from similarityFunctions.R
  # calculate tsne-points Dimensionality reduction to 2-D
  
  library(doMC) # use parallel processing on this machine through "foreach"
  registerDoMC(2) # As far as I know my MAC works on 2 cores
  #data_tsne_sample <- dplyr::sample_n(data_tsne,1000)
  data_tsne_sample <- filter(data_tsne,Season > "1995-1996")
  #%in% c("2012-2013","2013-2014","2014-2015","2015-2016"))
  #"2012-2013","2013-2014","2014-2015",
  
  if (nrow(data_tsne)>0){
    num_iter <- 400
    max_num_neighbors <- 50
    set.seed(456) # reproducitility
    tsne_points <- tsne(data_tsne_sample[,-c(1:5)], 
                        max_iter=as.numeric(num_iter), 
                        perplexity=as.numeric(max_num_neighbors), 
                        epoch=100)
    #plot(tsne_points)
  } else {
    tsne_points <- c()
  }
  write.csv(tsne_points, "data/tsne_points_All.csv",row.names = FALSE)
  
}

# write MVPs from past seasons
write_mvps <- function(){
  
  library(httr)
  library(rvest)
  
  mvps <- data.frame(Player=NULL, Season=NULL)
  for (thisSeason in 1980:as.numeric(thisYear)){
    
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_",
                  thisSeason,".html")
    
    thisPlayer <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="meta"]/div[2]/p[2]/a') %>%
      html_text()
    
    thisMVP <- data.frame(Player = thisPlayer, Season = paste0(thisSeason-1,"-",thisSeason))
    if (nrow(mvps) > 0) mvps <- rbind(mvps,thisMVP) else mvps <- thisMVP
  }
  
  write.csv(mvps, "data/mvps.csv", row.names = FALSE)
}

# compile league awards: mvp, def player, rookie of the year, etc.
write_Awards <- function(){ # Not working!
  
  library(httr)
  library(rvest)
  
  awards <- data.frame(Player=NULL, Season=NULL, award=NULL)
  for (thisSeason in 1980:as.numeric(thisYear)){
    
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_",
                  thisSeason,".html")
    
    thisPlayer <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="div_all-nba"]') %>%
      #html_children() %>%
      #html_children() %>%
      html_nodes('table')
    
    thisAwards <- data.frame(Player = thisPlayer, Season = paste0(thisSeason-1,"-",thisSeason), award = )
    if (nrow(awards) > 0) awards <- rbind(awards,thisAwards) else awards <- thisAwards
  }
  
  write.csv(mvps, "data/mvps.csv", row.names = FALSE)
}

# put together tsne_ready to load at the start of dashboards
# imports: tsne_points_All.csv
# calls helpers
write_tsne_ready_hist <- function() {
  
  source("helper_functions.R")
  #.teamsPredictedPower() 
  tsne_points <- read.csv("data/tsne_points_All.csv",stringsAsFactors = FALSE)
  
  # load data
  data_tsne <- .tSNE_prepare_All() # for tSNE visualization from similarityFunctions.R
  data_tsne_sample <- filter(data_tsne,Season > "1995-1996")
  # tsne_points are pre-calculated from write_tSNE_All.R and saved in data/ directory
  # using this function: tsne_points <- write_tSNE_compute_All()
  if (!nrow(data_tsne_sample)==nrow(tsne_points)){ # in case labels and coordinates have different sizes
    tsne_ready <- tsne_points
  } else {
    tsne_ready <- cbind(data_tsne_sample,tsne_points)
  }
  
  names(tsne_ready)[ncol(tsne_ready)-1] <- "x"
  names(tsne_ready)[ncol(tsne_ready)] <- "y"
  
  write.csv(tsne_ready, "data/tsne_ready_hist.csv", row.names = FALSE)
  
}

# precalculate t-SNE for predicted stats (new season)
# imports: playersNewPredicted_Final_adjMin.csv
write_tsne_points_newSeason <- function() {
  
  require(tsne)
  data_tsne_sample <- read.csv("data/playersNewPredicted_Final_adjMin.csv", stringsAsFactors = FALSE) %>%
    select_if(is.numeric) %>% select(-Pick)
  
  if (nrow(data_tsne_sample)>0){
    num_iter <- 600
    max_num_neighbors <- 20
    set.seed(456) # reproducitility
    tsne_points <- tsne(data_tsne_sample, 
                        max_iter=as.numeric(num_iter), 
                        perplexity=as.numeric(max_num_neighbors), 
                        epoch=100)
    plot(tsne_points)
  } else {
    tsne_points <- c()
  }
  write.csv(tsne_points, "data/tsne_points_newSeason.csv",row.names = FALSE)
  
}

# put together tsne_ready predicted to load at start of dashboards
# imports: tsne_points_newSeason.csv and playersNewPredicted_Final_adjMin.csv
# calls helpers
write_tsne_ready_newSeason <- function(){
  
  source("helper_functions.R")
  tsne_points <- read.csv("data/tsne_points_newSeason.csv",stringsAsFactors = FALSE)
  
  # load data
  data_tsne_sample <- read.csv("data/playersNewPredicted_Final_adjMin.csv", stringsAsFactors = FALSE) %>%
    select_if(is.character)
  # tsne_points are pre-calculated from write_tSNE_All.R and saved in data/ directory
  # using this function: tsne_points <- write_tSNE_compute_All()
  if (!nrow(data_tsne_sample)==nrow(tsne_points)){ # in case labels and coordinates have different sizes
    tsne_ready <- tsne_points
  } else {
    tsne_ready <- cbind(data_tsne_sample,tsne_points)
  }
  
  names(tsne_ready)[ncol(tsne_ready)-1] <- "x"
  names(tsne_ready)[ncol(tsne_ready)] <- "y"
  
  write.csv(tsne_ready, "data/tsne_ready_newSeason.csv", row.names = FALSE)
}

########## TEAMS ###########

# Write team stats by season. 
# Imports: playersHist.csv
write_teamStats <- function() {
  require(httr)
  require(tidyverse)
  library(rvest)
  thisYear <- substr(Sys.Date(),1,4)
  playersHist <- read.csv("data/playersHist.csv", stringsAsFactors = FALSE)
  
  ##### ALL SEASONS ########
  firstYear <- 1979
  
  ##### NEW SEASON ########
  firstYear <- thisYear
  
  # Read teams stats for all seasons
  teamStats <- data.frame()
  lastSeason <- as.numeric(substr(max(as.character(playersHist$Season)),1,4))
  for (year in firstYear:thisYear){
    url <- paste0("http://www.basketball-reference.com/leagues/NBA_",year,".html")
    # Eastern conference
    thisSeasonStats <- url %>%
      read_html() %>%
      #html_nodes(xpath='//*[@id="all_standings"]/table') %>%
      html_nodes(xpath='//*[@id="confs_standings_E"]') %>%
      html_table(fill = TRUE)
    thisSeasonStats_E <- thisSeasonStats[[1]]
    names(thisSeasonStats_E)[1] <- "Team"
    # Western conference
    thisSeasonStats <- url %>%
      read_html() %>%
      #html_nodes(xpath='//*[@id="all_standings"]/table') %>%
      html_nodes(xpath='//*[@id="confs_standings_W"]') %>%
      html_table(fill = TRUE)
    thisSeasonStats_W <- thisSeasonStats[[1]]
    names(thisSeasonStats_W)[1] <- "Team"
    # All together
    thisSeasonStats <- bind_rows(thisSeasonStats_E,thisSeasonStats_W)
    #thisSeasonStats <- thisSeasonStats[2:nrow(thisSeasonStats),1:8]
    #names(thisSeasonStats) <- c("Team",thisSeasonStats[1,2:ncol(thisSeasonStats)])
    #thisSeasonStats <- thisSeasonStats[-1,]
    #thisSeasonStats <- thisSeasonStats[!(thisSeasonStats$W=="W"),]
    #thisSeasonStats <- thisSeasonStats[!grepl("division",tolower(thisSeasonStats[,1])),]
    thisSeasonStats <- select(thisSeasonStats, -`W/L%`, -GB)
    thisSeasonStats <- mutate_each(thisSeasonStats, funs(as.numeric), -Team)
    thisSeasonStats$Team <- gsub("\\*?\\([0-9]+\\)","",thisSeasonStats$Team)
    thisSeasonStats$Team <- gsub("*","",trim,fixed = TRUE)
    thisSeasonStats$Team <- trimws(thisSeasonStats$Team)
    names(thisSeasonStats) <- gsub("PS/G","PTS",names(thisSeasonStats))
    names(thisSeasonStats) <- gsub("PA/G","PTSA",names(thisSeasonStats))
    # trim <- gsub("\\*?\\([0-9]+\\)","",thisSeasonStats$Team)
    # trim <- gsub("*","",trim,fixed = TRUE)
    # blank <- substr(trim[1],nchar(trim[1]),nchar(trim[1]))
    # trim <- gsub(blank,"",trim)
    # thisSeasonStats$Team <- trim
    thisSeasonStats$Season <- paste0(year-1,"-",year)
    if (nrow(teamStats)>0) {
      teamStats <- bind_rows(teamStats,thisSeasonStats)
    } else {
      teamStats <- thisSeasonStats
    }
  }
  
  if (firstYear==thisYear) {
    teamStatsOLD <- read.csv("data/teamStats.csv", stringsAsFactors = FALSE)
    teamStats <- bind_rows(teamStatsOLD,teamStats)
  } 
  
  write.csv(teamStats, "data/teamStats.csv",row.names = FALSE)
}

# compute tsne for teams
# imports: playersNewPredicted_Final_adjPer.csv
# calls helpers
write_tsne_points_teams <- function(){
  
  require(tsne)
  playersPredictedStats_adjPer <- read.csv("data/playersNewPredicted_Final_adjPer.csv", stringsAsFactors = FALSE)
  teamStats <- .computeTeamStats(data = playersPredictedStats_adjPer)
  data_tsne_sample <- teamStats %>% 
    select_if(is.numeric) %>%
    mutate_all(function(x) (x-min(x))/(max(x)-min(x)))
  
  if (nrow(data_tsne_sample)>0){
    num_iter <- 1500
    max_num_neighbors <- 2
    set.seed(456) # reproducitility
    tsne_points <- tsne(data_tsne_sample, 
                        max_iter=as.numeric(num_iter), 
                        perplexity=as.numeric(max_num_neighbors), 
                        epoch=100)
    plot(tsne_points)
  } else {
    tsne_points <- c()
  }
  write.csv(tsne_points, "data/tsne_points_teams.csv",row.names = FALSE)
}

# put together tsne_ready predicted to load at start of dashboards
# imports: tsne_points_teams.csv and playersNewPredicted_Final_adjPer.csv
# calls helpers
write_tsne_ready_teams <- function(){
  
  source("helper_functions.R")
  tsne_points <- read.csv("data/tsne_points_teams.csv",stringsAsFactors = FALSE)
  
  # load data
  playersPredictedStats_adjPer <- read.csv("data/playersNewPredicted_Final_adjPer.csv", stringsAsFactors = FALSE)
  data_tsne_sample <- .computeTeamStats(data = playersPredictedStats_adjPer) %>%
    select_if(is.character)
  # tsne_points are pre-calculated from write_tSNE_All.R and saved in data/ directory
  # using this function: tsne_points <- write_tSNE_compute_All()
  if (!nrow(data_tsne_sample)==nrow(tsne_points)){ # in case labels and coordinates have different sizes
    tsne_ready <- tsne_points
  } else {
    tsne_ready <- cbind(data_tsne_sample,tsne_points)
  }
  
  names(tsne_ready)[ncol(tsne_ready)-1] <- "x"
  names(tsne_ready)[ncol(tsne_ready)] <- "y"
  
  write.csv(tsne_ready, "data/tsne_ready_teams.csv", row.names = FALSE)
}

########## ROOKIES ###########

# Historical drafted rookies
write_rookiesHist <- function(){
  
  lastDraft <- as.numeric(substr(max(as.character(playersHist$Season)),1,4)) + 1
  
  firstDraft <- 1979
  rookiesHist <- data.frame()
  
  for (draftYear in firstDraft:lastDraft) {
    
    url <- paste0("http://www.basketball-reference.com/draft/NBA_",draftYear,".html")
    thisSeasonDraft <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="stats"]') %>%
      html_table(fill = TRUE)
    thisSeasonDraft <- thisSeasonDraft[[1]]
    names(thisSeasonDraft) <- thisSeasonDraft[1,]
    thisSeasonDraft <- as.data.frame(thisSeasonDraft[-1,])
    thisSeasonDraft <- thisSeasonDraft[,1:10]
    thisSeasonDraft <- dplyr::select(thisSeasonDraft, Pick = Pk, Team = Tm, Player, College)
    thisSeasonDraft <- thisSeasonDraft[which(!(thisSeasonDraft$Pick=="" | thisSeasonDraft$Pick=="Pk")),]
    thisSeasonDraft <- mutate(thisSeasonDraft, Season = draftYear)
    if (nrow(rookiesHist)>0){
      rookiesHist <- bind_rows(rookiesHist,thisSeasonDraft)
    } else{
      rookiesHist <- thisSeasonDraft
    }
  }
  write.csv(rookiesHist, "data/rookiesHist.csv",row.names = FALSE)
}

# writer college players historical data
write_collegePlayersHist <- function(){
  # Read stats from college players and match to drafted players
  # query college players who played at least col_G games. Min per games not recorded before 2009
  col_G <- 15
  num_pages <- 30
  # First 100 sorted desc by Total Points: 
  # http://www.sports-reference.com/cbb/play-index/psl_finder.cgi?request=1&match=single&year_min=1994&year_max=1994&conf_id=&school_id=&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y&pos_is_g=Y&pos_is_gf=Y&pos_is_f=Y&pos_is_fg=Y&pos_is_fc=Y&pos_is_c=Y&pos_is_cf=Y&games_type=A&qual=&c1stat=g&c1comp=gt&c1val=15&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=pts
  # subsequent players in batches of 100:
  # http://www.sports-reference.com/cbb/play-index/psl_finder.cgi?request=1&match=single&year_min=1994&year_max=1994&conf_id=&school_id=&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y&pos_is_g=Y&pos_is_gf=Y&pos_is_fg=Y&pos_is_f=Y&pos_is_fc=Y&pos_is_cf=Y&pos_is_c=Y&games_type=A&qual=&c1stat=g&c1comp=gt&c1val=15&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=pts&order_by_asc=&offset=100
  
  lastDraft <- as.numeric(substr(max(as.character(playersHist$Season)),1,4)) + 1
  
  firstDraft <- 1994
  collegePlayersHist <- data.frame()
  
  for (season in (firstDraft-1):(lastDraft-1)){
    
    url <- paste0("http://www.sports-reference.com/cbb/play-index/psl_finder.cgi?request=1&match=single&year_min=",
                  season,"&year_max=",season,"&conf_id=&school_id=&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y&pos_is_g=Y&pos_is_gf=Y&pos_is_f=Y&pos_is_fg=Y&pos_is_fc=Y&pos_is_c=Y&pos_is_cf=Y&games_type=A&qual=&c1stat=g&c1comp=gt&c1val=",
                  col_G,"&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=pts")
    
    thisCollege <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="stats"]') %>%
      html_table(fill = TRUE)
    thisCollege <- thisCollege[[1]]
    names(thisCollege) <- thisCollege[1,]
    thisCollege <- thisCollege[-1,]
    collegePlayers <- thisCollege[which(!(thisCollege$Rk=="" | thisCollege$Rk=="Rk")),]
    collegePlayers$Rk <- as.numeric(collegePlayers$Rk)
    
    for (page in 1:(num_pages-1)){ # read a total of num_pages*100 college players
      url <- paste0("http://www.sports-reference.com/cbb/play-index/psl_finder.cgi?request=1&match=single&year_min=",
                    season,"&year_max=",season,"&conf_id=&school_id=&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y&pos_is_g=Y&pos_is_gf=Y&pos_is_f=Y&pos_is_fg=Y&pos_is_fc=Y&pos_is_c=Y&pos_is_cf=Y&games_type=A&qual=&c1stat=g&c1comp=gt&c1val=",
                    col_G,"&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=pts&order_by_asc=&offset=",
                    page*100)
      thisCollege <- url %>%
        read_html() %>%
        html_nodes(xpath='//*[@id="stats"]') %>%
        html_table(fill = TRUE)
      thisCollege <- thisCollege[[1]]
      names(thisCollege) <- thisCollege[1,]
      thisCollege <- thisCollege[-1,]
      thisCollege <- thisCollege[which(!(thisCollege$Rk=="" | thisCollege$Rk=="Rk")),]
      thisCollege$Rk <- as.numeric(thisCollege$Rk)
      collegePlayers <- rbind(collegePlayers,thisCollege)
    }
    
    collegePlayers <- mutate(collegePlayers, Season = season)
    names(collegePlayers) <- gsub("2","X2",names(collegePlayers))
    names(collegePlayers) <- gsub("3","X3",names(collegePlayers))
    
    if (nrow(collegePlayersHist)>0){
      collegePlayersHist <- rbind(collegePlayersHist,collegePlayers)
    } else{
      collegePlayersHist <- collegePlayers
    }
  }
  write.csv(collegePlayersHist, "data/collegePlayersHist.csv", row.names = FALSE)
}

# Merge drafted players with college players.
# Imports rookiesHist.csv and collegePlayersHist.csv
write_rookieStatsHist <- function(){
  
  rookiesHist <- read.csv("data/rookiesHist.csv", stringsAsFactors = FALSE)
  collegePlayersHist <- read.csv("data/collegePlayersHist.csv", stringsAsFactors = FALSE)
  
  rookieStatsHist <- merge(rookiesHist, collegePlayersHist, by = c("Player","Season"))
  
  write.csv(rookieStatsHist, "data/rookieStatsHist.csv", row.names = FALSE)
}

# computes tsne for rookies
# uses helper .tSNE_prepareRookies()
write_tsne_pointsRookies <- function(){
  
  num_iter <- 400
  max_num_neighbors <- 10
  
  data_tsne <- .tSNE_prepareRookies()
  # calculate tsne-points Dimensionality reduction to 2-D
  if (nrow(data_tsne)>0){
    set.seed(456) # reproducitility
    tsne_points <- tsne(data_tsne[,-c(1:3)], 
                        max_iter=as.numeric(num_iter), 
                        perplexity=as.numeric(max_num_neighbors), 
                        epoch=num_iter)
  } else {
    tsne_points <- c()
  }
  write.csv(tsne_points, "data/tsne_pointsRookies.csv",row.names = FALSE)
}

# write rookies from last draft
write_rookiesDraft <- function(){
  
  rookies <- data.frame()
  lastDraft <- as.numeric(substr(max(as.character(playersHist$Season)),1,4)) + 1
  
  url <- paste0("http://www.basketball-reference.com/draft/NBA_",lastDraft,".html")
  
  thisSeasonDraft <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="stats"]') %>%
    html_table(fill = TRUE)
  thisSeasonDraft <- thisSeasonDraft[[1]]
  names(thisSeasonDraft) <- thisSeasonDraft[1,]
  thisSeasonDraft <- as.data.frame(thisSeasonDraft[-1,])
  rookies <- thisSeasonDraft[,1:10]
  rookies <- dplyr::select(rookies, Pick = Pk, Team = Tm, Player, College)
  rookies <- rookies[which(!(rookies$Pick=="" | rookies$Pick=="Pk")),]
  
  # Correct spelling errors 2016 draft
  # rookies[grepl("Chris",rookies$Player),]$Player <- "Marquese Chriss"
  # rookies[grepl("Dami",rookies$Player),]$Player <- "Damian Jones"
  # rookies[grepl("Zimmerm",rookies$Player),]$Player <- "Stephen Zimmerman Jr."
  
  write.csv(rookies, "data/rookiesDraft.csv",row.names = FALSE)
  
}

# write historical drafts
write_rookiesDraftHist <- function(){
  
  rookiesDraftHist <- data.frame()
  lastDraft <- as.numeric(substr(max(as.character(playersHist$Season)),1,4)) + 1
  
  for (d in (lastDraft-20):lastDraft) {
    
    url <- paste0("http://www.basketball-reference.com/draft/NBA_",d,".html")
    
    thisSeasonDraft <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="stats"]') %>%
      html_table(fill = TRUE)
    thisSeasonDraft <- thisSeasonDraft[[1]]
    names(thisSeasonDraft) <- thisSeasonDraft[1,]
    thisSeasonDraft <- as.data.frame(thisSeasonDraft[-1,])
    rookies <- thisSeasonDraft[,1:10]
    rookies <- dplyr::select(rookies, Pick = Pk, Team = Tm, Player, College)
    rookies <- rookies[which(!(rookies$Pick=="" | rookies$Pick=="Pk")),]
    rookies <- mutate(rookies, Year = as.character(d))
    if (nrow(rookiesDraftHist)>0) {
      rookiesDraftHist <- rbind(rookiesDraftHist,rookies)
    } else {
      rookiesDraftHist <- rookies
    }
  }
  
  write.csv(rookiesDraftHist, "data/rookiesDraftHist.csv",row.names = FALSE)
}

# write all rookies (all draft rounds and non drafted)
write_rookies <- function(){
  
  rookies <- data.frame()
  newSeason <- as.numeric(substr(max(as.character(playersHist$Season)),1,4)) + 2
  teamList <- unique(filter(playersHist,Season == paste0(newSeason-2,"-",newSeason-1))$Tm)
  teamList <- teamList[which(!(teamList == "TOT"))]
  for (team in teamList) {
    
    url <- paste0("http://www.basketball-reference.com/teams/",team,"/",newSeason,".html")
    #https://www.basketball-reference.com/teams/NYK/2018.html
    thisSeasonRookies <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="roster"]') %>%
      html_table(fill = TRUE)
    thisSeasonRookies <- thisSeasonRookies[[1]]
    thisSeasonRookies <- thisSeasonRookies[,names(thisSeasonRookies)[which(nchar(names(thisSeasonRookies))>0)]]
    thisRookies <- filter(thisSeasonRookies, Exp == "R")
    thisRookies <- mutate(thisRookies, 
                          Age = newSeason - as.numeric(substr(`Birth Date`,nchar(`Birth Date`)-4,nchar(`Birth Date`))))
    thisRookies <- dplyr::select(thisRookies, Player, Age, College) %>%
      mutate(Tm = team)
    
    if (nrow(rookies)>0) {
      rookies <- rbind(rookies, thisRookies)
    } else {
      rookies <- thisRookies
    }
  }
  
  write.csv(rookies, "data/rookies.csv",row.names = FALSE)
}

# write college players stats from last season
# imports playersHist.csv
write_collegePlayers <- function(){
  # Read stats from college players and match to drafted players
  # query college players who played at least col_G games and col_MP min/game last season
  from <- as.numeric(substr(max(as.character(playersHist$Season)),1,4)) - 1
  to <- as.numeric(substr(max(as.character(playersHist$Season)),1,4)) + 1
  collegePlayers <- data.frame()
  
  #for (lastDraft in from:to){
  col_G <- 15
  col_MP <- 7
  num_pages <- 90
  # First 100 sorted desc by PER: 
  # http://www.sports-reference.com/cbb/play-index/psl_finder.cgi?request=1&match=single&year_min=2016&year_max=2016&conf_id=&school_id=&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y&pos_is_g=Y&pos_is_gf=Y&pos_is_fg=Y&pos_is_f=Y&pos_is_fc=Y&pos_is_cf=Y&pos_is_c=Y&games_type=A&qual=&c1stat=g&c1comp=gt&c1val=25&c2stat=mp_per_g&c2comp=gt&c2val=20&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=per
  # subsequent players in batches of 100:
  # http://www.sports-reference.com/cbb/play-index/psl_finder.cgi?request=1&match=single&year_min=2016&year_max=2016&conf_id=&school_id=&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y&pos_is_g=Y&pos_is_gf=Y&pos_is_fg=Y&pos_is_f=Y&pos_is_fc=Y&pos_is_cf=Y&pos_is_c=Y&games_type=A&qual=&c1stat=g&c1comp=gt&c1val=25&c2stat=mp_per_g&c2comp=gt&c2val=20&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=per&order_by_asc=&offset=100
  #collegePlayers <- data.frame()
  #lastDraft <- as.numeric(substr(max(as.character(playersHist$Season)),1,4)) + 1
  
  url <- paste0("http://www.sports-reference.com/cbb/play-index/psl_finder.cgi?request=1&match=single&year_min=",
                from,"&year_max=",to,"&conf_id=&school_id=&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y&pos_is_g=Y&pos_is_gf=Y&pos_is_fg=Y&pos_is_f=Y&pos_is_fc=Y&pos_is_cf=Y&pos_is_c=Y&games_type=A&qual=&c1stat=g&c1comp=gt&c1val=",
                col_G,"&c2stat=mp_per_g&c2comp=gt&c2val=",col_MP,"&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=per")
  
  thisCollege <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="stats"]') %>%
    html_table(fill = TRUE)
  thisCollege <- thisCollege[[1]]
  names(thisCollege) <- thisCollege[1,]
  thisCollege <- thisCollege[-1,]
  collegePlayers <- thisCollege[which(!(thisCollege$Rk=="" | thisCollege$Rk=="Rk")),]
  
  for (page in 1:(num_pages-1)){ # read a total of num_pages*100 college players
    print(paste0("processing year: ",lastDraft, " page: ",page))
    url <- paste0("http://www.sports-reference.com/cbb/play-index/psl_finder.cgi?request=1&match=single&year_min=",
                  from,"&year_max=",to,"&conf_id=&school_id=&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y&pos_is_g=Y&pos_is_gf=Y&pos_is_fg=Y&pos_is_f=Y&pos_is_fc=Y&pos_is_cf=Y&pos_is_c=Y&games_type=A&qual=&c1stat=g&c1comp=gt&c1val=",
                  col_G,"&c2stat=mp_per_g&c2comp=gt&c2val=",col_MP,"&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=per&order_by_asc=&offset=",
                  page*100)
    thisCollege <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="stats"]') %>%
      html_table(fill = TRUE)
    thisCollege <- thisCollege[[1]]
    names(thisCollege) <- thisCollege[1,]
    thisCollege <- thisCollege[-1,]
    thisCollege <- thisCollege[which(!(thisCollege$Rk=="" | thisCollege$Rk=="Rk")),]
    collegePlayers <- bind_rows(collegePlayers,thisCollege)
  }
  
  write.csv(collegePlayers, "data/collegePlayers.csv", row.names = FALSE)
}

# Merge drafted players with college players
# imports: rookies.csv and collegePlayers.csv
write_rookieStats_europePlayers <- function(){
  
  rookies <- read.csv("data/rookies.csv", stringsAsFactors = FALSE) # from writeAllRookies
  collegePlayers <- read.csv("data/collegePlayers.csv", stringsAsFactors = FALSE) %>% # from write_CollegePlayers
    group_by(Player) %>%
    summarise_if(is.numeric, mean)
  # Correct spelling errors 2017 draft
  collegePlayers[grepl("Nazareth Mitrou-Long",collegePlayers$Player),]$Player <- "Naz Mitrou-Long"
  collegePlayers[grepl("Royce O'Neale",collegePlayers$Player),]$Player <- "Royce O'Neal"
  collegePlayers[grepl("Jacorey Williams",collegePlayers$Player),]$Player <- "JaCorey Williams"
  collegePlayers[grepl("Andrew White III",collegePlayers$Player),]$Player <- "Andrew White"
  collegePlayers[grepl("TJ Leaf",collegePlayers$Player),]$Player <- "T.J. Leaf"
  collegePlayers[grepl("Frank Mason",collegePlayers$Player),]$Player <- "Frank Mason III"
  collegePlayers[grepl("Akim Mitchell",collegePlayers$Player),]$Player <- "Akil Mitchell"
  
  
  #collegePlayers[grepl("Dennis Smith",collegePlayers$Player),]$Player <- "Dennis Smith Jr."
  #ollegePlayers[grepl("Leaf",collegePlayers$Player),]$Player <- "TJ Leaf"
  
  rookieStats <- merge(rookies, collegePlayers, by = "Player",all.x=TRUE) %>% 
    mutate(Age = as.character(Age)) %>%
    group_by(Player) %>% summarise_if(is.numeric,funs(mean(.,na.rm=TRUE))) %>% 
    left_join(rookies, c("Player"="Player")) %>%
    mutate(Age = as.numeric(Age))
  
  lastDraft <- as.numeric(substr(max(as.character(playersHist$Season)),1,4)) + 1
  
  rookieReady <- filter(rookieStats, !is.nan(G)) %>% select(one_of(names(playersHist)),College) %>%
    mutate(Season = lastDraft)
  rookieLeftout <- filter(rookieStats, is.nan(G)) %>% select(Player,Age,College,Tm)
  #billy-yakuba-ouattara
  
  # Find stats from european players drafted
  europePlayers <- data.frame()
  
  require(httr)
  for (i in 1:nrow(rookieLeftout)){
    
    if (rookieLeftout$College[i]==""){
      thisPlayer <- as.character(rookieLeftout$Player[i])
      name_edited <- tolower(thisPlayer)
      name_edited <- gsub(" ","-",name_edited)
      url <- paste0("https://www.basketball-reference.com/euro/players/",name_edited,"-1.html")
      table_type <- "ALL" # ALL,EUR,CLU
      if (status_code(GET(url))==200){ #european player
        thisEurope <- url %>%
          read_html() %>%
          html_nodes(xpath='//*[@id="per_gameALL0"]') %>%
          html_table(fill = TRUE)
        if (length(thisEurope)==0){ # try euroleague stats if total are unavailable
          thisEurope <- url %>%
            read_html() %>%
            html_nodes(xpath='//*[@id="per_gameEUR0"]') %>%
            html_table(fill = TRUE)
          table_type <- "EUR"
        }
        if (length(thisEurope)==0){ # try club stats if total or euroleague are unavailable
          thisEurope <- url %>%
            read_html() %>%
            html_nodes(xpath='//*[@id="per_gameCLU0"]') %>%
            html_table(fill = TRUE)
          table_type <- "CLU"
        }
        if (length(thisEurope)>0){
          rookieLeftout$College[i] <- "Europe"
          thisEurope <- thisEurope[[1]]
          print(paste0("Processing: ",thisPlayer))
          if (table_type == "CLU") names(thisEurope)[4] <- "Country"
          thisEurope <- thisEurope %>%
            filter(G == max(G)) %>%
            select(-contains("Club"), -contains("Country")) %>%
            mutate(Player = thisPlayer) %>%
            head(1)
          thisEurope$Tm <- rookieLeftout$Tm[i]
          thisEurope$College <- rookieLeftout$College[i]
          thisEurope$Age <- rookieLeftout$Age[i]
          europePlayers <- rbind(europePlayers,thisEurope)
        } else { # international or european without stats
          rookieLeftout$College[i] <- "International"
        }
      } else { # college player that didn't find a match in collegePlayers. Find the reason!
        rookieLeftout$College[i] <- "International"
      }
    }
  }
  
  rookieLeftout <- filter(rookieLeftout, !(College == "Europe")) %>% select(Player,Age,College,Tm)
  rookieLeftout$Season <- lastDraft
  
  # For international players or non-matched college players use averages of their respective groups for their stats
  averageCollegeRookie <- rookieReady %>%
    summarise_if(is.numeric, function(x) mean(x,na.rm = TRUE)) %>%
    select(-Season)
  
  names(europePlayers) <- gsub("%",".",names(europePlayers), fixed = TRUE)
  names(europePlayers) <- gsub("2","X2",names(europePlayers),fixed = TRUE)
  names(europePlayers) <- gsub("3","X3",names(europePlayers),fixed = TRUE)
  europePlayers <- select(europePlayers, Player, everything(), 
                          -c(`League(s)`,FG.,X3P.,X2P.,FT.))
  
  averageEuropeRookie <- europePlayers %>%
    summarise_if(is.numeric,function(x) mean(x,na.rm = TRUE))
  
  rookieLeftoutStats <- data.frame()
  for (i in 1:nrow(rookieLeftout)) {
    if (rookieLeftout$College[i] == "International") {
      thisRookie <- cbind(rookieLeftout[i,],averageEuropeRookie)
    } else {
      thisRookie <- cbind(rookieLeftout[i,],averageCollegeRookie)
    }
    rookieLeftoutStats <- bind_rows(rookieLeftoutStats,thisRookie)
  }
  
  rookieReady <- select(rookieReady, -Season)
  rookieLeftoutStats <- select(rookieLeftoutStats, -Season)
  europePlayers <- select(europePlayers, -Season)
  rookieStatsFinal <- bind_rows(rookieReady,rookieLeftoutStats,europePlayers) %>%
    mutate(FG. = ifelse(FGA == 0,0,FG/FGA), X3P. = ifelse(X3PA == 0,0,X3P/X3PA), 
           X2P. = ifelse(X2PA == 0,0,X2P/X2PA), FT. = ifelse(FTA == 0,0,FT/FTA), 
           Season = paste0(lastDraft,"-",lastDraft+1)) %>%
    mutate(Age = as.integer(Age))
  
  write.csv(rookieStatsFinal, "data/rookieStats.csv", row.names = FALSE)
  write.csv(europePlayers, "data/europePlayers.csv", row.names = FALSE)
}

# Once per game stats have been compiled for all rookies (college and international) compute their
# per-minute stats
# imports rookieStats.csv
write_rookieEfficientStats <- function() {
  
  rookieStats <- read.csv("data/rookieStats.csv", stringsAsFactors = FALSE)
  # In college and Europe they play 40-min game. I will calculate their pre-minute stats
  # based on a 48-min game as the "price" for being a rookie in the NBA
  rookieEffStats <- rookieStats %>%
    group_by(Player) %>%
    mutate(effMin = MP/3936, effFG = FG/(3936*effMin),
           effFGA = FGA/(3936*effMin),eff3PM = X3P/(3936*effMin),eff3PA = X3PA/(3936*effMin),
           eff2PM = X2P/(3936*effMin),eff2PA = X2PA/(3936*effMin),
           effFTM = FT/(3936*effMin),effFTA = FTA/(3936*effMin),
           effORB = ORB/(3936*effMin),effDRB = DRB/(3936*effMin),
           effTRB = TRB/(3936*effMin),effAST = AST/(3936*effMin),
           effSTL = STL/(3936*effMin),effBLK = BLK/(3936*effMin),
           effTOV = TOV/(3936*effMin),effPF = PF/(3936*effMin),
           effPTS = PTS/(3936*effMin)) %>%
    dplyr::select(Player,Age,Season,Tm,FGPer = FG.,FG3Per = X3P., FG2Per = X2P., effFGPer = eFG.,
                  FTPer = FT., starts_with("eff"),
                  -G,-MP,FG,-FGA,-X3P,-X3PA,-X2P,-X2PA,-FG,-FTA,-ORB,-DRB,-TRB,-AST,
                  -BLK,-TOV,-PF,-FT,-STL,-PTS)
  
  # Impute NAs by 0. If NA means no shot attempted, ie, 
  # either the player didn't play enough time or is really bad at this particular type of shot.
  for (i in 5:ncol(rookieEffStats)){
    rookieEffStats[is.na(rookieEffStats[,i]),i] <- 0
  }
  
  rookieEffStats <- as.data.frame(rookieEffStats)
  
  write.csv(rookieEffStats, "data/rookieEfficientStats.csv", row.names = FALSE)
}

########## SEASON ###########

# Real season schedule from basketball-reference
write_realSeasonSchedule <- function(){
  
  library(httr)
  library(rvest)
  
  # If not new data yet (transfers not finished so teams rosters not final), -----------------------------------------
  dataNewSeason <- FALSE
  if (dataNewSeason==FALSE){
    # use last season's as new data, removing PTS & PTSA
    team_statsNew <- team_stats %>%
      filter(Season == max(as.character(Season))) %>%
      mutate(W = 0, L = 0, PTS = 0, PTSA = 0, SRS = 0, 
             Season = paste0(as.numeric(substr(Season,1,4))+1,"-",as.numeric(substr(Season,1,4))+2)) %>%
      distinct(Team, .keep_all=TRUE)
    # same for players
    playersNew <- playersHist %>%
      filter(Season == max(as.character(Season))) %>%
      mutate(Season = as.factor(paste0(as.numeric(substr(Season,1,4))+1,"-",as.numeric(substr(Season,1,4))+2)))
  }
  
  thisSeason <- substr(as.character(playersNew$Season[1]),6,9)
  months_list <- c("october","november","december",'january',"february","march","april")
  
  season_schedule <- data.frame()
  for (month in months_list){
    
    url <- paste0("http://www.basketball-reference.com/leagues/NBA_",
                  thisSeason,"_games-",month,".html")
    
    thisMonthSchedule <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="schedule"]') %>%
      html_table(fill = TRUE)
    thisMonthSchedule <- thisMonthSchedule[[1]]
    season_schedule <- bind_rows(season_schedule,thisMonthSchedule)
  }
  season_schedule <- dplyr::select(season_schedule, Date,StartTime=`Start (ET)`,
                                   teamH=`Home/Neutral`, teamA=`Visitor/Neutral`)
  
  
  # Convert team names to team codes
  season_schedule <- merge(season_schedule,team_statsNew[,c("Team","teamCode")],by.x="teamH",by.y="Team",all.x=TRUE)
  season_schedule <- merge(season_schedule,team_statsNew[,c("Team","teamCode")],by.x="teamA",by.y="Team",all.x=TRUE)
  season_schedule <- season_schedule %>%
    dplyr::select(-teamA,-teamH, teamH = teamCode.x, teamA = teamCode.y) %>%
    mutate(Date = paste0(substr(Date,nchar(Date)-3,nchar(Date)),"-",
                         substr(Date,6,8),"-",substr(Date,10,11))) %>%
    mutate(Date = ifelse(grepl(",",Date),paste0(substr(Date,1,nchar(Date)-2),"0",
                                                substr(Date,nchar(Date)-1,nchar(Date)-1)),Date)) %>%
    mutate(Date = as.Date(Date,"%Y-%B-%d"), 
           StartTime = ifelse(nchar(StartTime)<8,paste0("0",StartTime),StartTime)) %>%
    arrange(Date,StartTime)
  
  write.csv(season_schedule,"data/realSeasonSchedule.csv",row.names = FALSE)
}

# Pre-calculate all games scores for 1 full season
# imports playersHist.csv
write_gameScores <- function() {
  
  require(httr)
  require(tidyverse)
  library(rvest)
  thisYear <- substr(Sys.Date(),1,4)
  playersHist <- read.csv("data/playersHist.csv", stringsAsFactors = FALSE)
  
  ##### ALL SEASONS ########
  firstYear <- 2000
  
  # Read teams stats for all seasons
  teamPoints <- data.frame()
  
  for (year in firstYear:thisYear){
    for (month in c("october","november","december","january","february","march","april","may","june")) {
      
      url <- paste0("https://www.basketball-reference.com/leagues/NBA_",year,"_games-",month,".html")
      if (status_code(GET(url)) == 200) {
        
        thisMonthStats <- url %>%
          read_html() %>%
          #html_nodes(xpath='//*[@id="all_standings"]/table') %>%
          html_nodes(xpath='//*[@id="schedule"]') %>%
          html_table(fill = TRUE)
        thisMonthStats <- thisMonthStats[[1]]
        thisMonthStats <- thisMonthStats[,c(1,4,6)]
        names(thisMonthStats) <- c("Date","pts_away","pts_home")
        
        if (nrow(teamPoints) > 0) {
          teamPoints <- rbind(teamPoints,thisMonthStats)
        } else {
          teamPoints <- thisMonthStats
        }
        
      }
      
    }
  }
  write.csv(teamPoints, "data/gameScores.csv", row.names = FALSE)
}

########## IMAGES ###########

# write team logo images
write_teamLogos <- function(){
  
  # From: https://metrumresearchgroup.github.io/slickR/
  library(svglite)
  #library(lattice)
  #library(ggplot2)
  library(rvest) 
  library(httr)
  library(reshape2)
  library(dplyr)
  #library(htmlwidgets)
  #library(slickR)
  library(xml2)
  
  ## Download Team logos --------
  nbaTeams=c("ATL","BOS","BKN","CHA","CHI","CLE","DAL","DEN","DET","GSW",
             "HOU","IND","LAC","LAL","MEM","MIA","MIL","MIN","NO","NYK",
             "OKC","ORL","PHI","PHX","POR","SAC","SA","TOR","UTAH","WSH")
  
  teamImg=sprintf("https://i.cdn.turner.com/nba/nba/.element/img/4.0/global/logos/512x512/bg.white/svg/%s.svg",nbaTeams)
  # use my codes
  nbaTeams <- gsub("BKN","BRK",nbaTeams)
  nbaTeams <- gsub("CHA","CHO",nbaTeams)
  nbaTeams <- gsub("PHX","PHO",nbaTeams)
  
  for (i in 1:length(teamImg)) {
    
    download.file(teamImg[i], destfile = paste0("images/",nbaTeams[i],".svg"))
  }
  
}

# write players pictures and save players pics codes from ESPN.com
write_playersPics <- function(){
  
  # From: https://metrumresearchgroup.github.io/slickR/
  library(svglite)
  #library(lattice)
  #library(ggplot2)
  library(rvest) 
  library(httr)
  library(reshape2)
  library(dplyr)
  #library(htmlwidgets)
  #library(slickR)
  library(xml2)
  
  ## Download Team logos --------
  nbaTeams=c("ATL","BOS","BKN","CHA","CHI","CLE","DAL","DEN","DET","GSW",
             "HOU","IND","LAC","LAL","MEM","MIA","MIL","MIN","NO","NYK",
             "OKC","ORL","PHI","PHX","POR","SAC","SA","TOR","UTAH","WSH")
  
  playersPredictedStats_adjPer <- read.csv("data/playersNewPredicted_Final_adjPer.csv", stringsAsFactors = FALSE)
  players_pics <- data.frame()
  
  for (t in nbaTeams) {
    url <- paste0("http://www.espn.com/nba/team/roster/_/name/",tolower(t))
    if (status_code(GET(url))==200) {
      thisTeam <- url %>%
        read_html() %>%
        html_nodes(xpath='//*[@id="my-players-table"]/div[2]/div/table[1]') %>%
        html_children() %>%
        html_children() %>%
        html_children() %>%
        html_attr('href')
      
      thisRoster <- thisTeam[6:length(thisTeam)]
      thisRoster <- gsub("http://www.espn.com/nba/player/_/id/","",thisRoster)
      thisPair <- strsplit(thisRoster,"/",fixed = TRUE) %>%
        as.data.frame(stringsAsFactors = FALSE) %>%
        t() %>%
        as.data.frame(stringsAsFactors = FALSE)
      
      names(thisPair) <- c("player_code","player_name")
      
      if (nrow(players_pics)>0){
        players_pics <- rbind(players_pics,thisPair)
      } else {
        players_pics <- thisPair
      }
    }  
  }
  write.csv(players_pics, "data/players_pics_codes.csv", row.names = FALSE)
  
  for (p in 103:nrow(players_pics)) {
    
    thisPic <- paste0("http://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/",
                      players_pics[p,1],".png&w=350&h=254")
    if (status_code(GET(thisPic))==200) {
      download.file(thisPic,destfile = paste0("images/",players_pics[p,2],".png"))
    }
  } 
}

