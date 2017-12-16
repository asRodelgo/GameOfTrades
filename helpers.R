#### Helpers ------------------------
# Here is where all the logic goes in the form of functions
#
# Alberto Sanchez Rodelgo
#### --------------------------------

########## SEASON ###########

# ----------  Simulate an artificial season schedule
.seasonSchedule <- function(){
  
  # Compute season fixtures ---------------
  # Rules:
  # Each team plays 82 regular season games
  # Of those, 52 against 14 same conference teams
  # Will play 10 of those teams 4 times (2 home + 2 away)
  # Will play remaining 4, 3 times (randomize between: 2 at home or 1 at home)
  # Of the 30 games against non-conference teams, will play 2 games each, 1 home, 1 away.
  # ---------------------------------------
  
  set.seed(111)
  teamsReordered <- sample(conferences$TeamCode)
  
  schedule <- matrix(nrow=30,ncol=30,dimnames = list(teamsReordered,teamsReordered))
  
  for (i in 1:29){
    count3games <- length(schedule[which(schedule[i,]+schedule[,i]==3)]) # each team plays 3 games against 4 of their fellow conference teams, 4 against the rest
    limit_home <- 0 # Out of those 3 games, 1 has to be away (twice), and 2 away (twice)
    for (j in (i+1):30){
      thisTeam <- dimnames(schedule)[[1]][i]
      vsTeam <- dimnames(schedule)[[2]][j]
      thisConf <- filter(conferences, TeamCode == thisTeam)$Conference
      vsConf <- filter(conferences, TeamCode == vsTeam)$Conference
      thisConfTeams <- filter(conferences, Conference == thisConf)$TeamCode
      if (!(thisConf == vsConf)){
        schedule[i,j] <- 1
        schedule[j,i] <- 1
      } else if (count3games < 4) {
        if (limit_home < 2) {
          schedule[i,j] <- 2
          schedule[j,i] <- 1
          limit_home <- limit_home + 1
        } else {
          schedule[i,j] <- 1
          schedule[j,i] <- 2
        }
        count3games <- count3games + 1
      } else {
        schedule[i,j] <- 2
        schedule[j,i] <- 2
      }
    }
  }
  
  # set up the schedule for the regular season as if each game was a marble in an urn.
  # Sampling games from the urn without replacement
  gamesUrn <- data.frame()
  
  urn_count <- 1
  for (i in 1:30){
    for (j in 1:30){
      if (!(i==j)){
        if (schedule[i,j]==2){
          gamesUrn[urn_count,1] <- i
          gamesUrn[urn_count,2] <- j
          urn_count <- urn_count + 1
          gamesUrn[urn_count,1] <- i
          gamesUrn[urn_count,2] <- j
          urn_count <- urn_count + 1
        } else {
          gamesUrn[urn_count,1] <- i
          gamesUrn[urn_count,2] <- j
          urn_count <- urn_count + 1
        }
      }
    }
  }
  
  season_schedule <- data.frame()
  set.seed(23)
  gamesUrnSamp <- sample_frac(gamesUrn) # reorder rows randomly
  
  day <- 1
  day_teams <- c()
  for (i in 1:nrow(gamesUrnSamp)){
    # avoid one team to play twice in the same day
    if (gamesUrnSamp[i,1] %in% day_teams | gamesUrnSamp[i,2] %in% day_teams){
      day <- day + 1
      day_teams <- c(gamesUrnSamp[i,1],gamesUrnSamp[i,2])
    } else { 
      day_teams <- c(day_teams,gamesUrnSamp[i,1],gamesUrnSamp[i,2])
    }
    season_schedule[i,1] <- day
    season_schedule[i,2] <- dimnames(schedule)[[1]][gamesUrnSamp[i,1]]
    season_schedule[i,3] <- dimnames(schedule)[[1]][gamesUrnSamp[i,2]]
    
  }
  return(season_schedule)
}

# simulate multiple seasons
simulate_n_seasons <- function(num_sim = 1) {
  
  # Simulate a few seasons
  regSeasonOutcome <- .standings(real = TRUE)
  # Initialize parameters
  regSeasonAvg2 <- data.frame(
    team = regSeasonOutcome[[1]][[168]]$team,
    teamCode = regSeasonOutcome[[1]][[168]]$teamCode,
    conference = regSeasonOutcome[[1]][[168]]$conference,
    win = 0,
    lose = 0,
    win2 = 0,
    sd = 0,
    probChamp = 0)
  
  num_seasons <- num_sim
  
  for (i in 1:num_seasons){
    
    final_standings <- regSeasonOutcome[[1]][[168]]
    #playoffs <- .getPlayoffResults(final_standings) %>% mutate(round = ifelse(round == 0,1,0)) %>%
    #  group_by(teamCode) %>% summarise(round = sum(round)) %>% ungroup()
    regSeasonAvg2$win <- regSeasonAvg2$win + final_standings$win
    regSeasonAvg2$win2 <- regSeasonAvg2$win2 + (final_standings$win)^2
    #probChamp <- merge(final_standings, playoffs[,c("teamCode","round")],by="teamCode",all.x=TRUE) %>%
    #  mutate(round = ifelse(is.na(round),0,round))
    #regSeasonAvg2$probChamp <- regSeasonAvg2$probChamp + probChamp$round
    # generate a new season outcome
    regSeasonOutcome <- .standings(real = TRUE)
    # keep count
    print(paste0("iteration: ",i))
  }
  
  regSeasonAvg2$win <- regSeasonAvg2$win/num_seasons
  regSeasonAvg2$lose <- 82 - regSeasonAvg2$win
  regSeasonAvg2$win2 <- regSeasonAvg2$win2/num_seasons
  regSeasonAvg2$sd <- sqrt(regSeasonAvg2$win2 - (regSeasonAvg2$win)^2)
  
  return(regSeasonAvg2)
}

# Playoff scores --------
# compute playoff series between 2 teams (best of 7)
.computeSeries <- function(teamA,teamB){
  
  # teamA has home court advantage
  series <- data.frame()
  winA <- winB <- 0
  while (max(winA,winB)<4){ # best of 7
    if ((winA + winB) %in% c(0,1,4,6)){ # teamA home games
      thisGame <- .calculateScore(teamA,teamB)
      if (thisGame[1]>thisGame[2]){
        winA <- winA + 1
      } else {
        winB <- winB + 1
      }
    } else { # teamB home games
      thisGame <- .calculateScore(teamB,teamA)
      if (thisGame[2]>thisGame[1]){
        winA <- winA + 1
      } else {
        winB <- winB + 1
      }
    }
    series[winA+winB,1] <- thisGame[1]
    series[winA+winB,2] <- thisGame[2]
    series[winA+winB,3] <- thisGame[3]
    series[winA+winB,4] <- winA
  }
  
  return(series)
}

# compute playoffs. Input is top 8 team codes from both conferences
.computePlayoffs <- function(pEast,pWest) {
  
  seriesEast <- data.frame(teamCode=pEast, conference = "E", round=8, stringsAsFactors = FALSE)
  seriesWest <- data.frame(teamCode=pWest, conference = "W", round=8, stringsAsFactors = FALSE)
  playoffSeries <- bind_rows(seriesEast,seriesWest)
  
  for (conf in c("E","W")){
    offset <- ifelse(conf=="E",0,8) # store East and West in data frame
    # Conf Quarters
    for (i in 1:4){
      thisSeries <- .computeSeries(playoffSeries[i+offset,1],playoffSeries[8-i+offset+1,1])
      if (thisSeries[nrow(thisSeries),4]==4){
        playoffSeries[16+i+offset,1] <- playoffSeries[i+offset,1]
      } else {
        playoffSeries[16+i+offset,1] <- playoffSeries[8-i+offset+1,1]
      }
      playoffSeries$round[16+i+offset] <- 4
      playoffSeries$conference[16+i+offset] <- conf
    }
    # Conf Semis
    for (i in 1:2){
      thisSeries <- .computeSeries(playoffSeries[16+i+offset,1],playoffSeries[20-i+offset+1,1])
      if (thisSeries[nrow(thisSeries),4]==4){
        playoffSeries[20+i+offset,1] <- playoffSeries[16+i+offset,1]
      } else {
        playoffSeries[20+i+offset,1] <- playoffSeries[20-i+offset+1,1]
      }
      playoffSeries$round[20+i+offset] <- 2
      playoffSeries$conference[20+i+offset] <- conf
    }
    # Conf Finals
    thisSeries <- .computeSeries(playoffSeries[21+offset,1],playoffSeries[22+offset,1])
    if (thisSeries[nrow(thisSeries),4]==4){
      playoffSeries[23+offset,1] <- playoffSeries[21+offset,1]
    } else {
      playoffSeries[23+offset,1] <- playoffSeries[22+offset,1]
    }
    playoffSeries$round[23+offset] <- 1
    playoffSeries$conference[23+offset] <- conf
  }
  playoffSeries <- filter(playoffSeries, !is.na(teamCode))
  
  # Finals
  finals <- .computeSeries(filter(playoffSeries,conference == "E", round == 1)$teamCode,
                           filter(playoffSeries,conference == "W", round == 1)$teamCode)
  playoffSeries[nrow(playoffSeries)+1,1] <- ifelse(finals[nrow(thisSeries),4]==4,
                                                   playoffSeries[23,1], playoffSeries[30,1])
  playoffSeries$round[nrow(playoffSeries)] <- 0
  playoffSeries$conference[nrow(playoffSeries)] <- conferences[conferences$TeamCode == playoffSeries[nrow(playoffSeries),1],3]
  
  return(playoffSeries)
}

# Plot playoff bracket
.getPlayoffResults <- function(standings){
  
  #standings <- standings[[1]]
  
  #pEast <- as.character(head(arrange(filter(dplyr::select(standings, teamCode,conference,win,lose), conference == "E"), desc(win/(win+lose))),8)$teamCode)
  #pWest <- as.character(head(arrange(filter(dplyr::select(standings, teamCode,conference,win,lose), conference == "W"), desc(win/(win+lose))),8)$teamCode)
  
  pEast <- as.character(head(arrange(filter(dplyr::select(standings[[tail(datesRange,1)]], teamCode,conference,win,lose), conference == "E"), desc(win/(win+lose))),8)$teamCode)
  pWest <- as.character(head(arrange(filter(dplyr::select(standings[[tail(datesRange,1)]], teamCode,conference,win,lose), conference == "W"), desc(win/(win+lose))),8)$teamCode)
  
  playoffs <- .computePlayoffs(pEast,pWest)
  return(playoffs)
  
}

# Compute playoff bracket
.playoffBracket <- function(round){
  
  # Print bracket -----------------
  x<-seq(0,220,(221/67))
  y<-0:66
  
  plot(x,y,type="l", col.axis="white", col.lab="white", bty="n", 
       axes=F, col="white")
  
  # left hand side bracket
  segments(40,c(3,11,19,27,37,45,53,61),60,c(3,11,19,27,37,45,53,61))
  segments(60,c(3,19,37,53),60,c(11,27,45,61))
  segments(60,c(7,23,41,57),80,c(7,23,41,57))
  segments(80,c(7,41),80,c(23,57))
  segments(80,c(15,49),100,c(15,49))
  # central part
  segments(100,c(27,37),120,c(27,37))
  # right hand side bracket
  segments(160,c(3,11,19,27,37,45,53,61),180,c(3,11,19,27,37,45,53,61))
  segments(160,c(3,19,37,53),160,c(11,27,45,61))
  segments(140,c(7,23,41,57),160,c(7,23,41,57))
  segments(140,c(7,41),140,c(23,57))
  segments(120,c(15,49),140,c(15,49))
  
  # team names ----------------------
  # West quarterfinals
  text(49.8,61.5,filter(playoffs,conference == "W",round==8)$teamCode[1],cex=1)
  text(49.8,53.5,filter(playoffs,conference == "W",round==8)$teamCode[8],cex=1)
  text(49.8,45.5,filter(playoffs,conference == "W",round==8)$teamCode[4],cex=1)
  text(49.8,37.5,filter(playoffs,conference == "W",round==8)$teamCode[5],cex=1)
  
  text(49.8,27.5,filter(playoffs,conference == "W",round==8)$teamCode[2],cex=1)
  text(49.8,19.5,filter(playoffs,conference == "W",round==8)$teamCode[7],cex=1)
  text(49.8,11.5,filter(playoffs,conference == "W",round==8)$teamCode[3],cex=1)
  text(49.8,3.5,filter(playoffs,conference == "W",round==8)$teamCode[6],cex=1)
  
  # East quarterfinals
  text(169.8,61.5,filter(playoffs,conference == "E",round==8)$teamCode[1],cex=1)
  text(169.8,53.5,filter(playoffs,conference == "E",round==8)$teamCode[8],cex=1)
  text(169.8,45.5,filter(playoffs,conference == "E",round==8)$teamCode[4],cex=1)
  text(169.8,37.5,filter(playoffs,conference == "E",round==8)$teamCode[5],cex=1)
  
  text(169.8,27.5,filter(playoffs,conference == "E",round==8)$teamCode[2],cex=1)
  text(169.8,19.5,filter(playoffs,conference == "E",round==8)$teamCode[7],cex=1)
  text(169.8,11.5,filter(playoffs,conference == "E",round==8)$teamCode[3],cex=1)
  text(169.8,3.5,filter(playoffs,conference == "E",round==8)$teamCode[6],cex=1)
  
  rounds <- c(8)
  if (round == "Conference Semifinals"){
    rounds <- c(8,4)
  } else if (round == "Conference Finals") {
    rounds <- c(8,4,2)
  } else if (round == "Finals"){
    rounds <- c(8,4,2,1)
  } else if (round == "Champion"){
    rounds <- c(8,4,2,1,0)
  }
  
  if (4 %in% rounds){
    # West semifinals
    text(69.8,57.5,filter(playoffs,conference == "W",round==4)$teamCode[1],cex=1)
    text(69.8,41.5,filter(playoffs,conference == "W",round==4)$teamCode[4],cex=1)
    text(69.8,23.5,filter(playoffs,conference == "W",round==4)$teamCode[2],cex=1)
    text(69.8,7.5,filter(playoffs,conference == "W",round==4)$teamCode[3],cex=1)
    # East semifinals
    text(149.8,57.5,filter(playoffs,conference == "E",round==4)$teamCode[1],cex=1)
    text(149.8,41.5,filter(playoffs,conference == "E",round==4)$teamCode[4],cex=1)
    text(149.8,23.5,filter(playoffs,conference == "E",round==4)$teamCode[2],cex=1)
    text(149.8,7.5,filter(playoffs,conference == "E",round==4)$teamCode[3],cex=1)
    
  }
  
  if (2 %in% rounds){
    # Conference Finals
    text(89.8,49.5,filter(playoffs,conference == "W",round==2)$teamCode[1],cex=1)
    text(129.8,49.5,filter(playoffs,conference == "E",round==2)$teamCode[1],cex=1)
    text(89.8,15.5,filter(playoffs,conference == "W",round==2)$teamCode[2],cex=1)
    text(129.8,15.5,filter(playoffs,conference == "E",round==2)$teamCode[2],cex=1)
  }
  
  if (1 %in% rounds){
    # Finals
    text(109.8,37.5,filter(playoffs,conference == "W",round==1)$teamCode[1],cex=1)
    text(109.8,27.5,filter(playoffs,conference == "E",round==1)$teamCode[1],cex=1)
  } 
  
  if (0 %in% rounds){
    # Champion
    text(109.8,32.5,filter(playoffs,round==0)$teamCode[1],cex=2)
  }
}

# Season scores --------
# compute score between any 2 teams during regular season
.calculateScore <- function(team_home,team_away){
  
  # Single game simulation ----------------
  #team_home <- "PHI"
  #team_away <- "BOS"
  # ---------------------------------------
  mean_predicted <- mean(c(teamsPredicted$TEAM_PTS,teamsPredicted$TEAM_PTSAG))
  # teamsPredicted contain predicted avg PTS and avg PTS Against per team for a new season
  teamH <- filter(teamsPredicted, TeamCode == team_home)
  teamA <- filter(teamsPredicted, TeamCode == team_away)
  
  # Define both Normal distributions. Empirical home-away difference is approx (2*home_away_factor) 6 points (+3, -3)
  muH <- teamH$TEAM_PTS + home_away_factor/2 + teamA$TEAM_PTSAG - mean_predicted
  muA <- teamA$TEAM_PTS - home_away_factor/2 + teamH$TEAM_PTSAG - mean_predicted
  
  pointsH <- round(rnorm(1,muH,sigma),0)
  pointsA <- round(rnorm(1,muA,sigma),0)
  
  numOT <- 0
  while (abs(pointsH-pointsA)<1){ # overtime tie-breaker
    extraH <- round(rnorm(1,muH*5/48,sigma/3),0)
    extraA <- round(rnorm(1,muA*5/48,sigma/3),0)
    pointsH <- pointsH + extraH
    pointsA <- pointsA + extraA
    numOT <- numOT + 1
  }
  #print(paste0(team_home,": ",pointsH," vs. ",team_away,": ",pointsA))
  return(c(pointsH,pointsA,numOT))
}

.calculateWinProbability <- function(data = teamsPredicted,team_home,team_away,home_away_f = home_away_factor){
  
  mean_predicted <- mean(c(data$TEAM_PTS,data$TEAM_PTSAG))
  # teamsPredicted contain predicted avg PTS and avg PTS Against per team for a new season
  teamH <- filter(data, TeamCode == team_home)
  teamA <- filter(data, TeamCode == team_away)
  
  # Define both Normal distributions. Empirical home-away difference is approx (2*home_away_factor) 6 points (+3, -3)
  #muH <- teamH$TEAM_PTS + home_away_f/2 + teamA$TEAM_PTSAG - global_mean
  #muA <- teamA$TEAM_PTS - home_away_f/2 + teamH$TEAM_PTSAG - global_mean
  muH <- teamH$TEAM_PTS + home_away_f/2 + teamA$TEAM_PTSAG - mean_predicted
  muA <- teamA$TEAM_PTS - home_away_f/2 + teamH$TEAM_PTSAG - mean_predicted
  
  prob_HvsA <- 1-pnorm(0,muH-muA,sqrt(2)*sigma)
  # equivalent simulated probability (to double check analytical probability)
  # prob_HvsA_sim <- length(which(rnorm(100000,muH-muA,sqrt(2)*sigma)>0))/100000
  
  return(prob_HvsA)
}

.computeScores <- function(real=FALSE){
  
  # Load season schedule
  if (real){
    season <- realSeasonSchedule %>%
      mutate(Date = paste(Date,StartTime)) %>%
      dplyr::select(-StartTime)
    
  } else {
    season <- seasonSchedule
  }
  
  # calculate all scores
  scores <- data.frame()
  for (i in 1:nrow(season)){
    thisGame <- .calculateScore(season[i,2],season[i,3])
    scores[i,1] <- thisGame[1]
    scores[i,2] <- thisGame[2]
    scores[i,3] <- thisGame[3]
  }
  return(scores)
}

.standings <- function(real=FALSE) {
  
  set.seed(as.integer(Sys.time())) # always a different seed
  # compute all scores for regular season  
  
  if (real){
    regSeasonScores <- .computeScores(real=TRUE)
    season <- bind_cols(realSeasonSchedule,regSeasonScores)
    names(season) <- c("day","time","home_team","away_team","home_points","away_points","numOT")
    datesRange <- unique(season$day)
  } else {
    regSeasonScores <- .computeScores()
    seasonSchedule <- .seasonSchedule()
    season <- bind_cols(seasonSchedule,regSeasonScores)
    names(season) <- c("day","home_team","away_team","home_points","away_points","numOT")
    datesRange <- c(1:tail(season,1)$day)
  }
  
  
  
  # compute standings by day for regular season
  the_standings <- list() # standings is a list in which each day of competition is a data.frame
  standings_aux <- data.frame(team = conferences$Team, teamCode = conferences$TeamCode,
                              conference = conferences$Conference, win = 0, lose = 0,
                              win_home = 0, lose_home = 0, win_home_perc = 0, 
                              win_conf = 0, lose_conf = 0, win_conf_perc = 0, 
                              tot_pts = 0, avg_pts = 0, tot_pts_ag = 0, avg_pts_ag = 0, 
                              streak = 0)
  
  for (i in datesRange){
    
    thisDay <- filter(season,day == i)
    for (j in 1:nrow(thisDay)){
      
      HT <- standings_aux[standings_aux$teamCode==thisDay$home_team[j],]
      AT <- standings_aux[standings_aux$teamCode==thisDay$away_team[j],]
      
      if (thisDay$home_points[j] > thisDay$away_points[j]){ # home team wins
        HT$win <- HT$win + 1
        AT$lose <- AT$lose + 1
        HT$win_home <- HT$win_home + 1
        HT$win_home_perc <- round(HT$win_home/(HT$win_home + HT$lose_home),2)
        HT$win_conf <- ifelse(HT$conference==AT$conference,HT$win_conf + 1,HT$win_conf)
        AT$lose_conf <- ifelse(HT$conference==AT$conference,AT$lose_conf + 1,AT$lose_conf)
        HT$win_conf_perc <- round(HT$win_conf/(HT$win_conf + HT$lose_conf),2)
        HT$streak <- ifelse(HT$streak <= 0,1,HT$streak + 1)
        AT$streak <- ifelse(AT$streak >= 0,-1,AT$streak - 1)
        
      } else { # away team wins
        AT$win <- AT$win + 1
        HT$lose <- HT$lose + 1
        HT$lose_home <- HT$lose_home + 1
        AT$win_home_perc <- round(AT$win_home/(AT$win_home + AT$lose_home),2)
        AT$win_conf <- ifelse(AT$conference==HT$conference,AT$win_conf + 1,AT$win_conf)
        HT$lose_conf <- ifelse(HT$conference==AT$conference,HT$lose_conf + 1,HT$lose_conf)
        AT$win_conf_perc <- round(AT$win_conf/(AT$win_conf + AT$lose_conf),2)
        AT$streak <- ifelse(AT$streak <= 0,1,AT$streak + 1)
        HT$streak <- ifelse(HT$streak >= 0,-1,HT$streak - 1)
      }
      # points don't depend on outcome of game
      HT$tot_pts <- HT$tot_pts + thisDay$home_points[j]
      HT$tot_pts_ag <- HT$tot_pts_ag + thisDay$away_points[j]
      HT$avg_pts <- round(HT$tot_pts/(HT$win + HT$lose),1)
      HT$avg_pts_ag <- round(HT$tot_pts_ag/(HT$win + HT$lose),1)
      AT$tot_pts <- AT$tot_pts + thisDay$away_points[j]
      AT$tot_pts_ag <- AT$tot_pts_ag + thisDay$home_points[j]
      AT$avg_pts <- round(AT$tot_pts/(AT$win + AT$lose),1)
      AT$avg_pts_ag <- round(AT$tot_pts_ag/(AT$win + AT$lose),1)
      
      standings_aux[standings_aux$teamCode==thisDay$home_team[j],] <- HT
      standings_aux[standings_aux$teamCode==thisDay$away_team[j],] <- AT
    }
    
    the_standings[[i]] <- standings_aux
    
  }
  return(list(the_standings,season)) # list of standings (list) and reg season scores (data.frame)
  
}

.getConferenceStandings <- function(conf,day){
  
  standings <- regSeasonOutcome[[1]]
  #day <- length(standings)
  confPredStandings <- arrange(filter(select(standings[[day]], conference, team,W=win,L=lose,`%W Home`=win_home_perc,`%W Conf`=win_conf_perc,
                                             PTS=avg_pts,PTSA=avg_pts_ag,Strk=streak), conference == conf), desc(W/(W+L)))
  confPredStandings <- select(confPredStandings,-conference) %>%
    mutate_if(is.numeric, function(x) round(x,1))
  
  return(confPredStandings)
}

.getGames <- function(conf,this_day){
  
  games <- regSeasonOutcome[[2]]
  #day <- length(standings)
  confPredGames <- dplyr::select(filter(games,day==this_day), away_team,home_team,
                                 away_points,home_points) %>%
    mutate(game = paste0(away_team," @ ",home_team))
  confPredGames <- dplyr::select(confPredGames,game,A=away_points,H=home_points)
  
  return(confPredGames)
}

.getGameProbability <- function(conf,this_day){
  
  games <- regSeasonOutcome[[2]]
  #day <- length(standings)
  confPredGamesProbs <- filter(games,day==this_day) %>% 
    select(away_team,home_team,away_points,home_points) %>%
    mutate(game = paste0(away_team," @ ",home_team)) %>%
    group_by(game) %>%
    mutate(Prob = .calculateWinProbability(teamsPredicted,home_team,away_team)) %>%
    select(game,A=away_points,H=home_points,Prob)
  
  return(confPredGamesProbs)
}

.winProbability_matrix <- function(){
  
  prob_matrix <- data.frame()
  k <- 1
  for (i in 1:length(teamDashboard$Tm)){
    for (j in 1:length(teamDashboard$Tm)){
      prob_matrix[k,1] <- teamDashboard$Tm[i]
      prob_matrix[k,2] <- teamDashboard$Tm[j]
      prob_matrix[k,3] = .calculateWinProbability(teamsPredicted,teamDashboard$Tm[i],teamDashboard$Tm[j])
      k <- k + 1
    }
  }
  
  names(prob_matrix) <- c("Home_Team", "Away_Team", "Win_Prob")
  
  prob_matrix2 <- spread(prob_matrix,Away_Team,Win_Prob)
  
  return(prob_matrix2)
}

########## PLAYERS ###########

# Remove players duplicates names
.rename_PlayerDuplicates <- function(data) {
  
  data <- mutate(data, Player = gsub("*","",Player, fixed=TRUE)) %>%
    group_by(Player) %>%
    mutate(yearBorn = as.numeric(substr(Season,1,4)) - Age) %>%
    as.data.frame()
  
  # Players with the same name will create silly duplicates. Identify them
  playerDups <- group_by(data,Player) %>%
    filter(max(yearBorn)-min(yearBorn) > 1) %>%
    distinct(Player, Season, .keep_all = TRUE) %>%
    arrange(Player,desc(Season)) %>%
    group_by(Player,yearBorn) %>%
    arrange(Player, yearBorn) %>%
    distinct(Player,yearBorn,.keep_all=TRUE) %>%
    dplyr::select(Player,yearBorn) %>%
    group_by(Player) %>%
    mutate(id = row_number()) %>%
    as.data.frame()
  # Rename them: second: 2, third: 3, etc.
  data <- merge(data,playerDups, by=c("Player","yearBorn"),all.x = TRUE) %>%
    mutate(Player = ifelse(!is.na(id), ifelse(id > 1, paste(Player,id),Player), Player)) %>%
    select(-id, -yearBorn) %>% distinct(Player, Tm, Season, .keep_all=TRUE) %>% as.data.frame()
  
}

# when all players have been updated and predicted, only leftovers remain (players who were NBA at some point
# and return after 1 or more absent seasons)
.computePredictedPlayerStats_Leftovers <- function(thesePlayers) {
  
  # update currentRosters, europePlayers and College players from write_rookiesDraft.R
  #current_rosters <- read.csv("data/currentRosters.csv", stringsAsFactors = FALSE)
  #rookies <- read.csv("data/rookies.csv",stringsAsFactors = FALSE)
  #collegePlayers <- read.csv("data/collegePlayers.csv", stringsAsFactors = FALSE)
  #rookieStats <- read.csv("data/rookieStats.csv", stringsAsFactors = FALSE)
  #europePlayers <- read.csv("data/europePlayers.csv", stringsAsFactors = FALSE)
  playersNew <- playersHist %>%
    filter(Season == max(as.character(Season))) %>%
    mutate(Season = as.factor(paste0(as.numeric(substr(Season,1,4))+1,"-",as.numeric(substr(Season,1,4))+2)))
  
  thesePlayersPredicted <- data.frame()
  for (team in unique(thesePlayers$Tm)){
    #for (team in c("CLE")){
    #thisTeam <- filter(current_rosters, Tm == team)
    thisTeam <- filter(thesePlayers, Tm == team)
    thisTeamStats <- data.frame()
    for (player in thisTeam$Player){
      #if (!(player %in% thesePlayersPredicted$Player)){ # skip running all. Start over where it failed
      thisPlayer <- filter(thisTeam, Player == player)
      print(paste0("Team: ", team,": Processing ",thisPlayer$Player))
      #if (thisPlayer$Exp %in% seq(1,25,1)){ # not a rookie
      if (thisPlayer$Age < 20) { # not enough players to compare to at age 19 or younger
        thisPlayer$Age <- 20
      }
      if (thisPlayer$Age > 39) { # not enough players to compare to at age 41 or older
        thisPlayer$Age <- 39
      }
      thisPlayerStats <- .predictPlayer(thisPlayer$Player,20,thisPlayer$Age,10) %>% 
        select(Player,Pos,Season,Age,everything())
      
      if (nrow(thisPlayerStats)>0){ # in case thisPlayerStats return a non empty data.frame
        if (!is.na(thisPlayerStats$effPTS)){ # rosters not yet updated so include R (last season rookies)
          #if (thisPlayer$Exp %in% c(seq(1,25,1),"R")){ # rosters not yet updated so include R (last season rookies)
          print("NBA player: OK!")
          print(thisPlayerStats)
          
        } else if (player %in% playersHist$Player) { # NBA player that didn't play last season so I look him up in historical seasons
          thisPlayerStats <- .team_preparePredict(filter(playersHist, Player == player))  %>%
            mutate(Age = thisPlayer$Age) %>%
            select(Player,Pos,Season,Age,everything())
          
          print("NBA player: Empty predicted stats!")
          print(thisPlayerStats)
        } else { # compute rookie player average stats for this player
          thisPlayerStats <- .calculate_AvgPlayer(playersNew, thisPlayer$Age) %>%
            mutate(Player = as.character(thisPlayer$Player), Pos = as.character(thisPlayer$Pos), 
                   G = 10, GS = 0, Tm = team) 
          thisPlayerStats <- .team_preparePredict(data = thisPlayerStats, thisTeam = as.character(thisPlayer$Tm),singlePlayer = TRUE)
          print("Average player: OK!")
          print(thisPlayerStats)
        }
      } else {
        thisPlayerStats <- .calculate_AvgPlayer(playersNew, thisPlayer$Age) %>%
          mutate(Player = as.character(thisPlayer$Player), Pos = as.character(thisPlayer$Pos), 
                 G = 10, GS = 0, Tm = team) 
        thisPlayerStats <- .team_preparePredict(data = thisPlayerStats, thisTeam = as.character(thisPlayer$Tm),singlePlayer = TRUE)
        #MP = as.numeric(thisPlayer$MP))
        print("Average player: OK!")
        print(thisPlayerStats)
      }  
      
      if (nrow(thisTeamStats)>0){
        thisTeamStats <- bind_rows(thisTeamStats,thisPlayerStats)
      } else{
        thisTeamStats <- thisPlayerStats
      }
    }
    #}
    if (nrow(thisTeamStats) > 0) {
      thisTeamStats <- mutate(thisTeamStats, Tm = team)
      if (nrow(thesePlayersPredicted)>0){
        thesePlayersPredicted <- bind_rows(thesePlayersPredicted,thisTeamStats)
      } else{
        thesePlayersPredicted <- thisTeamStats
      }
    }
    
    
  }
  thesePlayersPredicted <- distinct(thesePlayersPredicted, Player, Tm, .keep_all=TRUE)
  limitMinutes <- .005
  defaultMinutes <- .001 # assign low minutes to outliers as they most likely belong to players with very little playing time
  thesePlayersPredicted2 <- mutate(thesePlayersPredicted,effMin = ifelse(effMin > limitMinutes, defaultMinutes,effMin))
  #write.csv(thesePlayersPredicted, "data/thesePlayersPredicted.csv", row.names = FALSE)
  return(thesePlayersPredicted2)
} 

# Find similar players ------------------------------
#
# Using t-sne algorithm, find players that have similar characteristics to a given player.
# The objective is to predict his performance in a given year based on the historical performance
# of similar players (see: Nate Silver's CARMELO or PECOTA systems)
#
# Ex: If I want to predict Pau Gasol numbers for the season he will turn 36, I will start
# with his numbers in the previous seasons and I will adjust according to the average
# evolution of similar players when they turned 36.
#
# Ex: To be able to assign predicted characteristics to a rookie player, I will do a
# similar approach. See functions related to rookies and draft
#

.tSNE_prepareSelected <- function(inputPlayers){
  # Players that changed teams in the season have a column Tm == "TOT" with their total stats
  # and because I don't care about the team, this should be enough filter
  # playerAge <- 34
  # num_iter <- 300
  # max_num_neighbors <- 20
  # playerName <- "Pau Gasol"
  data_prepared <- inputPlayers %>%
    group_by(Player) %>%
    mutate(keep = ifelse(n() > 1, 1, 0), effMin = MP/3936, effFG = FG/(3936*effMin),
           effFGA = FGA/(3936*effMin),eff3PM = X3P/(3936*effMin),eff3PA = X3PA/(3936*effMin),
           eff2PM = X2P/(3936*effMin),eff2PA = X2PA/(3936*effMin),
           effFTM = FT/(3936*effMin),effFTA = FTA/(3936*effMin),
           effORB = ORB/(3936*effMin),effDRB = DRB/(3936*effMin),
           effTRB = TRB/(3936*effMin),effAST = AST/(3936*effMin),
           effSTL = STL/(3936*effMin),effBLK = BLK/(3936*effMin),
           effTOV = TOV/(3936*effMin),effPF = PF/(3936*effMin),
           effPTS = PTS/(3936*effMin)) %>%
    filter(keep == 0 | Tm == "TOT") %>%
    filter(effMin*G >= .15) %>% # Played at least 15% of total available minutes
    dplyr::select(Player,Pos,Season,Age,FGPer = FG.,FG3Per = X3P., FG2Per = X2P., effFGPer = eFG.,
                  FTPer = FT., starts_with("eff"),
                  -Tm,-keep,-G,-GS,-MP,FG,-FGA,-X3P,-X3PA,-X2P,-X2PA,-FG,-FTA,-ORB,-DRB,-TRB,-AST,
                  -BLK,-TOV,-PF,-FT,-STL,-PTS)
  
  # some players can be the same age during 2 seasons. Pick the one with the most minutes played
  data_prepared <- data_prepared %>%
    group_by(Player) %>%
    filter(effMin >= max(effMin)-.0001)
  
  # t-sne doesn't like NAs. Impute by assigning 0. If NA means no shot attempted, ie, 
  # either the player didn't play enough time or is really bad at this particular type of shot.
  for (i in 4:(ncol(data_prepared)-1)){
    data_prepared[is.na(data_prepared[,i]),i] <- 0
  }
  
  data_prepared <- as.data.frame(data_prepared)
  return(data_prepared)
  
}

.tSNE_prepare <- function(playerAge,per_Min){
  # Players that changed teams in the season have a column Tm == "TOT" with their total stats
  # and because I don't care about the team, this should be enough filter
  # playerAge <- 34
  # num_iter <- 300
  # max_num_neighbors <- 20
  # playerName <- "Pau Gasol"
  data_tsne <- playersHist %>%
    group_by(Player,Season) %>%
    mutate(keep = ifelse(n() > 1, 1, 0), effMin = MP/3936, effFG = FG/(3936*effMin),
           effFGA = FGA/(3936*effMin),eff3PM = X3P/(3936*effMin),eff3PA = X3PA/(3936*effMin),
           eff2PM = X2P/(3936*effMin),eff2PA = X2PA/(3936*effMin),
           effFTM = FT/(3936*effMin),effFTA = FTA/(3936*effMin),
           effORB = ORB/(3936*effMin),effDRB = DRB/(3936*effMin),
           effTRB = TRB/(3936*effMin),effAST = AST/(3936*effMin),
           effSTL = STL/(3936*effMin),effBLK = BLK/(3936*effMin),
           effTOV = TOV/(3936*effMin),effPF = PF/(3936*effMin),
           effPTS = PTS/(3936*effMin)) %>%
    filter(keep == 0 | Tm == "TOT") %>%
    filter(effMin*G >= per_Min) %>% # Played at least X% of total available minutes
    dplyr::select(Player,Pos,Season,Age,FGPer = FG.,FG3Per = X3P., FG2Per = X2P., effFGPer = eFG.,
                  FTPer = FT., starts_with("eff"),
                  -Tm,-keep,-G,-GS,-MP,FG,-FGA,-X3P,-X3PA,-X2P,-X2PA,-FG,-FTA,-ORB,-DRB,-TRB,-AST,
                  -BLK,-TOV,-PF,-FT,-STL,-PTS)
  
  # Filter by selected age 
  data_tsne <- data_tsne %>%
    filter(Age == playerAge) %>%
    dplyr::select(-Age) # redundant column, same value (playerAge) for all observations
  
  # some players can be the same age during 2 seasons. Pick the one with the most minutes played
  data_tsne <- data_tsne %>%
    group_by(Player) %>%
    filter(effMin >= max(effMin)-.0001)
  
  # t-sne doesn't like NAs. Impute by assigning 0. If NA means no shot attempted, ie, 
  # either the player didn't play enough time or is really bad at this particular type of shot.
  for (i in 4:(ncol(data_tsne)-1)){
    data_tsne[is.na(data_tsne[,i]),i] <- 0
  }
  
  data_tsne <- as.data.frame(data_tsne)
  return(data_tsne)
  
}

# Use this for the write_tsne_data_All
.tSNE_prepare_All <- function(){
  # Players that changed teams in the season have a column Tm == "TOT" with their total stats
  # and because I don't care about the team, this should be enough filter
  # , effFG = FG/(3936*effMin),effFGA = FGA/(3936*effMin),eff3PM = X3P/(3936*effMin),
  # eff2PM = X2P/(3936*effMin),effFTM = FT/(3936*effMin),
  data_tsne <- playersHist %>%
    group_by(Player,Season) %>%
    mutate(keep = ifelse(n() > 1, 1, 0), effMin = MP*82/3936,
           eff3PA = X3PA/(3936*effMin),eff3PM = X3P/(3936*effMin),
           eff2PA = X2PA/(3936*effMin),eff2PM = X2P/(3936*effMin),
           effFTA = FTA/(3936*effMin),effFTM = FT/(3936*effMin),
           effORB = ORB/(3936*effMin),effDRB = DRB/(3936*effMin),
           effAST = AST/(3936*effMin),
           effSTL = STL/(3936*effMin),effBLK = BLK/(3936*effMin),
           effTOV = TOV/(3936*effMin),effPF = PF/(3936*effMin),
           effPTS = PTS/(3936*effMin)) %>%
    filter(keep == 0 | Tm == "TOT") %>%
    filter(effMin >= .15) %>% # Played at least 15% of total available minutes
    dplyr::select(Player,Pos,Season,Age,Tm, starts_with("eff"))
  #P2Per = X2P., P3Per = X3P., FTPer = FT.
  
  # t-sne doesn't like NAs. Impute by assigning 0. If NA means no shot attempted, ie, 
  # either the player didn't play enough time or is really bad at this particular type of shot.
  for (i in 6:(ncol(data_tsne)-1)){
    data_tsne[is.na(data_tsne[,i]),i] <- 0
  }
  # exponential transformation to improve tsne layout
  #   for (i in 6:(ncol(data_tsne)-1)){
  #     data_tsne[,i] <- expm1(2*data_tsne[,i])
  #   }
  
  # Try scaling to [0,1] to improve tsne final shape
  #   maxs <- apply(data_tsne[,-c(1:5)], 2, max) 
  #   mins <- apply(data_tsne[,-c(1:5)], 2, min)
  #   data_tsne[,-c(1:5)] <- as.data.frame(scale(data_tsne[,-c(1:5)], center = mins, scale = maxs - mins))
  
  data_tsne <- as.data.frame(data_tsne)
  return(data_tsne)
  
}

.tSNE_compute <- function(num_iter, max_num_neighbors, playerAge){
  
  data_tsne <- .tSNE_prepare(playerAge,per_Min = .15)
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
  return(tsne_points) 
  
}

# compute colors for regions
.getColors <- function(num_iter, max_num_neighbors,playerAge,colVar){
  
  data_tsne <- .tSNE_prepare(playerAge,per_Min=.15)
  if (colVar == "Season"){
    colors <- rainbow(length(unique(data_tsne$Season)))
    names(colors) <- unique(data_tsne$Season)
  } else {
    colors <- rainbow(length(unique(data_tsne$Pos)))
    names(colors) <- unique(data_tsne$Pos)
  }
  return(colors)
}

# tsne chart ---------------------------------------------------------
.tSNE_plot <- function(playerName, num_iter, max_num_neighbors, playerAge, colVar){
  
  #tsne_points <- .tSNE_compute(num_iter, max_num_neighbors, playerAge)
  tsne_points <- tsneBlock[[playerAge]]
  if (length(tsne_points)>0){
    par(mar=c(0,0,0,0))
    plot(tsne_points,t='n', axes=FALSE, frame.plot = FALSE, xlab = "",ylab = ""); 
    graphics::text(tsne_points,labels=as.character(data_tsne$Player), col=.getColors(num_iter, max_num_neighbors,playerAge,colVar))
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Not enough data", col="red", cex=2)
  }
  
}

# tsne dist ---------------------------------------------------------
.tSNE_dist <- function(playerName, num_iter, max_num_neighbors, playerAge, firstSeason = NULL){
  
  if(is.null(firstSeason)) firstSeason <- "1979-1980"
  
  data_tsne <- .tSNE_prepare(playerAge,per_Min=.15)
  #tsne_points <- .tSNE_compute(num_iter, max_num_neighbors, playerAge)
  tsne_points <- tsneBlock[[playerAge]]
  if (length(tsne_points)>0 & nrow(filter(data_tsne, Player == playerName, Season >= firstSeason))>0){
    # calculate the euclidean distance between the selected player and the rest
    dist_mat <- cbind(tsne_points,as.character(data_tsne$Player))
    dist_mat <- as.data.frame(dist_mat, stringsAsFactors=FALSE)
    dist_mat$V1 <- as.numeric(dist_mat$V1)
    dist_mat$V2 <- as.numeric(dist_mat$V2)
    distCou1 <- dist_mat[dist_mat[,3]==playerName,1]
    distCou2 <- dist_mat[dist_mat[,3]==playerName,2]
    dist_mat <- mutate(dist_mat, dist = sqrt((V1-distCou1)^2+(V2-distCou2)^2))
    # order by closest distance to selected player
    dist_mat <- arrange(dist_mat, dist)[,c(3,4)]
    names(dist_mat) <- c("Player","Euclid. distance")
  } else {
    dist_mat <- data_frame()
  }
  
  return(dist_mat)
} 

#similarPlayers <- .tSNE_dist("Russell Westbrook",300,20,27)
#head(similarPlayers,20)

# return similar players based on last 5 years performances
# For retired players this will return similar players according to their last 5 seasons
# as NBA player. Unless pickAge is explicitly entered
.similarPlayers <- function(playerName,numberPlayersToCompare, pickAge){
  
  thisAgeFrame <- filter(playersHist, Player == playerName, Season >= paste0(as.numeric(thisYear)-pickAge+18,"-",as.numeric(thisYear)-pickAge+19))
  
  if (nrow(thisAgeFrame) > 0){
    #thisAge <- filter(thisAgeFrame, Season == max(as.character(Season)))$Age
    minAge <- min(filter(thisAgeFrame, Player == playerName)$Age)
    maxAge <- max(filter(thisAgeFrame, Player == playerName)$Age)
    if (pickAge >= minAge & pickAge <= maxAge){
      thisAge <- pickAge
    } else{
      thisAge <- maxAge
    }
    
    simPlayers <- data.frame()
    t <- thisAge-5
    while (t <= thisAge){
      if (t >= minAge){
        thisSimilar <- .tSNE_dist(playerName,300,20,t)
        if (nrow(thisSimilar)>0){
          thisSimilar <- head(thisSimilar,numberPlayersToCompare)
          thisSimilar$Age <- t
          if (nrow(simPlayers)>0){
            simPlayers <- bind_rows(simPlayers,thisSimilar)
          } else {
            simPlayers <- thisSimilar
          }
          t <- t + 1
        } else {
          t <- t + 1
        }
      } else {
        t <- t + 1
      }
    }
    if (nrow(simPlayers)>0){ 
      simPlayers_5years <- simPlayers %>%
        filter(!(Player == playerName)) %>%
        group_by(Player) %>%
        mutate(numYears = n(),rank5years = mean(`Euclid. distance`)) %>%
        distinct(Player, numYears, rank5years) %>%
        arrange(desc(numYears),rank5years)
      
      return(simPlayers_5years)
    } else { # Player didn't play enough minutes during the period considered
      return()
    }
    
  } else { # Player doesn't exist
    return()
  }
}

.predictPlayer <- function(playerName, numberPlayersToCompare,pickAge,numberTeamsForVariation){
  
  # Top 10 more similar to selected player for past 5 years
  top10_similar <- head(.similarPlayers(playerName,numberPlayersToCompare,pickAge),numberTeamsForVariation)$Player
  thisAgeFrame <- filter(playersHist, Player == playerName, Season >= paste0(as.numeric(thisYear)-pickAge+18,"-",as.numeric(thisYear)-pickAge+19))
  
  if (nrow(thisAgeFrame)>0){
    thisAge <- max(filter(thisAgeFrame, Player == playerName)$Age) 
  } else { # this player has been out of the league for way too long
    lastSeasonPlayed <- filter(playersHist, Player == playerName) %>%
      arrange(desc(Season)) %>%
      head(1)
    thisAge <- max(pickAge, pickAge + as.numeric(substr(thisSeason,1,4)) - (as.numeric(substr(lastSeasonPlayed$Season,1,4))+1))
  }
  
  # Now calculate average variation in their stats when they went from current age to age + 1
  thisAgeData <- .tSNE_prepare(thisAge,per_Min=.001)
  #thisAgeData <- read.csv(paste0("data/tsneBlock_",thisAge,".csv"))
  namesKeep <- names(thisAgeData)
  names(thisAgeData)[2:ncol(thisAgeData)] <- sapply(names(thisAgeData)[2:ncol(thisAgeData)],
                                                    function(x) paste0(x,"_",thisAge))
  #thisAgeData$Age <- thisAge
  nextAgeData <- .tSNE_prepare(thisAge+1,per_Min=.001)
  #nextAgeData$Age <- thisAge + 1
  names(nextAgeData)[2:ncol(nextAgeData)] <- sapply(names(nextAgeData)[2:ncol(nextAgeData)],
                                                    function(x) paste0(x,"_",thisAge+1))
  
  ageData <- merge(thisAgeData,nextAgeData, by="Player")
  
  top10 <- ageData %>%
    filter(Player %in% top10_similar)
  
  top10_var <- data.frame()
  numCols <- ncol(thisAgeData)
  for (i in 1:nrow(top10)){
    top10_var[i,1] <- top10$Player[i]  
    for (j in 4:numCols){
      top10_var[i,j-2] <- ifelse(top10[i,j]==0,0,(top10[i,j+numCols-1]-top10[i,j])/top10[i,j])
    }
  }
  names(top10_var) <- namesKeep[c(1,4:length(namesKeep))]
  # Median variations for top 10 most similar players 
  #top10_var <- summarise_each(top10_var, funs(median(.)),-Player)
  top10_var <- mutate_if(top10_var,is.logical, as.numeric) %>%
    summarise_if(is.numeric, median)
  # Apply this variation to predict stats for this player for next season  
  ##  ### NOTE: This may fail when player didn't play much at this age. Think about alternatives
  predAgeData <- filter(thisAgeData, Player == playerName)
  if (nrow(predAgeData[1])>0){
    for (i in 1:ncol(top10_var)){
      predAgeData[i+3] <- predAgeData[i+3]*(1+top10_var[i])
    }
    names(predAgeData) <- namesKeep
    # Update the Season and Age of the player
    predAgeData$Season <- paste0(as.numeric(substr(predAgeData$Season,1,4))+1,"-",
                                 as.numeric(substr(predAgeData$Season,1,4))+2)
    predAgeData$Age <- thisAge + 1
    
  } else {
    names(predAgeData) <- namesKeep
    predAgeData <- mutate(predAgeData, Age = NA, effPTS = NA)
  }
  
  
  
  return(predAgeData)
  
}

# Calculate centroid and other measures for selected cluster of points from tSNE
.clusterMath <- function(colTeam,colSeason,colPlayer,colAge,colSkill){
  
  points <- .tSNE_plot_filter(colTeam,colSeason,colPlayer,colAge,colSkill)
  
  centroid <- c(mean(points$x),mean(points$y))
  dispersion <- c(sd(points$x),sd(points$y))
}

# Plot tsne chart ---------------------------------------------------------
.tSNE_plot_All <- function(data = tsne_ready,colTeam,colSeason,colPlayer,colAge,colSkill,num_clusters = 10){
  # tsne_points contains pairs of coordinate points to plot
  # Parameters -----------
  # 
  #colSeason <- "2015-2016"
  #colPlayer <- "All"
  #colTeam <- "All"
  #colAge <- "All"
  #colSkill <- "All"
  # ----------------------
  #
  # ------------------------------------
  # Check whether the user has selected any input
  color_details <- FALSE
  
  if (colPlayer=="All") colPlayer <- players_list else color_details <- TRUE
  if (colAge=="All") colAge <- ages_list else color_details <- TRUE
  if (colTeam=="All") colTeam <- teams_list else color_details <- TRUE
  if (colSeason=="All") colSeason <- seasons_list else color_details <- TRUE
  # Add clusters
  set.seed(456)
  playerCluster <- kmeans(data[, c("x","y")], num_clusters, nstart = 10, iter.max = 20)
  data <- cbind(data, cluster = playerCluster$cluster)
  
  #centroid <- data.frame(x=(mean(tsne_points_filter$x)),y=mean(tsne_points_filter$y))
  #x_limit <- c(min(tsne_points_filter$x)-5,max(tsne_points_filter$x)+10)
  #y_limit <- c(min(tsne_points_filter$y)-5,max(tsne_points_filter$y)+10)
  
  cluster_representative <- group_by(data, cluster) %>%
    mutate(x_mean = mean(x), y_mean=mean(y)) %>%
    mutate(dist = sqrt((x-x_mean)^2+(y-y_mean)^2)) %>%
    filter(dist==min(dist)) %>%
    select(cluster, Player,Season, x,y,dist) %>%
    ungroup()
  
  if (length(data)>0){ # if data do stuff
    par(mar=c(0,0,0,0))
    
    tsne_ready_plot <- data %>% # by default all colored grey
      mutate(color = "lightgrey", colorDots = "grey")
    
    # General Filters
    tsne_points_filter <- tsne_ready_plot %>%
      filter(Player %in% colPlayer & Age %in% colAge
             & Tm %in% colTeam & Season %in% colSeason)
    
    centroid <- data.frame(x=(mean(tsne_points_filter$x)),y=mean(tsne_points_filter$y))
    
    tsne_points_filter_out <- tsne_ready_plot %>%
      filter(!(Player %in% colPlayer & Age %in% colAge
               & Tm %in% colTeam & Season %in% colSeason))
    # Skills filter
    
    #if (color_details) {
    if (colSkill == "K-means" & color_details) { # color by K-means clusters
      ggplot(NULL, aes(x,y)) +
        geom_point(data=data,aes(color = as.factor(cluster)),alpha = 0.2) +
        geom_jitter(data=tsne_points_filter,color = "red",size=4,width = 1.5, height = 1.5) +
        geom_text(data = cluster_representative, aes(label = paste0(Player," (",Season,")")),color = "grey",size=3, nudge_y = 1) +
        geom_text(data=tsne_points_filter,aes(label = Player),color="blue",size=3, nudge_y = 1) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.text = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks = element_blank())
    } else if (colSkill == "K-means"){
      ggplot(NULL, aes(x,y)) +
        geom_point(data=data,aes(color = as.factor(cluster)),alpha = 0.2) +
        geom_text(data = cluster_representative, aes(label = paste0(Player," (",Season,")")),color = "grey",size=3, nudge_y = 1) +
        #scale_color_gradient2(midpoint=mean(eval(parse(text=paste0("tsne_points_filter$",colSkill)))), low="blue", mid="white",high="red")+
        #geom_point(data=tsne_points_filter_out,color=alpha("lightgrey",0.1)) + 
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.text = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks = element_blank())
    } else {
      ggplot(NULL, aes(x,y)) +
        geom_point(data=tsne_points_filter,aes(color = eval(parse(text=colSkill))),size=2) +
        scale_color_gradient2(midpoint=mean(eval(parse(text=paste0("tsne_points_filter$",colSkill)))), low="blue", mid="white",high="red")+
        geom_point(data=tsne_points_filter_out,color=alpha("lightgrey",0.1)) + 
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.text = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks = element_blank())
    }
    # } else {
    #   ggplot(NULL, aes(x,y)) +
    #     geom_point(data=tsne_points_filter,aes(color = eval(parse(text=colSkill))),size=2) +
    #     scale_color_gradient2(midpoint=mean(eval(parse(text=paste0("tsne_points_filter$",colSkill)))), low="blue", mid="white",high="red")+
    #     geom_point(data=tsne_points_filter_out,color=alpha("lightgrey",0.1)) + 
    #     theme(legend.key=element_blank(),
    #           legend.title=element_blank(),
    #           legend.text = element_blank(),
    #           panel.border = element_blank(),
    #           panel.background = element_blank(),
    #           axis.text.x = element_blank(),
    #           axis.text.y = element_blank(),
    #           axis.title.x = element_blank(),
    #           axis.title.y = element_blank(),
    #           axis.ticks = element_blank())
    # }
    # } else {
    #   
    #     # starPlayers <- c("LeBron James","Russell Westbrook","Kawhi Leonard","Stephen Curry",
    #     #                  "Draymond Green","Kevin Durant","Anthony Davis",
    #     #                  "James Harden","Klay Thompson","DeMarcus Cousins",
    #     #                  "Chris Paul","John Wall","Paul George",
    #     #                  "Marc Gasol","JaVale McGee","Andre Drummond",
    #     #                  "Kelly Oubre","Ryan Anderson","Kelly Olynyk","Channing Frye",
    #     #                  "Brandon Jennings","Tony Allen","Tristan Thompson","Ian Mahinmi",
    #     #                  "Dennis Schroder","Justin Harper","Luis Scola")
    #     #labelsPlayers <- filter(tsne_ready_plot, Player %in% starPlayers, Season == "2016-2017")
    #     ggplot(NULL, aes(x,y)) +  
    #     geom_point(data=tsne_points_filter,color = "blue",size=4) +
    #     geom_text(data=tsne_points_filter,aes(label = paste0(Player," (",Season,")"))) +
    #     geom_text(data = cluster_representative, aes(label = paste0(Player," (",Season,")")),color = "grey",size=3, nudge_y = 1) +
    #     #geom_text(data=labelsPlayers,aes(label = Player,group = Player,color = Player)) +  
    #     geom_point(data=tsne_points_filter_out,color=alpha("lightgrey",0.1)) +
    #     #geom_point(data=centroid,color="red",size=3) + 
    #     theme(legend.key=element_blank(),
    #           legend.title=element_blank(),
    #           legend.text = element_blank(),
    #           legend.position = "top",
    #           panel.border = element_blank(),
    #           panel.background = element_blank(),
    #           axis.text.x = element_blank(),
    #           axis.text.y = element_blank(),
    #           axis.title.x = element_blank(),
    #           axis.title.y = element_blank(),
    #           axis.ticks = element_blank())
    # }
    
    #plot(tsne_points,type = "p", pch = 19, axes=FALSE, frame.plot = FALSE, xlab = "",ylab = "",col = tsne_ready$colorDots); 
    #    graphics::text(tsne_points_filter[,c("x","y")],
    #                   labels=paste0(as.character(tsne_points_filter$Player)," (",tsne_points_filter$Season,")"),
    #                   col=tsne_points_filter$color)
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Not enough data", col="red", cex=2)
  }
  
}

# Plot tsne chart ---------------------------------------------------------
.tSNE_plot_filter <- function(data = tsne_ready, colTeam,colSeason,colPlayer,colAge,colSkill){
  #
  if (colPlayer=="All") colPlayer <- players_list
  if (colAge=="All") colAge <- ages_list
  if (colTeam=="All") colTeam <- teams_list
  if (colSeason=="All") colSeason <- seasons_list
  
  if (length(data)>0){ # if data do stuff
    # General Filters
    tsne_points_filter <- data %>%
      filter(Player %in% colPlayer & Age %in% colAge
             & Tm %in% colTeam & Season %in% colSeason)
    tsne_points_filter_out <- data %>%
      filter(!(Player %in% colPlayer & Age %in% colAge
               & Tm %in% colTeam & Season %in% colSeason))
    
  } else{ return()}
  #plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
  #graphics::text(1.5, 1,"Not enough data", col="red", cex=2)
  
  return(tsne_points_filter)
}

# Density plots
.densityPlots <- function(colTeam,colSeason,colPlayer,colAge,colSkill,clickPlayer,clickSeason,horiz=TRUE){
  
  tsne_points_filter <- .tSNE_plot_filter(colTeam,colSeason,colPlayer,colAge,colSkill)
  tsne_points_filter <- gather(tsne_points_filter, skill, value, -Player,-Tm,-Age,-Season,-Pos,-x,-y)
  tsne_ready_gather <- gather(tsne_ready, skill, value, -Player,-Tm,-Age,-Season,-Pos,-x,-y)
  
  if (!horiz){
    
    if (is.null(clickPlayer)){
      
      ggplot(data=tsne_ready_gather,aes(value)) + 
        geom_density(data=tsne_ready_gather,aes(y=..density..),alpha=.8, fill="grey",colour="grey",size=1) +  
        geom_histogram(data=tsne_points_filter,aes(y=..density..),alpha=.6, fill="blue") +  
        facet_wrap(~skill, ncol=1, scales="free_x") +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title = element_text(lineheight=.5),
              #axis.text.x = element_blank(),
              #axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank()
              #axis.ticks = element_blank()
        )
      
    } else {
      
      verticalLine <- tsne_points_filter %>%
        filter(Player == clickPlayer, Season == clickSeason) %>%
        dplyr::select(skill, value)
      
      ggplot(data=tsne_ready_gather,aes(value)) + 
        geom_density(data=tsne_ready_gather,aes(y=..density..),alpha=.8, fill="grey",colour="grey",size=1) +  
        geom_histogram(data=tsne_points_filter,aes(y=..density..),alpha=.8, fill="blue") +  
        facet_wrap(~skill, ncol=1, scales="free_x") +
        geom_vline(data=verticalLine, aes(xintercept = value), colour="red", size = 1) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title = element_text(lineheight=.5),
              #axis.text.x = element_blank(),
              #axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank()
              #axis.ticks = element_blank()
        )
    } 
    
  } else {
    if (is.null(clickPlayer)){
      
      ggplot(data=tsne_ready_gather,aes(value)) + 
        geom_density(data=tsne_ready_gather,aes(y=..density..),alpha=.8, fill="grey",colour="grey",size=1) +  
        geom_histogram(data=tsne_points_filter,aes(y=..density..),alpha=.6, fill="blue") +  
        facet_wrap(~skill, nrow=1, scales="free_x") +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title = element_text(lineheight=.5),
              #axis.text.x = element_blank(),
              #axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank()
              #axis.ticks = element_blank()
        )
      
    } else {
      
      verticalLine <- tsne_points_filter %>%
        filter(Player == clickPlayer, Season == clickSeason) %>%
        dplyr::select(skill, value)
      
      ggplot(data=tsne_ready_gather,aes(value)) + 
        geom_density(data=tsne_ready_gather,aes(y=..density..),alpha=.8, fill="grey",colour="grey",size=1) +  
        geom_histogram(data=tsne_points_filter,aes(y=..density..),alpha=.8, fill="blue") +  
        facet_wrap(~skill, nrow=1, scales="free_x") +
        geom_vline(data=verticalLine, aes(xintercept = value), colour="red", size = 1) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title = element_text(lineheight=.5),
              #axis.text.x = element_blank(),
              #axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank()
              #axis.ticks = element_blank()
        )
    }
  }
}

.radarPlot <- function(brushPoints){#,Off_Deff="All"){
  
  #   if (Off_Deff == "Offense"){
  #     list_skills <- c("effPTS","eff2PM","eff2PA","eff3PM","eff3PA","effFTA","effFTM","effAST")
  #   } else if (Off_Deff == "Defense"){
  #     list_skills <- c("effBLK","effDRB","effORB","effSTL","effTOV","effPF")
  #   } else {
  list_skills <- c("effPTS","eff2PM","eff2PA","eff3PM","eff3PA","effFTA","effFTM","effAST",
                   "effSTL","effBLK","effDRB","effORB","effTOV","effPF","effMin")
  #     }
  
  tsne_radar <- tsne_ready %>%
    dplyr::select(-Age,-Pos,-Tm,-x,-y) %>%
    mutate_at(vars(starts_with("eff")), funs(max,mean)) %>%
    #filter(Season == colSeason) %>%
    dplyr::select(-Season)
  
  #brushPoints <- filter(tsne_ready, Tm == "CHI")
  brushPoints <- as.data.frame(brushPoints)
  
  if (nrow(brushPoints)>0){
    #brushPoints <- merge(tsne_ready, brushPoints, by = c("Player","Season"))
    tsne_mean <- brushPoints %>%
      dplyr::select(-Age,-Pos,-Tm,-x,-y,-Season) %>%
      mutate_at(vars(starts_with("eff")), funs(mean)) %>%
      #dplyr::select(ends_with("_mean")) %>%
      mutate(Player = "mean of selected") %>%
      distinct(.keep_all=TRUE) %>%
      dplyr::select(Player, everything())
    
    names(tsne_mean) <- gsub("_mean","",names(tsne_mean))
    
  } else {
    tsne_mean <- tsne_radar %>%
      dplyr::select(ends_with("_mean")) %>%
      distinct(.keep_all=TRUE) %>%
      mutate(Player = "mean of selected") %>%
      dplyr::select(Player, everything())
    
    names(tsne_mean) <- gsub("_mean","",names(tsne_mean))
  }
  
  tsne_max <- tsne_radar %>%
    dplyr::select(ends_with("_max")) %>%
    distinct(.keep_all=TRUE) %>%
    mutate(Player = "max") %>%
    dplyr::select(Player, everything())
  
  names(tsne_max) <- gsub("_max","",names(tsne_max))
  
  tsne_radar <- bind_rows(tsne_mean,tsne_max)
  tsne_radar <- dplyr::select(tsne_radar, Player, one_of(list_skills))
  #ez.radarmap(df, "model", stats="mean", lwd=1, angle=0, fontsize=0.6, facet=T, facetfontsize=1, color=id, linetype=NULL)
  ez.radarmap(tsne_radar, "Player", stats="none", lwd=1, angle=0, fontsize=1.5, facet=F, facetfontsize=1, color=id, linetype=NULL) +
    theme(legend.key=element_blank(),
          legend.title=element_blank(),
          legend.position = "bottom",
          #panel.border = element_blank(),
          #panel.background = element_blank(),
          plot.title = element_text(lineheight=.5),
          #axis.text.x = element_blank(),
          #axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
          #axis.ticks = element_blank()
    )  
  
}

.brushTable <- function(brushPoints){
  
  brushPoints <- as.data.frame(brushPoints)
  #return(str(brushPoints))
}

.compare10_click <- function(colSeason,colPlayer,data=tsne_ready){
  
  if (colPlayer=="All" || is.null(colPlayer)) colPlayer <- players_list
  if (colSeason=="All" || is.null(colSeason)) colSeason <- seasons_list
  
  if (length(data)>0){ # if data do stuff
    
    # coordenates x,y of clicked point
    distCouPerX <- filter(data,Player %in% colPlayer, Season %in% colSeason)$x
    distCouPerY <- filter(data,Player %in% colPlayer, Season %in% colSeason)$y
    
    tsne_points_filter <- data %>%
      dplyr::select(Season,Player,x,y) %>%
      mutate(dist = sqrt((x-distCouPerX)^2+(y-distCouPerY)^2)) %>%
      arrange(dist) %>%
      dplyr::select(-x,-y)
    
  } else{ return()}
  
  return(tsne_points_filter)
}

# calculate t-SNE for predicted stats (new season)
.compute_players_tSNE_points <- function(data, num_iter, max_num_neighbors) {
  
  require(tsne)
  data_tsne_sample <- data %>%
    select_if(is.numeric) %>% select(-Pick)
  
  if (nrow(data_tsne_sample)>0){
    #num_iter <- 500
    #max_num_neighbors <- 20
    set.seed(456) # reproducitility
    tsne_points <- tsne(data_tsne_sample, 
                        max_iter=as.numeric(num_iter), 
                        perplexity=as.numeric(max_num_neighbors), 
                        epoch=100)
    plot(tsne_points)
  } else {
    tsne_points <- c()
  }
  return(tsne_points)
  
}

# put together tsne_ready predicted to load at start of dashboards
.compute_tSNE_ready_players <- function(data){
  
  tsne_points <- .compute_players_tSNE_points(data=data,num_iter = 600,max_num_neighbors = 20)
  
  # load data
  data_tsne_sample <- data %>%
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
  
  return(tsne_ready)
}

# Make changes to teams rosters in 2 ways:
# A. Same roster, minutes adjustments (eg: in playoffs star players take bigger share of minutes)
# B. Changes in roster: drafted players, aging players, leaving players, etc.

# A. Minutes adjustments
# start with playersNew
# playersNew <- playersHist %>%
#   filter(Season == max(as.character(Season))) %>%
#   mutate(Season = as.factor(paste0(as.numeric(substr(Season,1,4))+1,"-",as.numeric(substr(Season,1,4))+2)))
.adjust_Minutes <- function(data,increment,topHeavy = 5,convertEffMin = FALSE){
  #increment <- 0.2  
  if (convertEffMin) { # transform effMin back to MP per game
    data <- mutate(data, MP = effMin*82*48)
  }
  minAdjust <- filter(data,!(Tm == "TOT"))
  playersAdj <- data.frame()  
  for (team in unique(minAdjust$Tm)) {
    atl <- filter(minAdjust, Tm == team) %>% arrange(desc(MP))
    total_min <- sum(atl$MP)
    leftout_min <- sum(atl$MP[(topHeavy+1):nrow(atl)])
    # top 5 highest minutes played increase their play time by x% 
    # Set a limit of 46 out of 48 max minutes per game per player
    atl$MP[1:topHeavy] <- ifelse(atl$MP[1:topHeavy]*(1+increment)>46,46,atl$MP[1:topHeavy]*(1+increment))
    # adjust the rest to sum up to total_min
    increm_min <- sum(atl$MP[1:topHeavy])
    leftout_coef <- (total_min-increm_min)/leftout_min
    atl$MP[(topHeavy+1):nrow(atl)] <- leftout_coef*atl$MP[(topHeavy+1):nrow(atl)]
    if (nrow(playersAdj)>0) playersAdj <- bind_rows(playersAdj,atl) else playersAdj <- atl
    
    #print(round(total_min,1) == round(sum(atl$MP),1))
  } 
  
  if (convertEffMin) { # transform effMin back to MP per game
    data <- mutate(data, effMin = MP/(82*48)) %>% select(-MP)
  }
  
  return(playersAdj)
  
}

.convertMin2Percent <- function(data){
  
  data <- group_by(data, Tm) %>% 
    mutate(TotEffMin = sum(effMin)) %>%
    ungroup() %>%
    mutate(effMin = effMin/TotEffMin) %>%
    select(-TotEffMin) %>%
    data.frame()
  
  return(data)
}

.redistributeMinutes <- function(data,topHeavy = 7,topMinShare = .6, min_share_top1 = .1){
  # Usually a few players amasse a great amount of the minutes. 
  # Ex: Heuristically, top 7 players play 60% of total time
  playersAdj <- data.frame()  
  for (team in unique(data$Tm)) {
    # introduce jitter as some players have the same effMin (they were averaged out at the prediction phase)
    jitter <- runif(nrow(filter(data, Tm == team)))/1000000
    atl <- filter(data, Tm == team) %>% arrange(desc(effMin)) %>%
      as.data.frame() %>%
      mutate(effMin = effMin + jitter) 
    bottomHeavy <- nrow(atl)-topHeavy
    total_min <- sum(atl$effMin)
    topHeavy_min <- sum(atl$effMin[1:(topHeavy)])
    topHeavy_min_target <- total_min*topMinShare
    delta_min <- topHeavy_min_target - topHeavy_min
    # # adjust the rest to sum up to total_min
    atl_top <- top_n(atl, topHeavy, effMin) %>%
      mutate(effMin = effMin + delta_min/topHeavy)
    atl_bottom <- top_n(atl, bottomHeavy, -effMin) %>%
      mutate(effMin = effMin - delta_min/bottomHeavy)
    atl <- rbind(atl_top, atl_bottom)
    # check results
    #sum(atl$effMin[1:(topHeavy)])/total_min
    #sum(atl$effMin[(topHeavy+1):nrow(atl)])/total_min
    
    # double check cases in which after adjustment a player's minutes go beyond the realistic (limit_time_player)
    # No player can have more than 10.5% (or other value provided as parameter) of total team play time (empirically per .minutesDensity())
    player_time_limit <- total_min*min_share_top1
    overplay <- 0
    atl$overplay <- 0
    for (i in 1:topHeavy) {
      if (atl$effMin[i] > player_time_limit) {
        overplay <- overplay + atl$effMin[i] - player_time_limit
        atl$effMin[i] <- player_time_limit
        atl$overplay[i] <- 1 # this player surpassed minutes allowed
      }
    }
    # distribute the extra minutes among all players (except those exceeding)
    overplay_share <- overplay/nrow(filter(atl, overplay == 0))
    atl <- mutate(atl, effMin = ifelse(overplay == 0, effMin+overplay_share,effMin)) %>% 
      select(-overplay)
    # check results
    #sum(atl$effMin[1:(topHeavy)])/total_min
    #sum(atl$effMin[(topHeavy+1):nrow(atl)])/total_min
    
    # run the adjustment again until settled
    iter <- 1 # avoid infinite loops
    while (abs(delta_min) > .0001 & iter <= 10) {
      
      total_min <- sum(atl$effMin)
      topHeavy_min <- sum(atl$effMin[1:(topHeavy)])
      topHeavy_min_target <- total_min*topMinShare
      delta_min <- topHeavy_min_target - topHeavy_min
      # # adjust the rest to sum up to total_min
      atl_top <- top_n(atl, topHeavy, effMin) %>%
        mutate(effMin = effMin + delta_min/topHeavy)
      atl_bottom <- top_n(atl, bottomHeavy, -effMin) %>%
        mutate(effMin = effMin - delta_min/bottomHeavy)
      atl <- rbind(atl_top, atl_bottom)
      #
      player_time_limit <- total_min*min_share_top1
      overplay <- 0
      atl$overplay <- 0
      for (i in 1:topHeavy) {
        if (atl$effMin[i] > player_time_limit) {
          overplay <- overplay + atl$effMin[i] - player_time_limit
          atl$effMin[i] <- player_time_limit
          atl$overplay[i] <- 1 # this player surpassed minutes allowed
        }
      }
      # distribute the extra minutes among all players (except those exceeding)
      overplay_share <- overplay/nrow(filter(atl, overplay == 0))
      atl <- mutate(atl, effMin = ifelse(overplay == 0, effMin+overplay_share,effMin)) %>% 
        select(-overplay)
      
      iter <- iter + 1
    }
    
    if (nrow(playersAdj)>0) playersAdj <- bind_rows(playersAdj,atl) else playersAdj <- atl
  }
  # add jitter to the final effMin to avoid duplicated effMin
  jitter <- runif(nrow(playersAdj))/1000000
  playersAdj <- mutate(playersAdj, effMin = effMin + jitter)
  
  return(playersAdj)
}
# Trade players
# data <- playersNew
# playA <- "Paul George"
# playB <- "Edy Tavares"
# tmA <- "IND"
# tmB <- "CLE"
.trade_Players <- function(data,playA,tmA=NULL,playB=NULL,tmB=NULL){
  
  if (is.null(tmA)) tmA <- filter(data, Player == playA)$Tm
  
  if (is.null(tmB)) { # player is traded out of NBA or retires
    
    #playerA_row <- filter(data, Player %in% playA, Tm == tmA)
    data <- filter(data, !(Player %in% playA))
    
  } else if (is.null(playB)) {  # playerA is traded to teamB
    
    playerA_row <- filter(data, Player %in% playA, Tm == tmA) %>% mutate(Tm = tmB)
    data <- filter(data, !(Player %in% c(playA))) %>% 
      bind_rows(playerA_row)
    
  } else { # trade between 2 NBA teams
    
    playerA_row <- filter(data, Player %in% playA, Tm == tmA) %>% mutate(Tm = tmB)
    playerB_row <- filter(data, Player %in% playB, Tm == tmB) %>% mutate(Tm = tmA)
    data <- filter(data, !(Player %in% c(playA, playB))) %>% 
      bind_rows(playerA_row) %>% bind_rows(playerB_row)  
    
  }
  
  return(data)
  
}

# injured player
.disabled_list <- function(data,player_list) {
  
  data <- mutate(data, effMin = ifelse(Player %in% player_list, 0, effMin))
  
  return(data)
  
}

# Calculate the average player
.calculate_AvgPlayer <- function(data, age=NULL) {
  thisSeason <- data$Season[1]
  if (is.null(age)){ # average all
    avgPlayer <- summarise_if(data,is.numeric,funs(mean)) %>% 
      mutate(Player = "Average Player", Pos = "X", Tm = "X", Season = thisSeason) %>%
      select(Player,Pos,Age,Tm,everything())
  } else { # average by age
    avgPlayer <- filter(data, Age == age) %>% 
      summarise_if(is.numeric,funs(mean)) %>% 
      mutate(Player = "Average Player", Pos = "X", Tm = "X", Season = thisSeason) %>%
      select(Player,Pos,Age,Tm,everything())
  }
  
  
  return(avgPlayer)
}

# Calculate average distribution of minutes for top 1,2,3,...10 players to help adjust minutes before prediction
.minutes_density <- function(data, seasons = 5) {
  
  data_team <- data %>%
    filter(Season %in% top_n(distinct(data,Season),seasons)$Season, !(Tm == "TOT")) %>% # take last 5 seasons as sample
    group_by(Player) %>%
    mutate(effMin = MP/3936) %>%
    select(Player,Season,Tm,effMin)
  
  # Adjust effMin to reflect percentage of total team time played, ie, sum(effMin) = 5
  data_team2 <- data_team %>%
    group_by(Tm,Season) %>%
    mutate(effMin = effMin/sum(effMin,na.rm=TRUE)) %>%
    arrange(Season,Tm,desc(effMin)) %>%
    top_n(18,effMin) %>%
    mutate(top18 = sum(effMin)) %>%
    top_n(17,effMin) %>%
    mutate(top17 = sum(effMin)) %>%
    top_n(16,effMin) %>%
    mutate(top16 = sum(effMin)) %>%
    top_n(15,effMin) %>%
    mutate(top15 = sum(effMin)) %>%
    top_n(14,effMin) %>%
    mutate(top14 = sum(effMin)) %>%
    top_n(13,effMin) %>%
    mutate(top13 = sum(effMin)) %>%
    top_n(12,effMin) %>%
    mutate(top12 = sum(effMin)) %>%
    top_n(11,effMin) %>%
    mutate(top11 = sum(effMin)) %>%
    top_n(10,effMin) %>%
    mutate(top10 = sum(effMin)) %>%
    top_n(9,effMin) %>%
    mutate(top9 = sum(effMin)) %>%
    top_n(8,effMin) %>%
    mutate(top8 = sum(effMin)) %>%
    top_n(7,effMin) %>%
    mutate(top7 = sum(effMin)) %>%
    top_n(6,effMin) %>%
    mutate(top6 = sum(effMin)) %>%
    top_n(5,effMin) %>%
    mutate(top5 = sum(effMin)) %>%
    top_n(4,effMin) %>%
    mutate(top4 = sum(effMin)) %>%
    top_n(3,effMin) %>%
    mutate(top3 = sum(effMin)) %>%
    top_n(2,effMin) %>%
    mutate(top2 = sum(effMin)) %>%
    top_n(1,effMin) %>%
    mutate(top1 = sum(effMin)) %>%
    distinct(Season, Tm, .keep_all=TRUE) %>%
    ungroup() %>%
    select(-c(Player,effMin)) %>%
    as.data.frame()
  
  return(data_team2)
}

########## TEAMS ###########

# ----------------------------
# Calculations by team
# thisTeam <- "GSW"
#thisTeamPlayers <- as.character(filter(playersHist, Tm == thisTeam, Season == max(as.character(Season)))$Player)

# compute effective stats for most current season
.team_prepare <- function(thisTeam="All"){
  
  if (thisTeam == "All"){
    
    data_team <- playersHist %>%
      filter(Season == max(as.character(Season))) %>%
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
      dplyr::select(Player,Pos,Season,Tm,Age,FGPer = FG.,FG3Per = X3P., FG2Per = X2P., effFGPer = eFG.,
                    FTPer = FT., starts_with("eff"),
                    -G,-GS,-MP,FG,-FGA,-X3P,-X3PA,-X2P,-X2PA,-FG,-FTA,-ORB,-DRB,-TRB,-AST,
                    -BLK,-TOV,-PF,-FT,-STL,-PTS)
    
    # Impute NAs by 0. If NA means no shot attempted, ie, 
    # either the player didn't play enough time or is really bad at this particular type of shot.
    for (i in 5:ncol(data_team)){
      data_team[is.na(data_team[,i]),i] <- 0
    }
    # Adjust effMin to reflect percentage of total team time played, ie, sum(effMin) = 5
    data_team <- data_team %>%
      group_by(Tm) %>%
      mutate(effMin = 5*effMin/sum(effMin,na.rm=TRUE))
    
    data_team <- as.data.frame(data_team)
    
  } else{
    data_team <- playersHist %>%
      filter(Tm == thisTeam, Season == max(as.character(Season))) %>%
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
      select(Player,Pos,Season,Age,FGPer = FG.,FG3Per = X3P., FG2Per = X2P., effFGPer = eFG.,
             FTPer = FT., starts_with("eff"),
             -Tm,-G,-GS,-MP,FG,-FGA,-X3P,-X3PA,-X2P,-X2PA,-FG,-FTA,-ORB,-DRB,-TRB,-AST,
             -BLK,-TOV,-PF,-FT,-STL,-PTS)
    
    # Impute NAs by 0. If NA means no shot attempted, ie, 
    # either the player didn't play enough time or is really bad at this particular type of shot.
    for (i in 4:ncol(data_team)){
      data_team[is.na(data_team[,i]),i] <- 0
    }
    # Adjust effMin to reflect percentage of total team time played, ie, sum(effMin) = 5
    teamMinutes <- sum(data_team$effMin)
    data_team$effMin <- 5*data_team$effMin/teamMinutes
    
    data_team <- as.data.frame(data_team)
    
  }
  return(data_team)
}

# compute effective stats for one new season
.team_preparePredict <- function(data = playersNew, thisTeam="All", singlePlayer = FALSE){
  
  if (thisTeam == "All"){
    
    data_team <- data %>%
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
      dplyr::select(Player,Pos,Season,Tm,Age,FGPer = FG.,FG3Per = X3P., FG2Per = X2P., effFGPer = eFG.,
                    FTPer = FT., starts_with("eff"),
                    -G,-GS,-MP,FG,-FGA,-X3P,-X3PA,-X2P,-X2PA,-FG,-FTA,-ORB,-DRB,-TRB,-AST,
                    -BLK,-TOV,-PF,-FT,-STL,-PTS)
    
    # Impute NAs by 0. If NA means no shot attempted, ie, 
    # either the player didn't play enough time or is really bad at this particular type of shot.
    for (i in 5:ncol(data_team)){
      data_team[is.na(data_team[,i]),i] <- 0
    }
    # Adjust effMin to reflect percentage of total team time played, ie, sum(effMin) = 5
    data_team <- data_team %>%
      group_by(Tm) %>%
      mutate(effMin = 5*effMin/sum(effMin,na.rm=TRUE))
    
    data_team <- as.data.frame(data_team)
    
  } else {
    data_team <- data %>%
      filter(Tm == thisTeam) %>%
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
      select(Player,Pos,Season,Age,FGPer = FG.,FG3Per = X3P., FG2Per = X2P., effFGPer = eFG.,
             FTPer = FT., starts_with("eff"),
             -Tm,-G,-GS,-MP,FG,-FGA,-X3P,-X3PA,-X2P,-X2PA,-FG,-FTA,-ORB,-DRB,-TRB,-AST,
             -BLK,-TOV,-PF,-FT,-STL,-PTS)
    
    # Impute NAs by 0. If NA means no shot attempted, ie, 
    # either the player didn't play enough time or is really bad at this particular type of shot.
    for (i in 4:ncol(data_team)){
      data_team[is.na(data_team[,i]),i] <- 0
    }
    # Adjust effMin to reflect percentage of total team time played, ie, sum(effMin) = 5
    if (singlePlayer==FALSE){
      teamMinutes <- sum(data_team$effMin)
      data_team$effMin <- 5*data_team$effMin/teamMinutes
    }
    
    data_team <- as.data.frame(data_team)
    
  }
  return(data_team)
}

# compute effective stats for all seasons. Uses playersHist
.team_prepareAll <- function(){
  
  playersHist <- read.csv("data/playersHist.csv", stringsAsFactors = FALSE) # read historical players from write_playersHist.R
  playersHist <- .rename_PlayerDuplicates(playersHist) # differentiate different players with the same name
  
  data_team <- playersHist %>%
    group_by(Player,Season) %>%
    mutate(effMin = MP/3936, effFG = FG/(3936*effMin),
           effFGA = FGA/(3936*effMin),eff3PM = X3P/(3936*effMin),eff3PA = X3PA/(3936*effMin),
           eff2PM = X2P/(3936*effMin),eff2PA = X2PA/(3936*effMin),
           effFTM = FT/(3936*effMin),effFTA = FTA/(3936*effMin),
           effORB = ORB/(3936*effMin),effDRB = DRB/(3936*effMin),
           effTRB = TRB/(3936*effMin),effAST = AST/(3936*effMin),
           effSTL = STL/(3936*effMin),effBLK = BLK/(3936*effMin),
           effTOV = TOV/(3936*effMin),effPF = PF/(3936*effMin),
           effPTS = PTS/(3936*effMin)) %>%
    dplyr::select(Player,Pos,Season,Tm,Age,FGPer = FG.,FG3Per = X3P., FG2Per = X2P., effFGPer = eFG.,
                  FTPer = FT., starts_with("eff"),
                  -G,-GS,-MP,FG,-FGA,-X3P,-X3PA,-X2P,-X2PA,-FG,-FTA,-ORB,-DRB,-TRB,-AST,
                  -BLK,-TOV,-PF,-FT,-STL,-PTS)
  
  # Impute NAs by 0. If NA means no shot attempted, ie, 
  # either the player didn't play enough time or is really bad at this particular type of shot.
  for (i in 5:ncol(data_team)){
    data_team[is.na(data_team[,i]),i] <- 0
  }
  # Adjust effMin to reflect percentage of total team time played, ie, sum(effMin) = 5
  data_team <- data_team %>%
    group_by(Tm,Season) %>%
    mutate(effMin = 5*effMin/sum(effMin,na.rm=TRUE))
  
  data_team <- as.data.frame(data_team)
  
  return(data_team)
}

# calculate distance between 2 sample distributions
# Distance between 2 teams using Bhattacharyya distance: https://en.wikipedia.org/wiki/Bhattacharyya_distance

# data_tsne <- tsne_ready
# data_Players <- playerDashboard
# teamA <- "CLE"
# teamB <- "GSW"
# num_clusters <- 5
# useEffMin <- TRUE

BC_distance <- function(data_tsne = values$playersTSNE,data_Players = values$playersDatabase,
                        teamA,teamB,num_clusters = input$num_clusters,useEffMin = TRUE) {
  
  set.seed(123)
  playerCluster <- kmeans(data_tsne[, c("x","y")], num_clusters, nstart = 10, iter.max = 20)
  tsne_ready2 <- cbind(data_tsne, cluster = playerCluster$cluster) %>%
    merge(data_Players[,c("Player","Tm","effMin","Offense","Defense")], by=c("Player","Tm")) %>%
    mutate(Defense = -Defense) %>%
    group_by(Player) %>%
    mutate(effMin = ifelse(useEffMin, effMin, 1)) %>%
    ungroup()
  
  if (teamA == teamB) {
    partB <- filter(tsne_ready2, Tm == teamA) %>%
      mutate(Tm = paste0(teamA,"_2")) 
    
    BC <- filter(tsne_ready2, Tm == teamA) %>%
      bind_rows(partB) %>%
      group_by(cluster, Tm) %>%
      mutate(bc_Tm = sum(effMin*1000, na.rm=TRUE)) %>% # variation from the original: multiply by 10 and add 1 as my "sample sizes" are really small numbers representing weights
      ungroup() %>%
      distinct(cluster,Tm,bc_Tm) %>%
      group_by(cluster) %>%
      mutate(bc_cluster = sqrt(prod(bc_Tm))) %>%
      distinct(cluster,bc_cluster) %>%
      ungroup() %>%
      summarise(dist = 1/log1p(sum(bc_cluster,na.rm=TRUE))) # variation from the original, it's more intuitive for me to use BC as denominator
    
  } else {
    BC <- filter(tsne_ready2, Tm %in% c(teamA,teamB)) %>%
      group_by(cluster, Tm) %>%
      mutate(bc_Tm = sum(effMin*1000, na.rm=TRUE)) %>%
      ungroup() %>%
      distinct(cluster,Tm,bc_Tm) %>%
      group_by(cluster) %>%
      mutate(bc_cluster = sqrt(prod(bc_Tm))) %>%
      distinct(cluster,bc_cluster) %>%
      ungroup() %>%
      summarise(dist = 1/log1p(sum(bc_cluster,na.rm=TRUE))) # variation from the original, it's more intuitive for me to use BC as denominator
  }
  
  
  return(as.numeric(BC))
}

# t-SNE teams

.compute_teams_tSNE_points <- function(data, num_iter, max_num_neighbors,removeEffMin = TRUE){
  
  require(tsne)
  teamStats <- .computeTeamStats(data=data,removeEffMin = removeEffMin)
  data_tsne_sample <- teamStats %>% 
    select_if(is.numeric) %>%
    mutate_all(function(x) (x-min(x))/(max(x)-min(x)))
  
  if (nrow(data_tsne_sample)>0){
    #num_iter <- 500
    #max_num_neighbors <- 2
    set.seed(456) # reproducitility
    tsne_points <- tsne(data_tsne_sample, 
                        max_iter=as.numeric(num_iter), 
                        perplexity=as.numeric(max_num_neighbors), 
                        epoch=100)
    plot(tsne_points)
  } else {
    tsne_points <- c()
  }
  return(tsne_points)
}

# put together tsne_ready predicted to load at start of dashboards
.compute_tSNE_ready_teams <- function(data,removeEffMin = TRUE){
  
  tsne_points <- .compute_teams_tSNE_points(data,num_iter = 500,max_num_neighbors = 2,removeEffMin)
  
  # load data
  data_tsne_sample <- .computeTeamStats(data=data,removeEffMin=removeEffMin) %>%
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
  
  return(tsne_ready)
}

# Phase 2 ------------------------------------
# Once rosters are updated (Phase 1), predict avg points and avg points against
# per team for the new season or for a season in the past (back to 1979-1980)
# compute avg PTS as offensive power and PTSA as defensive power
.computePower <- function(data = playersNew, Off_or_Def, thisTeam = "All", defaultMinutes = NULL, removeEffMin = TRUE, 
                          actualOrPredicted = "actual", maxs_vector = NULL, mins_vector = NULL){
  
  # specifically, this function will prepare playersNew dataset by default
  # It is understood, playersNew is the updated rosters at the beginning of a new season
  if (actualOrPredicted=="actual"){ # whether actual data (before prediction) or predicted data
    playersSumm <- .prepareModelPrediction(data = data, thisTeam)  
  } else {
    playersSumm <- .prepareModelOncePredicted(data_team = data, thisTeam = thisTeam)  
  }
  
  # effMin is 1 of the variables that get averaged weighted by effMin, in case it adds noise to the
  # neural network 
  if (!is.null(defaultMinutes)) { 
    playersSumm$effMin <- defaultMinutes
  }
  if (removeEffMin & ncol(select(playersSumm, one_of("effMin")))>0) { 
    playersSumm <- select(playersSumm, -effMin)
  }
  ## Strip linearly relationed columns: FG, FGA, FG%,3P%,2P%,FT%,effFG%, effPTS
  #playersSumm <- select(playersSumm, -contains("Per"), -effFG, -effFGA, -effPTS, -effTRB)
  ## End of Strip
  # scale the variables the same way the training dataset was scaled so the nnet makes sense
  # scaleMaxMin needs a reference column (team, player, etc.). I need to make sure it's the first column
  playersSumm <- select(playersSumm, team_season, everything())
  # The scale has to be preserved all the way during trades, otherwise, every single change 
  # in the composition of rosters will trigger a new scale and Offense and Defense powers
  # will be messed up
  if ((!is.null(maxs_vector)) & (!is.null(mins_vector))) {
    if (Off_or_Def == "PTS"){
      maxs <- maxs_vector[-nrow(maxs_vector),1]
      mins <- mins_vector[-nrow(maxs_vector),1]
    } else {
      maxs <- maxs_vector[-nrow(maxs_vector),2]
      mins <- mins_vector[-nrow(maxs_vector),2]
    }
  } else {
    scaleMaxMin <- .getScaleLimits(Off_or_Def, playersSumm)
    maxs <- scaleMaxMin$maxs
    mins <- scaleMaxMin$mins
  }
  team_season <- playersSumm[,1]
  scaled <- as.data.frame(scale(playersSumm[,-1], center = mins, scale = maxs - mins))
  scaled <- cbind(team_season,scaled)
  
  # NNet model pre-calculated (2 models, Offense and Defense)
  #nn <- .selectedModel(Off_or_Def) 
  if (Off_or_Def == "PTS"){
    nn <- nn_Offense
  } else {
    nn <- nn_Defense  
  }
  
  # Prediction
  scaled <- dplyr::select(scaled, -team_season)
  pr.nn <- compute(nn,scaled)
  #test_pr <- compute(nn,testing[,-ncol(testing)])
  # Model results are scaled so need to re-scale them back to normal
  #pr.nn_ <- pr.nn$net.result*(scaleMaxMin["PTS","maxs"]-scaleMaxMin["PTS","mins"])+scaleMaxMin["PTS","mins"]
  Off_or_Def_teamStats <- select(team_stats, one_of(Off_or_Def))
  pr.nn_ <- pr.nn$net.result*(max(Off_or_Def_teamStats)-min(Off_or_Def_teamStats))+min(Off_or_Def_teamStats)
  pred_PTS <- as.numeric(pr.nn_)
  # bring back the team names
  pr_pts <- cbind(team_season,pred_PTS)
  pr_pts <- as.data.frame(pr_pts)
  
  return(pr_pts)
}

# Put together teams and predicted powers as input to a new regular season
.teamsPredictedPower <- function(data = playersNew, defaultMin = NULL, actualOrPred="actual",
                                 maxs_vector = NULL, mins_vector = NULL) {
  
  Def <- .computePower(data = data,Off_or_Def = "PTSA",thisTeam = "All",defaultMinutes = defaultMin,
                       actualOrPredicted = actualOrPred, maxs_vector = maxs_vector, mins_vector = mins_vector)
  Off <- .computePower(data = data,Off_or_Def = "PTS", thisTeam = "All",defaultMinutes = defaultMin,
                       actualOrPredicted = actualOrPred, maxs_vector = maxs_vector, mins_vector = mins_vector)
  team_power <- merge(Off,Def,by="team_season")
  
  team_power <- team_power %>%
    mutate(teamCode = substr(team_season,1,3),
           Season = substr(team_season, 5,13))
  
  team_power <- as.data.frame(team_power)
  names(team_power) <- c("team_season","TEAM_PTS","TEAM_PTSAG","TeamCode","Season")
  
  team_power$TEAM_PTS <- as.numeric(as.character(team_power$TEAM_PTS))
  team_power$TEAM_PTSAG <- as.numeric(as.character(team_power$TEAM_PTSAG))
  
  return(team_power)
  
}

# Analytical calculation of wins based on teamsPowers and Normal distribution defined by those powers
.computeWins <- function(data){
  
  # Load season schedule
  season <- realSeasonSchedule %>%
    mutate(Date = paste(Date,StartTime)) %>%
    dplyr::select(-StartTime)
  
  # calculate wins
  wins <- data.frame()
  for (i in 1:nrow(season)){
    thisGame <- .calculateWinProbability(data,season[i,2],season[i,3],home_away_factor)
    wins[i,1] <- thisGame
    wins[i,2] <- 1-thisGame
  }
  return(wins)
}


.teamsPredictedWins <- function(data) {
  
  set.seed(456) 
  # use the actual schedule
  
  
  regSeasonProbs <- .computeWins(data)
  seasonProbs <- bind_cols(realSeasonSchedule,regSeasonProbs)
  names(seasonProbs) <- c("day","time","home_team","away_team","home_team_winProb","away_team_winProb")
  datesRange <- unique(seasonProbs$day)
  
  homeWins <- group_by(seasonProbs, home_team) %>%
    mutate(home_wins = sum(home_team_winProb)) %>%
    select(team = home_team,wins = home_wins) %>%
    distinct()
  awayWins <- group_by(seasonProbs, away_team) %>%
    mutate(away_wins = sum(away_team_winProb)) %>%
    select(team = away_team,wins = away_wins) %>%
    distinct()
  
  seasonWins <- rbind(homeWins,awayWins) %>%
    group_by(team) %>%
    summarise_if(is.numeric,sum) %>%
    as.data.frame()
  
  return(seasonWins) 
  
}

# Compute teams average stats
.computeTeamStats <- function(data = playersNew, thisTeam = "All", actualOrPredicted="predicted", defaultMinutes = NULL,removeEffMin = TRUE) {
  
  if (actualOrPredicted=="actual"){ # whether actual data (before prediction) or predicted data
    playersSumm <- .prepareModelPrediction(data=data, thisTeam=thisTeam)  
  } else {
    playersSumm <- .prepareModelOncePredicted(data_team=data, thisTeam=thisTeam)  
  }
  
  # effMin is 1 of the variables that get averaged weighted by effMin, in case it adds noise to the
  # neural network 
  if (!is.null(defaultMinutes)) { 
    playersSumm$effMin <- defaultMinutes
  }
  if (removeEffMin & ncol(select(playersSumm, one_of("effMin")))>0) { 
    playersSumm <- select(playersSumm, -effMin)
  }
  
  playersSumm <- mutate(playersSumm, Tm = substr(team_season,1,3), Season = substr(team_season,5,nchar(team_season))) %>%
    select(Tm, Season, everything(), -team_season)
  
  return(playersSumm)
  
}

########## ROOKIES ###########

# Find similar players ------------------------------
#
# Using t-sne algorithm, find players that have similar characteristics to a given player.
# The objective is to predict his performance in a given year based on the historical performance
# of similar players (see: Nate Silver's CARMELO or PECOTA systems)
#
# Ex: If I want to predict Pau Gasol numbers for the season he will turn 36, I will start
# with his numbers in the previous seasons and I will adjust according to the average
# evolution of similar players when they turned 36.
#
# Ex: To be able to assign predicted characteristics to a rookie player, I will do a
# similar approach. See functions related to rookies and draft
#
.tSNE_prepareRookies <- function(){
  # num_iter <- 300
  # max_num_neighbors <- 20
  # playerName <- "Stephen Curry"
  rookieStats <- read.csv("data/rookieStats.csv", stringsAsFactors = FALSE)
  rookieStats <- filter(rookieStats, !(College %in% c("International", "Europe")))
  
  rookieStatsHist <- read.csv("data/rookieStatsHist.csv", stringsAsFactors = FALSE)
  # transform rookieStatsHist stats to relative numbers
  rookieStatsHist <- rookieStatsHist %>%
    group_by(Player,Season) %>%
    mutate(MP = MP/G, FG = FG/G,
           FGA = FGA/G,X3P = X3P/G,X3PA = X3PA/G,
           X2P = X2P/G,X2PA = X2PA/G,
           FT = FT/G,FTA = FTA/G,
           ORB = ORB/G,DRB = DRB/G,
           TRB = TRB/G,AST = AST/G,
           STL = STL/G,BLK = BLK/G,
           TOV = TOV/G,PF = PF/G,
           PTS = PTS/G)
  # all together, ready for tsne
  collegeHist <- bind_rows(rookieStats,rookieStatsHist)
  
  data_tsne <- collegeHist %>%
    group_by(Player,Season) %>%
    mutate(effFG = FG,
           effFGA = FGA,eff3PM = X3P,eff3PA = X3PA,
           eff2PM = X2P,eff2PA = X2PA,
           effFTM = FT,effFTA = FTA,
           effORB = ORB,effDRB = DRB,
           effTRB = TRB,effAST = AST,
           effSTL = STL,effBLK = BLK,
           effTOV = TOV,effPF = PF,
           effPTS = PTS) %>%
    dplyr::select(Player,Pos,Season,Pick,starts_with("eff"))
  
  # t-sne doesn't like NAs. Impute by assigning the average of the variable. 
  # If NA means no shot attempted, ie, 
  # either the player didn't play enough time or is really bad at this particular type of shot.
  data_tsne <- as.data.frame(data_tsne)
  for (i in 4:ncol(data_tsne)){
    data_tsne[is.na(data_tsne[,i]),i] <- mean(data_tsne[,i],na.rm=TRUE)
  }
  
  return(data_tsne)
  
}

.tSNE_computeRookies <- function(num_iter, max_num_neighbors){
  
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
  
  return(tsne_points) 
  
}

# compute colors for regions
.getColorsRookies <- function(num_iter, max_num_neighbors,colVar){
  #colVar <- "Pos"
  data_tsne <- .tSNE_prepareRookies()
  if (colVar == "Season"){
    colors <- rainbow(length(unique(data_tsne$Season)))
    names(colors) <- unique(data_tsne$Season)
  } else {
    colors <- rainbow(length(unique(data_tsne$Pos)))
    names(colors) <- unique(data_tsne$Pos)
  }
  return(colors)
}

# tsne chart ---------------------------------------------------------
.tSNE_plotRookies <- function(playerName, num_iter, max_num_neighbors, colVar){
  
  #tsne_points <- .tSNE_compute(num_iter, max_num_neighbors, playerAge)
  tsne_points <- read.csv("data/tsne_pointsRookies.csv",stringsAsFactors = FALSE)
  if (length(tsne_points)>0){
    par(mar=c(0,0,0,0))
    plot(tsne_points,t='n', axes=FALSE, frame.plot = FALSE, xlab = "",ylab = ""); 
    graphics::text(tsne_points,labels=as.character(data_tsne$Player), col=.getColorsRookies(num_iter, max_num_neighbors,colVar))
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Not enough data", col="red", cex=2)
  }
  
}

# tsne dist ---------------------------------------------------------
.tSNE_distRookies <- function(playerName){
  
  data_tsne <- .tSNE_prepareRookies()
  lastDraft <- max(data_tsne$Season)
  
  #tsne_points <- .tSNE_compute(num_iter, max_num_neighbors, playerAge)
  tsne_points <- read.csv("data/tsne_pointsRookies.csv",stringsAsFactors = FALSE)
  if (length(tsne_points)>0 & nrow(filter(data_tsne, Player == playerName))>0){
    # calculate the euclidean distance between the selected player and the rest
    dist_mat <- cbind(tsne_points,as.character(data_tsne$Player),data_tsne$Season)
    if (filter(data_tsne, Player == playerName)$Season == lastDraft){
      dist_mat <- dist_mat[!(data_tsne$Season==lastDraft & !(data_tsne$Player==playerName)),]
    }
    dist_mat <- as.data.frame(dist_mat, stringsAsFactors=FALSE)
    dist_mat$V1 <- as.numeric(dist_mat$V1)
    dist_mat$V2 <- as.numeric(dist_mat$V2)
    distCou1 <- dist_mat[dist_mat[,3]==playerName,1]
    distCou2 <- dist_mat[dist_mat[,3]==playerName,2]
    dist_mat <- mutate(dist_mat, dist = sqrt((V1-distCou1)^2+(V2-distCou2)^2))
    # order by closest distance to selected player
    dist_mat <- arrange(dist_mat, dist)[,c(3,4)]
    names(dist_mat) <- c("Player","Euclid. distance")
  } else {
    dist_mat <- data_frame()
  }
  
  return(dist_mat)
} 

# return predicted stats rookie season for any drafted player from college
.predictPlayerCollegeRookie <- function(playerName){
  
  data_tsne <- .tSNE_prepareRookies()
  similarPlayers <- .tSNE_distRookies(playerName)
  
  theirStats <- filter(data_tsne, Player %in% head(similarPlayers[-1,1],5))
  
  rookieNBAStats <- playersHist %>%
    group_by(Player) %>%
    filter(Season == min(as.character(Season)))
  rookieNBAStats <- as.data.frame(rookieNBAStats)
  thisSelection <- filter(rookieNBAStats, Player %in% theirStats$Player)
  thisSelectionPrep <- .tSNE_prepareSelected(thisSelection)
  this_numRows <- nrow(thisSelectionPrep)
  for (i in 4:ncol(thisSelectionPrep)){
    thisSelectionPrep[this_numRows+1,i] <- mean(thisSelectionPrep[1:this_numRows,i])
  }
  thisPlayer <- filter(data_tsne, Player == playerName)
  thisSelectionPrep$Player <- as.character(thisSelectionPrep$Player)
  thisSelectionPrep$Pos <- as.character(thisSelectionPrep$Pos)
  thisSelectionPrep$Season <- as.character(thisSelectionPrep$Season)
  thisSelectionPrep$Player[nrow(thisSelectionPrep)] <- thisPlayer$Player
  thisSelectionPrep$Pos[nrow(thisSelectionPrep)] <- thisPlayer$Pos
  thisSelectionPrep$Season[nrow(thisSelectionPrep)] <- as.character(playersNew$Season[1])
  
  playerPredicted <- filter(thisSelectionPrep, Player == playerName)
  
  return(playerPredicted)
}

# Non college players predicted stats
.predictPlayerNonCollegeRookie <- function(playerName){
  # Remove from playersHist those who played college and average out their stats by position.
  collegePlayersHist <- read.csv("data/collegePlayersHist.csv",stringsAsFactors = FALSE)
  collegePlayersHist <- collegePlayersHist %>%
    group_by(Player) %>%
    filter(Season == max(Season))
  onlyCollegeRookies <- dplyr::select(collegePlayersHist,Player)
  onlyCollegeRookies <- onlyCollegeRookies$Player
  
  nonCollegeRookies <- playersHist %>%
    filter(!(Player %in% onlyCollegeRookies)) %>%
    group_by(Player) %>%
    filter(Season == min(Season)) %>%
    distinct(Player, .keep_all=TRUE)
  
  # Calculate average stats for nonCollegeRookies on their first NBA season by position.
  # This will provide players without much statistical background in NBA or College with 
  # some prior stats. 
  # Can't do the above so I will do overall priors with no filter
  
  nonCollegeRookies_Stats <- nonCollegeRookies %>%
    filter(Season >= "1994-1995") %>%
    group_by() %>%
    summarise_at(c(5:(ncol(nonCollegeRookies)-1)),funs(mean(.,na.rm=TRUE)))
  
  # assign stats to input player and then adjust those stats like in .tsnePrepare
  # get player's postition
  rookieStats <- read.csv("data/rookieStats.csv", stringsAsFactors = FALSE)
  rookieStats <- filter(rookieStats, College %in% c("International", "Europe"))
  #rookieStats <- rookieStats[,1:29]
  
  playerPredicted <- rookieStats %>%
    filter(Player == playerName) %>%
    dplyr::select(Player,Tm=Team,Pick) %>%
    mutate(Season = paste0(lastDraft,"-",lastDraft+1))
  playerPredicted <- bind_cols(playerPredicted,nonCollegeRookies_Stats)
  
  playerPredicted <- playerPredicted %>%
    mutate(MP = MP/G, FG = FG/G,
           FGA = FGA/G,X3P = X3P/G,X3PA = X3PA/G,
           X2P = X2P/G,X2PA = X2PA/G,
           FT = FT/G,FTA = FTA/G,
           ORB = ORB/G,DRB = DRB/G,
           TRB = TRB/G,AST = AST/G,
           STL = STL/G,BLK = BLK/G,
           TOV = TOV/G,PF = PF/G,
           PTS = PTS/G)
  
  playerPredicted <- playerPredicted %>%
    mutate(effFG = FG,effMin = MP/3936, # this will underestimate the minutes played
           # but I don't mind as he's a rookie and will most likely play fewer minutes
           effFGA = FGA,eff3PM = X3P,eff3PA = X3PA,
           eff2PM = X2P,eff2PA = X2PA,
           effFTM = FT,effFTA = FTA,
           effORB = ORB,effDRB = DRB,
           effTRB = TRB,effAST = AST,
           effSTL = STL,effBLK = BLK,
           effTOV = TOV,effPF = PF,
           effPTS = PTS) %>%
    dplyr::select(Player,Season,FGPer = FG.,FG3Per = X3P., FG2Per = X2P., effFGPer = eFG.,
                  FTPer = FT., starts_with("eff")) %>% mutate(Pos = "X")
  
  return(playerPredicted)
  
}

########## NEURALNET ###########

.prepareModel <- function(Off_or_Def, removeEffMin = TRUE){ 
  # Approach: Summarize variables at team level to obtain input vector for the model
  # 1. By team: calculate weighted average of each characteristic: FGM, FGA, etc...
  data_team <- .team_prepareAll() # If no arguments, will calculate for all teams for all seasons
  # 2. Weights correspond to percentage of total team time played, ie, sum(effMin) = 5
  # Probably more efficient to scale weights to add up to 1: Wt = effMin/5
  playersSumm <- data_team %>%
    filter(!(Tm == "TOT")) %>% # Those who played for more than 1 team have a Total team
    mutate(Wt = effMin/5) %>%
    group_by(Tm,Season) %>%
    #summarise_if(is.numeric, funs(sum(.*Wt, na.rm=TRUE))) %>%
    summarise_if(is.numeric, funs(weighted.mean(.,Wt, na.rm=TRUE))) %>%
    dplyr::select(-Wt) %>%
    distinct(.keep_all=TRUE)
  
  # Paste Team and Season to have 1 field as identifier
  playersSumm <- playersSumm %>%
    mutate(team_season = paste0(Tm,"_",Season))
  playersSumm <- playersSumm[,!(names(playersSumm) %in% c("Tm","Season"))]
  
  if (removeEffMin) {
    playersSumm <- select(playersSumm, -effMin)
  }
  playersSumm <- as.data.frame(playersSumm)
  
  ## add team's average points in season (output variable y~ in the regression)
  # Paste Team and Season to have 1 field as identifier
  team_stats2 <- team_stats %>%
    mutate(team_season = paste0(teamCode,"_",Season))
  team_stats2 <- team_stats2[,!(names(team_stats2) %in% c("Team","teamCode","Season"))]
  # merge
  playersSumm <- merge(playersSumm, team_stats2[,c("team_season",Off_or_Def)], by = "team_season", all.x=TRUE)
  
  # No matter if I calculate Off or Def pts per season, I call this variable PTS for practical purposes
  names(playersSumm)[ncol(playersSumm)] <- "PTS" 
  
  return(playersSumm)
}

# For prediction before the prediction, i.e., no PTS per game data available
.prepareModelPrediction <- function(data = playersNew, thisTeam = "All", removeEffMin = TRUE){ 
  # Approach: Summarize variables at team level to obtain input vector for the model
  # 1. By team: calculate weighted average of each characteristic: FGM, FGA, etc...
  data_team <- .team_preparePredict(data, thisTeam) # If no arguments (or thisTeam = "All") will calculate for all teams
  # 2. Weights correspond to percentage of total team time played, ie, sum(effMin) = 5
  # Probably more efficient to scale weights to add up to 1: Wt = effMin/5
  if (!(thisTeam == "All")) data_team <- mutate(data_team, Tm = thisTeam)
  
  playersSumm <- data_team %>%
    filter(!(Tm == "TOT")) %>% # Those who played for more than 1 team have a Total team
    mutate(Wt = effMin/5) %>%
    group_by(Tm,Season) %>%
    #summarise_if(is.numeric, funs(sum(.*Wt, na.rm=TRUE))) %>%
    summarise_if(is.numeric, funs(weighted.mean(.,Wt, na.rm=TRUE))) %>%
    dplyr::select(-Wt) %>%
    distinct(.keep_all=TRUE)
  
  # Paste Team and Season to have 1 field as identifier
  playersSumm <- playersSumm %>%
    mutate(team_season = paste0(Tm,"_",Season))
  playersSumm <- playersSumm[,!(names(playersSumm) %in% c("Tm","Season"))]
  
  if (removeEffMin) {
    playersSumm <- select(playersSumm, -effMin)
  }
  playersSumm <- as.data.frame(playersSumm)
  
  #   ## add team's average points in season (output variable y~ in the regression)
  #   # Paste Team and Season to have 1 field as identifier
  #   team_stats2 <- team_statsNew %>%
  #     mutate(team_season = paste0(teamCode,"_",Season))
  #   team_stats2 <- team_stats2[,!(names(team_stats2) %in% c("Team","teamCode","Season"))]
  
  return(playersSumm)
}

# Once predicted, to be able to computePower, i.e., no PTS per game data available
.prepareModelOncePredicted <- function(data_team = playersNewPredicted, thisTeam = "All", removeEffMin = TRUE){ 
  # Approach: Summarize variables at team level to obtain input vector for the model
  # 1. By team: calculate weighted average of each characteristic: FGM, FGA, etc...
  # data_team <- .team_preparePredict(data, thisTeam) # If no arguments (or thisTeam = "All") will calculate for all teams
  # 2. Weights correspond to percentage of total team time played, ie, sum(effMin) = 5
  # Probably more efficient to scale weights to add up to 1: Wt = effMin/5
  if (!(thisTeam == "All")) data_team <- filter(data_team, Tm == thisTeam)
  
  playersSumm <- data_team %>%
    filter(!(Tm == "TOT")) %>% # Those who played for more than 1 team have a Total team
    mutate(Wt = effMin) %>%
    group_by(Tm,Season) %>%
    #summarise_if(is.numeric, funs(sum(.*Wt, na.rm=TRUE))) %>%
    summarise_if(is.numeric, funs(weighted.mean(.,Wt, na.rm=TRUE))) %>%
    dplyr::select(-Wt) %>%
    distinct(.keep_all=TRUE)
  
  # Paste Team and Season to have 1 field as identifier
  playersSumm <- playersSumm %>%
    mutate(team_season = paste0(Tm,"_",Season))
  playersSumm <- playersSumm[,!(names(playersSumm) %in% c("Tm","Season"))]
  
  if (removeEffMin) {
    playersSumm <- select(playersSumm, -effMin)
  }
  playersSumm <- as.data.frame(playersSumm)
  
  #   ## add team's average points in season (output variable y~ in the regression)
  #   # Paste Team and Season to have 1 field as identifier
  #   team_stats2 <- team_statsNew %>%
  #     mutate(team_season = paste0(teamCode,"_",Season))
  #   team_stats2 <- team_stats2[,!(names(team_stats2) %in% c("Team","teamCode","Season"))]
  
  return(playersSumm)
}

# Max and Min for all variables in the available data. Used to rescale later on
.getScaleLimits <- function(Off_or_Def, data = NULL) {
  
  if (is.null(data)) {
    playersSumm <- .prepareModel(Off_or_Def)
  } else {
    playersSumm <- data
  }
  
  # scale the data for easier convergence of backpropagation algorithm
  maxs <- apply(playersSumm[,-1], 2, max) 
  mins <- apply(playersSumm[,-1], 2, min)
  scaleMaxMin <- data.frame(maxs,mins)
  
  return(scaleMaxMin)
}  

# Find the best parameters for the NNet using CV
.computeModel <- function(Off_or_Def) {
  
  playersSumm <- .prepareModel(Off_or_Def)
  scaleMaxMin <- .getScaleLimits(Off_or_Def)
  # scale the data [0,1] for easier convergence of backpropagation algorithm
  maxs <- scaleMaxMin$maxs 
  mins <- scaleMaxMin$mins
  
  team_season <- playersSumm[,1]
  scaled <- as.data.frame(scale(playersSumm[,-1], center = mins, scale = maxs - mins))
  scaled <- cbind(team_season,scaled)
  
  # CROSS VALIDATION --------------------------------------------
  # k: number of splits train-test
  # train_split: number of teams or percentage of data in training set
  set.seed(450)
  cv.error <- NULL
  cv_tr.error <- NULL
  k <- 10
  perc <- 0.80
  train_split <- round(perc*nrow(playersSumm))
  hidden_neurons <- c(6,4,2)
  #c(4,2)
  #c(6,4,2)
  # neuralnet requires explicit formula for the model (f)
  n <- names(scaled[,-1])
  f <- as.formula(paste("PTS ~", paste(n[!n %in% "PTS"], collapse = " + ")))
  
  for(i in 1:k){
    teams_train <- sample(playersSumm$team_season,train_split)
    teams_test <- filter(playersSumm, !(team_season %in% teams_train))$team_season
    training <- filter(scaled, team_season %in% teams_train)
    testing <- filter(scaled, team_season %in% teams_test)
    
    # remove non-numeric variables
    train_teamSeasonCodes <- training$team_season
    test_teamSeasonCodes <- testing$team_season
    training <- training[,-1]
    testing <- testing[,-1]
    
    ## Model Neural Network
    # Hidden layers and neurons per layer specified by hidden. 
    # Number of input neurons is the number of columns
    # Output neurons is 1 as we are doing regression (linear.output=T)
    # For classification problem, linear.output=F
    nn <- neuralnet(f,data=training,hidden=hidden_neurons,linear.output=T)
    
    # Prediction on testing dataset (out of sample)
    pr.nn <- compute(nn,testing[,-ncol(testing)])
    # Model results are scaled so need to scale them back to normal
    pr.nn_ <- pr.nn$net.result*(max(playersSumm$PTS)-min(playersSumm$PTS))+min(playersSumm$PTS)
    test.r <- (testing$PTS)*(max(playersSumm$PTS)-min(playersSumm$PTS))+min(playersSumm$PTS)
    # out of sample error
    cv.error[i] <- sum((test.r - pr.nn_)^2)/nrow(testing)
    
    # Prediction on training dataset (in sample)
    pr_tr.nn <- compute(nn,training[,-ncol(training)])
    pr_tr.nn_ <- pr_tr.nn$net.result*(max(playersSumm$PTS)-min(playersSumm$PTS))+min(playersSumm$PTS)
    train.r <- (training$PTS)*(max(playersSumm$PTS)-min(playersSumm$PTS))+min(playersSumm$PTS)
    # in sample error
    cv_tr.error[i] <- sum((train.r - pr_tr.nn_)^2)/nrow(training)
    
  }
  
  return(nn) # returns nnet model based on training data (perc of the total teams)
}

.computeModel_neuralnet <- function(Off_or_Def){
  
  playersSumm <- .prepareModel(Off_or_Def)
  ## Strip linearly relationed columns: FG, FGA, FG%,3P%,2P%,FT%,effFG%, effPTS
  playersSumm <- select(playersSumm, -contains("Per"), -effFG, -effFGA, -effPTS, -effTRB)
  ## End of Strip
  scaleMaxMin <- .getScaleLimits(Off_or_Def, data = playersSumm)
  # scale the data [0,1] for easier convergence of backpropagation algorithm
  maxs <- scaleMaxMin$maxs 
  mins <- scaleMaxMin$mins
  
  team_season <- playersSumm[,1]
  scaled <- as.data.frame(scale(playersSumm[,-1], center = mins, scale = maxs - mins))
  scaled <- cbind(team_season,scaled)
  
  ###
  set.seed(998)
  perc <- 0.75
  train_split <- round(perc*nrow(playersSumm))
  
  teams_train <- sample(playersSumm$team_season,train_split)
  teams_test <- filter(playersSumm, !(team_season %in% teams_train))$team_season
  training <- filter(scaled, team_season %in% teams_train)
  testing <- filter(scaled, team_season %in% teams_test)
  
  # remove non-numeric variables
  train_teamSeasonCodes <- training$team_season
  test_teamSeasonCodes <- testing$team_season
  training <- training[,-1]
  testing <- testing[,-1]
  
  fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10,
    ## repeated ten times
    repeats = 10)
  
  nnetGrid <-  expand.grid(layer1 = c(4,5,6), 
                           layer2 = c(3,4), 
                           layer3 = c(2,3)
  )
  
  library(neuralnet)
  library(caret)
  #library(tidyverse)
  
  set.seed(825)
  # uses rmse for regression and softmax for classification by default (corresponds to parameter out_activation)
  nnetFit <- train(PTS ~ ., data = training, 
                   method = "neuralnet", 
                   trControl = fitControl, 
                   tuneGrid = nnetGrid)
  
  ##########################################################################################
  # Model checking ----------------------------------------
  ##########################################################################################
  
  # check predictions
  model <- nnetFit
  #save(model, file = paste0("data/model_","nnetFit","_",Sys.Date(),".Rdata"))
  predict_data <- training
  predicted <- predict(model, newdata = predict_data)
  save(model, file = paste0("data/modelNeuralnet5_",Off_or_Def,".Rdata"))
  predictions <- data.frame(actual_PTS = predict_data$PTS, predicted_PTS = predicted)
  plot(predictions)
}

# Once a model is selected, use this function to calculate team powers
.selectedModel <- function(Off_or_Def,removeEffMin = TRUE) {
  
  playersSumm <- .prepareModel(Off_or_Def, removeEffMin)
  # scale the data for easier convergence of backpropagation algorithm
  scaleMaxMin <- .getScaleLimits(Off_or_Def)
  maxs <- scaleMaxMin$maxs 
  mins <- scaleMaxMin$mins
  
  team_season <- playersSumm[,1]
  scaled <- as.data.frame(scale(playersSumm[,-1], center = mins, scale = maxs - mins))
  scaled <- cbind(team_season,scaled)
  
  # train_split: number of teams or percentage of data in training set
  set.seed(450)
  perc <- 0.80
  train_split <- round(perc*nrow(playersSumm))
  hidden_neurons <- c(6,4,2)
  #c(4,2)
  #c(6,4,2)
  # neuralnet requires explicit formula for the model (f)
  n <- names(scaled[,-1])
  f <- as.formula(paste("PTS ~", paste(n[!n %in% "PTS"], collapse = " + ")))
  
  teams_train <- sample(playersSumm$team_season,train_split)
  #teams_test <- filter(playersSumm, !(team_season %in% teams_train))$team_season
  training <- filter(scaled, team_season %in% teams_train)
  #testing <- filter(scaled, team_season %in% teams_test)
  
  # remove non-numeric variables
  train_teamSeasonCodes <- training$team_season
  #test_teamSeasonCodes <- testing$team_season
  training <- training[,-1]
  #testing <- testing[,-1]
  
  ## Model Neural Network
  # Hidden layers and neurons per layer specified by hidden. 
  # Number of input neurons is the number of columns
  # Output neurons is 1 as we are doing regression (linear.output=T)
  # For classification problem, linear.output=F
  nn <- neuralnet(f,data=training,hidden=hidden_neurons,linear.output=T)
  
  return(nn) # returns nnet model based on training data (perc of the total teams)
}

# Plotting --------------------------------
# # plot MSE distribution after C-V
# boxplot(cv.error,xlab='MSE CV',col='cyan',
#         border='blue',names='CV error (MSE)',
#         main='CV error (MSE) for NN',horizontal=TRUE)
# 
# plot(pr.nn_, test.r)
# plot(pr_tr.nn_, train.r)


