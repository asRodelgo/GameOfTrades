# server.R
source("global_cache.R", local = TRUE)

###########################

function(input, output, session) {

  # Reactive Values for Trade Market
  values <- reactiveValues(playersAdjMin = NULL, playersDatabase = NULL, playersRanks = NULL, teamsDatabase = NULL, teamsRanks = NULL, teamsStats = NULL, teamsMax = NULL, playersTSNE = NULL, teamsTSNE = NULL, showTradeText = FALSE)
  values$playersAdjMin <- playersPredictedStats_adjMin
  values$playersDatabase <- playerDashboard
  values$playersRanks <- playerRanks
  values$teamsDatabase <- teamDashboard
  values$teamsRanks <- teamRanks
  values$teamsStats <- teamStats
  values$teamsMax <- teamMax
  values$playersTSNE <- tsne_ready
  values$teamsTSNE <- tsne_ready_teams

  #### Players ####
  output$playerLogo <- renderImage({
    filename <- normalizePath(file.path('./images',
                                        paste(tolower(gsub(" ","-",input$playerName)), '.png', sep='')))

    # Return a list containing the filename and alt text
    list(src = filename,
         width = 200,
         height = 150,
         alt = paste("Player: ", input$playerName))

  }, deleteFile = FALSE)

  output$playerLogo2 <- renderImage({
    filename <- normalizePath(file.path('./images',
                                        paste(tolower(gsub(" ","-",input$playerName2)), '.png', sep='')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         width = 200,
         height = 150,
         alt = paste("Player: ", input$playerName2))
    
  }, deleteFile = FALSE)
  
  barplotPlayerStats <- reactive({

    off_def <- filter(values$playersDatabase, Player == input$playerName) %>%
      select(Offense, Defense)
    #data <- filter(playerDashboard, Player == "LeBron James") %>%
    data <- filter(values$playersDatabase, Player == input$playerName) %>%
      select(contains("eff"), contains("Per")) %>%
      gather(Stat, Value)
    #mutate(Value = ifelse(grepl("Per",Stat), paste0(round(Value*100,1),"%"),
    #                     ifelse(grepl("Min",Stat), paste0(round(Value*1000,1),"%"),round(Value,2))))

    ranks <- filter(values$playersRanks, Player == input$playerName) %>%
      #ranks <- filter(playerRanks, Player == "LeBron James") %>%
      select(contains("eff"), contains("Per")) %>%
      gather(Stat, Value) %>%
      mutate(color = ifelse(grepl("TOV|PF",Stat),
                            ifelse(Value > nrow(values$playersDatabase)/3,"green",
                                   ifelse(Value > nrow(values$playersDatabase)*2/3,"lightblue","red")),
                            ifelse(Value < nrow(values$playersDatabase)/3,"green",
                                   ifelse(Value < nrow(values$playersDatabase)*2/3,"lightblue","red")))) %>%
      mutate(Rank = Value) %>%
      select(-Value)

    data <- merge(data,ranks[,c("Stat","Rank","color")],by="Stat") %>%
      mutate(order = c(8,7,11,10,15,20,17,5,4,2,14,13,23,18,22,1,19,21,16,6,9,3,12)) %>%
      mutate(Stat = as.factor(Stat)) %>%
      arrange(order)

    maxs <- select(playerMax, contains("eff"), contains("Per")) %>%
      gather(Stat, Value) %>%
      mutate(Stat = as.factor(Stat)) %>%
      merge(data[,c("Stat","order")],by="Stat") %>%
      arrange(order)

    data$Stat = factor(data$Stat, levels = data$Stat[order(data$order, decreasing = FALSE)], ordered=TRUE,
                       labels = c("Points","effFG%","FG%","FG_A","FG_M","FG_2P%","FG_2PM","FG_2PA","FG_3P%","FG_3PM","FG_3PA","FT%","FT_M","FT_A","AST","TRB","DRB","ORB","STL","BLK","TOV","PF","Usage"))

    maxs$Stat = factor(maxs$Stat, levels = maxs$Stat[order(data$order, decreasing = FALSE)], ordered=TRUE,
                       labels = c("Points","effFG%","FG%","FG_A","FG_M","FG_2P%","FG_2PM","FG_2PA","FG_3P%","FG_3PM","FG_3PA","FT%","FT_M","FT_A","AST","TRB","DRB","ORB","STL","BLK","TOV","PF","Usage"))

    ggplot(NULL) +
      geom_bar(data = data, aes(Stat, Value), stat = "identity", fill=factor(data$color)) +
      geom_bar(data = maxs, aes(Stat, Value), stat = "identity", fill = "grey", alpha = 0.3) +
      #coord_flip() +
      geom_text(data = data, aes(Stat,0.95,label = ifelse(grepl("%",Stat), paste0(round(Value*100,1),"%"),
                                                          ifelse(grepl("Usage",Stat), paste0(round(Value*1000,1),"%"),round(Value,2))))) +
      geom_text(data = data, aes(Stat,.85,label = paste0("(",Rank,")"))) +
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            legend.text = element_blank(),
            legend.position="none",
            plot.title = element_text(size=9),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.text.x = element_text(size = 8),
            #axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks = element_blank())
  })

  output$barplotstats_p <- renderPlot(barplotPlayerStats())

  similar <- reactive({

    similarPlayers <- .compare10_click(thisSeason,input$playerName,data=values$playersTSNE) %>%
      #similarPlayers <- .compare10_click(thisSeason,"Kyrie Irving",data=values$playersTSNE) %>%
      top_n(11,-dist) %>% tail(10) %>%
      merge(values$playersDatabase[,c("Player","Offense","Defense","effMin","Age","Tm")], by="Player") %>%
      mutate(Offense = round(Offense,1), Defense = round(Defense,1),dist = round(dist,2), effMin = paste0(formatC(effMin*1000,digits=1,format="f"),"%")) %>%
      arrange(dist) %>%
      select(Player,Team = Tm, Age, Offense, Defense, `Usage Rate` = effMin,`Euclidean dist` = dist)

    datatable(similarPlayers, extensions = 'Scroller', options = list(dom="t", scrollY = 150,scroller = TRUE,initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#fff', 'color': '#575859', 'font-size': '10px', 'font-weight': 'bold'});","}")
                                                                      #, columnDefs = list(list(visible = FALSE, targets = c((ncol(roster)-2):(ncol(roster)-1))))
    )) %>%
      formatStyle(0, target = 'row', fontSize = '70%', lineHeight = '70%') %>%
      formatStyle(c('Usage Rate'), textAlign = 'right')
  })

  output$similar_p <- renderDataTable(similar())

  similar_hist <- reactive({
    similarPlayers_Hist <- data.frame()
    player_seasons <- filter(tsne_ready_hist, Player == input$playerName) %>%
      arrange(desc(Season)) %>% select(Season) %>% head(5)
    for (s in player_seasons$Season) {
      thisSeason <- .compare10_click(s,input$playerName,data=tsne_ready_hist) %>%
        top_n(11,-dist) %>% tail(10) %>% mutate(player_season = paste0(Player," (",Season,")")) %>% select(player_season)
      names(thisSeason) <- c(paste0("In Season: ",s))
      if (nrow(similarPlayers_Hist) > 0) similarPlayers_Hist <- cbind(similarPlayers_Hist,thisSeason) else similarPlayers_Hist <- thisSeason
    }

    datatable(similarPlayers_Hist, extensions = 'Scroller', options = list(dom="t",scrollY = 150,scroller = TRUE,initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#fff', 'color': '#575859', 'font-size': '10px', 'font-weight': 'bold'});","}")
                                                                           #, columnDefs = list(list(visible = FALSE, targets = c((ncol(roster)-2):(ncol(roster)-1))))
    )) %>%
      formatStyle(0, target = 'row', fontSize = '70%', lineHeight = '70%')
  })

  output$similarHist_p <- renderDataTable(similar_hist())

  tsne_predicted <- reactive({

    set.seed(456)
    playerCluster <- kmeans(values$playersTSNE[, c("x","y")], input$player_num_clusters, nstart = 10, iter.max = 20)
    tsne_ready2 <- cbind(values$playersTSNE, cluster = playerCluster$cluster)

    tsne_points_filter <- filter(tsne_ready2, Player == input$playerName)
    #centroid <- data.frame(x=(mean(tsne_points_filter$x)),y=mean(tsne_points_filter$y))
    x_limit <- c(min(tsne_points_filter$x)-5,max(tsne_points_filter$x)+10)
    y_limit <- c(min(tsne_points_filter$y)-5,max(tsne_points_filter$y)+10)

    cluster_representative <- group_by(tsne_ready2, cluster) %>%
      mutate(x_mean = mean(x), y_mean=mean(y)) %>%
      mutate(dist = sqrt((x-x_mean)^2+(y-y_mean)^2)) %>%
      filter(dist==min(dist)) %>%
      select(cluster, Player,Season, x,y,dist) %>%
      ungroup()

    if (nrow(tsne_points_filter) > 0) {
      ggplot(NULL, aes(x,y)) +
        geom_point(data=tsne_ready2,aes(color = as.factor(cluster)),alpha = 0.5) +
        geom_point(data=tsne_points_filter,color = "red",size=4) +
        geom_text(data = cluster_representative, aes(label = paste0(Player," (",Season,")")),color="grey",size=3, nudge_y = 1) +
        geom_text(data=tsne_points_filter,aes(label = paste0(Player," (",Season,")")),color="blue",size=3, nudge_y = 1) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.text = element_blank(),
              legend.position="none",
              plot.title = element_text(size=9),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks = element_blank())
    } else {
      ggplot(NULL, aes(x,y)) +
        geom_point(data=tsne_ready2,aes(color = as.factor(cluster)),alpha = 0.5) +
        geom_text(data = cluster_representative, aes(label = paste0(Player," (",Season,")")),size=3, nudge_y = 1) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.text = element_blank(),
              legend.position="none",
              plot.title = element_text(size=9),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks = element_blank())
    }

  })

  output$tsnePredicted_p <- renderPlot(tsne_predicted())

  tsne_historical <- reactive({

    set.seed(456)
    playerCluster <- kmeans(tsne_ready_hist[, c("x","y")], input$player_num_clusters, nstart = 10, iter.max = 20)
    tsne_ready_hist2 <- cbind(tsne_ready_hist, cluster = playerCluster$cluster)

    tsne_points_filter <- filter(tsne_ready_hist2, Player == input$playerName)
    #centroid <- data.frame(x=(mean(tsne_points_filter$x)),y=mean(tsne_points_filter$y))
    x_limit <- c(min(tsne_points_filter$x)-5,max(tsne_points_filter$x)+10)
    y_limit <- c(min(tsne_points_filter$y)-5,max(tsne_points_filter$y)+10)

    cluster_representative <- group_by(tsne_ready_hist2, cluster) %>%
      mutate(x_mean = mean(x), y_mean=mean(y)) %>%
      mutate(dist = sqrt((x-x_mean)^2+(y-y_mean)^2)) %>%
      filter(dist==min(dist)) %>%
      select(cluster, Player,Season, x,y,dist) %>%
      ungroup()

    if (nrow(tsne_points_filter) > 0) {
      ggplot(NULL, aes(x,y)) +
        geom_point(data=tsne_ready_hist2,aes(color = as.factor(cluster)),alpha = 0.2) +
        geom_jitter(data=tsne_points_filter,color = "red",size=4,width = 1.5, height = 1.5) +
        geom_text(data = cluster_representative, aes(label = paste0(Player," (",Season,")")),color = "grey",size=3, nudge_y = 1) +
        geom_text(data=tsne_points_filter,aes(label = Season),color="blue",size=3, nudge_y = 1) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.text = element_blank(),
              legend.position="none",
              plot.title = element_text(size=9),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks = element_blank())
    } else {
      ggplot(NULL, aes(x,y)) +
        geom_point(data=tsne_ready_hist2,aes(color = as.factor(cluster)),alpha = 0.2) +
        geom_text(data = cluster_representative, aes(label = paste0(Player," (",Season,")")),size=3, nudge_y = 1) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.text = element_blank(),
              legend.position="none",
              plot.title = element_text(size=9),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks = element_blank())
    }
  })

  output$tsneHist_p <- renderPlot(tsne_historical())

  output$radarPlayerStats <- renderChartJSRadar({
    library(radarchart)
    
    data <- filter(values$playersDatabase, Player == input$playerName) %>%
    #data <- filter(playerDashboard, Player == "Kevin Durant") %>%
      select(contains("eff"), contains("Per")) %>%
      gather(Stat, Value)
    #mutate(Value = ifelse(grepl("Per",Stat), paste0(round(Value*100,1),"%"),
    #                     ifelse(grepl("Min",Stat), paste0(round(Value*1000,1),"%"),round(Value,2))))
    
    ranks <- filter(values$playersRanks, Player == input$playerName) %>%
      #ranks <- filter(playerRanks, Player == "Kevin Durant") %>%
      select(contains("eff"), contains("Per")) %>%
      gather(Stat, Value) %>%
      mutate(Value = (nrow(playerRanks)-Value + 1)/nrow(playerRanks)) %>%
      spread(Stat, Value) %>%
      mutate(Scoring = effPTS,
               #(3*eff3PM + 2*eff2PM + effFTM)/6,
             Accuracy = effFGPer,
             Assists = effAST,
             Rebounds = effTRB,
             Blocks = effBLK,
             Three_Point = (2*eff3PM + eff3PA)/3,
             Usage = effMin) %>%
        select(-contains("eff"), -contains("Per")) %>%
        gather(Stat, Value)
    #   mutate(color = ifelse(grepl("TOV|PF",Stat),
    #                         ifelse(Value > nrow(values$playersDatabase)/3,"green",
    #                                ifelse(Value > nrow(values$playersDatabase)*2/3,"lightblue","red")),
    #                         ifelse(Value < nrow(values$playersDatabase)/3,"green",
    #                                ifelse(Value < nrow(values$playersDatabase)*2/3,"lightblue","red")))) %>%
    #   mutate(Rank = Value) %>%
    #   select(-Value)
    
    # data <- merge(data,ranks[,c("Stat","Rank","color")],by="Stat") %>%
    #   mutate(order = c(8,7,11,10,15,20,17,5,4,2,14,13,23,18,22,1,19,21,16,6,9,3,12)) %>%
    #   mutate(Stat = as.factor(Stat)) %>%
    #   arrange(order)
    
    # maxs <- select(playerMax, contains("eff"), contains("Per")) %>%
    #   gather(Stat, Value) %>%
    #   mutate(Stat = as.factor(Stat)) %>%
    #   merge(data[,c("Stat","order")],by="Stat") %>%
    #   arrange(order)
    # 
    #data$Stat = factor(data$Stat, levels = data$Stat[order(data$order, decreasing = FALSE)], ordered=TRUE,
    #                   labels = c("Points","effFG%","FG%","FG_A","FG_M","FG_2P%","FG_2PM","FG_2PA","FG_3P%","FG_3PM","FG_3PA","FT%","FT_M","FT_A","AST","TRB","DRB","ORB","STL","BLK","TOV","PF","Usage"))
    
    # maxs$Stat = factor(maxs$Stat, levels = maxs$Stat[order(data$order, decreasing = FALSE)], ordered=TRUE,
    #                    labels = c("Points","effFG%","FG%","FG_A","FG_M","FG_2P%","FG_2PM","FG_2PA","FG_3P%","FG_3PM","FG_3PA","FT%","FT_M","FT_A","AST","TRB","DRB","ORB","STL","BLK","TOV","PF","Usage"))
    # 
    stats <- ranks$Stat
    
    scores <- list(
      #eval(parse(text=input$playerName)) = ranks$Value
      "this player" = ranks$Value
    )
    
    chartJSRadar(scores = scores, labs = stats, maxScale = 1, polyAlpha = 0)
    
  })
  
  output$radarPlayerStats2 <- renderChartJSRadar({
    library(radarchart)
    
    data <- filter(values$playersDatabase, Player == input$playerName2) %>%
      #data <- filter(playerDashboard, Player == "Kevin Durant") %>%
      select(contains("eff"), contains("Per")) %>%
      gather(Stat, Value)
    #mutate(Value = ifelse(grepl("Per",Stat), paste0(round(Value*100,1),"%"),
    #                     ifelse(grepl("Min",Stat), paste0(round(Value*1000,1),"%"),round(Value,2))))
    
    ranks <- filter(values$playersRanks, Player == input$playerName2) %>%
      #ranks <- filter(playerRanks, Player == "Kevin Durant") %>%
      select(contains("eff"), contains("Per")) %>%
      gather(Stat, Value) %>%
      mutate(Value = (nrow(playerRanks)-Value + 1)/nrow(playerRanks)) %>%
      spread(Stat, Value) %>%
      mutate(Scoring = effPTS,
             #(3*eff3PM + 2*eff2PM + effFTM)/6,
             Accuracy = effFGPer,
             Assists = effAST,
             Rebounds = effTRB,
             Blocks = effBLK,
             Three_Point = (2*eff3PM + eff3PA)/3,
             Usage = effMin) %>%
      select(-contains("eff"), -contains("Per")) %>%
      gather(Stat, Value)

    stats <- ranks$Stat
    scores <- list(
      #eval(parse(text=input$playerName)) = ranks$Value
      "this player" = ranks$Value
    )
    chartJSRadar(scores = scores, labs = stats, maxScale = 1, polyAlpha = 0)
    
  })
  
## TEAMS
  source("server_files/server_game.R", local = TRUE)
  
}



