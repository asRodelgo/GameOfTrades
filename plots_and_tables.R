## objects

# barplot_PlayerStats(playerDashboard, playerRanks, "LeBron James")
barplot_PlayerStats <- function(playersDatabase, playersRanks, playerName){
  
  playerMax <- summarise_if(playersDatabase, is.numeric, max)
  playerMin <- summarise_if(playersDatabase, is.numeric, min)
  
  off_def <- filter(playersDatabase, Player == playerName) %>%
    select(Offense, Defense)

  data <- filter(playersDatabase, Player == playerName) %>%
    select(contains("eff"), contains("Per")) %>%
    gather(Stat, Value)
  
  ranks <- filter(playersRanks, Player == playerName) %>%
    #ranks <- filter(playerRanks, Player == "LeBron James") %>%
    select(contains("eff"), contains("Per")) %>%
    gather(Stat, Value) %>%
    mutate(color = ifelse(grepl("TOV|PF",Stat),
                          ifelse(Value > nrow(playersDatabase)/3,"green",
                                 ifelse(Value > nrow(playersDatabase)*2/3,"lightblue","red")),
                          ifelse(Value < nrow(playersDatabase)/3,"green",
                                 ifelse(Value < nrow(playersDatabase)*2/3,"lightblue","red")))) %>%
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
  
  p <- ggplot(NULL) +
    geom_bar(data = data, aes(Stat, Value), stat = "identity", fill=factor(data$color)) +
    geom_bar(data = maxs, aes(Stat, Value), stat = "identity", fill = "grey", alpha = 0.3) +
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
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank())
  
  return(p)
}

# similar_players(playersDashboard, tsne_ready, "LeBron James") 
similar_players <- function(playersDatabase, playersTSNE, playerName){
  
  similarPlayers <- .compare10_click(thisSeason,playerName,data=playersTSNE) %>%
    top_n(11,-dist) %>% tail(10) %>%
    merge(playersDatabase[,c("Player","Offense","Defense","effMin","Age","Tm")], by="Player") %>%
    mutate(Offense = round(Offense,1), Defense = round(Defense,1),dist = round(dist,2), effMin = paste0(formatC(effMin*1000,digits=1,format="f"),"%")) %>%
    arrange(dist) %>%
    select(Player,Team = Tm, Age, Offense, Defense, `Usage Rate` = effMin,`Euclidean dist` = dist)
  
  dt <- datatable(similarPlayers, extensions = 'Scroller', options = list(dom="t", scrollY = 150,scroller = TRUE,
                                                                    initComplete = JS("function(settings, json) {
                                                                                      ","$(this.api().table().header()).css({
                                                                                      'background-color': '#fff', 
                                                                                      'color': '#575859', 
                                                                                      'font-size': '10px', 
                                                                                      'font-weight': 'bold'});","}")
                                                                    )) %>%
    formatStyle(0, target = 'row', fontSize = '70%', lineHeight = '70%') %>%
    formatStyle(c('Usage Rate'), textAlign = 'right')
  
  return(dt)
}

# plot_tsne_predicted(tsne_ready, 10, "LeBron James")
plot_tsne_predicted <- function(playersTSNE, player_num_clusters, playerName){
  
  set.seed(456)
  playerCluster <- kmeans(playersTSNE[, c("x","y")], player_num_clusters, nstart = 10, iter.max = 20)
  tsne_ready2 <- cbind(playersTSNE, cluster = playerCluster$cluster)
  
  tsne_points_filter <- filter(tsne_ready2, Player == playerName)
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
    p <- ggplot(NULL, aes(x,y)) +
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
    p <- ggplot(NULL, aes(x,y)) +
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
  return(p)
}

