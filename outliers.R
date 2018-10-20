# compareQuantile <- function(v,x,q) {
#   if (v >= quantile(eval(parse(text=paste0("playerStats_quantiles99","$",x))), p=q)) y <- 1 else y <- 0
#   return(y)
# }

get_PlayerOutliers <- function(data) {

  playerStats_quantiles99 <- playersNewPredicted %>%
    summarise_if(is.numeric, quantile, p=.99) %>%
    gather(skill, quantile99)

  playerStats_quantiles01 <- playersNewPredicted %>%
    summarise_if(is.numeric, quantile, p=.01) %>%
    gather(skill, quantile01)

  playerOutliers <- select(playersNewPredicted,-Tm) %>%
    filter(effMin < .006) %>%
    gather(skill, value, -Player) %>%
    left_join(playerStats_quantiles01, by = "skill") %>%
    left_join(playerStats_quantiles99, by = "skill") %>%
    mutate(outlier = if_else(value > quantile99, 1, 0)) %>%
    filter(outlier == 1) %>%
    group_by(Player) %>%
    summarise(outlier_score = sum(outlier))

  p_outliers <- filter(playerOutliers, outlier_score >= 4)$Player

  return(p_outliers)
}





