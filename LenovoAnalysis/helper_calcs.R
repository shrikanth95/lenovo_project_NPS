library(dplyr)

#
# IMPORTANT: all functions assume the df is already grouped
#

# 
# Calculates PSI for grouped data
#
calcPSI <- function(df)
{
  if(!is.grouped_df(df))
  {
    warning("input tibble/dataframe must already be grouped")
    return
  }

  df %>%
    summarise(pos = sum(Sentiment == "POSITIVE"),
                       neg = sum(Sentiment == "NEGATIVE"),
                       stars = mean(Stars.Rating)) %>%
    mutate(psi = 100 * (pos - neg)/(pos +  neg))
}

# 
# Calculates PSI for grouped data
#
calcNPS <- function(df)
{
  if(!is.grouped_df(df))
  {
    warning("input tibble/dataframe must already be grouped")
    return
  }
  
  df %>%
    summarise(promoter = sum(NPS >= 9),
              detractor = sum(NPS <= 6),
              total = n()) %>%
    mutate(nps = 100 * (promoter - detractor) / total)
}

#
# Calculates PSI transitions and transition matrix 
#
PSITransitions <- function(df, VERY_HIGH, MOD_HIGH, NORMAL, MOD_LOW)
{
  # browser()
  
  if(!is.grouped_df(df))
  {
    warning("input tibble/dataframe must already be grouped")
    return
  }
  
  data.psi <- df %>%
    calcPSI() %>%
    mutate(psi_cat = case_when((psi <= MOD_LOW) ~ "VERY_LOW",
                               (MOD_LOW < psi & psi <= NORMAL) ~ "MOD_LOW",
                               (NORMAL < psi & psi <= MOD_HIGH) ~ "NORMAL",
                               (MOD_HIGH < psi & psi <= VERY_HIGH) ~ "MOD_HIGH",
                               (VERY_HIGH < psi) ~ "VERY_HIGH")) %>%
    ungroup()
  
  n <- length(data.psi$psi)
  
  data.psi$psi_cat <- factor(data.psi$psi_cat, levels=c("VERY_LOW", "MOD_LOW", "NORMAL",
                                                        "MOD_HIGH", "VERY_HIGH"))
  
  
  # Now create data.frame with two psi columns lagged by 1 week
  
  tmp <- data.frame(t0 = data.psi$psi_cat[1:n-1],
                    t1 = data.psi$psi_cat[2:n])
  
  counts <- tmp %>%
    group_by(t0, t1) %>%
    summarize(count = n()) %>%
    spread(t1, count, fill=0) %>%
    ungroup()
  
  ncol <- dim(counts)[2]
  total <- rowSums(counts[,2:ncol])
  P <- counts
  P[,2:ncol] <- counts[,2:ncol] / total
  counts$total = total
  
  list(P = P,
       counts = counts)
}

#
# Calculates NPS transitions and transition matrix
#
NPSTransitions <- function(df, VERY_HIGH, MOD_HIGH, NORMAL, MOD_LOW)
{
  #browser()
  
  if(!is.grouped_df(df))
  {
    warning("input tibble/dataframe must already be grouped")
    return
  }
  
  data.nps <- df %>%
    calcNPS() %>%
    mutate(nps_cat = case_when((nps <= MOD_LOW) ~ "VERY_LOW",
                               (MOD_LOW < nps & nps <= NORMAL) ~ "MOD_LOW",
                               (NORMAL < nps & nps <= MOD_HIGH) ~ "NORMAL",
                               (MOD_HIGH < nps & nps <= VERY_HIGH) ~ "MOD_HIGH",
                               (VERY_HIGH < nps) ~ "VERY_HIGH")) %>%
    ungroup()
  
  n <- length(data.nps$nps)
  
  data.nps$nps_cat <- factor(data.nps$nps_cat, levels=c("VERY_LOW", "MOD_LOW", "NORMAL",
                                                        "MOD_HIGH", "VERY_HIGH"))
  
  
  # Now create data.frame with two nps columns lagged by 1 week
  
  tmp <- data.frame(t0 = data.nps$nps_cat[1:n-1],
                    t1 = data.nps$nps_cat[2:n])
  
  counts <- tmp %>%
    group_by(t0, t1) %>%
    summarize(count = n()) %>%
    spread(t1, count, fill=0) %>%
    ungroup()
  
  ncol <- dim(counts)[2]
  total <- rowSums(counts[,2:ncol])
  P <- counts
  P[,2:ncol] <- counts[,2:ncol] / total
  counts$total = total
  
  list(P = P,
       counts = counts)
}