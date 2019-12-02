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
    mutate(psi_cat = 
             case_when((psi <= MOD_LOW) ~ "VERY_LOW",
                       (MOD_LOW < psi & psi <= NORMAL) ~ "MOD_LOW",
                       (NORMAL < psi & psi <= MOD_HIGH) ~ "NORMAL",
                       (MOD_HIGH < psi & psi <= VERY_HIGH) ~ "MOD_HIGH",
                       (VERY_HIGH < psi) ~ "VERY_HIGH")) %>%
    mutate(next_psi_cat = lead(psi_cat, 1))
  
  data.psi$psi_cat <- factor(data.psi$psi_cat, 
                             levels=c("VERY_LOW", "MOD_LOW", "NORMAL",
                                      "MOD_HIGH", "VERY_HIGH"))
  data.psi$next_psi_cat <- factor(data.psi$next_psi_cat, 
                                  levels=c("VERY_LOW", "MOD_LOW", "NORMAL",
                                            "MOD_HIGH", "VERY_HIGH"))
  
  counts <- data.psi %>%
    ungroup() %>%
    group_by(psi_cat, next_psi_cat) %>%
    filter(!is.na(psi_cat) & !is.na(next_psi_cat)) %>%
    summarize(count = n()) %>%
    spread(next_psi_cat, count, fill=0) %>%
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
    mutate(nps_cat = 
             case_when((nps <= MOD_LOW) ~ "VERY_LOW",
                       (MOD_LOW < nps & nps <= NORMAL) ~ "MOD_LOW",
                       (NORMAL < nps & nps <= MOD_HIGH) ~ "NORMAL",
                       (MOD_HIGH < nps & nps <= VERY_HIGH) ~ "MOD_HIGH",
                       (VERY_HIGH < nps) ~ "VERY_HIGH")) %>%
    mutate(next_nps_cat = lead(nps_cat, 1))
  
  n <- length(data.nps$nps)
  
  data.nps$nps_cat <- factor(data.nps$nps_cat, 
                             levels=c("VERY_LOW", "MOD_LOW", "NORMAL",
                                      "MOD_HIGH", "VERY_HIGH"))
  
  data.nps$next_nps_cat <- factor(data.nps$next_nps_cat, 
                                  levels=c("VERY_LOW", "MOD_LOW", "NORMAL",
                                           "MOD_HIGH", "VERY_HIGH"))
  
  counts <- data.nps %>%
    ungroup() %>%
    group_by(nps_cat, next_nps_cat) %>%
    filter(!is.na(nps_cat) & !is.na(next_nps_cat)) %>%
    summarize(count = n()) %>%
    spread(next_nps_cat, count, fill=0) %>%
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
# Combines two transition matrices
#
combine.matrix <- function(A,B,add=FALSE) 
{
  nA <- nrow(A)
  nB <- nrow(B)
  n <- nA * nB
  tmp <- array(0, dim = c(n, n))
  
  for(row in 1:nB)
  {
    for(col in 1:nB) 
    {
      blk_row = (1+(row-1)*nA):(nA*(row))
      blk_col = (1+(col-1)*nA):(nA*(col))
      if (add)
      {
        tmp[blk_row, blk_col] <- A + B[row,col]
      }
      else
      {
        tmp[blk_row, blk_col] <- A * B[row,col]
      }
    }
 
  }
  
  return(tmp)
}