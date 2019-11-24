library(dplyr)

# IMPORTANT: both functions assume the df is already grouped
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