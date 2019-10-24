#' Filters RawData files to just common data sets and saves as .Rdata
#' 
#' @param None
#' @return Success or Exists

library(tidyverse)
library(lubridate)

filter_raw_data <- function()
{
  # Check if .Rdata file already exists
  if (file.exists("../CleanData/filtered_sentiment_data.Rdata") &&
      file.exists("../CleanData/filtered_survey_data.Rdata") &&
      file.exists("../CleanData/filtered_product_lists.RData"))
  {
    return("Rdata files already exist")
  }
  
  # Read in all sentiment data
  sentiment <- read.csv("../RawData/CID_Web_Sentiment.csv")
  
  # Read in all NPS data
  survey <- read.csv("../RawData/Lenovo_Survey_Data_pNPS_Rev2.csv")
  
  # TO DO: nps$Segment needs some cleaning
  # Need to handle "blanks", "-" and convert "Consumer" to "Lenovo-Consumer" for Segmentt
  # Need to handle "-" for Product.NPS
  
  ######### Commercial Segment ###############
  
  # Filter Commercial data
  sentiment.comm.all <- sentiment %>%
    filter(Business.Group == "LENOVO - COMMERCIAL") %>%
    mutate(ProductName = I(toupper(as.character(Product)))) 
  
  
  survey.comm.all <- survey %>%
    filter(Segment == "Lenovo - Commercial") %>%
    mutate(ProductName = I(toupper(as.character(Product))), 
           SeriesName = I(toupper(as.character(Series)))) %>%
    mutate(NPS = as.numeric(as.character(Product.NPS))) %>%
    drop_na(NPS)
  
  # Get set of Commercial products
  comm.prods.df1 <- sort(unique(sentiment.comm.all$ProductName))
  comm.prods.df2 <- sort(unique(survey.comm.all$ProductName))
  comm.prods <- data.frame(name = I(unique(c(comm.prods.df1, comm.prods.df2)))) %>%
    mutate(have_sentiment = name %in% comm.prods.df1,
           have_survey = name %in% comm.prods.df2) %>%
    filter(have_sentiment & have_survey)
  
  # Filter to only consider products with both sentiment and survey data
  sentiment.comm <- sentiment.comm.all  %>%
    filter(ProductName %in% comm.prods$name)
  
  survey.comm <- survey.comm.all %>%
    filter(ProductName %in% comm.prods$name)
  
  ######### SMB Segment ###############
  
  # Filter Sentiment 
  sentiment.smb.all <- sentiment %>%
    filter(Business.Group == "LENOVO - SMB") %>%
    mutate(ProductName = I(toupper(as.character(Product))))
  
  # Filter Survey
  survey.smb.all <- survey %>%
    filter(Segment == "Lenovo - SMB") %>%
    mutate(ProductName = I(toupper(as.character(Product)))) %>%
    mutate(NPS = as.numeric(as.character(Product.NPS))) %>%
    drop_na(NPS)       # TO DO: Right now this drops rows where NPS is "-"
  #        Check Amber's answer whether or not these need to be counted in total
  
  # Get set of SMB products
  smb.prods.df1 <- sort(unique(sentiment.smb.all$ProductName))
  smb.prods.df2 <- sort(unique(survey.smb.all$ProductName))
  
  smb.prods <- data.frame(name = I(unique(c(smb.prods.df1, smb.prods.df2)))) %>%
    mutate(have_sentiment = name %in% smb.prods.df1,
           have_survey = name %in% smb.prods.df2) %>%
    filter(have_sentiment & have_survey)
  
  # Filter survey and sentiment to only SMB products with data in both
  sentiment.smb <- sentiment.smb.all %>%
    filter(ProductName %in% smb.prods$name)
  
  survey.smb <- survey.smb.all %>%
    filter(ProductName %in% smb.prods$name)
  
  ########## TO DO: CONSUMER DATA #########################
  
  # Save objects to Rdata file
  save(sentiment.comm.all, sentiment.comm, sentiment.smb.all, sentiment.smb, 
       file = "../CleanData/filtered_sentiment_data.RData")
  save(survey.comm.all, survey.comm, survey.smb, survey.smb.all,
       file = "../CleanData/filtered_survey_data.RData")
  save(comm.prods, smb.prods,
       file = "../CleanData/filtered_product_lists.RData")
  
  return("Successfully created Rdata files")
  
}