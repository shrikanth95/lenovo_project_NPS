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
  sentiment_orig <- read.csv("../RawData/CID_Web_Sentiment.csv") %>%
    mutate(ProductName = I(toupper(as.character(Product)))) 
  
  
  # Read in all NPS data
  #survey <- read.csv("../RawData/Lenovo_Survey_Data_pNPS.csv") %>%
  survey <- read.csv("../RawData/Lenovo_Survey_Data_pNPS_Rev2.csv") %>%
    
    mutate(ProductName = I(toupper(as.character(Product))), 
           SeriesName = I(toupper(as.character(Series))))
  
  # Series is in the NPS file but not sentiment file so we need
  # to do some massaging to match that data up first; 
  # this is kludgy but works 
  tmp1 <- survey %>%
    select(ProductName, SeriesName) %>%
    unique()
  
  sentiment <- left_join(sentiment_orig, tmp1, by=c("ProductName"))
  
  ######### Commercial Segment ###############
  
  # Filter Commercial data
  sentiment.comm.all <- sentiment %>%
    filter(Business.Group == "LENOVO - COMMERCIAL" |
          (Business.Group == "LENOVO - SMB" & SeriesName == "V SERIES") |
          (Business.Group == "LENOVO - SMB" & SeriesName == "E SERIES"))  %>%
    mutate(Segment = "COMMERCIAL")
  
  
  survey.comm.all <- survey %>%
    filter(Segment == "Lenovo - Commercial" |
          (Segment == "Lenovo - SMB" & SeriesName == "V SERIES") |
          (Segment == "Lenovo - SMB" & SeriesName == "E SERIES"))  %>%
    mutate(NPS = as.numeric(as.character(Product.NPS)),
           Segment = "COMMERCIAL") %>%
    drop_na(NPS)
  
  # Get set of Commercial products
  comm.prods.df1 <- sort(unique(sentiment.comm.all$ProductName))
  comm.prods.df2 <- sort(unique(survey.comm.all$ProductName))
  comm.prods <- data.frame(name = I(unique(c(comm.prods.df1, 
                                             comm.prods.df2)))) %>%
    mutate(have_sentiment = name %in% comm.prods.df1,
           have_survey = name %in% comm.prods.df2) %>%
    filter(have_sentiment & have_survey)
  
  # Filter to only consider products with both sentiment and survey data
  sentiment.comm <- sentiment.comm.all  %>%
    filter(ProductName %in% comm.prods$name)
  
  survey.comm <- survey.comm.all %>%
    filter(ProductName %in% comm.prods$name)
  
  ########## CONSUMER DATA #########################
  
  # Filter Consumer data
  sentiment.consumer.all <- sentiment %>%
    filter(Business.Group == "LENOVO - CONSUMER" |
          (Business.Group == "LENOVO - SMB" & SeriesName == "B SERIES") |
          (Business.Group == "LENOVO - SMB" & SeriesName == "M SERIES")) %>%
    mutate(Segment = "CONSUMER")
  
  survey.consumer.all <- survey %>%
    filter(Segment == "Lenovo - Consumer" |
          (Segment == "Lenovo - SMB" & SeriesName == "B SERIES") |
          (Segment == "Lenovo - SMB" & SeriesName == "M SERIES")) %>%
    mutate(NPS = as.numeric(as.character(Product.NPS)),
           Segment = "CONSUMER") %>%
    drop_na(NPS)
  
  # Get set of Consumer products
  consumer.prods.df1 <- sort(unique(sentiment.consumer.all$ProductName))
  consumer.prods.df2 <- sort(unique(survey.consumer.all$ProductName))
  consumer.prods <- data.frame(name = I(unique(c(consumer.prods.df1, 
                                                 consumer.prods.df2)))) %>%
    mutate(have_sentiment = name %in% consumer.prods.df1,
           have_survey = name %in% consumer.prods.df2) %>%
    filter(have_sentiment & have_survey)
  
  # Filter to only consider products with both sentiment and survey data
  sentiment.consumer <- sentiment.consumer.all  %>%
    filter(ProductName %in% consumer.prods$name)
  
  survey.consumer <- survey.consumer.all %>%
    filter(ProductName %in% consumer.prods$name)
  
  ############# Save objects to Rdata file #######################
  
  save(sentiment.comm.all, sentiment.comm,  
       sentiment.consumer.all, sentiment.consumer,
       file = "../CleanData/filtered_sentiment_data.RData")
  save(survey.comm.all, survey.comm, 
       survey.consumer.all, survey.consumer,
       file = "../CleanData/filtered_survey_data.RData")
  save(comm.prods, consumer.prods,
       file = "../CleanData/filtered_product_lists.RData")
  
  return("Successfully created Rdata files")
  
}
