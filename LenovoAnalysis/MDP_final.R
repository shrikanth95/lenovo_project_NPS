
rm(list=ls())

library(tidyverse)
library(lubridate)
library(MDPtoolbox)

source("LenovoAnalysis/data_prep.R")
source("LenovoAnalysis/helper_calcs.R")

filter_raw_data()

load("CleanData/filtered_sentiment_data.Rdata")
load("CleanData/filtered_survey_data.Rdata")
load("CleanData/filtered_product_lists.Rdata")

# Define NPS states
NPS_VERY_HIGH <- 60.0
NPS_MOD_HIGH <- 45.0
NPS_NORMAL <- 35.0
NPS_MOD_LOW <- 8.0

res <- survey.consumer.all %>%
  group_by(SeriesName,
           time = floor_date(as.Date(Date.Survey, 
                                     format = "%m/%d/%y"), 
                             unit = "month")) %>%
  NPSTransitions(NPS_VERY_HIGH, NPS_MOD_HIGH, 
                 NPS_NORMAL, NPS_MOD_LOW)

p.s = as.matrix(res$P[,2:6])

# Define telemetry matrix
p.tel = matrix(c(0.75, 0.25, 0.25, 0.75),nrow = 2, ncol = 2, byrow = TRUE)
rownames(p.tel) <- c("N","A")
colnames(p.tel) <- c("N","A")

# Combined matrix (i.e., Do Nothing)
P1 = combine.matrix(p.s,
                    p.tel)
rownames(P1) <- c("- -, N"," -, N"," 0, N"," +, N","++, N",
                  "- - , A"," -, A"," 0, A"," +, A","++, A")
colnames(P1) <- c("- -, N"," -, N"," 0, N"," +, N","++, N",
                  "- - , A"," -, A"," 0, A"," +, A","++, A")

n <- nrow(res$P)

# Select parameters
r1 <- 10
r2 <- 5
cost <- 7
d <- 0.75
# Discount factor
discount <- 0.95 

###### Define Rewards Matrices ######

# Do Nothing
Rnps <- matrix(rep(c(-2*r1, -1*r1, 0, r1, 2*r1),n),
               nrow=nrow(p.s),
               byrow=TRUE)
Rtel <- matrix(c(2*r2, r2, -r2, -2*r2), nrow=nrow(p.tel))

R_A1 <- combine.matrix(Rnps, Rtel, TRUE)
rownames(R_A1) <- c("- -, N"," -, N"," 0, N"," +, N","++, N",
                    "- - , A"," -, A"," 0, A"," +, A","++, A")
colnames(R_A1) <- c("- -, N"," -, N"," 0, N"," +, N","++, N",
                    "- - , A"," -, A"," 0, A"," +, A","++, A")

# Root Cause Analysis
# Set cost to "infinity" for states where action is not available
INF <- 9999
R_A2 <- matrix(-INF, nrow=nrow(R_A1), ncol=ncol(R_A1))
R_A2[1,] <- R_A1[1,] - cost
R_A2[2,] <- R_A1[2,] - cost
R_A2[6,] <- R_A1[6,] - cost
R_A2[7,] <- R_A1[7,] - cost

rownames(R_A2) <- c("- -, N"," -, N"," 0, N"," +, N","++, N",
                    "- - , A"," -, A"," 0, A"," +, A","++, A")
colnames(R_A2) <- c("- -, N"," -, N"," 0, N"," +, N","++, N",
                    "- - , A"," -, A"," 0, A"," +, A","++, A")

for (p in seq(0.1, 0.9, 0.1))
{
  
  #### Define Root Cause Transition Matrix ########
  
  P2 <- P1
  
  # Modify VERY_LOW, Normal Telemetry
  delta <- d * p * P1[1,1]/sum(P1[1,1:5])
  P2[1,1] <- P1[1,1] - delta
  P2[1,2] <- P1[1,2] + delta/(n-1)
  P2[1,3] <- P1[1,3] + delta/(n-1)
  P2[1,4] <- P1[1,4] + delta/(n-1)
  P2[1,5] <- P1[1,5] + delta/(n-1)
  delta <- d * p * P1[1,6]/sum(P1[1,6:10])
  P2[1,6] <- P1[1,6] - delta
  P2[1,7] <- P1[1,7] + delta/(n-1)
  P2[1,8] <- P1[1,8] + delta/(n-1)
  P2[1,9] <- P1[1,9] + delta/(n-1)
  P2[1,10] <- P1[1,10] + delta/(n-1)
  
  # Modify MOD_LOW, Normal Telemetry
  delta <- d *p * P1[2,1]/sum(P1[2,1:5])
  P2[2,1] <- P1[2,1] - delta
  P2[2,2] <- P1[2,2] + delta/(n-1)
  P2[2,3] <- P1[2,3] + delta/(n-1)
  P2[2,4] <- P1[2,4] + delta/(n-1)
  P2[2,5] <- P1[2,5] + delta/(n-1)
  delta <- d *p * P1[2,6]/sum(P1[2,6:10])
  P2[2,6] <- P1[2,6] - delta
  P2[2,7] <- P1[2,7] + delta/(n-1)
  P2[2,8] <- P1[2,8] + delta/(n-1)
  P2[2,9] <- P1[2,9] + delta/(n-1)
  P2[2,10] <- P1[2,10] + delta/(n-1)
  
  # Modify VERY_LOW, Abnormal Telemetry
  delta <- p * P1[6,1]/sum(P1[6,1:5])
  P2[6,1] <- P1[6,1] - delta
  P2[6,2] <- P1[6,2] + delta/(n-1)
  P2[6,3] <- P1[6,3] + delta/(n-1)
  P2[6,4] <- P1[6,4] + delta/(n-1)
  P2[6,5] <- P1[6,5] + delta/(n-1)
  delta <- p * P1[6,6]/sum(P1[6,6:10])
  P2[6,6] <- P1[6,6] - delta
  P2[6,7] <- P1[6,7] + delta/(n-1)
  P2[6,8] <- P1[6,8] + delta/(n-1)
  P2[6,9] <- P1[6,9] + delta/(n-1)
  P2[6,10] <- P1[6,10] + delta/(n-1)
  
  # Modify MOD_LOW, Normal Telemetry
  delta <- p * P1[7,1]/sum(P1[7,1:5])
  P2[7,1] <- P1[7,1] - delta
  P2[7,2] <- P1[7,2] + delta/(n-1)
  P2[7,3] <- P1[7,3] + delta/(n-1)
  P2[7,4] <- P1[7,4] + delta/(n-1)
  P2[7,5] <- P1[7,5] + delta/(n-1)
  delta <- p * P1[7,1]/sum(P1[7,6:10])
  P2[7,6] <- P1[7,6] - delta
  P2[7,7] <- P1[7,7] + delta/(n-1)
  P2[7,8] <- P1[7,8] + delta/(n-1)
  P2[7,9] <- P1[7,9] + delta/(n-1)
  P2[7,10] <- P1[7,10] + delta/(n-1)
  
  ##### Set up MDP #####
  # Transitions
  P <- array(c(P1, P2), dim=c(dim(P1), 2))
  
  # Rewards
  R <- array(c(R_A1, R_A2), dim=c(dim(P1), 2))
  
  # Check validity
  mdp_check(P, R)
  
  # Objective is to maximize rewards
  # Linear Programming
  print(paste("p = ", p))
  print(mdp_LP(P, R, discount))
}