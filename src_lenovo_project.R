library(lubridate)

## Functions used for the lenovo project in ISE560
# Description: Calculates the PIS of the sentiment data with each product. NOTE  "IDEAPAD 120S 14" and "IDEAPAD 120S 15" are considered to be two different products
# Input: CIS dataset
# Output: PIS scores for each available product and year

calculate.PIS <- function(cis.working) {
  
  cis.working$Comment.time =  format(cis.working$Comment.Date, "%Y-%m")
  
  list.products = unique(cis.working$Product)
  list.comment.time = unique(cis.working$Comment.time)
  len.working = length(list.products)*length(list.comment.time)
  pis.working <- data.frame(Product = character(len.working), 
                            Comment.time = character(len.working),
                            PIS = numeric(len.working), 
                            sample.size = numeric(len.working),
                            stringsAsFactors = FALSE)
  row = 1
  
  for(prod in list.products){
    for(comment.time in list.comment.time){
      cis.slice = cis.working[(cis.working$Product == prod)&(cis.working$Comment.time == comment.time), ]
      num.neg = length(which(cis.slice$Sentiment=="NEGATIVE"))
      num.pos = length(which(cis.slice$Sentiment=="POSITIVE"))
      pis.working$Product[row] = prod
      pis.working$Comment.time[row] = comment.time
      pis.working$sample.size[row] = nrow(cis.slice)
      if(is.numeric(num.neg)&is.numeric(num.pos)) 
        pis.working$PIS[row] = (num.pos-num.neg)/(num.pos+num.neg)*10
      else 
        pis.working$PIS[row] = -1
      row = row + 1
    }
  }
  pis.working$Survey.time =  format(lubridate::ymd(paste0(year_month = pis.working$Comment.time, day = "30")), "%Y-%m")
  return(pis.working)
}

# Description: Calculates the pNPS from NPS surveys for each product. NOTE  "IDEAPAD 120S 14" and "IDEAPAD 120S 15" are considered to be two different products
# Input: pNPS dataset
# Output: pNPS scores for each available product and year

calculate.NPS <- function(nps.dataset) {
  
  durations <- c("Less than 3 months", "Between 3 - 6 months", "Between 7 – 12 months","More than 12 months", "Not available")
  pnps.working.main <- data.frame()
  nps.dataset$Survey.time =  format(nps.dataset$Date.Survey, "%Y-%m")
  for(duration in durations){
    nps.working = nps.dataset[nps.dataset$Ownership.Period == duration,]
    list.products = unique(nps.working$Product)
    list.survey.time = unique(nps.working$Survey.time)
    len.working = length(list.products)*length(list.survey.time)
    
    pnps.working <- data.frame(Product = character(len.working), 
                               pNPS = numeric(len.working), 
                               Survey.time = character(len.working),
                               sample.size = numeric(len.working),
                               ownership.duration = rep(duration, len.working),
                               stringsAsFactors = FALSE)
    row = 1
    for(prod in list.products){
      for(survey.time in list.survey.time){
        nps.slice = nps.working[(nps.working$Product == prod)&(nps.working$Survey.time == survey.time), ]
        pnps.working$Product[row] = prod
        pnps.working$Survey.time[row] = survey.time
        pnps.working$sample.size[row] = nrow(nps.slice)
        
        if(nrow(nps.slice)>2){ 
          num.prom = nrow(nps.slice[(nps.slice$Product.NPS>8)&(nps.slice$Product.NPS<11),])
          num.det = nrow(nps.slice[(nps.slice$Product.NPS>-1)&(nps.slice$Product.NPS<7),])
          pnps.working$pNPS[row] = num.prom - num.det
        }
        else{
          pnps.working$pNPS[row] = NA
          # pnps.working$bNPS[row] = -1
        }
        row = row + 1
      }
    }
    pnps.working.main = rbind(pnps.working.main, pnps.working)
  }
  pnps.working.main$Survey.time =  format(lubridate::ymd(paste0(year_month = pnps.working.main$Survey.time, day = "30")), "%Y-%m")
  return(pnps.working.main)
}
