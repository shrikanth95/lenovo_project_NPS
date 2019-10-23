library(lubridate)

## Functions used for the lenovo project in ISE560
# Description: Calculates the psi of the sentiment data with each product. NOTE  "IDEAPAD 120S 14" and "IDEAPAD 120S 15" are considered to be two different products
# Input: 
#   - CIS dataset
#   - format.type: 
#       - use 1 for scores downsampled for each product and time(Year-Month); 
#       - use 2 for scores downsampled for each product
# Output: 
#   - psi scores for each format

calculate.PSI <- function(cis.working, format.type = 1) {
  
  if(format.type == 1) {
    cis.working$Comment.time =  format(cis.working$Comment.Date, "%Y-%m")
    list.comment.time = unique(cis.working$Comment.time)
    list.products = unique(cis.working$Product)
    
    len.working = length(list.products)*length(list.comment.time)
    psi.working <- data.frame(Product = character(len.working), 
                              Comment.time = character(len.working),
                              avg.star.rating = numeric(len.working),
                              psi = numeric(len.working), 
                              sample.size = numeric(len.working),
                              stringsAsFactors = FALSE)
    row = 1
    
    for(prod in list.products){
      for(comment.time in list.comment.time){
        cis.slice = cis.working[(cis.working$Product == prod)&(cis.working$Comment.time == comment.time), ]
        num.neg = length(which(cis.slice$Sentiment=="NEGATIVE"))
        num.pos = length(which(cis.slice$Sentiment=="POSITIVE"))
        psi.working$Product[row] = prod
        psi.working$Comment.time[row] = comment.time
        psi.working$sample.size[row] = nrow(cis.slice)
        if(is.numeric(num.neg)&is.numeric(num.pos)){
          psi.working$avg.star.rating[row] = mean(cis.slice$Stars.Rating,na.rm = TRUE)
          psi.working$psi[row] = (num.pos-num.neg)/(num.pos+num.neg)*10
        }
        
        else 
          psi.working$psi[row] = -1
        row = row + 1
      }
    }
    psi.working$Comment.time =  format(lubridate::ymd(paste0(year_month = psi.working$Comment.time, day = "30")), "%Y-%m")
  }
  else{
    
    list.products = unique(cis.working$Product)
    len.working = length(list.products)
    psi.working <- data.frame(Product = character(len.working), 
                              avg.star.rating = numeric(len.working),
                              psi = numeric(len.working), 
                              sample.size = numeric(len.working),
                              stringsAsFactors = FALSE)
    row = 1
    
    for(prod in list.products){
      cis.slice = cis.working[(cis.working$Product == prod), ]
      num.neg = length(which(cis.slice$Sentiment=="NEGATIVE"))
      num.pos = length(which(cis.slice$Sentiment=="POSITIVE"))
      psi.working$Product[row] = prod
      psi.working$sample.size[row] = nrow(cis.slice)
      if(is.numeric(num.neg)&is.numeric(num.pos)) {
        psi.working$psi[row] = (num.pos-num.neg)/(num.pos+num.neg)*100
        psi.working$avg.star.rating[row] = mean(cis.slice$Stars.Rating,na.rm = TRUE)
      }
      else 
        psi.working$psi[row] = -1
      row = row + 1
    }
  }
  return(psi.working)
}

# Description: Calculates the pNPS from NPS surveys for each product. NOTE  "IDEAPAD 120S 14" and "IDEAPAD 120S 15" are considered to be two different products
# Input: 
#   - NPS dataset
#   - format.type: 
#       - use 1 for scores downsampled for each product and time(Year-Month); 
#       - use 2 for scores downsampled for each product
# Output: 
#   - pNPS scores for each available format

calculate.NPS <- function(nps.dataset, format.type = 1) {
  
  durations <- unique(nps.dataset$Ownership.Period)
  pnps.working.main <- data.frame()
  if(format.type == 1) {
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
            pnps.working$pNPS[row] = (num.prom/nrow(nps.slice))*100 - (num.det/nrow(nps.slice))*100
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
  }
  else{
    for(duration in durations){
      nps.working = nps.dataset[nps.dataset$Ownership.Period == duration,]
      list.products = unique(nps.working$Product)
      len.working = length(list.products)
      pnps.working <- data.frame(Product = character(len.working), 
                                 pNPS = numeric(len.working), 
                                 sample.size = numeric(len.working),
                                 ownership.duration = rep(duration, len.working),
                                 stringsAsFactors = FALSE)
      
      row = 1
      for(prod in list.products){
        nps.slice = nps.working[(nps.working$Product == prod), ]
        pnps.working$Product[row] = prod
        pnps.working$sample.size[row] = nrow(nps.slice)
        
        if(nrow(nps.slice)>2){ 
          num.prom = nrow(nps.slice[(nps.slice$Product.NPS>8)&(nps.slice$Product.NPS<11),])
          num.det = nrow(nps.slice[(nps.slice$Product.NPS>-1)&(nps.slice$Product.NPS<7),])
          pnps.working$pNPS[row] = (num.prom/nrow(nps.slice))*100 - (num.det/nrow(nps.slice))*100
        }
        else{
          pnps.working$pNPS[row] = NA
          # pnps.working$bNPS[row] = -1
        }
        row = row + 1
      }
      pnps.working.main = rbind(pnps.working.main, pnps.working)
    }
  }
  return(pnps.working.main)
  
}
