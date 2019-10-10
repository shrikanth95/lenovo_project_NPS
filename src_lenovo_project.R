library(lubridate)

## Functions used for the lenovo project in ISE560
# Description: Calculates the PIS of the sentiment data with each product. NOTE  "IDEAPAD 120S 14" and "IDEAPAD 120S 15" are considered to be two different products
# Input: CIS dataset
# Output: PIS scores for each available product and year

calculate.PIS <- function(cis.working) {

  list.products = unique(cis.working$Product)
  
  pis.working <- data.frame(Product = character(length(list.products)), 
                            Year.make = character(length(list.products)), 
                            PIS = numeric(length(list.products)), 
                            sample.size = numeric(length(list.products)),
                            stringsAsFactors = FALSE)
  row = 1
  for(prod in list.products){
      cis.slice = cis.working[cis.working$Product == prod, ]
      num.neg = length(which(cis.slice$Sentiment=="NEGATIVE"))
      num.pos = length(which(cis.slice$Sentiment=="POSITIVE"))
      pis.working$Product[row] = prod
      pis.working$Year.make[row] = ifelse(!is.null(strsplit(cis.slice$Product[1], " ")[[1]][3]), 
                                          yes = strsplit(cis.slice$Product[1], " ")[[1]][3],
                                          no = -1)
      pis.working$sample.size[row] = nrow(cis.slice)
      if(is.numeric(num.neg)&is.numeric(num.pos)) 
        pis.working$PIS[row] = (num.pos-num.neg)/(num.pos+num.neg)*10
      else 
        pis.working$PIS[row] = -1
      row = row + 1
  }
  return(pis.working)
}
