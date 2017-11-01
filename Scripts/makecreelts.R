
makecreelts <- function(ds) { 
  
  library(plyr)
  
  ds$sampleday <- 1
  ds$allsalmon <- apply(ds[,c('chinook', 'coho', 'pink', 'chum')], 1, FUN = sum)
  
  datestart <- min(ds$datestamp) #- 1 
  dateend <- max(ds$datestamp)
  
  dates <- as.POSIXct(seq(datestart, dateend, by = 'day'))
  
  cal <- as.data.frame(expand.grid(dates, unique(ds$Site)))
  colnames(cal) <- c('datestamp', 'Site')
  cal$Site <- as.character(cal$Site)

  ts <- merge(cal, ds, by.x = c('datestamp', 'Site'),
                        by.y = c('datestamp', 'Site'), 
                        all.x = T)
  
  
  ## NOTE: FIRST TWO COLUMNS SHOULD BE datestamp, site; REMAINING COLS MUST BE NUMERIC 
  #replace NAs with 0s
  for(i in 3:dim(ts)[2]) { 
    t0 <- ts[,i]
    t0[is.na(t0)] <- 0
    
    ts[,i] <- t0
    
  }
  
  
  #APPEND COLUMNS FOR CUMULATIVE SUM
  ts <- ts[order(ts$Site, ts$datestamp), ]
  sites <- unique(ts$Site)
  tlist <- list(NULL)
  
 for (s in 1:length(sites)) { 
   
   t <- ts[ts$Site == sites[s], ]
   
   t <- t[order(t$datestamp), ]
   
   dimhold <- dim(t)[2]
   t <- cbind(t, as.data.frame(apply(t[ , 3:dim(t)[2]], 2, FUN = cumsum)))
   
   colnames(t)[(dimhold+1):dim(t)[2]] <-  paste('cu', colnames(t)[3:dimhold], sep = '')
     
   tlist[[s]] <- t
    
   
   }
  
  ts <- do.call('rbind', tlist)
  
  return(ts)

}