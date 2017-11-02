cleanrawcreel <- function(d) { 
  
  library(plyr)
  
  #reformat the chinook variable 
  d$chinook <- as.numeric(unlist(lapply(d[,5], function(x) strsplit(x, split = ' ')[[1]][1])))
  
  d <- d[, c(1:4, 6:dim(d)[2])] #remove old chinook variable that contains the "(per angler)" 
  
  #better column names 
  colnames(d) <- c('Site', 
                   'catcharea', 
                   'interviews', 
                   'anglers', 
                   'coho', 
                   'chum', 
                   'pink', 
                   'sockeye', 
                   'lingcod', 
                   'halibut', 
                   'datetxt', 
                   'chinook')
  
  ###
  #format the date 
  d$month <- unlist(lapply(d[, 'datetxt'], function(x) strsplit(x, split = ' ')[[1]][1]))
  d$year <- as.numeric(unlist(lapply(d[, 'datetxt'], function(x) strsplit(x, split = ' ')[[1]][3])))
  day0 <- unlist(lapply(d[, 'datetxt'], function(x) strsplit(x, split = ' ')[[1]][2]))
  d$day <- as.numeric(unlist(lapply(day0, function(x) strsplit(x, split = ',')[[1]][1])))
  
  monthhash <- data.frame(month = month.name)
  monthhash$monthno <- seq(1,dim(monthhash)[1], 1)
  
  d <- merge(d, monthhash, by = 'month', all.x = T)
  
  d$datestamp <- as.POSIXct(paste(d$year, '-', d$monthno, '-', d$day, sep=''), format = '%Y-%m-%d')
  ###
  
  #reduce to columns of interest 
  d <- d[,c('datestamp', 
            'Site', 
            'catcharea', 
            'interviews', 
            'anglers', 
            'chinook', 
            'coho', 
            'pink', 
            'chum', 
            'lingcod', 
            'halibut')]
  
  
  #aggregate to callsite, 1 obs per day
  write.csv(d$Site[!(d$Site %in% rawsites$Site)], './Scripts/sitestoadd.csv', row.names = F)
  d <- merge(d, rawsites, by = 'Site') #, all.x = T)
  d <- merge(d, callsites, by.x = 'callname', by.y = 'Site', all.x = T)
  
  colnames(d)[c(1,2)] <- c('Site', 'rawsite')
  
  dagg <- ddply(d, c('Site', 'datestamp'), function (x) { c(
    
    interviews = sum(x$interviews, na.rm = T), 
    anglers = sum(x$anglers, na.rm = T), 
    chinook = sum(x$chinook, na.rm = T),
    coho = sum(x$coho, na.rm = T),
    pink = sum(x$pink, na.rm = T),
    chum = sum(x$chum, na.rm = T),
    lingcod = sum(x$lingcod, na.rm = T),
    halibut = sum(x$halibut, na.rm = T)
    
  )})
  return(dagg)
  
  
}
