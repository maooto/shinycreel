circlemaker <- function(fish , cratechoice ) {
  
  ## restrict to user-selected data 
  d <- as.data.frame(creelcumu[, c(as.character(fish), 'Site', 'Sample days', 'Anglers', 'Date', 'sampled')])
  
  d$crate <- d[,as.character(fish)]/d$Anglers/d[,c('Sample days')]
  d$crate[is.nan(d$crate) == T] <- 0 #replace NaNs (e.g. unsampled days) w/ 0s
  
  
  
  sitelist <- unique(d$Site)
  lastdlist <- list(NULL)
  firstdlist <- list(NULL)
  
  ## get the first and last obs for each site
  for (s in 1:length(sitelist)) { 
    
    lastdlist[[s]] <- d[d$Site == sitelist[s] & 
                    d$Date == max(d$Date[d$sampled == 1 & 
                                         d$Site == sitelist[s]]),]
    
    firstdlist[[s]] <- d[d$Site == sitelist[s] & 
                          d$Date == min(d$Date[d$sampled == 1 & 
                                                 d$Site == sitelist[s]]),]
    
  }
  
  lastcreel <- do.call('rbind', lastdlist)
  firstcreel <- do.call('rbind', firstdlist)
  
  colnames(firstcreel)[c(1, 5, 7)] <- c('firstfish', 'firstdate', 'firstcrate')
  
  creel <- merge(lastcreel, firstcreel[, c('Site', 'firstfish', 'firstcrate')], by = 'Site', all.x = T)
  
  
  ## calculate percent changes between most recent and least recent 
  ## reports for each site 
  creel$fishchange <- (creel[,as.character(fish)] - creel$firstfish)/creel$firstfish
  creel$fishchange[is.nan(creel$fishchange) == T] <- 0
  creel$fishchange[is.infinite(creel$fishchange)] <- 1500
  
  creel$cratechange <- (creel$crate - creel$firstcrate)/creel$firstcrate
  creel$cratechange[is.infinite(creel$cratechange) == T] <- 1500
  creel$cratechange[is.nan(creel$cratechange) == T] <- 0
  creel$cratechange <- 100*creel$cratechange
  
  
  if (cratechoice == 'No') {
    
  ## circle colors
  creel$colbin <- 0
  
  creel$colbin[creel$fishchange > 0 & 
             creel$fishchange < 20] <- 1
  
  creel$colbin[creel$fishchange >= 20 & 
             creel$fishchange < 40] <- 2
  
  creel$colbin[creel$fishchange >= 40 & 
             creel$fishchange < 60] <- 3
  
  creel$colbin[creel$fishchange >= 60 & 
             creel$fishchange < 80] <- 4
  
  creel$colbin[creel$fishchange >= 80 & 
             creel$fishchange <= 100] <- 5
  
  creel$colbin[creel$pctchange > 100] <- 6
  
  
  ## circle radii 
  creel$radbin <- 0
  
  creel$radbin[creel[,as.character(fish)] > 0 & 
             creel[,as.character(fish)] < 10] <- 1
  
  creel$radbin[creel[,as.character(fish)] >= 10 & 
             creel[,as.character(fish)] < 25] <- 2
  
  creel$radbin[creel[,as.character(fish)] >= 25 & 
             creel[,as.character(fish)] < 50] <- 3
  
  creel$radbin[creel[,as.character(fish)] >= 50 & 
             creel[,as.character(fish)] < 100] <- 4
  
  creel$radbin[creel[,as.character(fish)] >= 100] <- 5
  
  
  creel <- merge(creel, mycolors, by = 'colbin')
  creel <- merge(creel, rads, by = 'radbin')
  
    
  } else { 
    
  
  ## circle colors
  creel$colbin <- 0
  
  creel$colbin[creel$cratechange <= -75] <- 1
  
  creel$colbin[creel$cratechange > -75 & 
                 creel$cratechange < -50] <- 2
  
  creel$colbin[creel$cratechange >= -50 & 
                 creel$cratechange < -25] <- 3
  
  creel$colbin[creel$cratechange >= -25 & 
                 creel$cratechange < 0] <- 4
  
  creel$colbin[creel$cratechange == 0] <- 5
  
  creel$colbin[creel$cratechange > 0 & 
                 creel$cratechange < 25] <- 6
  
  creel$colbin[creel$cratechange >= 25 & 
                 creel$cratechange < 50] <- 7
  
  
  creel$colbin[creel$cratechange >= 50 & 
                 creel$cratechange < 75] <- 8
  
  creel$colbin[creel$cratechange >= 75] <- 9
  
  
  ## circle radii 
  creel$radbin <- 0
  
  creel$radbin[creel$crate > 0 & 
                 creel$crate < .10] <- 1
  
  creel$radbin[creel$crate >= .10 & 
                 creel$crate < .25] <- 2
  
  creel$radbin[creel$crate >= .25 & 
                 creel$crate < .4] <- 3
  
  creel$radbin[creel$crate >= .4 & 
                 creel$crate < .5] <- 4
  
  creel$radbin[creel$crate >= .5] <- 5
  
  creel <- merge(creel, rads, by = 'radbin')
  creel <- merge(creel, mycolors2, by = 'colbin')
  
  }
  
  creel <- merge(creel, callsites, by = 'Site')
  
  creel <- creel[, c('Site', 
                     as.character(fish), 
                     "Sample days", 
                     "Anglers", 
                     'Date', 
                     'crate',
                     'firstfish', 
                     'firstcrate', 
                     'fishchange', 
                     'cratechange', 
                     'rad', 
                     'color', 
                     'lat', 
                     'long')]
  colnames(creel)[2] <- 'fish'
  
  return(creel)
  

  
}
