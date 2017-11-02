circlemaker <- function(fish) {
  
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
  
  creel <- merge(lastcreel, firstcreel[, c('Site', 'firstfish', 'firstcrate')], by = 'Site')
  
  
  ## calculate percent changes between most recent and least recent 
  ## reports for each site 
  creel$fishchange <- (creel[,as.character(fish)] - creel$firstfish)/creel$firstfish
  creel$cratechange <- (creel$crate - creel$firstcrate)/creel$firstcrate
  
  if (input$catchrate == 'No') {
    
  creel <- creel[, c(as.character(fish), 'Site', 'fishchange')]  
    
  } else { 
    
  creel <- creel[, c('Site', 'crate', 'cratechange')]
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
}

#function to choose circle color
colormaker <- function(fish) { 
  
  if (input$catchrate == 'No') {
    
    d <- ddply(creelcumu[, c(as.character(fish), 'Site')], 'Site', function(x) { 
      c(
        pctchange = 100*(max(x[,as.character(fish)]) - min(x[,as.character(fish)]))/min(x[,as.character(fish)])
      )
    })
    
    d$pctchange[(is.nan(d$pctchange) == T | 
                   is.infinite(d$pctchange) == T)] <- 0 
    
    
    d$colbin <- 0
    
    d$colbin[d$pctchange > 0 & 
               d$pctchange < 20] <- 1
    
    d$colbin[d$pctchange >= 20 & 
               d$pctchange < 40] <- 2
    
    d$colbin[d$pctchange >= 40 & 
               d$pctchange < 60] <- 3
    
    d$colbin[d$pctchange >= 60 & 
               d$pctchange < 80] <- 4
    
    d$colbin[d$pctchange >= 80 & 
               d$pctchange <= 100] <- 5
    
    d$colbin[d$pctchange > 100] <- 6
    
    d <- merge(d, mycolors, by = 'colbin')
    
    
  } else { 
    
    creelcumu$crate <- creelcumu[,as.character(fish)]/creelcumu$Anglers/creelcumu[,c('Sample days')]
    
    creelcumu <- creelcumu[order(creelcumu$Site, creelcumu$Date), ]
    
    d <- ddply(creelcumu[, c('crate', 'Site')], 'Site', function(x) { 
      c(
        pctchange = 100*(tail(x$crate,1) - head(x$crate,1))/head(x$crate, 1)
      )
    })
    
    d$pctchange[(is.nan(d$pctchange) == T | 
                   is.infinite(d$pctchange) == T)] <- 0 
    
    
    d$colbin <- 0
    
    d$colbin[d$pctchange <= -75] <- 1
    
    d$colbin[d$pctchange > -75 & 
               d$pctchange < -50] <- 2
    
    d$colbin[d$pctchange >= -50 & 
               d$pctchange < -25] <- 3
    
    d$colbin[d$pctchange >= -25 & 
               d$pctchange < 0] <- 4
    
    d$colbin[d$pctchange == 0] <- 5
    
    d$colbin[d$pctchange > 0 & 
               d$pctchange < 25] <- 6
    
    d$colbin[d$pctchange >= 25 & 
               d$pctchange < 50] <- 7
    
    
    d$colbin[d$pctchange >= 50 & 
               d$pctchange < 75] <- 8
    
    d$colbin[d$pctchange >= 75] <- 9
    
    d <- merge(d, mycolors2, by = 'colbin')
  }
  
  return(d$color)
  



radmaker <- function(fish) { 
  
  #d <- as.data.frame(lastcreelcumu[, c(as.character(fish), 'Site', 'lat', 'long', 'Sample days', 'Anglers')])
  
  d <- as.data.frame(creelcumu[, c(as.character(fish), 'Site', 'lat', 'long', 'Sample days', 'Anglers')])
  
  
  d$crate <- d[,as.character(fish)]/d$Anglers/d[,c('Sample days')]
  
  
  total <- sum(d[,as.character(fish)])
  
  if (total == 0 ) {
    
    d$rad <- rep(1, dim(d)[1])  #deal with edge case of empty dataset
    
  } else { 
    
    
    if (input$catchrate == 'No') { 
      
      d$radbin <- 0
      
      d$radbin[d[,as.character(fish)] > 0 & 
                 d[,as.character(fish)] < 10] <- 1
      
      d$radbin[d[,as.character(fish)] >= 10 & 
                 d[,as.character(fish)] < 25] <- 2
      
      d$radbin[d[,as.character(fish)] >= 25 & 
                 d[,as.character(fish)] < 50] <- 3
      
      d$radbin[d[,as.character(fish)] >= 50 & 
                 d[,as.character(fish)] < 100] <- 4
      
      d$radbin[d[,as.character(fish)] >= 100] <- 5
      
    } else { 
      
      d$radbin <- 0
      
      d$radbin[d$crate > 0 & 
                 d$crate < .10] <- 1
      
      d$radbin[d$crate >= .10 & 
                 d$crate < .25] <- 2
      
      d$radbin[d$crate >= .25 & 
                 d$crate < .4] <- 3
      
      d$radbin[d$crate >= .4 & 
                 d$crate < .5] <- 4
      
      d$radbin[d$crate >= .5] <- 5
      
    }
    
    rads <- data.frame(radbin = c(1:5), 
                       rad = c(1000, 2000, 3000, 5000, 7500))
    
    d <- merge(d, rads, by = 'radbin')
    
  } 
  
  return(d)
  
}
