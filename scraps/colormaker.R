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
  
}
