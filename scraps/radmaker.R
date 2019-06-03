#function to create vector for radi
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
