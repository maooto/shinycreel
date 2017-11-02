labelmaker <- function(results) { 
  
  labeldf <- data.frame(labels = rep('o', dim(results)[1]), stringsAsFactors = F)
  
  for (c in 1:dim(results)[1]) { 
    
    labeldf$labels[c] <- paste("<h5>", results$Site[c], "</h5>",
                               "<b>Cumulative fish catch: </b>", results$fish[c], "<br>",
                               "<b>%change in cumulative count: </b>", sprintf("%.1f%%", results$fishchange[c]),"<br>",
                               "<br>", 
                               "<b>Cumulative catch rate: </b>", results$crate[c],"<br>",
                               "<b>%change in catch rate: </b>", sprintf("%.1f%%", results$cratechange[c]),"<br>",
                               "<br>", 
                               "<b>Last day sampled: </b>", results$Date[c], "<br>",
                               sep = "")
    
    
    
  }
 
  return(as.list(labeldf$labels))
  
}

