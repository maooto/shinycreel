getrawcreel <- function() { 
  
  library('rvest')
  
  url <- 'http://wdfw.wa.gov/fishing/creel/puget'
  
  page <- read_html(url)
  
  #identify the tables and table titles (dates)
  tablenodes <- html_nodes(page, 'table')
  datenodes <- html_nodes(page, 'caption')
  
  #read the tables and dates
  tabs <- html_table(tablenodes, fill = T)
  dats <- html_text(datenodes, trim = T)
  
  
  #hardcode selection of particular tables that contain the creel data 
  #s <- 19
  #e <- 30
  #shift <- s - 1 #(to adjust the loop index for dates)
  
  
  #convert list of tables to single data frame
  
  for(l in 1:length(tablenodes)) { 
    
    if (l == 1) { 
      
      d <- tabs[[l]]
      d$datetxt <- dats[[l]]
      
    } else { 
      
      d0 <- tabs[[l]]
      d0$datetxt <- dats[[l]]
      d <- rbind(d, d0)  
    }  
    
  }
  
  write.csv(d, paste('./Data/data cache/draw_', Sys.Date(), '.csv'), row.names = F)
  
  return(d)
  
}




