getrawcreel <- function() { library('rvest')
  library('rvest')
  
  url <- 'http://wdfw.wa.gov/fishing/creel/puget'
  
  page <- read_html(url)
  
  #identify the tables and table titles (dates)
  tablenodes <- html_nodes(page, 'table')
  datenodes <- html_nodes(page, 'h3')
  
  #read the tables and dates
  tabs <- html_table(tablenodes, fill = T)
  dats <- html_text(datenodes)
  
  #hardcode selection of particular tables that contain the creel data 
  s <- 19
  e <- 30
  shift <- s - 1 #(to adjust the loop index for dates)
  
  
  #convert list of tables to single data frame 
  for(l in s:e) { 
    
    if (l == s) { 
      
      d <- tabs[[l]]
      d$datetxt <- dats[[(l-shift)]]
      
    } else { 
      
      d0 <- tabs[[l]]
      d0$datetxt <- dats[[(l - shift)]]
      d <- rbind(d, d0)  
    }  
    
  }
  
  return(d)
  
}




