
### SHINY SERVER

shinyServer(function(input, output) { 

  
  
  ## MAIN LEAFLET MAP
  
  output$creelmap <- renderLeaflet({
    leaflet() %>% 
      setView(lat = initlat, lng = initlong, zoom = initzoom)  %>% 
      addProviderTiles(providers$Stamen.TonerLite, 
                       options = providerTileOptions(noWrap = TRUE) 
                       ) 
    
    # %>%
    #   addCircleMarkers(radius = 2, 
    #                    color = 'brown', stroke = FALSE, fillOpacity = 1, 
    #                    lng = rawsites$long, lat = rawsites$lat, 
    #                    label = rawsites$Site)
    })
  

  #function to create vector for radi
  radmaker <- function(fish) { 
    
    d <- as.data.frame(lastcreelcumu[, c(as.character(fish), 'Site', 'lat', 'long', 'Sample days', 'Anglers')])

    d$crate <- d[,as.character(fish)]/d$Anglers/d[,c('Sample days')]
    

    total <- sum(d[,as.character(fish)])
    
    if (total == 0 ) {
      
      d$rad <- rep(1, dim(d)[1])  
      
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
  
  #function to make legend
  legmaker <- function() { 
    
    if (input$catchrate == 'No') { 
      
      leg <- mycolors
      
    } else { 
        
      leg <- mycolors2
      
    }
    
    return(leg)
  }

  #OBSERVER FOR CIRCLE SIZE MAPPING
  observe ({ 
    
    circcolor <- colormaker(input$fishtype)
      
    circradius <- radmaker(input$fishtype)
    
    legendary <- legmaker()
    

    leafletProxy("creelmap", data = circradius) %>% 
      clearShapes() %>% 
      addCircles(~long, ~lat, #layerId = Site, 
                 stroke = FALSE, fillOpacity = .85, 
                 fillColor = circcolor, 
                 radius = ~rad, 
                 label = ~Site) %>% 
      addLegend('bottomleft', colors = legendary$color, 
                labels = legendary$labels, 
                title = ifelse(input$catchrate == 'Yes', yes = '% change in catch rate', no = '% change in creel count'), 
                layerId = 'legend')
    })

  
})
  
#######old code ################  
#  output$creelplotcumu <- renderPlot({
#   
#   
#   cumuplotter <- function(fish) { 
#     
#     #ds <- creelcumu[, c('Date', 'Site', as.character(fish))]
#     
#     ggplot() + 
#       geom_line(data = creelcumu, aes(x = Date, y = creelcumu[,as.character(fish)], group = Site))
#     
#     }
#   
#   cumuplotter(input$fishtype)
#   
#   
# })

# #FUNCTION TO POPUP DISPLAY OF MOST RECENT COUNTS 
# popup <- function(site, lat, lng) {
#   
#   clickedsitedata <- creel[creel$Site == site, ] 
#   clickedsitedata <- clickedsitedata[clickedsitedata$Date == max(clickedsitedata$Date), ]
#   
#   
#                           
#   content <- as.character(tagList(
#     tags$h4(paste("Site: ", callsites$Site, sep = '')), 
#     tages$strong("Data from most recent creel check:" )
#   ))
#   
#   leafletProxy("creelmap") %>% addPopups(lng, lat, content, layerId = site)
# }
# 
# 
# observe({
#   
#   leafletProxy("creelmap") %>% clearPopups()
#   event <- input$map_shap_click
#   if(is.null(event))
#     return()
#   
#   isolate({
#     popup(event$id, event$lat, event$lng)
#   })
# })
# 

