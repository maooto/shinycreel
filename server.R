
### SHINY SERVER

shinyServer(function(input, output) { 

  
  
  ## MAIN LEAFLET MAP
  
  output$creelmap <- renderLeaflet({
    leaflet() %>% 
      setView(lat = initlat, lng = initlong, zoom = initzoom)  %>% 
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite, #Stamen.TonerLite, 
                       options = providerTileOptions(noWrap = TRUE) 
                       ) 
    })
  

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
    
    
    legendary <- legmaker()
    
    creelres <- circlemaker(fish = input$fishtype, cratechoice = input$catchrate)

    leafletProxy("creelmap") %>% 
      clearShapes() %>% 
      addCircles(data = creelres, 
                 lng = ~long, 
                 lat = ~lat,
                 stroke = TRUE, 
                 weight = 1,
                 color = 'black',
                 fillOpacity = .85, 
                 fillColor = ~color, 
                 radius = ~rad, 
                 label = lapply(labelmaker(results = creelres), htmltools::HTML)) %>% 
      addLegend('bottomleft', colors = legendary$color, 
                labels = legendary$labels, 
                title = ifelse(input$catchrate == 'Yes', yes = '% change in catch rate', no = '% change in creel count'), 
                layerId = 'legend')
    })

  
})



