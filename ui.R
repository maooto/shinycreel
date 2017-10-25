shinyUI(fluidPage(
  
  #Application Title
  navbarPage("WDFW Creel Count T1000 Visualizer (v1.0)", id = 'nav',
  
    tabPanel('Map', 
             div(class = 'outer', 
                 
                 leafletOutput("creelmap", height = 833.33), #, height = 833.33, width = 1000
                 
                 absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                               draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                               width = 330, height = "auto",
                               
                               h2("Creel Data Explorer"),
                               
                               ###DROPDOWNS
                               selectInput("fishtype",
                                           "Choose your fish:",
                                           choices = names(creelcumu)[c(5:10, 12)] #[3:dim(creel)[2]] 
                               ),
                               selectInput("catchrate", 
                                           "Display as fish per angler per sample day (catch intensity)?", 
                                           choices = c('No', 'Yes')), 
                               helpText("The visualizer displays the most recent cumulative count of fish (circle size), as reported to WDFW creel checkers at select sites in the last 15 days."),
                               helpText(" "),
                               helpText("Creel data from: http://wdfw.wa.gov/fishing/creel/puget"),
                               helpText(paste("WDFW creel data as of : ", max(lastcreelcumu$Date), sep = '')),
                               helpText(" "),
                               helpText(" "))
             )))))
                 
                 
                               
    