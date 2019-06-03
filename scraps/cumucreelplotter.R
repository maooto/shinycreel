cumucreelplotter <- function() {
  

  
  
  
  
  
  
  
  
  
  plotter <- function(xmin, xmax, ymin, ymax, labx, laby) { 
    ggplot(data = results, aes(x = xvar, y = yvar, group = init)) + 
      scale_x_continuous(breaks = seq(xmin, xmax, by = 1),
                         limits = c(xmin-scaleshift, xmax+scaleshift),
                         minor_breaks = NULL, 
                         labels = function(x) str_wrap(labx, width = 12)) + 
      scale_y_continuous(breaks = seq(ymin, ymax, by = 1),
                         limits = c(ymin-scaleshift, ymax+scaleshift),
                         minor_breaks = NULL, 
                         labels = function(x) str_wrap(laby, width = 12)) + 
      geom_text(data = results, aes(x = xvar, y = yvar, label = init, col = init), 
                position = position_jitter(width = jittshiftw, height = jittshifth ), 
                fontface = 'bold', 
                angle = 30, 
                size = 10) + 
      geom_point(data = results, aes(x = xvar, y = yvar, col = init), 
                 size = 11, alpha = .2) + 
      theme(legend.position = 'none',
            axis.title.x = element_text(size = textsize+2, face = 'bold'), 
            axis.title.y = element_text(size = textsize+2, face = 'bold'),
            axis.text.x = element_text(size = textsize), 
            axis.text.y = element_text(size = textsize)) + 
      labs(x = as.character(input$Q1),
           y = as.character(input$Q2))
  }
  
  
  
}




