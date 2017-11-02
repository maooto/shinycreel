### 0 - SETUP #####

library(stringr)
library(ggplot2)
library(shiny)
library(plyr)
library(rsconnect)
library(devtools)
library(arules)
library(leaflet)
library(htmltools)

source('./Scripts/getrawcreel.R')
source('./Scripts/cleanrawcreel.R')
source('./Scripts/makecreelts.R')
source('./Scripts/circlemaker.R')
source('./Scripts/labelmaker.R')

initlat <- 47.871008
initlong <- -122.494435
initzoom <- 9

## radii for circles
rads <- data.frame(radbin = c(0:5), 
                   rad = c(100, 1000, 2000, 3000, 5000, 7500))


mycolors <- data.frame(colbin = c(0:6), 
                       color = as.character(c('#878787', #no change
                                 '#74add1', # (0 - 20%)
                                 '#f4a582', # [20 - 40)
                                 '#d6604d', # [40 - 60)
                                 '#b2182b', # [60 - 80)
                                 '#67001f', # [80 - 100]
                                 '#7a0177')), # > 100 
                       labels = as.character(c('no change',
                                  '(0% , 20%)',
                                  '[20% , 40%)',
                                  '[40% , 60%)',
                                  '[60% , 80%)',
                                  '[80% , 100%]',
                                  '> 100% increase'))
)

#colors for catchrates 
mycolors2 <- data.frame(colbin = c(1:9), 
                       color = as.character(c('#003c30',
                                 '#01665e',
                                 '#35978f',
                                 '#80cdc1',
                                 '#bababa',
                                 '#dfc27d',
                                 '#bf812d',
                                 '#8c510a',
                                 '#543005')), 
                       labels = as.character(c(' <= -75%', 
                                  '(-75%, -50%]', 
                                  '', 
                                  '', 
                                  'no change',
                                  '', 
                                  '', 
                                  '[50% , 75%)', 
                                  ' > 75%'))
)

#SITE DATA 
rawsites <- read.csv(file = './Data/rawsites.csv', stringsAsFactors = F)
colnames(rawsites) <- c('Site', 'lat', 'long', 'callname')
callsites <- read.csv(file = './Data/callsites.csv', stringsAsFactors = F)
colnames(callsites) <- c('Site', 'lat', 'long')

### 1 - SCRAPE/PREP DATA  #####

creelraw <- getrawcreel()

creelclean <- cleanrawcreel(creelraw)

creeltsfull <- makecreelts(creelclean)


### 2 - MORE DATA POLISHING  #####

creelcumu <- creeltsfull[ , c(1:2, 13:dim(creeltsfull)[2])]

creelcumu <- creeltsfull[ , c('datestamp', 
                              'Site', 
                              'sampleday', 
                              'cuinterviews', 
                              'cuanglers', 
                              'cuchinook', 
                              'cucoho', 
                              'cupink', 
                              'cuchum', 
                              'culingcod', 
                              'cuhalibut', 
                              'cusampleday',
                              'cuallsalmon')]


names(creelcumu) <- c('Date', 'Site', 'sampled', 'Interviews', 'Anglers', 'Chinook', 'Coho', 'Pink', 'Chum', 'Lingcod', 'Halibut', 'Sample days', 'Total Salmon')
