### 0 - SETUP #####

library(stringr)
library(ggplot2)
library(shiny)
library(plyr)
library(rsconnect)
library(devtools)
library(arules)
library(leaflet)

source('./Scripts/getrawcreel.R')
source('./Scripts/cleanrawcreel.R')
source('./Scripts/makecreelts.R')

initlat <- 47.871008
initlong <- -122.494435
initzoom <- 9

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

#creelcumu <- merge(creelcumu, callsites, by = 'Site')

# #grab most recent data from each site
# last1 <- by(creelcumu, creelcumu$Site, FUN = tail, 1)
# last1 <- ddply(creelcumu, c(''))
# lastcreelcumu <- do.call('rbind', last1)
# 
# #grab recent two counts from each site for % change
# change1 <- by(creelcumu, creelcumu$Site, FUN = tail, 2)
# change2 <- do.call('rbind', change1)
# 
# #calc % change between recent two counts at each site 
# creelchange <- ddply(creelcumu[,c('Site', 'Chinook', 'Coho', 'Pink', 'Chum', 'Lingcod', 'Halibut', 'Total Salmon')], 'Site', function(x) { 
#   
#   sapply(x[,2:dim(x)[2]], function (x) { (max(x) - min(x))/min(x) })
#   
#   })
# 
# #REPLACE NaNs w/ 0 (cases of 0/0)
# for(i in 2:dim(creelchange)[2]) { 
#   c <- creelchange[,i]
#   c[is.nan(c)] <- 0
#   c[is.infinite(c)] <- 0
#   creelchange[i] <- c
# }
# 
