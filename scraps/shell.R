# rm(list = ls())
# gc(reset = T)

##### 0 - SETUP ########
source('./0 - Scripts/getrawcreel.R')
source('./0 - Scripts/cleanrawcreel.R')
source('./0 - Scripts/makecreelts.R')

##### 1 - GET DATA #####

## CREEL DATA ##

creelraw <- getrawcreel()

creel <- cleanrawcreel(creelraw)

creelts <- makecreelts(creel)

##


#### 2 - 