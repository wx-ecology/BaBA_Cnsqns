#baba around 2 with all animals
setwd("/Users/Mushy 1/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/BaBA_Season2")
target.crs <- "+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
target.crs <- "+init=epsg:32612"

library(tidyverse)
library(lubridate)
library(BaBA)

## read files and prep for BaBA
pronghorn <- read_csv("./data/Animal_Location_All.csv") %>% mutate(Date = ymd_hms(Date, tz = "US/Mountain")) %>% 
  rename (date = Date, Animal.ID = Location.ID) 
# calculate GPS data interval
Intervals <- tapply(pronghorn$date, pronghorn$Animal.ID, function(x) as.numeric(names(which.max(table(as.numeric(diff(x), units = units))))))
Intervals <- data.frame(Animal.ID = names(Intervals), interval = Intervals)
pronghorn <- pronghorn %>% left_join(Intervals)
pronghorn.2h <-pronghorn %>% filter(interval ==2) # 63 animals
pronghorn.3h <- pronghorn %>% filter(interval ==3) # 115 animals

pronghorn.2h.sp <- SpatialPointsDataFrame(coords = cbind(pronghorn.2h$Easting, pronghorn.2h$Northing), data = pronghorn.2h[,1:2], proj4string = CRS(target.crs))
pronghorn.3h.sp <- SpatialPointsDataFrame(coords = cbind(pronghorn.3h$Easting, pronghorn.3h$Northing), data = pronghorn.3h[,1:2], proj4string = CRS(target.crs))

#read fence file
fence.sp <- readOGR("./data/", 'Fence_may2021_single')
fence.segs.sp <- readOGR("./data/", 'Fence_may2021_2kmSegs')

## run baba 
pronghorn.2h.baba.110 <- BaBA(animal = pronghorn.2h.sp, barrier = fence.sp, d = 110, interval =2, units = "hours")

