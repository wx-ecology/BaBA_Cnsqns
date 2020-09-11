setwd("C:/Users/wenjing.xu/Google Drive/RESEARCH/Pronghorn/Analysis/BaBA_Season2")
setwd("/Users/Mushy 1/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis/BaBA_Season2")

library(tidyverse)
library(lubridate)
library(patchwork) # for plotting multiple 
library(plotly) # interactive plot
library(htmlwidgets)
library(BaBA)

# load previous results
data <- read.csv("./data/I2_PRON_FB110_B4_P36_FinalCls.csv")
data$date <- ymd_hm (data$burstID, tz = "US/Mountain") 
 #ttt

# total encounter monthly trend
data1 <- data %>% mutate(month = month(date)) %>% 
  group_by(AnimalID, month) %>% summarize(freq = n())

p1 <- ggplot(data = data1, aes(x = month, y = freq, color = AnimalID)) +
  geom_line() +
  theme_bw() + 
  theme(legend.position = "none") 
  

# normal BaBA monthly trend
data2 <- data %>% mutate(month = month(date),
                         eventCAT = ifelse((eventTYPE %in% c("Bounce", "Trace", "Back-n-forth", "Trapped")), "altered", "normal")) %>% 
  group_by(AnimalID, month, eventCAT) %>% summarize(freq = n())

p2 <- ggplot(data = data2 %>% filter(eventCAT == "normal"), aes(x = month, y = freq, color = AnimalID)) +
  geom_line() +
  theme_bw() + 
  theme(legend.position = "none") 

# altered BaBA monthly trend
data3 <- data %>% mutate(month = month(date),
                         eventCAT = ifelse((eventTYPE %in% c("Bounce", "Trace", "Back-n-forth", "Trapped")), "altered", "normal")) %>% 
  group_by(AnimalID, month, eventCAT) %>% summarize(freq = n())

p3 <- ggplot(data = data3 %>% filter(eventCAT == "altered"), aes(x = month, y = freq, color = AnimalID)) +
  geom_line() +
  theme_bw() + 
  theme(legend.position = "none") 

p1 + p2 + p3
ggplotly(p1)
ggplotly(p3)

# fence ranking
data_2014 <- data %>% filter(year == 2014)
data_2016 <- data %>% filter(year == 2016)
Ba_Ranking_2014 = BaRanking(data_2014,fences,d = 90,index_fun = expression(total_enc))
Ba_Ranking_2016 = BaRanking(data_2016,fences,d = 90,index_fun = expression(total_enc))

Ba_Ranking_2014 <- Ba_Ranking_2014 %>% filter(!is.na(total_enc))
Ba_Ranking_2016 <- Ba_Ranking_2016 %>% filter(!is.na(total_enc))

View(Ba_Ranking_2014 %>% arrange(-total_enc))
View(Ba_Ranking_2016 %>% arrange(-total_enc))

#fence 1283 got 74 in 2014 and 81 in 2016
Fence_1283_2014 <- Ba_Ranking_2014 %>% filter(FID_Fence0 == "1283")
Fence_1283_2016 <- Ba_Ranking_2016 %>% filter(FID_Fence0 == "1283")

