library(readxl)
library(dplyr)
library(leaflet)

# TMDL analysis stations
VAstations <- read_excel("data/ChipMasterQuery_EVJ.xlsx",sheet="MasterQuery")

#----------------------------------------------------------------------------------------------------
#### Preprocessing Data ####
## 1) work on creating factor levels for select variables
VAstationselect <- select(VAstations,c(sampleID,Longitude,Latitude,Basin
                                       ,VSCIAll,DO,pH,SpCond,TP,TN,TotHab))
VAstationselect$DOfactor <- cut(VAstationselect$DO,c(0,7,8,10,20)
                                ,labels=c('high stress','medium stress','low stress','non-stressor')) 
# trick it into level order needed for pal
levels(VAstationselect$DOfactor) <- list('non-stressor'='non-stressor','low stress'='low stress','medium stress'='medium stress ','high stress'='high stress')
# trick it to have 4 levels for color pal purposes
VAstationselect$pHfactor <- cut(VAstationselect$pH,c(-1,0,6,9,15,16)
                                ,labels=rev(c('high stress','medium stress (-)','low stress','medium stress (+)'
                                              ,'non-stressor'))) 
levels(VAstationselect$pHfactor) <- list('non-stressor'='non-stressor','low stress'='low stress','medium stress'=c('medium stress (-)','medium stress (+)'),'high stress'='high stress')
VAstationselect$SpCondfactor <- cut(VAstationselect$SpCond,c(0,250,350,500,4000)
                                    ,labels=c('non-stressor','low stress','medium stress','high stress')) 
VAstationselect$TPfactor <- cut(VAstationselect$TP,c(0,0.02,0.05,0.1,5)
                                ,labels=c('non-stressor','low stress','medium stress','high stress')) 
VAstationselect$TNfactor <- cut(VAstationselect$TN,c(0,0.5,1,2,100)
                                ,labels=c('non-stressor','low stress','medium stress','high stress')) 
VAstationselect$TotHabfactor <- cut(VAstationselect$TotHab,c(0,100,130,150,200)
                                    ,labels=c('high stress','medium stress','low stress','non-stressor')) 
# trick it into level order needed for pal
levels(VAstationselect$TotHabfactor) <- list('non-stressor'='non-stressor','low stress'='low stress','medium stress'='medium stress ','high stress'='high stress')

# double check all factors came in correctly
str(VAstationselect)

## 2) work on palette colors for genaric factor levels
# fake df with correct factor levels
colOptions <- data.frame(stressLevels=as.factor(c("non-stressor","low stress","medium stress","high stress")))
pal <- colorFactor(c("blue","limegreen","yellow","red"),levels=colOptions$stressLevels, ordered=T)

parameterList <- as.factor(c('DO','pH','SpCond','TP','TN','TotHab'))

saveRDS(VAstationselect,'data/VAstationselect1.rds')



## Change colors in datatable
DO <- select(template,c(StationID,FieldpH,DoProbe,TN))
datatable(DO)
str(DO)


brks <- quantile(DO$DoProbe, probs = seq(.05, .95, .05), na.rm = TRUE)
brks2 <- c(0,7,8,10)
brks3 <- c(6,9)
clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
{paste0("rgb(255,", ., ",", ., ")")}
clrs2 <- c("gray","red","yellow","limegreen","blue")
clrs3 <- c("yellow","limegreen","yellow")
datatable(DO) %>% formatStyle("DoProbe", backgroundColor = styleInterval(brks, clrs))
datatable(DO) %>% formatStyle("DoProbe", backgroundColor = styleInterval(brks2, clrs2))%>%
  formatStyle("FieldpH",backgroundColor = styleInterval(brks3,clrs3))


# save CDF data with appropriate Indicator names
cdfdata2 <- read.csv('data/cdfdataFINAL.csv')
saveRDS(cdfdata2,'data/cdfdataFINAL.RDS')

