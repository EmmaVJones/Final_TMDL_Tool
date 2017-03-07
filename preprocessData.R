library(readxl)
library(dplyr)
library(leaflet)

# New template to incorporate GIS
templateGIS <- mutate(template,Longitude=-80.000833333,Latitude=37.78861111)%>%select(StationID,CollectionDateTime,Longitude,Latitude,everything())
write.csv(templateGIS,'data/templateGIS.csv',row.names=F)

# TMDL analysis stations
VAstations <- read_excel("data/ProbMetrics_2001-2014_Final_Web_March_3_2017.xlsx",sheet="Final_ProbMetrics")

#----------------------------------------------------------------------------------------------------
#### Preprocessing Data ####
## 1) work on creating factor levels for select variables
VAstationselect <- select(VAstations,c(StationID,LongitudeDD,LatitudeDD,Year,Basin,VSCIVCPMI,DO,pH,SpCond,
                                       TP,TN,TotHab,LRBS,MetalCCU,TDS,Na,K,Cl,Sf))%>%
  dplyr::rename(Longitude=LongitudeDD,Latitude=LatitudeDD)
# Factor level work
VAstationselect$DOfactor <- cut(VAstationselect$DO,c(0,7,8,10,20)
                                ,labels=c('High Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','No Probability of Stress to Aquatic Life')) 
# trick it into level order needed for pal
levels(VAstationselect$DOfactor) <- list('No Probability of Stress to Aquatic Life'='No Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life'='Low Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life'='Medium Probability of Stress to Aquatic Life ','High Probability of Stress to Aquatic Life'='High Probability of Stress to Aquatic Life')
# trick it to have 4 levels for color pal purposes
VAstationselect$pHfactor <- cut(VAstationselect$pH,c(-1,0,6,9,15,16)
                                ,labels=rev(c('High Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life (-)','Low Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life (+)'
                                              ,'No Probability of Stress to Aquatic Life'))) 
levels(VAstationselect$pHfactor) <- list('No Probability of Stress to Aquatic Life'='No Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life'='Low Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life'=c('Medium Probability of Stress to Aquatic Life (-)','Medium Probability of Stress to Aquatic Life (+)'),'High Probability of Stress to Aquatic Life'='High Probability of Stress to Aquatic Life')
VAstationselect$SpCondfactor <- cut(VAstationselect$SpCond,c(0,250,350,500,4000)
                                    ,labels=c('No Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life','High Probability of Stress to Aquatic Life')) 
VAstationselect$TPfactor <- cut(VAstationselect$TP,c(0,0.02,0.05,0.1,5)
                                ,labels=c('No Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life','High Probability of Stress to Aquatic Life')) 
VAstationselect$TNfactor <- cut(VAstationselect$TN,c(0,0.5,1,2,100)
                                ,labels=c('No Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life','High Probability of Stress to Aquatic Life')) 
VAstationselect$TotHabfactor <- cut(VAstationselect$TotHab,c(0,100,130,150,200)
                                    ,labels=c('High Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','No Probability of Stress to Aquatic Life')) 
# trick it into level order needed for pal
levels(VAstationselect$TotHabfactor) <- list('No Probability of Stress to Aquatic Life'='No Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life'='Low Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life'='Medium Probability of Stress to Aquatic Life ','High Probability of Stress to Aquatic Life'='High Probability of Stress to Aquatic Life')

VAstationselect$TDSfactor <- cut(VAstationselect$TDS,c(0,100,250,350,50000)
                                ,labels=c('No Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life','High Probability of Stress to Aquatic Life')) 
VAstationselect$MetalCCUfactor <- cut(VAstationselect$MetalCCU,c(0,0.75,1.5,2.0,50)
                                   ,labels=c('No Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life','High Probability of Stress to Aquatic Life')) 
# trick it to have 4 levels for color pal purposes
VAstationselect$LRBSfactor <- cut(VAstationselect$LRBS,c(-20,-1.5,-1.0,-0.5,0.5,15)
                                ,labels=rev(c('High Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life (-)','Low Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life (+)'
                                              ,'No Probability of Stress to Aquatic Life'))) 
levels(VAstationselect$LRBSfactor) <- list('No Probability of Stress to Aquatic Life'='No Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life'='Low Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life'=c('Medium Probability of Stress to Aquatic Life (-)','Medium Probability of Stress to Aquatic Life (+)'),'High Probability of Stress to Aquatic Life'='High Probability of Stress to Aquatic Life')
VAstationselect$Nafactor <- cut(VAstationselect$Na,c(0,7,10,20,500)
                                      ,labels=c('No Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life','High Probability of Stress to Aquatic Life')) 
VAstationselect$Kfactor <- cut(VAstationselect$K,c(0,1,2,10,500)
                                  ,labels=c('No Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life','High Probability of Stress to Aquatic Life')) 
VAstationselect$Clfactor <- cut(VAstationselect$Cl,c(0,10,25,50,500)
                                  ,labels=c('No Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life','High Probability of Stress to Aquatic Life')) 
VAstationselect$Sffactor <- cut(VAstationselect$Sf,c(0,10,25,75,500)
                                  ,labels=c('No Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life','High Probability of Stress to Aquatic Life')) 
VAstationselect$VSCIfactor <- cut(VAstationselect$VSCIVCPMI,c(0,42,60,72,115)
                                  ,labels=c('High Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','No Probability of Stress to Aquatic Life')) 


# double check all factors came in correctly
str(VAstationselect)

## 2) work on palette colors for genaric factor levels
# fake df with correct factor levels
colOptions <- data.frame(stressLevels=as.factor(c("No Probability of Stress to Aquatic Life","Low Probability of Stress to Aquatic Life","Medium Probability of Stress to Aquatic Life","High Probability of Stress to Aquatic Life")))
pal <- colorFactor(c("blue","limegreen","yellow","red"),levels=colOptions$stressLevels, ordered=T)

parameterList <- as.factor(c('VSCIVCPMI','DO','pH','SpCond','TP','TN','TotHab',"TDS","MetalCCU","LRBS","Na","K","Cl","Sf"))

saveRDS(VAstationselect,'data/VAstationselectMarch2017update_final.RDS')

# Make new dat4 dataset
VAstationselect_ <- mutate(VAstationselect,VSCIAll=VSCIVCPMI,VSCI=VSCIVCPMI)%>%
  rename(Longitude=LongitudeDD,Latitude=LatitudeDD)%>%
  select(-c(Year,VSCIVCPMI,VSCI,DO,pH,SpCond,TP,TN,TotHab,TDS,MetalCCU,LRBS,Na,K,Cl,Sf))%>%
  reshape2::melt(id=c('StationID',"Longitude","Latitude","Basin","VSCIAll"))%>%
  filter(!is.na(value))%>%
  mutate(joincolumn=gsub("(.*)f.*","\\1",variable))

# do the same with regular data so it iwll be available for a popup in the final leaflet map
VAstationselect_1 <- mutate(VAstationselect,VSCI=VSCIVCPMI,VSCIAll=VSCIVCPMI)%>%
  rename(Longitude=LongitudeDD,Latitude=LatitudeDD)%>%select(-VSCIVCPMI)%>%
  select(c(StationID,Longitude,Latitude,Basin,VSCIAll,VSCI,DO,pH,SpCond,TP,TN,TotHab,TDS,
           MetalCCU,LRBS,Na,K,Cl,Sf))%>%
  reshape2::melt(id=c('StationID',"Longitude","Latitude","Basin" ,"VSCIAll"))%>%
  filter(!is.na(value))%>%mutate(joincolumn=variable)%>%
  merge(VAstationselect_,by=c('joincolumn','StationID',"Longitude","Latitude","Basin" ,"VSCIAll"))%>%
  rename(Parameter=variable.x,ParameterMeasure=value.x,ParameterFactor=variable.y,ParameterFactorLevel=value.y)%>%
  select(-joincolumn)%>%
  mutate(units=Parameter)# make column with units for popups
VAstationselect_1$units <- dplyr::recode(VAstationselect_1$units,"VSCI"="(unitless)","DO"="mg/L","pH"="(unitless)","SpCond"="uS/cm","TP"="mg/L","TN"="mg/L","TotHab"="(unitless)",
                                         "TDS"="mg/L","MetalCCU"="(unitless)","LRBS"="(unitless)","Na"="mg/L","K"="mg/L","Cl"="mg/L",
                                         "Sf"="mg/L")



saveRDS(VAstationselect_1,'data/VirginiaStations.RDS')





# save CDF data with appropriate Indicator names
cdfdata2 <- read.csv('data/cdfdataMarch2017update_final.csv')
# Work up new Dissolved Metals data
metalsCDF <- read_excel("data/DissMetalStatus.CDF.xlsx",sheet="biostatus.CDF")%>%
  select(-c(Type))%>%
  filter(!(Subpopulation=='Subpopulation'|is.na(Subpopulation)))
cdfdata2 <- rbind(cdfdata2,metalsCDF)

# Add units to cdfdata final
cdfdata2 <- mutate(cdfdata2,units=Indicator)
cdfdata2$units <- dplyr::recode(cdfdata2$units,"VSCIAll"="(unitless)","DChloride"="mg/L","DO"="mg/L","DPotassium"="mg/L","DSodium"="mg/L","DSulfate"="mg/L",
                                "LRBS"="(unitless)", "MetalsCCU"="(unitless)","pH"="(unitless)","SpCond"="uS/cm","TDS"="mg/L",
                                "TN"="mg/L","TotalHabitat"="(unitless)","TP"="mg/L","ANTIMONY"="ug/L","ALUMINUM"="ug/L",
                                "ARSENIC"="ug/L","BARIUM"="ug/L","BERY"="ug/L","CADMIUM"="ug/L","CALCUIM"="mg/L","CHROMIUM"="ug/L",
                                "COPPER"="ug/L","IRON"="ug/L","LEAD"="ug/L","MAGN"="mg/L","MANGANESE"="ug/L","NICKEL"="ug/L",
                                "SELENIUM"="ug/L","SILVER"="ug/L","THALLIUM"="ug/L","ZINC"="ug/L","HARDNESS"="mg/L")    
cdfdata2$Indicator <- dplyr::recode(cdfdata2$Indicator,'CALCUIM'='CALCIUM','BERY'='BERYLLIUM','MAGN'='MAGNESIUM')
saveRDS(cdfdata2,'data/cdfdataMarch2017update_final.RDS')                               


# Work up new Dissolved Metals data
metalsCDF <- read_excel("data/probmon_trend_status.xlsx",sheet="dissMetal_Status")%>%select(-c(Type))
metalsCDF <- metalsCDF[-1,]
saveRDS(metalsCDF,'data/metalsCDF_March2017Update.RDS')

# Now for station specific data, make these into factor classes for leaflet map
## 1) work on creating factor levels for select variables
metalsStations <- read_excel("data/DissMetalStatus.CDF.xlsx",sheet="Data")[c(1:535,538),]%>%
  select(c(sampleID,Longitude-DD,Latitude-DD,Basin))
# pause on this for now


# Make template_metals, include Nickel violation to test when sites blow standards
metalsStations <- as.data.frame(read_excel("data/DissMetalStatus.CDF.xlsx",sheet="Data")[,c(1,19,3:4,40:58)])
names(metalsStations) <- c("StationID","CollectionDateTime","Longitude","Latitude","Calcium",           
                           "Magnesium","Arsenic","Barium","Beryllium","Cadmium",
                           "Chromium","Copper","Iron","Lead","Manganese",         
                           "Thallium","Nickel","Silver","Zinc","Antimony",        
                           "Aluminum","Selenium","Hardness")
template_metals <- filter(metalsStations,StationID %in% c( '2-HAM000.37_2014','6BHAR002.41','2-XUD000.15',
                                                           '6CGAS000.45','1BMSS001.35'))
write.csv(template_metals,'data/template_metals.csv',row.names=F)

