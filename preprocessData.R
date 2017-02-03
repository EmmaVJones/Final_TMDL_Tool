library(readxl)
library(dplyr)
library(leaflet)

# New template to incorporate GIS
templateGIS <- mutate(template,Longitude=-80.000833333,Latitude=37.78861111)%>%select(StationID,CollectionDateTime,Longitude,Latitude,everything())
write.csv(templateGIS,'data/templateGIS.csv',row.names=F)

# TMDL analysis stations
VAstations <- read_excel("data/ChipMasterQuery_EVJ.xlsx",sheet="MasterQuery")

#----------------------------------------------------------------------------------------------------
#### Preprocessing Data ####
## 1) work on creating factor levels for select variables
VAstationselect <- select(VAstations,c(sampleID,Longitude,Latitude,Year,Basin,VSCIAll,DO,pH,SpCond,TP,TN,TotHab))
# now deal with stupid named columns
VAstationsselect1 <- VAstations[,c(1,3,4,17,30,31,34,36:39)] 
colnames(VAstationsselect1) <- c('sampleID','Longitude','Latitude','Year','TDS_V','MetalCCU','LRBS','NA_V','K_V','Cl_V','Sf_V')
# combine again
VAstationselect <- merge(VAstationselect,VAstationsselect1,by=c('sampleID','Longitude','Latitude','Year'))
rm(VAstationsselect1)
# Factor level work
VAstationselect$DOfactor <- cut(VAstationselect$DO,c(0,7,8,10,20)
                                ,labels=c('High Risk to Aquatic Life','Medium Risk to Aquatic Life','Low Risk to Aquatic Life','No Risk to Aquatic Life')) 
# trick it into level order needed for pal
levels(VAstationselect$DOfactor) <- list('No Risk to Aquatic Life'='No Risk to Aquatic Life','Low Risk to Aquatic Life'='Low Risk to Aquatic Life','Medium Risk to Aquatic Life'='Medium Risk to Aquatic Life ','High Risk to Aquatic Life'='High Risk to Aquatic Life')
# trick it to have 4 levels for color pal purposes
VAstationselect$pHfactor <- cut(VAstationselect$pH,c(-1,0,6,9,15,16)
                                ,labels=rev(c('High Risk to Aquatic Life','Medium Risk to Aquatic Life (-)','Low Risk to Aquatic Life','Medium Risk to Aquatic Life (+)'
                                              ,'No Risk to Aquatic Life'))) 
levels(VAstationselect$pHfactor) <- list('No Risk to Aquatic Life'='No Risk to Aquatic Life','Low Risk to Aquatic Life'='Low Risk to Aquatic Life','Medium Risk to Aquatic Life'=c('Medium Risk to Aquatic Life (-)','Medium Risk to Aquatic Life (+)'),'High Risk to Aquatic Life'='High Risk to Aquatic Life')
VAstationselect$SpCondfactor <- cut(VAstationselect$SpCond,c(0,250,350,500,4000)
                                    ,labels=c('No Risk to Aquatic Life','Low Risk to Aquatic Life','Medium Risk to Aquatic Life','High Risk to Aquatic Life')) 
VAstationselect$TPfactor <- cut(VAstationselect$TP,c(0,0.02,0.05,0.1,5)
                                ,labels=c('No Risk to Aquatic Life','Low Risk to Aquatic Life','Medium Risk to Aquatic Life','High Risk to Aquatic Life')) 
VAstationselect$TNfactor <- cut(VAstationselect$TN,c(0,0.5,1,2,100)
                                ,labels=c('No Risk to Aquatic Life','Low Risk to Aquatic Life','Medium Risk to Aquatic Life','High Risk to Aquatic Life')) 
VAstationselect$TotHabfactor <- cut(VAstationselect$TotHab,c(0,100,130,150,200)
                                    ,labels=c('High Risk to Aquatic Life','Medium Risk to Aquatic Life','Low Risk to Aquatic Life','No Risk to Aquatic Life')) 
# trick it into level order needed for pal
levels(VAstationselect$TotHabfactor) <- list('No Risk to Aquatic Life'='No Risk to Aquatic Life','Low Risk to Aquatic Life'='Low Risk to Aquatic Life','Medium Risk to Aquatic Life'='Medium Risk to Aquatic Life ','High Risk to Aquatic Life'='High Risk to Aquatic Life')

VAstationselect$TDS_Vfactor <- cut(VAstationselect$TDS_V,c(0,100,250,350,50000)
                                ,labels=c('No Risk to Aquatic Life','Low Risk to Aquatic Life','Medium Risk to Aquatic Life','High Risk to Aquatic Life')) 
VAstationselect$MetalCCUfactor <- cut(VAstationselect$MetalCCU,c(0,0.75,1.5,2.0,50)
                                   ,labels=c('No Risk to Aquatic Life','Low Risk to Aquatic Life','Medium Risk to Aquatic Life','High Risk to Aquatic Life')) 
# trick it to have 4 levels for color pal purposes
VAstationselect$LRBSfactor <- cut(VAstationselect$LRBS,c(-20,-1.5,-1.0,-0.5,0.5,15)
                                ,labels=rev(c('High Risk to Aquatic Life','Medium Risk to Aquatic Life (-)','Low Risk to Aquatic Life','Medium Risk to Aquatic Life (+)'
                                              ,'No Risk to Aquatic Life'))) 
levels(VAstationselect$LRBSfactor) <- list('No Risk to Aquatic Life'='No Risk to Aquatic Life','Low Risk to Aquatic Life'='Low Risk to Aquatic Life','Medium Risk to Aquatic Life'=c('Medium Risk to Aquatic Life (-)','Medium Risk to Aquatic Life (+)'),'High Risk to Aquatic Life'='High Risk to Aquatic Life')
VAstationselect$NA_Vfactor <- cut(VAstationselect$NA_V,c(0,7,10,20,500)
                                      ,labels=c('No Risk to Aquatic Life','Low Risk to Aquatic Life','Medium Risk to Aquatic Life','High Risk to Aquatic Life')) 
VAstationselect$K_Vfactor <- cut(VAstationselect$K_V,c(0,1,2,10,500)
                                  ,labels=c('No Risk to Aquatic Life','Low Risk to Aquatic Life','Medium Risk to Aquatic Life','High Risk to Aquatic Life')) 
VAstationselect$Cl_Vfactor <- cut(VAstationselect$Cl_V,c(0,10,25,50,500)
                                  ,labels=c('No Risk to Aquatic Life','Low Risk to Aquatic Life','Medium Risk to Aquatic Life','High Risk to Aquatic Life')) 
VAstationselect$Sf_Vfactor <- cut(VAstationselect$Sf_V,c(0,10,25,75,500)
                                  ,labels=c('No Risk to Aquatic Life','Low Risk to Aquatic Life','Medium Risk to Aquatic Life','High Risk to Aquatic Life')) 
VAstationselect$VSCIfactor <- cut(VAstationselect$VSCIAll,c(0,42,60,72,115)
                                  ,labels=c('High Risk to Aquatic Life','Medium Risk to Aquatic Life','Low Risk to Aquatic Life','No Risk to Aquatic Life')) 


# double check all factors came in correctly
str(VAstationselect)

## 2) work on palette colors for genaric factor levels
# fake df with correct factor levels
colOptions <- data.frame(stressLevels=as.factor(c("No Risk to Aquatic Life","Low Risk to Aquatic Life","Medium Risk to Aquatic Life","High Risk to Aquatic Life")))
pal <- colorFactor(c("blue","limegreen","yellow","red"),levels=colOptions$stressLevels, ordered=T)

parameterList <- as.factor(c('VSCIAll','DO','pH','SpCond','TP','TN','TotHab',"TDS_V","MetalCCU","LRBS","NA_V","K_V","Cl_V","Sf_V"))

saveRDS(VAstationselect,'data/VAstationselect_final.RDS')

# Make new dat4 dataset
VAstationselect_ <- mutate(VAstationselect,VSCI=VSCIAll)%>%
  select(-c(Year,VSCI,DO,pH,SpCond,TP,TN,TotHab,TDS_V,MetalCCU,LRBS,NA_V,K_V,Cl_V,Sf_V))%>%
  reshape2::melt(id=c('sampleID',"Longitude","Latitude","Basin" ,"VSCIAll"))%>%
  filter(!is.na(value))%>%
  mutate(joincolumn=gsub("(.*)f.*","\\1",variable))

# do the same with regular data so it iwll be available for a popup in the final leaflet map
VAstationselect_1 <- mutate(VAstationselect,VSCI=VSCIAll)%>%
  select(c(sampleID,Longitude,Latitude,Basin,VSCIAll,VSCI,DO,pH,SpCond,TP,TN,TotHab,TDS_V,
           MetalCCU,LRBS,NA_V,K_V,Cl_V,Sf_V))%>%
  reshape2::melt(id=c('sampleID',"Longitude","Latitude","Basin" ,"VSCIAll"))%>%
  filter(!is.na(value))%>%mutate(joincolumn=variable)%>%
  merge(VAstationselect_,by=c('joincolumn','sampleID',"Longitude","Latitude","Basin" ,"VSCIAll"))%>%
  rename(Parameter=variable.x,ParameterMeasure=value.x,ParameterFactor=variable.y,ParameterFactorLevel=value.y)%>%
  select(-joincolumn)%>%
  mutate(units=Parameter)# make column with units for popups
VAstationselect_1$units <- dplyr::recode(VAstationselect_1$units,"VSCI"="(unitless)","DO"="mg/L","pH"="(unitless)","SpCond"="uS/cm","TP"="mg/L","TN"="mg/L","TotHab"="(unitless)",
                                         "TDS_V"="mg/L","MetalCCU"="(unitless)","LRBS"="(unitless)","NA_V"="mg/L","K_V"="mg/L","Cl_V"="mg/L",
                                         "Sf_V"="mg/L")
  


saveRDS(VAstationselect_1,'data/dat4_final.RDS')

  

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
# Work up new Dissolved Metals data
metalsCDF <- read_excel("data/DissMetalStatus.CDF.xlsx",sheet="biostatus.CDF")%>%
  select(-c(Type))%>%
  filter(!(Subpopulation=='Subpopulation'|is.na(Subpopulation)))
cdfdata2 <- rbind(cdfdata2,metalsCDF)

# Add units to cdfdata final
cdfdata2 <- mutate(cdfdata,units=Indicator)
cdfdata2$units <- dplyr::recode(cdfdata2$units,"DChloride"="mg/L","DO"="mg/L","DPotassium"="mg/L","DSodium"="mg/L","DSulfate"="mg/L",
                                "LRBS"="(unitless)", "MetalsCCU"="(unitless)","pH"="(unitless)","SpCond"="uS/cm","TDS"="mg/L",
                                "TN"="mg/L","TotalHabitat"="(unitless)","TP"="mg/L","ANTIMONY"="ug/L","ALUMINUM"="ug/L",
                                "ARSENIC"="ug/L","BARIUM"="ug/L","BERY"="ug/L","CADMIUM"="ug/L","CALCUIM"="mg/L","CHROMIUM"="ug/L",
                                "COPPER"="ug/L","IRON"="ug/L","LEAD"="ug/L","MAGN"="mg/L","MANGANESE"="ug/L","NICKEL"="ug/L",
                                "SELENIUM"="ug/L","SILVER"="ug/L","THALLIUM"="ug/L","ZINC"="ug/L","HARDNESS"="mg/L")    
cdfdata2$Indicator <- dplyr::recode(cdfdata2$Indicator,'CALCUIM'='CALCIUM','BERY'='BERYLLIUM','MAGN'='MAGNESIUM')
saveRDS(cdfdata2,'data/cdfdataFINAL.RDS')                               


# Work up new Dissolved Metals data
metalsCDF <- read_excel("data/DissMetalStatus.CDF.xlsx",sheet="biostatus.CDF")%>%select(-c(Type))
saveRDS(metalsCDF,'data/metalsCDF.RDS')

# Now for station specific data, make these into factor classes for leaflet map
## 1) work on creating factor levels for select variables
metalsStations <- read_excel("data/DissMetalStatus.CDF.xlsx",sheet="Data")[c(1:535,538),]%>%
  select(c(sampleID,Longitude-DD,Latitude-DD,Basin))
# pause on this for now