library(shiny)
library(leaflet)
library(rgdal)
library(ggplot2)
library(plyr)
library(dplyr)
library(DT)
library(tidyr)
library(reshape2)

# Local data locations
VAstationselect <- readRDS('data/VAstationselectMarch2017update_final.RDS')
virginiaStations <- readRDS('data/VirginiaStations.RDS')
cdfdata <- readRDS('data/cdfdataMarch2017update_final.RDS')
template <- read.csv('data/templateGIS.csv')
template_metals <- read.csv('data/template_metals.csv')
metalsCDF <- readRDS('data/metalsCDF_March2017Update.RDS')

# Color breaks and table formatting
brkspH <- c(0,6,9)
clrspH <- c("gray","yellow","limegreen","yellow")
brksDO <- c(0,7,8,10)
clrsDO <- c("gray","red","yellow","limegreen","cornflowerblue")
brksTN <- c(0,0.5,1,2)
clrsTN <- c("gray","cornflowerblue","limegreen","yellow","red")
brksTP <- c(0,0.02,0.05,0.1)
clrsTP <- c("gray","cornflowerblue","limegreen","yellow","red")
brksTotHab <- c(0,100,130,150)
clrsTotHab <- c("gray","red","yellow","limegreen","cornflowerblue")
brksLRBS <- c(-3,-1.5,-1,-0.5,0.5)
clrsLRBS <- c("gray","red","yellow","limegreen","cornflowerblue","yellow")
brksMCCU <- c(0,0.75,1.5,2.0)
clrsMCCU <- c("gray","cornflowerblue","limegreen","yellow","red")
brksSpCond <- c(0,250,350,500)
clrsSpCond <- c("gray","cornflowerblue","limegreen","yellow","red")
brksTDS <- c(0,100,250,350)
clrsTDS <- c("gray","cornflowerblue","limegreen","yellow","red")
brksDS <- c(0,10,25,75)
clrsDS <- c("gray","cornflowerblue","limegreen","yellow","red")
brksDChl <- c(0,10,25,50)
clrsDChl <- c("gray","cornflowerblue","limegreen","yellow","red")
brksDK <- c(0,1,2,10)
clrsDK <- c("gray","cornflowerblue","limegreen","yellow","red")
brksDNa <- c(0,7,10,20)
clrsDNa <- c("gray","cornflowerblue","limegreen","yellow","red")

# Risk table
riskHightoLow <- c('High Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','No Probability of Stress to Aquatic Life')
risk <- data.frame(Risk_Category=c('High Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','No Probability of Stress to Aquatic Life'))
brksrisk <- c('High Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','No Probability of Stress to Aquatic Life')
clrsrisk <- c("red","yellow","limegreen","cornflowerblue")

# VLOOKUP (Excel function hack) by Julin Maloof
vlookup <- function(ref, #the value or values that you want to look for
                    table, #the table where you want to look for it; will look in first column
                    column, #the column that you want the return data to come from,
                    range=FALSE, #if there is not an exact match, return the closest?
                    larger=FALSE) #if doing a range lookup, should the smaller or larger key be used?)
{
  if(!is.numeric(column) & !column %in% colnames(table)) {
    stop(paste("can't find column",column,"in table"))
  }
  if(range) {
    if(!is.numeric(table[,1])) {
      stop(paste("The first column of table must be numeric when using range lookup"))
    }
    table <- table[order(table[,1]),] 
    index <- findInterval(ref,table[,1])
    if(larger) {
      index <- ifelse(ref %in% table[,1],index,index+1)
    }
    output <- table[index,column]
    output[!index <= dim(table)[1]] <- NA
    
  } else {
    output <- table[match(ref,table[,1]),column]
    output[!ref %in% table[,1]] <- NA #not needed?
  }
  dim(output) <- dim(ref)
  output
}


# Return percentile 
percentileTable <- function(statsTable,parameter,userBasin,userEco,userOrder,stationName){
  out <- statsTable%>%select_("Statistic",parameter)%>% spread_("Statistic",parameter)%>%
    mutate(Statistic=stationName)%>%select(Statistic,everything())
  va <- filter(cdfdata,Subpopulation=='Virginia',Indicator==parameter)%>%select(Value,Estimate.P)
  basin <- filter(cdfdata,Subpopulation==userBasin,Indicator==parameter)%>%select(Value,Estimate.P)
  eco <- filter(cdfdata,Subpopulation==userEco,Indicator==parameter)%>%select(Value,Estimate.P)
  order <- filter(cdfdata,Subpopulation==userOrder,Indicator==parameter)%>%select(Value,Estimate.P)
  va2 <- data.frame(Statistic='Virginia',Average=vlookup(out$Average,va,2,range=TRUE),Median=vlookup(out$Median,va,2,range=TRUE))
  basin2 <- data.frame(Statistic=userBasin,Average=vlookup(out$Average,basin,2,TRUE),Median=vlookup(out$Median,basin,2,TRUE))
  eco2 <- data.frame(Statistic=userEco,Average=vlookup(out$Average,eco,2,TRUE),Median=vlookup(out$Median,eco,2,TRUE))
  order2 <- data.frame(Statistic=userOrder,Average=vlookup(out$Average,order,2,TRUE),Median=vlookup(out$Median,order,2,TRUE))
  out_final <- rbind(out,va2,basin2,eco2,order2)
  return(out_final)
}

percentileTable_metals <- function(measurementTable,parameter,stationName){
  sub <- select_(measurementTable,"StationID",parameter)%>%filter(StationID%in%as.character(stationName))
  parametercap <- toupper(parameter)
  unit <- filter(cdfdata,Subpopulation=="Virginia",Indicator%in%parametercap)
  va <- filter(cdfdata,Subpopulation=="Virginia",Indicator%in%parametercap)%>%select(Value,Estimate.P)
  final <- data.frame(Dissolved_Metal=paste(parameter," (",unit$units[1],")",sep=""),Measure=sub[1,2],
                      Statewide_Percentile=formatC(vlookup(sub[1,2],va,2,TRUE),digits=3))
  return(final)
}

subFunction <- function(cdftable,parameter,userInput){
  return(filter(cdftable,Subpopulation%in%userInput & Indicator%in%parameter))
}

subFunction2 <- function(cdftable,userValue){
  return(filter(cdftable,Estimate.P%in%userValue))
}

# Metals Criterion Table
metalsCriteria <- function(Hardness){
  criteriaHardness <- ifelse(Hardness<25,25,ifelse(Hardness>400,400,Hardness))
  x <- data.frame(Calcium=NA,Magnesium=NA,Arsenic=150,Barium=NA,Beryllium=NA,
                  Cadmium=format((exp(0.7852*(log(criteriaHardness))-3.49)),digits=3), 
                  Chromium=format(((exp(0.819*(log(criteriaHardness))+0.6848))*0.86),digits=3),
                  Copper=format(((exp(0.8545*(log(criteriaHardness))-1.702))*0.96),digits=3),
                  Iron=1000,Lead=format(((exp(1.273*(log(criteriaHardness))-3.259))),digits=3),
                  Manganese=50,Thallium=0.24,Nickel=format(((exp(0.846*(log(criteriaHardness))-0.884))*0.997),digits=3),
                  Silver=format(((exp(1.72*(log(criteriaHardness))-6.52))*0.85),digits=3),
                  Zinc=format(((exp(0.8473*(log(criteriaHardness))+0.884))*0.986),digits=3),
                  Antimony=5.6,Aluminum=150,Selenium=5,Hardness=NA)
  return(as.data.frame(t(x)))}


# Metals CCU Calculation
metalsCCUcalc <- function(Hardness,Aluminum,Arsenic,Cadmium,Chromium,Copper,Lead,Nickel,Selenium,Zinc){
  criteriaHardness <- ifelse(Hardness<25,25,ifelse(Hardness>400,400,Hardness))
  AluminumEPAChronic <- Aluminum/150
  ArsenicChronic <- Arsenic/150
  CadmiumChronic <- Cadmium/(exp(0.7852*(log(criteriaHardness))-3.49))
  ChromiumChronic <- Chromium/((exp(0.819*(log(criteriaHardness))+0.6848))*0.86)
  CopperChronic <- Copper/((exp(0.8545*(log(criteriaHardness))-1.702))*0.96)
  LeadChronic <- Lead/((exp(1.273*(log(criteriaHardness))-3.259)))
  NickelChronic <- Nickel/((exp(0.846*(log(criteriaHardness))-0.884))*0.997)
  SeleniumChronic <- Selenium/5
  ZincChronic <- Zinc/((exp(0.8473*(log(criteriaHardness))+0.884))*0.986)
  return(sum(AluminumEPAChronic,ArsenicChronic,CadmiumChronic,ChromiumChronic,CopperChronic,LeadChronic,
             NickelChronic,SeleniumChronic,ZincChronic))
}

# Metals CCU Calculation for dataframes
metalsCCUcalcDF <- function(df){
  Hardness <- df$Hardness
  Aluminum <- df$Aluminum
  Arsenic <- df$Arsenic
  Cadmium <- df$Cadmium
  Chromium <- df$Chromium
  Copper <- df$Copper
  Lead <- df$Lead
  Nickel <- df$Nickel 
  Selenium <- df$Selenium
  Zinc <- df$Zinc
  met <- data.frame(MetalsCCU=NA)
  for(i in 1:nrow(df)){
    met[i,] <- format(metalsCCUcalc(df$Hardness[i],df$Aluminum[i],df$Arsenic[i],df$Cadmium[i],
                                    df$Chromium[i],df$Copper[i],df$Lead[i],df$Nickel[i],
                                    df$Selenium[i],df$Zinc[i]),digits=4)
  }
  return(met)
}
