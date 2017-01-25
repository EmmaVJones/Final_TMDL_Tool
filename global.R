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
VAstationselect <- readRDS('data/VAstationselect_final.RDS')
dat4 <- readRDS('data/dat4_final.RDS')
cdfdata <- readRDS('data/cdfdataFINAL.RDS')
template <- read.csv('data/templateGIS.csv')
template_metals <- read.csv('data/template_metals.csv')
metalsCDF <- readRDS('data/metalsCDF.RDS')

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
riskHightoLow <- c('High Risk to Aquatic Life','Medium Risk to Aquatic Life','Low Risk to Aquatic Life','No Risk to Aquatic Life')
risk <- data.frame(Risk_Category=c('High Risk to Aquatic Life','Medium Risk to Aquatic Life','Low Risk to Aquatic Life','No Risk to Aquatic Life'))
brksrisk <- c('High Risk to Aquatic Life','Medium Risk to Aquatic Life','Low Risk to Aquatic Life','No Risk to Aquatic Life')
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
  sub <- filter(measurementTable,StationID==stationName)%>%select_("StationID",parameter)
  parametercap <- toupper(parameter)
  unit <- filter(cdfdata,Subpopulation=="Virginia",Indicator==parametercap)
  va <- filter(cdfdata,Subpopulation=="Virginia",Indicator==parametercap)%>%select(Value,Estimate.P)
  final <- data.frame(Dissolved_Metal=paste(parameter," (",unit$units[1],")",sep=""),Measure=sub[,2],
                      Statewide_Percentile=vlookup(sub[,2],va,2,TRUE))
  return(final)
}

subFunction <- function(cdftable,parameter,userInput){
  return(filter(cdftable,Subpopulation==userInput & Indicator==parameter))
}

subFunction2 <- function(cdftable,userValue){
  return(filter(cdftable,Estimate.P==userValue))
}

# Metals CCU Calculation
metalsCCUcalc <- function(Hardness,Arsenic,Chromium,Copper,Lead,Nickel,Zinc){
  ArStand <- 150
  ChromStand <- (exp(0.819*(log(Hardness))+0.6848))*0.86
  CoStand <- (exp(0.8545*(log(Hardness))-1.702))*0.96
  LdStand <- (exp(1.273*(log(Hardness))-3.259))
  NiStand <- (exp(0.846*(log(Hardness))-0.884))*0.997
  ZnStand <- (exp(0.8473*(log(Hardness))+0.884))*0.986
  return(sum(Arsenic/ArStand,Chromium/ChromStand,Copper/CoStand,Lead/LdStand,Nickel/NiStand,Zinc/ZnStand))
}
# Metals CCU Calculation for dataframes
metalsCCUcalcDF <- function(df){
  Hardness <- df$Hardness
  Arsenic <- df$Arsenic
  Chromium <- df$Chromium
  Copper <- df$Copper
  Lead <- df$Lead
  Nickel <- df$Nickel 
  Zinc <- df$Zinc
  met <- data.frame(MetalsCCU=NA)
  for(i in 1:nrow(df)){
    met[i,] <- metalsCCUcalc(df$Hardness[i],df$Arsenic[i],df$Chromium[i],df$Copper[i],df$Lead[i],df$Nickel[i],df$Zinc[i])
  }
  return(met)
  #return(list(df$Hardness,df$Arsenic,df$Chromium,df$Copper,df$Lead,df$Nickel,df$Zinc))
}
