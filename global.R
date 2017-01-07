library(shiny)
library(leaflet)
library(rgdal)
library(ggplot2)
library(plyr)
library(dplyr)
library(DT)

# Local data locations
VAstationselect <- readRDS('data/VAstationselect2.RDS')
dat4 <- readRDS('data/dat4_2.RDS')
cdfdata <- readRDS('data/AllcdfData.RDS')
template <- read.csv('data/template.csv')

# Color breaks and table formatting
brkspH <- c(0,6,9)
clrspH <- c("gray","yellow","limegreen","yellow")
brksDO <- c(0,7,8,10)
clrsDO <- c("gray","red","yellow","limegreen","blue")
brksTN <- c(0,0.5,1,2)
clrsTN <- c("gray","blue","limegreen","yellow","red")
brksTP <- c(0,0.02,0.05,0.1)
clrsTP <- c("gray","blue","limegreen","yellow","red")
brksTotHab <- c(0,100,130,150)
clrsTotHab <- c("gray","red","yellow","limegreen","blue")
brksLRBS <- c(-3,-1.5,-1,-0.5,0.5)
clrsLRBS <- c("gray","red","yellow","limegreen","blue","yellow")
brksMCCU <- c(0,0.75,1.5,2.0)
clrsMCCU <- c("gray","blue","limegreen","yellow","red")
brksSpCond <- c(0,250,350,500)
clrsSpCond <- c("gray","blue","limegreen","yellow","red")
brksTDS <- c(0,100,250,350)
clrsTDS <- c("gray","blue","limegreen","yellow","red")
brksDS <- c(0,10,25,75)
clrsDS <- c("gray","blue","limegreen","yellow","red")
brksDChl <- c(0,10,25,50)
clrsDChl <- c("gray","blue","limegreen","yellow","red")
brksDK <- c(0,1,2,10)
clrsDK <- c("gray","blue","limegreen","yellow","red")
brksDNa <- c(0,7,10,20)
clrsDNa <- c("gray","blue","limegreen","yellow","red")



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
