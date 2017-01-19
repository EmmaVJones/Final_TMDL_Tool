# Upload GIS data here to avoid uploading it twice (if it were in the global.R file)
#eco2 <- readOGR('C:/HardDriveBackup/R/ShinyApp_TMDLworkgroup','vaECOREGIONlevel3__proj84')
#supaB2 <- readOGR('C:/HardDriveBackup/R/ShinyApp_TMDLworkgroup','VAsuperbasins_proj84')

options(DT.options = list(dom = 't'))

shinyServer(function(input, output, session) {
  ## Data Upload Tab, bring in chemistry/field data
  # Download data template
  output$downloadTemplate <- downloadHandler(filename=function(){'template.csv'},
                                             content=function(file){write.csv(template,file)})
  
  # Upload taxa list
  inputFile <- reactive({inFile <- input$siteData
  if(is.null(inFile))
    return(NULL)
  read.csv(inFile$datapath)
  })
  output$inputTable <- DT::renderDataTable({inputFile()})
  
  # Calculate statistics on input table
  stats <- reactive({inFile <- input$siteData
  if(is.null(inFile))
    return(NULL)
  datamean <- select(template,-c(StationID,CollectionDateTime))%>%
    summarise_each(funs(mean(., na.rm = TRUE)))%>%mutate(Statistic="Average")
  datamedian <- select(template,-c(StationID,CollectionDateTime))%>%
    summarise_each(funs(median(., na.rm = TRUE)))%>%mutate(Statistic="Median")
  data_all <- rbind(datamean,datamedian)%>%select(Statistic,everything())
  return(data_all)
  })
  output$summaryStats <- renderTable(stats())
 
  # Output Colored datatable
  output$colors <- DT::renderDataTable({
    datatable(stats()) %>% formatStyle("pH", backgroundColor = styleInterval(brkspH, clrspH))%>%
      formatStyle("DO", backgroundColor = styleInterval(brksDO, clrsDO)) %>%
      formatStyle("TN", backgroundColor = styleInterval(brksTN, clrsTN))%>%
      formatStyle("TP", backgroundColor = styleInterval(brksTP, clrsTP))%>%
      formatStyle("TotalHabitat", backgroundColor = styleInterval(brksTotHab, clrsTotHab))%>%
      formatStyle("LRBS", backgroundColor = styleInterval(brksLRBS, clrsLRBS))%>%
      formatStyle("MetalsCCU", backgroundColor = styleInterval(brksMCCU, clrsMCCU))%>%
      formatStyle("SpCond", backgroundColor = styleInterval(brksSpCond, clrsSpCond))%>%
      formatStyle("TDS", backgroundColor = styleInterval(brksTDS, clrsTDS))%>%
      formatStyle("DSulfate", backgroundColor = styleInterval(brksDS, clrsDS))%>%
      formatStyle("DChloride", backgroundColor = styleInterval(brksDChl, clrsDChl))%>%
      formatStyle("DPotassium", backgroundColor = styleInterval(brksDK, clrsDK))%>%
      formatStyle("DSodium", backgroundColor = styleInterval(brksDNa, clrsDNa))
    })
  
  # Output Colored Risk datatable
  output$riskTableInfo <- DT::renderDataTable({
    datatable(risk,colnames=c('Risk Category'),rownames=FALSE) %>% 
      formatStyle('Risk_Category',backgroundColor=styleEqual(brksrisk,clrsrisk))
  })
  
  # Table with Yearly Stats and percentile lookup
  percentilespH <- reactive({
    if(is.null(stats()))
      return(NULL)
    return(percentileTable(stats(),"pH",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID)))
  })
  percentilesDO <- reactive({
  if(is.null(stats()))
    return(NULL)
  return(percentileTable(stats(),"DO",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID)))
    })
  percentilesTN <- reactive({
    if(is.null(stats()))
      return(NULL)
    return(percentileTable(stats(),"TN",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID)))
  })
  percentilesTP <- reactive({
    if(is.null(stats()))
      return(NULL)
    return(percentileTable(stats(),"TP",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID)))
  })
  percentilesTotalHabitat <- reactive({
    if(is.null(stats()))
      return(NULL)
    return(percentileTable(stats(),"TotalHabitat",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID)))
  })
  percentilesLRBS <- reactive({
    if(is.null(stats()))
      return(NULL)
    return(percentileTable(stats(),"LRBS",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID)))
  })
  percentilesMetalsCCU <- reactive({
    if(is.null(stats()))
      return(NULL)
    return(percentileTable(stats(),"MetalsCCU",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID)))
  })
  percentilesSpCond <- reactive({
    if(is.null(stats()))
      return(NULL)
    return(percentileTable(stats(),"SpCond",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID)))
  })
  percentilesTDS <- reactive({
    if(is.null(stats()))
      return(NULL)
    return(percentileTable(stats(),"TDS",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID)))
  })
  percentilesDSulfate <- reactive({
    if(is.null(stats()))
      return(NULL)
    return(percentileTable(stats(),"DSulfate",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID)))
  })
  percentilesDChloride <- reactive({
    if(is.null(stats()))
      return(NULL)
    return(percentileTable(stats(),"DChloride",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID)))
  })
  percentilesDPotassium <- reactive({
    if(is.null(stats()))
      return(NULL)
    return(percentileTable(stats(),"DPotassium",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID)))
  })
  percentilesDSodium <- reactive({
    if(is.null(stats()))
      return(NULL)
    return(percentileTable(stats(),"DSodium",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID)))
  })

  # pH Summary Page   
  output$pHtable_Site <- DT::renderDataTable({datatable(percentilespH()[1,],colnames = c('StationID','Average (unitless)','Median (unitless)'),rownames = F)%>%
      formatStyle(c("Average","Median"), backgroundColor = styleInterval(brkspH, clrspH))})
  output$pHtable <- DT::renderDataTable({datatable(percentilespH()[2:5,],colnames=c('Subpopulation','Average Percentile','Median Percentile'),rowname=F)})
  output$riskTablepH <- DT::renderDataTable({
    datatable(data.frame(Risk_Category=c('Medium Risk to Aquatic Life','Low Risk to Aquatic Life','Medium Risk to Aquatic Life'),pH=c("< 6","6 - 9","> 9")),
              colnames=c('Risk Category','pH (unitless)'),rownames = F) %>%#,options=list(columnDefs=list(list(targets=3,visible=F)))
      formatStyle('Risk_Category',target='row',backgroundColor=styleEqual(c('Medium Risk to Aquatic Life','Low Risk to Aquatic Life'),c('yellow','limegreen')))})
  output$pHdataset <- renderUI({
    selectInput("pHdataset_","Select Dataset to Plot",percentilespH()$Statistic[2:5])
  })
  output$pHplot_ <- renderUI({
    plotOutput("p_pH")
  })
  output$p_pH <- renderPlot({
    if(is.null(input$pHdataset_))
      return(NULL)
    cdfsubset <- subFunction(cdfdata,"pH",input$pHdataset_)
    avg1 <- as.numeric(subset(percentilespH(),Statistic==input$pHdataset_)[2])
    avg <- subFunction2(cdfsubset,avg1)
    med1 <- as.numeric(subset(percentilespH(),Statistic==input$pHdataset_)[3])
    med <- subFunction2(cdfsubset,med1)
    p1 <- ggplot(cdfsubset, aes(x=Value,y=Estimate.P)) + geom_point() + labs(x="pH (unitless)",y="Percentile") +
      ggtitle(paste(input$pHdataset_," pH Percentile Graph",sep="")) + 
      theme(plot.title = element_text(hjust=0.5,face='bold',size=15)) +
      theme(axis.title = element_text(face='bold',size=12))
    p1+ geom_point(data=avg,color='orange',size=4) + geom_text(data=avg,label='Average',hjust=1.2) +
      geom_point(data=med,color='gray',size=4)+ geom_text(data=med,label='Median',hjust=1.2) 
  })

  # Dissolved Oxygen Summary Page
  output$DOtable_Site <- DT::renderDataTable({datatable(percentilesDO()[1,],colnames = c('StationID','Average (mg/L)','Median (mg/L)'),rownames = F)%>%
      formatStyle(c("Average","Median"), backgroundColor = styleInterval(brksDO, clrsDO))})
  output$DOtable <- DT::renderDataTable({datatable(percentilesDO()[2:5,],colnames=c('Subpopulation','Average Percentile','Median Percentile'),rowname=F)})
  output$riskTableDO <- DT::renderDataTable({
    datatable(cbind(risk,DO=c('< 7','> 7, < 8','> 8, < 10','> 10')),colnames=c('Risk Category','DO (mg/L)'),rownames = F)%>%
      formatStyle('Risk_Category',target='row',backgroundColor=styleEqual(riskHightoLow,clrsDO[2:5]))})
  output$DOdataset <- renderUI({
    selectInput("DOdataset_","Select Dataset to Plot",percentilesDO()$Statistic[2:5])
  })
  output$DOplot_ <- renderUI({
    plotOutput("p_DO")
  })
  output$p_DO <- renderPlot({
    if(is.null(input$DOdataset_))
      return(NULL)
    cdfsubset <- subFunction(cdfdata,"DO",input$DOdataset_)
    avg1 <- as.numeric(subset(percentilesDO(),Statistic==input$DOdataset_)[2])
    avg <- subFunction2(cdfsubset,avg1)
    med1 <- as.numeric(subset(percentilesDO(),Statistic==input$DOdataset_)[3])
    med <- subFunction2(cdfsubset,med1)
    p1 <- ggplot(cdfsubset, aes(x=Value,y=Estimate.P)) + geom_point() + labs(x="Dissolved Oxygen (mg/L)",y="Percentile") +
      ggtitle(paste(input$DOdataset_," Dissolved Oxygen Percentile Graph",sep="")) + 
      theme(plot.title = element_text(hjust=0.5,face='bold',size=15)) +
      theme(axis.title = element_text(face='bold',size=12))
    p1+ geom_point(data=avg,color='orange',size=4) + geom_text(data=avg,label='Average',hjust=1.2) +
      geom_point(data=med,color='gray',size=4)+ geom_text(data=med,label='Median',hjust=1.2) 
  })
  
  # Total Nitrogen Summary Page
  output$TNtable_Site <- DT::renderDataTable({datatable(percentilesTN()[1,],colnames = c('StationID','Average (mg/L)','Median (mg/L)'),rownames = F)%>%
      formatStyle(c("Average","Median"), backgroundColor = styleInterval(brksTN, clrsTN))})
  output$TNtable <- DT::renderDataTable({datatable(percentilesTN()[2:5,],colnames=c('Subpopulation','Average Percentile','Median Percentile'),rowname=F)})
  output$riskTableTN <- DT::renderDataTable({
    datatable(cbind(risk,TN=c('> 2','> 1, < 2','> 0.5, < 1','< 0.5')),colnames=c('Risk Category','Total Nitrogen (mg/L)'),rownames = F)%>%
      formatStyle('Risk_Category',target='row',backgroundColor=styleEqual(riskHightoLow,clrsTN[5:2]))})# reverse colors
  output$TNdataset <- renderUI({
    selectInput("TNdataset_","Select Dataset to Plot",percentilesTN()$Statistic[2:5])
  })
  output$TNplot_ <- renderUI({
    plotOutput("p_TN")
  })
  output$p_TN <- renderPlot({
    if(is.null(input$TNdataset_))
      return(NULL)
    cdfsubset <- subFunction(cdfdata,"TN",input$TNdataset_)
    avg1 <- as.numeric(subset(percentilesTN(),Statistic==input$TNdataset_)[2])
    avg <- subFunction2(cdfsubset,avg1)
    med1 <- as.numeric(subset(percentilesTN(),Statistic==input$TNdataset_)[3])
    med <- subFunction2(cdfsubset,med1)
    p1 <- ggplot(cdfsubset, aes(x=Value,y=Estimate.P)) + geom_point() + labs(x="Total Nitrogen (mg/L)",y="Percentile") +
      ggtitle(paste(input$TNdataset_," Total Nitrogen Percentile Graph",sep="")) + 
      theme(plot.title = element_text(hjust=0.5,face='bold',size=15)) +
      theme(axis.title = element_text(face='bold',size=12))
    p1+ geom_point(data=avg,color='orange',size=4) + geom_text(data=avg,label='Average',hjust=1.2) +
      geom_point(data=med,color='gray',size=4)+ geom_text(data=med,label='Median',hjust=1.2) 
  })
  
  # Total Phosphorus Summary Page
  output$TPtable_Site <- DT::renderDataTable({datatable(percentilesTP()[1,],colnames = c('StationID','Average (mg/L)','Median (mg/L)'),rownames = F)%>%
      formatStyle(c("Average","Median"), backgroundColor = styleInterval(brksTP, clrsTP))})
  output$TPtable <- DT::renderDataTable({datatable(percentilesTP()[2:5,],colnames=c('Subpopulation','Average Percentile','Median Percentile'),rowname=F)})
  output$riskTableTP <- DT::renderDataTable({
    datatable(cbind(risk,TP=c('> 0.1','> 0.05, < 0.1','> 0.02, < 0.05','< 0.02')),colnames=c('Risk Category','Total Phosphorus (mg/L)'),rownames = F)%>%
      formatStyle('Risk_Category',target='row',backgroundColor=styleEqual(riskHightoLow,clrsDO[2:5]))})
  output$TPdataset <- renderUI({
    selectInput("TPdataset_","Select Dataset to Plot",percentilesTP()$Statistic[2:5])
  })
  output$TPplot_ <- renderUI({
    plotOutput("p_TP")
  })
  output$p_TP <- renderPlot({
    if(is.null(input$TPdataset_))
      return(NULL)
    cdfsubset <- subFunction(cdfdata,"TP",input$TPdataset_)
    avg1 <- as.numeric(subset(percentilesTP(),Statistic==input$TPdataset_)[2])
    avg <- subFunction2(cdfsubset,avg1)
    med1 <- as.numeric(subset(percentilesTP(),Statistic==input$TPdataset_)[3])
    med <- subFunction2(cdfsubset,med1)
    p1 <- ggplot(cdfsubset, aes(x=Value,y=Estimate.P)) + geom_point() + labs(x="Total Phosphorus (mg/L)",y="Percentile") +
      ggtitle(paste(input$TPdataset_," Total Phosphorus Percentile Graph",sep="")) + 
      theme(plot.title = element_text(hjust=0.5,face='bold',size=15)) +
      theme(axis.title = element_text(face='bold',size=12))
    p1+ geom_point(data=avg,color='orange',size=4) + geom_text(data=avg,label='Average',hjust=1.2) +
      geom_point(data=med,color='gray',size=4)+ geom_text(data=med,label='Median',hjust=1.2) 
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$TotalHabitattable <- DT::renderDataTable({percentilesTotalHabitat()})
  output$riskTableTotalHabitat <- DT::renderDataTable({
    datatable(cbind(risk,TotalHabitat=c('< 100','> 100, < 130','> 130, < 150','> 150')),colnames=c('Risk Category','Total Habitat (unitless)'),rownames = F)%>%
      formatStyle('Risk_Category',target='row',backgroundColor=styleEqual(riskHightoLow,clrsDO[2:5]))})
  output$LRBStable <- DT::renderDataTable({percentilesLRBS()})
  output$riskTableLRBS <- DT::renderDataTable({
    datatable(cbind(rbind(risk,'Medium Risk to Aquatic Life'),LRBS=c('< -1.5','> -1.5, < -1.0','> -0.5, < -1.0','> -0.5, < 0.5','> 0.5')),colnames=c('Risk Category','Relative Bed Stability (unitless)'),rownames = F)%>%
      formatStyle('Risk_Category',target='row',backgroundColor=styleEqual(c(riskHightoLow,'Medium Risk to Aquatic Life'),clrsLRBS[2:6]))})
  output$MetalsCCUtable <- DT::renderDataTable({percentilesMetalsCCU()})
  output$riskTableMetalsCCU <- DT::renderDataTable({
    datatable(cbind(risk,MetalsCCU=c('> 2.0','> 1.5, < 2.0','> 0.75, < 1.5','< 0.75')),colnames=c('Risk Category','Metals CCU (unitless)'),rownames = F)%>%
      formatStyle('Risk_Category',target='row',backgroundColor=styleEqual(riskHightoLow,clrsDO[2:5]))})
  output$SpCondtable <- DT::renderDataTable({percentilesSpCond()})
  output$riskTableSpCond <- DT::renderDataTable({
    datatable(cbind(risk,SpCond=c('> 500','> 350, < 500','> 250, < 350','< 250')),colnames=c('Risk Category','Specific Conductivity (uS/cm)'),rownames = F)%>%
      formatStyle('Risk_Category',target='row',backgroundColor=styleEqual(riskHightoLow,clrsDO[2:5]))})
  output$TDStable <- DT::renderDataTable({percentilesTDS()})
  output$riskTableTDS <- DT::renderDataTable({
    datatable(cbind(risk,TDS=c('> 350','> 250, < 350','> 100, < 250','< 100')),colnames=c('Risk Category','Total Dissolved Solids (mg/L)'),rownames = F)%>%
      formatStyle('Risk_Category',target='row',backgroundColor=styleEqual(riskHightoLow,clrsDO[2:5]))})
  output$DSulfatetable <- DT::renderDataTable({percentilesDSulfate()})
  output$riskTableDSulfate <- DT::renderDataTable({
    datatable(cbind(risk,DSulfate=c('> 75','> 25, < 75','> 10, < 25','< 10')),colnames=c('Risk Category','Dissolved Sulfate (mg/L)'),rownames = F)%>%
      formatStyle('Risk_Category',target='row',backgroundColor=styleEqual(riskHightoLow,clrsDO[2:5]))})
  output$DChloridetable <- DT::renderDataTable({percentilesDChloride()})
  output$riskTableDChloride <- DT::renderDataTable({
    datatable(cbind(risk,DSulfate=c('> 50','> 25, < 50','> 10, < 25','< 10')),colnames=c('Risk Category','Dissolved Chloride (mg/L)'),rownames = F)%>%
      formatStyle('Risk_Category',target='row',backgroundColor=styleEqual(riskHightoLow,clrsDO[2:5]))})
  output$DPotassiumtable <- DT::renderDataTable({percentilesDPotassium()})
  output$riskTableDPotassium <- DT::renderDataTable({
    datatable(cbind(risk,DPotassium=c('> 10','> 2, < 10','> 1, < 2','< 1')),colnames=c('Risk Category','Dissolved Potassium (mg/L)'),rownames = F)%>%
      formatStyle('Risk_Category',target='row',backgroundColor=styleEqual(riskHightoLow,clrsDO[2:5]))})
  output$DSodiumtable <- DT::renderDataTable({percentilesDSodium()})
  output$riskTableDSodium <- DT::renderDataTable({
    datatable(cbind(risk,DSodium=c('> 20','> 10, < 20','> 7, < 10','< 7')),colnames=c('Risk Category','Dissolved Sodium (mg/L)'),rownames = F)%>%
      formatStyle('Risk_Category',target='row',backgroundColor=styleEqual(riskHightoLow,clrsDO[2:5]))})
})

#output$test <- reactive({
#  if(is.null(percentilesDO()))
#    return(NULL)
#  input$DOdataset_})
#percentilesDO()$Statistic})
#as.numeric(subset(percentilesDO(),Statistic==input$DOdataset_)[3])})
#x1 <- subset(percentilesDO(),Statistic==input$DOdataset_)
#return(x1)})#%>%select(Average)})


#output$DOplot <- renderPlot({
#  cdfsubset <- subFunction(cdfdata,"DO","Virginia")
#  avg <- subFunction2(cdfsubset,percentilesDO()$Average[2])
#  med <- subFunction2(cdfsubset,percentilesDO()$Median[2])
#  p1 <- ggplot(cdfsubset, aes(x=Value,y=Estimate.P)) + geom_point() + labs(x="Dissolved Oxygen (mg/L)",y="Percentile")
#  p1+ geom_point(data=avg,color='orange',size=4) + geom_text(data=avg,label='Average',hjust=1.2) +
#    geom_point(data=med,color='gray',size=4)+ geom_text(data=med,label='Median',hjust=1.2) 
#})
