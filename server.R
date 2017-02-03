# Upload GIS data here to avoid uploading it twice (if it were in the global.R file)
eco2 <- readOGR('data/','vaECOREGIONlevel3__proj84')
supaB2 <- readOGR('data/','VAsuperbasins_proj84')

options(DT.options = list(dom = 't'))

shinyServer(function(input, output, session) {
  ## Data Upload Tab, bring in chemistry/field data
  # Download data template
  output$downloadTemplate <- downloadHandler(filename=function(){'template.csv'},
                                             content=function(file){write.csv(template,file,row.names=FALSE)})
  
  # Upload site data
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
  datamean <- select(inputFile(),-c(StationID,CollectionDateTime,Longitude,Latitude))%>%
    summarise_each(funs(mean(., na.rm = TRUE)))%>%mutate(Statistic="Average")
  datamedian <- select(inputFile(),-c(StationID,CollectionDateTime,Longitude,Latitude))%>%
    summarise_each(funs(median(., na.rm = TRUE)))%>%mutate(Statistic="Median")
  data_all <- rbind(datamean,datamedian)%>%select(Statistic,everything())
  return(data_all)
  })
  
  stats_wGIS <- reactive({
    if(is.null(stats()))
      return(data.frame(StationID="test",Longitude=45.55,Latitude=0))
    data_GIS <- reshape2::melt(stats(),c("Statistic"))%>%
      dcast(variable~Statistic)%>%
      mutate(StationID=unique(inputFile()$StationID),
                       Longitude=unique(inputFile()$Longitude),
                       Latitude=unique(inputFile()$Latitude))
    return(data_GIS)
    # dont know why I need to run all this extra code, debug later
  })
  
  output$summaryStats <- renderTable(stats())
 
 
  # Metals CCU Calculation
  mCCU <- reactive({
    if(is.null(input$Hardness&input$Arsenic&input$Chromium&input$Copper&input$Lead&input$Nickel&input$Zinc))
      return(NULL)
    return(metalsCCUcalc(input$Hardness,input$Arsenic,input$Chromium,input$Copper,input$Lead,input$Nickel,input$Zinc))
  })
  
  output$metalsTable <- renderTable({
    if(is.null(mCCU))#|is.null(input$StationID&input$SampleDate))
      return(data.frame(StationID=input$StationID,SampleDate=input$SampleDate,MetalsCCU=NA))
    return(data.frame(StationID=input$StationID,SampleDate=input$SampleDate,MetalsCCU=mCCU()))
  },  digits=4,border=TRUE)
  output$test <- renderText({class(input$SampleDate)})
  
  # Output Colored datatable
  output$colors <- DT::renderDataTable({
    if(is.null(stats()))
      return(NULL)
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
  
  # Total Habitat Summary Page
  output$TotalHabitattable_Site <- DT::renderDataTable({datatable(percentilesTotalHabitat()[1,],colnames = c('StationID','Average (unitless)','Median (unitless)'),rownames = F)%>%
      formatStyle(c("Average","Median"), backgroundColor = styleInterval(brksTotHab, clrsTotHab))})
  output$TotalHabitattable <- DT::renderDataTable({datatable(percentilesTotalHabitat()[2:5,],colnames=c('Subpopulation','Average Percentile','Median Percentile'),rowname=F)})
  output$riskTableTotalHabitat <- DT::renderDataTable({
    datatable(cbind(risk,TotalHabitat=c('< 100','> 100, < 130','> 130, < 150','> 150')),colnames=c('Risk Category','Total Habitat (unitless)'),rownames = F)%>%
      formatStyle('Risk_Category',target='row',backgroundColor=styleEqual(riskHightoLow,clrsDO[2:5]))})
  output$TotalHabitatdataset <- renderUI({
    selectInput("TotalHabitatdataset_","Select Dataset to Plot",percentilesTotalHabitat()$Statistic[2:5])
  })
  output$TotalHabitatplot_ <- renderUI({
    plotOutput("p_TotalHabitat")
  })
  output$p_TotalHabitat <- renderPlot({
    if(is.null(input$TotalHabitatdataset_))
      return(NULL)
    cdfsubset <- subFunction(cdfdata,"TotalHabitat",input$TotalHabitatdataset_)
    avg1 <- as.numeric(subset(percentilesTotalHabitat(),Statistic==input$TotalHabitatdataset_)[2])
    avg <- subFunction2(cdfsubset,avg1)
    med1 <- as.numeric(subset(percentilesTotalHabitat(),Statistic==input$TotalHabitatdataset_)[3])
    med <- subFunction2(cdfsubset,med1)
    p1 <- ggplot(cdfsubset, aes(x=Value,y=Estimate.P)) + geom_point() + labs(x="Total Habitat (unitless)",y="Percentile") +
      ggtitle(paste(input$TotalHabitatdataset_," Total Habitat Percentile Graph",sep="")) + 
      theme(plot.title = element_text(hjust=0.5,face='bold',size=15)) +
      theme(axis.title = element_text(face='bold',size=12))
    p1+ geom_point(data=avg,color='orange',size=4) + geom_text(data=avg,label='Average',hjust=1.2) +
      geom_point(data=med,color='gray',size=4)+ geom_text(data=med,label='Median',hjust=1.2) 
  })
  
  # LRBS Summary Page
  output$LRBStable_Site <- DT::renderDataTable({datatable(percentilesLRBS()[1,],colnames = c('StationID','Average (unitless)','Median (unitless)'),rownames = F)%>%
      formatStyle(c("Average","Median"), backgroundColor = styleInterval(brksLRBS, clrsLRBS))})
  output$LRBStable <- DT::renderDataTable({datatable(percentilesLRBS()[2:5,],colnames=c('Subpopulation','Average Percentile','Median Percentile'),rowname=F)})
  output$riskTableLRBS <- DT::renderDataTable({
    datatable(cbind(rbind(risk,'Medium Risk to Aquatic Life'),LRBS=c('< -1.5','> -1.5, < -1.0','> -0.5, < -1.0','> -0.5, < 0.5','> 0.5')),colnames=c('Risk Category','Relative Bed Stability (unitless)'),rownames = F)%>%
      formatStyle('Risk_Category',target='row',backgroundColor=styleEqual(c(riskHightoLow,'Medium Risk to Aquatic Life'),clrsLRBS[2:6]))})
  output$LRBSdataset <- renderUI({
    selectInput("LRBSdataset_","Select Dataset to Plot",percentilesLRBS()$Statistic[2:5])
  })
  output$LRBSplot_ <- renderUI({
    plotOutput("p_LRBS")
  })
  output$p_LRBS <- renderPlot({
    if(is.null(input$LRBSdataset_))
      return(NULL)
    cdfsubset <- subFunction(cdfdata,"LRBS",input$LRBSdataset_)
    avg1 <- as.numeric(subset(percentilesLRBS(),Statistic==input$LRBSdataset_)[2])
    avg <- subFunction2(cdfsubset,avg1)
    med1 <- as.numeric(subset(percentilesLRBS(),Statistic==input$LRBSdataset_)[3])
    med <- subFunction2(cdfsubset,med1)
    p1 <- ggplot(cdfsubset, aes(x=Value,y=Estimate.P)) + geom_point() + labs(x="LRBS (unitless)",y="Percentile") +
      ggtitle(paste(input$LRBSdataset_," LRBS Percentile Graph",sep="")) + 
      theme(plot.title = element_text(hjust=0.5,face='bold',size=15)) +
      theme(axis.title = element_text(face='bold',size=12))
    p1+ geom_point(data=avg,color='orange',size=4) + geom_text(data=avg,label='Average',hjust=1.2) +
      geom_point(data=med,color='gray',size=4)+ geom_text(data=med,label='Median',hjust=1.2) 
  })
  
  # Metals CCU Summary Page
  output$MetalsCCUtable_Site <- DT::renderDataTable({datatable(percentilesMetalsCCU()[1,],colnames = c('StationID','Average (unitless)','Median (unitless)'),rownames = F)%>%
      formatStyle(c("Average","Median"), backgroundColor = styleInterval(brksMCCU, clrsMCCU))})
  output$MetalsCCUtable <- DT::renderDataTable({datatable(percentilesMetalsCCU()[2:5,],colnames=c('Subpopulation','Average Percentile','Median Percentile'),rowname=F)})
  output$riskTableMetalsCCU <- DT::renderDataTable({
    datatable(cbind(risk,MetalsCCU=c('> 2.0','> 1.5, < 2.0','> 0.75, < 1.5','< 0.75')),colnames=c('Risk Category','Metals CCU (unitless)'),rownames = F)%>%
      formatStyle('Risk_Category',target='row',backgroundColor=styleEqual(riskHightoLow,clrsDO[2:5]))})
  output$MetalsCCUdataset <- renderUI({
    selectInput("MetalsCCUdataset_","Select Dataset to Plot",percentilesMetalsCCU()$Statistic[2:5])
  })
  output$MetalsCCUplot_ <- renderUI({
    plotOutput("p_MetalsCCU")
  })
  output$p_MetalsCCU <- renderPlot({
    if(is.null(input$MetalsCCUdataset_))
      return(NULL)
    cdfsubset <- subFunction(cdfdata,"MetalsCCU",input$MetalsCCUdataset_)
    avg1 <- as.numeric(subset(percentilesMetalsCCU(),Statistic==input$MetalsCCUdataset_)[2])
    avg <- subFunction2(cdfsubset,avg1)
    med1 <- as.numeric(subset(percentilesMetalsCCU(),Statistic==input$MetalsCCUdataset_)[3])
    med <- subFunction2(cdfsubset,med1)
    p1 <- ggplot(cdfsubset, aes(x=Value,y=Estimate.P)) + geom_point() + labs(x="Metals CCU (unitless)",y="Percentile") +
      ggtitle(paste(input$MetalsCCUdataset_," Metals CCU Percentile Graph",sep="")) + 
      theme(plot.title = element_text(hjust=0.5,face='bold',size=15)) +
      theme(axis.title = element_text(face='bold',size=12))
    p1+ geom_point(data=avg,color='orange',size=4) + geom_text(data=avg,label='Average',hjust=1.2) +
      geom_point(data=med,color='gray',size=4)+ geom_text(data=med,label='Median',hjust=1.2) 
  })
  
  # Specific Conductivity Summary Page
  output$SpCondtable_Site <- DT::renderDataTable({datatable(percentilesSpCond()[1,],colnames = c('StationID','Average (unitless)','Median (unitless)'),rownames = F)%>%
      formatStyle(c("Average","Median"), backgroundColor = styleInterval(brksSpCond, clrsSpCond))})
  output$SpCondtable <- DT::renderDataTable({datatable(percentilesSpCond()[2:5,],colnames=c('Subpopulation','Average Percentile','Median Percentile'),rowname=F)})
  output$riskTableSpCond <- DT::renderDataTable({
    datatable(cbind(risk,SpCond=c('> 500','> 350, < 500','> 250, < 350','< 250')),colnames=c('Risk Category','Specific Conductivity (uS/cm)'),rownames = F)%>%
      formatStyle('Risk_Category',target='row',backgroundColor=styleEqual(riskHightoLow,clrsDO[2:5]))})
  output$SpConddataset <- renderUI({
    selectInput("SpConddataset_","Select Dataset to Plot",percentilesSpCond()$Statistic[2:5])
  })
  output$SpCondplot_ <- renderUI({
    plotOutput("p_SpCond")
  })
  output$p_SpCond <- renderPlot({
    if(is.null(input$SpConddataset_))
      return(NULL)
    cdfsubset <- subFunction(cdfdata,"SpCond",input$SpConddataset_)
    avg1 <- as.numeric(subset(percentilesSpCond(),Statistic==input$SpConddataset_)[2])
    avg <- subFunction2(cdfsubset,avg1)
    med1 <- as.numeric(subset(percentilesSpCond(),Statistic==input$SpConddataset_)[3])
    med <- subFunction2(cdfsubset,med1)
    p1 <- ggplot(cdfsubset, aes(x=Value,y=Estimate.P)) + geom_point() + labs(x="Specific Conductivity (uS/cm)",y="Percentile") +
      ggtitle(paste(input$SpConddataset_," Specific Conductivity Percentile Graph",sep="")) + 
      theme(plot.title = element_text(hjust=0.5,face='bold',size=15)) +
      theme(axis.title = element_text(face='bold',size=12))
    p1+ geom_point(data=avg,color='orange',size=4) + geom_text(data=avg,label='Average',hjust=1.2) +
      geom_point(data=med,color='gray',size=4)+ geom_text(data=med,label='Median',hjust=1.2) 
  })
   
  # TDS Summary Page
  output$TDStable_Site <- DT::renderDataTable({datatable(percentilesTDS()[1,],colnames = c('StationID','Average (mg/L)','Median (mg/L)'),rownames = F)%>%
      formatStyle(c("Average","Median"), backgroundColor = styleInterval(brksTDS, clrsTDS))})
  output$TDStable <- DT::renderDataTable({datatable(percentilesTDS()[2:5,],colnames=c('Subpopulation','Average Percentile','Median Percentile'),rowname=F)})
  output$riskTableTDS <- DT::renderDataTable({
    datatable(cbind(risk,TDS=c('> 350','> 250, < 350','> 100, < 250','< 100')),colnames=c('Risk Category','Total Dissolved Solids (mg/L)'),rownames = F)%>%
      formatStyle('Risk_Category',target='row',backgroundColor=styleEqual(riskHightoLow,clrsDO[2:5]))})
  output$TDSdataset <- renderUI({
    selectInput("TDSdataset_","Select Dataset to Plot",percentilesTDS()$Statistic[2:5])
  })
  output$TDSplot_ <- renderUI({
    plotOutput("p_TDS")
  })
  output$p_TDS <- renderPlot({
    if(is.null(input$TDSdataset_))
      return(NULL)
    cdfsubset <- subFunction(cdfdata,"TDS",input$TDSdataset_)
    avg1 <- as.numeric(subset(percentilesTDS(),Statistic==input$TDSdataset_)[2])
    avg <- subFunction2(cdfsubset,avg1)
    med1 <- as.numeric(subset(percentilesTDS(),Statistic==input$TDSdataset_)[3])
    med <- subFunction2(cdfsubset,med1)
    p1 <- ggplot(cdfsubset, aes(x=Value,y=Estimate.P)) + geom_point() + labs(x="Total Dissolved Solids (mg/L)",y="Percentile") +
      ggtitle(paste(input$TDSdataset_," Total Dissolved Solids Percentile Graph",sep="")) + 
      theme(plot.title = element_text(hjust=0.5,face='bold',size=15)) +
      theme(axis.title = element_text(face='bold',size=12))
    p1+ geom_point(data=avg,color='orange',size=4) + geom_text(data=avg,label='Average',hjust=1.2) +
      geom_point(data=med,color='gray',size=4)+ geom_text(data=med,label='Median',hjust=1.2) 
  })
  
  # Dissolved Sulfate Summary Page
  output$DSulfatetable_Site <- DT::renderDataTable({datatable(percentilesDSulfate()[1,],colnames = c('StationID','Average (mg/L)','Median (mg/L)'),rownames = F)%>%
      formatStyle(c("Average","Median"), backgroundColor = styleInterval(brksDChl, clrsDChl))})
  output$DSulfatetable <- DT::renderDataTable({datatable(percentilesDSulfate()[2:5,],colnames=c('Subpopulation','Average Percentile','Median Percentile'),rowname=F)})
  output$riskTableDSulfate <- DT::renderDataTable({
    datatable(cbind(risk,DSulfate=c('> 75','> 25, < 75','> 10, < 25','< 10')),colnames=c('Risk Category','Dissolved Sulfate (mg/L)'),rownames = F)%>%
      formatStyle('Risk_Category',target='row',backgroundColor=styleEqual(riskHightoLow,clrsDO[2:5]))})
  output$DSulfatedataset <- renderUI({
    selectInput("DSulfatedataset_","Select Dataset to Plot",percentilesDSulfate()$Statistic[2:5])
  })
  output$DSulfateplot_ <- renderUI({
    plotOutput("p_DSulfate")
  })
  output$p_DSulfate <- renderPlot({
    if(is.null(input$DSulfatedataset_))
      return(NULL)
    cdfsubset <- subFunction(cdfdata,"DSulfate",input$DSulfatedataset_)
    avg1 <- as.numeric(subset(percentilesDSulfate(),Statistic==input$DSulfatedataset_)[2])
    avg <- subFunction2(cdfsubset,avg1)
    med1 <- as.numeric(subset(percentilesDSulfate(),Statistic==input$DSulfatedataset_)[3])
    med <- subFunction2(cdfsubset,med1)
    p1 <- ggplot(cdfsubset, aes(x=Value,y=Estimate.P)) + geom_point() + labs(x="Dissolved Sulfate (mg/L)",y="Percentile") +
      ggtitle(paste(input$DSulfatedataset_," Dissolved Sulfate Percentile Graph",sep="")) + 
      theme(plot.title = element_text(hjust=0.5,face='bold',size=15)) +
      theme(axis.title = element_text(face='bold',size=12))
    p1+ geom_point(data=avg,color='orange',size=4) + geom_text(data=avg,label='Average',hjust=1.2) +
      geom_point(data=med,color='gray',size=4)+ geom_text(data=med,label='Median',hjust=1.2) 
  })
  
  # Dissolved Chloride Summary Page
  output$DChloridetable_Site <- DT::renderDataTable({datatable(percentilesDChloride()[1,],colnames = c('StationID','Average (mg/L)','Median (mg/L)'),rownames = F)%>%
      formatStyle(c("Average","Median"), backgroundColor = styleInterval(brksDS, clrsDS))})
  output$DChloridetable <- DT::renderDataTable({datatable(percentilesDChloride()[2:5,],colnames=c('Subpopulation','Average Percentile','Median Percentile'),rowname=F)})
  output$riskTableDChloride <- DT::renderDataTable({
    datatable(cbind(risk,DChloride=c('> 50','> 25, < 50','> 10, < 25','< 10')),colnames=c('Risk Category','Dissolved Chloride (mg/L)'),rownames = F)%>%
      formatStyle('Risk_Category',target='row',backgroundColor=styleEqual(riskHightoLow,clrsDO[2:5]))})
  output$DChloridedataset <- renderUI({
    selectInput("DChloridedataset_","Select Dataset to Plot",percentilesDChloride()$Statistic[2:5])
  })
  output$DChlorideplot_ <- renderUI({
    plotOutput("p_DChloride")
  })
  output$p_DChloride <- renderPlot({
    if(is.null(input$DChloridedataset_))
      return(NULL)
    cdfsubset <- subFunction(cdfdata,"DChloride",input$DChloridedataset_)
    avg1 <- as.numeric(subset(percentilesDChloride(),Statistic==input$DChloridedataset_)[2])
    avg <- subFunction2(cdfsubset,avg1)
    med1 <- as.numeric(subset(percentilesDChloride(),Statistic==input$DChloridedataset_)[3])
    med <- subFunction2(cdfsubset,med1)
    p1 <- ggplot(cdfsubset, aes(x=Value,y=Estimate.P)) + geom_point() + labs(x="Dissolved Chloride (mg/L)",y="Percentile") +
      ggtitle(paste(input$DChloridedataset_," Dissolved Chloride Percentile Graph",sep="")) + 
      theme(plot.title = element_text(hjust=0.5,face='bold',size=15)) +
      theme(axis.title = element_text(face='bold',size=12))
    p1+ geom_point(data=avg,color='orange',size=4) + geom_text(data=avg,label='Average',hjust=1.2) +
      geom_point(data=med,color='gray',size=4)+ geom_text(data=med,label='Median',hjust=1.2) 
  })
  
  # Dissolved Potassium Summary Page
  output$DPotassiumtable_Site <- DT::renderDataTable({datatable(percentilesDPotassium()[1,],colnames = c('StationID','Average (mg/L)','Median (mg/L)'),rownames = F)%>%
      formatStyle(c("Average","Median"), backgroundColor = styleInterval(brksDK, clrsDK))})
  output$DPotassiumtable <- DT::renderDataTable({datatable(percentilesDPotassium()[2:5,],colnames=c('Subpopulation','Average Percentile','Median Percentile'),rowname=F)})
  output$riskTableDPotassium <- DT::renderDataTable({
    datatable(cbind(risk,DPotassium=c('> 10','> 2, < 10','> 1, < 2','< 1')),colnames=c('Risk Category','Dissolved Potassium (mg/L)'),rownames = F)%>%
      formatStyle('Risk_Category',target='row',backgroundColor=styleEqual(riskHightoLow,clrsDO[2:5]))})
  output$DPotassiumdataset <- renderUI({
    selectInput("DPotassiumdataset_","Select Dataset to Plot",percentilesDPotassium()$Statistic[2:5])
  })
  output$DPotassiumplot_ <- renderUI({
    plotOutput("p_DPotassium")
  })
  output$p_DPotassium <- renderPlot({
    if(is.null(input$DPotassiumdataset_))
      return(NULL)
    cdfsubset <- subFunction(cdfdata,"DPotassium",input$DPotassiumdataset_)
    avg1 <- as.numeric(subset(percentilesDPotassium(),Statistic==input$DPotassiumdataset_)[2])
    avg <- subFunction2(cdfsubset,avg1)
    med1 <- as.numeric(subset(percentilesDPotassium(),Statistic==input$DPotassiumdataset_)[3])
    med <- subFunction2(cdfsubset,med1)
    p1 <- ggplot(cdfsubset, aes(x=Value,y=Estimate.P)) + geom_point() + labs(x="Dissolved Potassium (mg/L)",y="Percentile") +
      ggtitle(paste(input$DPotassiumdataset_," Dissolved Potassium Percentile Graph",sep="")) + 
      theme(plot.title = element_text(hjust=0.5,face='bold',size=15)) +
      theme(axis.title = element_text(face='bold',size=12))
    p1+ geom_point(data=avg,color='orange',size=4) + geom_text(data=avg,label='Average',hjust=1.2) +
      geom_point(data=med,color='gray',size=4)+ geom_text(data=med,label='Median',hjust=1.2) 
  })
  
  # Dissolved Sodium Summary Page
  output$DSodiumtable_Site <- DT::renderDataTable({datatable(percentilesDSodium()[1,],colnames = c('StationID','Average (mg/L)','Median (mg/L)'),rownames = F)%>%
      formatStyle(c("Average","Median"), backgroundColor = styleInterval(brksDNa, clrsDNa))})
  output$DSodiumtable <- DT::renderDataTable({datatable(percentilesDSodium()[2:5,],colnames=c('Subpopulation','Average Percentile','Median Percentile'),rowname=F)})
  output$riskTableDSodium <- DT::renderDataTable({
    datatable(cbind(risk,DSodium=c('> 20','> 10, < 20','> 7, < 10','< 7')),colnames=c('Risk Category','Dissolved Sodium (mg/L)'),rownames = F)%>%
      formatStyle('Risk_Category',target='row',backgroundColor=styleEqual(riskHightoLow,clrsDO[2:5]))})
  output$DSodiumdataset <- renderUI({
    selectInput("DSodiumdataset_","Select Dataset to Plot",percentilesDSodium()$Statistic[2:5])
  })
  output$DSodiumplot_ <- renderUI({
    plotOutput("p_DSodium")
  })
  output$p_DSodium <- renderPlot({
    if(is.null(input$DSodiumdataset_))
      return(NULL)
    cdfsubset <- subFunction(cdfdata,"DSodium",input$DSodiumdataset_)
    avg1 <- as.numeric(subset(percentilesDSodium(),Statistic==input$DSodiumdataset_)[2])
    avg <- subFunction2(cdfsubset,avg1)
    med1 <- as.numeric(subset(percentilesDSodium(),Statistic==input$DSodiumdataset_)[3])
    med <- subFunction2(cdfsubset,med1)
    p1 <- ggplot(cdfsubset, aes(x=Value,y=Estimate.P)) + geom_point() + labs(x="Dissolved Sodium (mg/L)",y="Percentile") +
      ggtitle(paste(input$DSodiumdataset_," Dissolved Sodium Percentile Graph",sep="")) + 
      theme(plot.title = element_text(hjust=0.5,face='bold',size=15)) +
      theme(axis.title = element_text(face='bold',size=12))
    p1+ geom_point(data=avg,color='orange',size=4) + geom_text(data=avg,label='Average',hjust=1.2) +
      geom_point(data=med,color='gray',size=4)+ geom_text(data=med,label='Median',hjust=1.2) 
  })
  
  ## Map
  colOptions <- data.frame(stressLevels=as.factor(c("No Risk to Aquatic Life","Low Risk to Aquatic Life","Medium Risk to Aquatic Life","High Risk to Aquatic Life")))
  pal <- colorFactor(c("blue","limegreen","yellow","red"),levels=colOptions$stressLevels, ordered=T)
  # First interpret user selection to enable filtering)
  dataSelect <- reactive({switch(input$parameterToPlot
                                 ,'VSCI'='VSCIfactor','Dissolved Oxygen'='DOfactor'
                                 ,'pH'='pHfactor','Specific Conductivity'='SpCondfactor'
                                 ,'Total Phosphorus'='TPfactor','Total Nitrogen'='TNfactor'
                                 ,'Total Habitat'='TotHabfactor','Total Dissolved Solids'='TDS_Vfactor'
                                 ,'Metals CCU'='MetalCCUfactor','LRBS'='LRBSfactor','Dissolved Sodium'='NA_Vfactor'
                                 ,'Dissolved Potassium'='K_Vfactor','Dissolved Chloride'='Cl_Vfactor','Dissolved Sulfate'='Sf_Vfactor')})
  dataSelectcdf <- reactive({switch(input$parameterToPlot
                                    ,'Dissolved Oxygen'='DO','pH'='pH','Specific Conductivity'='SpCond'
                                    ,'Total Phosphorus'='TP','Total Nitrogen'='TN','Total Habitat'='TotalHabitat'
                                    ,'LRBS'='LRBS','Metals CCU'='MetalsCCU','Total Dissolved Solids'='TDS'
                                    ,'Dissolved Sulfate'='DSulfate','Dissolved Chloride'='DChloride'
                                    ,'Dissovled Potassium'='DPotassium','Dissolved Sodium'='DSodium')})
  filteredData <- reactive({
    df2 <- subset(dat4,ParameterFactor==dataSelect())
  })
  
  output$VAmap <- renderLeaflet({
    leaflet(VAstationselect) %>% addProviderTiles('Thunderforest.Outdoors') %>%
      fitBounds(~min(Longitude),~min(Latitude)
                ,~max(Longitude),~max(Latitude))
  })
  
  observe({if(input$showUserSite==TRUE){
    #if(!is.null(stats_wGIS())){
    leafletProxy('VAmap') %>%
      addMarkers(data=stats_wGIS()[1,],~Longitude,~Latitude,
                 popup=paste(sep= "<br/>",strong('User Input Site'),
                             paste(strong("StationID: "),stats_wGIS()$StationID)))}
  })
  
  output$colors2 <- DT::renderDataTable({
    if(is.null(stats()))
      return(NULL)
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
  
  # Update map markers when user changes parameter
  observe({
    leafletProxy('VAmap',data=filteredData()) %>% clearMarkers() %>%
      addCircleMarkers(color=~pal(ParameterFactorLevel),fillOpacity=1,stroke=FALSE
                       ,popup=paste(sep = "<br/>",paste(strong("StationID: "),filteredData()$sampleID,sep="")
                                           ,paste(strong("VSCI Score: "),prettyNum(filteredData()$VSCIAll,digits=3),sep="")
                                           ,paste(strong(filteredData()$Parameter[1]),": ",
                                                  prettyNum(filteredData()$ParameterMeasure,digits=4)," ",
                                                  filteredData()$units,sep="")))})
  
  # Plot Ecoregion Shapefile
  observe({if(input$eco==TRUE){
    leafletProxy('VAmap') %>% 
      addPolygons(data=eco2, fill = 0.4, stroke = 0.2, color = 'gray',group = "US_L3NAME"
                  ,popup=paste('Ecoregion: ',eco2@data$US_L3NAME))
  }else(leafletProxy('VAmap')%>%clearGroup('US_L3NAME'))})
  
  # Plot Basin shapefile
  observe({if(input$basins==TRUE){
    leafletProxy('VAmap') %>% 
      addPolygons(data=supaB2,fill=0.4,stroke=0.2,color='brown',group='SUPERBASIN'
                  ,popup=paste('Superbasin: ',supaB2@data$SUPERBASIN))
  }else(leafletProxy('VAmap')%>%clearGroup('SUPERBASIN'))})
  
  # Separate observe() to recreate legend as needed
  observe({
    proxy <- leafletProxy('VAmap',data=filteredData())
    proxy %>% clearControls() %>%
      addLegend("bottomright",pal=pal, values=levels(as.factor(filteredData()$ParameterFactorLevel)),title="Benthic TMDL Stressor Thresholds")})
  
  filteredDatacdf <- reactive({
    df <- subset(cdfdata,Indicator==dataSelectcdf() & Subpopulation=="Virginia")})
  
  output$Statewidecdf <- renderPlot({if(input$showcdf==T){
    if(input$parameterToPlot!='VSCI'){
      p <- ggplot(filteredDatacdf(), aes(Value,Estimate.P))+
        labs(list(title=paste("Statewide",input$parameterToPlot,"\nPercentile Graph")
                  ,x=paste(input$parameterToPlot,filteredDatacdf()$units,sep=" ")
                  ,y='Statewide Percentile'))
      p+geom_point()+geom_line(aes(y=LCB95Pct.P),colour='gray')+
        geom_line(aes(y=UCB95Pct.P),colour='gray')
      }}})
  
  
  
  
  
  
  # Dissolved Metals Section
  # Download data template
  output$downloadTemplate_metals <- downloadHandler(filename=function(){'template_metals.csv'},
                                             content=function(file){write.csv(template_metals,file,row.names=FALSE)})
  
  
  
  # Upload site data
  inputFile_metals <- reactive({inFile2 <- input$siteData_metals
  if(is.null(inFile2))
    return(NULL)
  read.csv(inFile2$datapath)})
  output$inputTable_metals <- DT::renderDataTable({inputFile_metals()})
 
  # Calculate Metals CCU on input table
  metalsCCU_results <- reactive({inFile2 <- input$siteData_metals
  if(is.null(inputFile_metals()))
    return(NULL)
  calc <- metalsCCUcalcDF(inputFile_metals())%>%
    mutate(StationID=inputFile_metals()$StationID)%>%
    select(StationID,MetalsCCU)
  return(calc)})  
   
  # Metal CCU table 
  output$summary_MetalsCCU <- renderTable({
    if(is.null(metalsCCU_results()))
      return(NULL)
    return(metalsCCU_results())},digits=4,border=TRUE)

  output$metalsSitesUI <- renderUI({inFile2 <- input$siteData_metals
  if(is.null(inputFile_metals()))
    return(NULL)
    selectInput("metalsSites_", "Select Site to Review", inputFile_metals()[,1] )
  })
 
  # Dissolved Metals Lookup Functions
  percentilesDissolvedMetals <- reactive({
    if(is.null(metalsCCU_results()))
      return(NULL)
    Calcium <- percentileTable_metals(inputFile_metals(),'Calcium',input$metalsSites_)
    Magnesium <- percentileTable_metals(inputFile_metals(),'Magnesium',input$metalsSites_)
    Arsenic <- percentileTable_metals(inputFile_metals(),'Arsenic',input$metalsSites_)
    Barium <- percentileTable_metals(inputFile_metals(),'Barium',input$metalsSites_)
    Beryllium <- percentileTable_metals(inputFile_metals(),'Beryllium',input$metalsSites_)
    Cadmium <- percentileTable_metals(inputFile_metals(),'Cadmium',input$metalsSites_)
    Chromium <- percentileTable_metals(inputFile_metals(),'Chromium',input$metalsSites_)
    Copper <- percentileTable_metals(inputFile_metals(),'Copper',input$metalsSites_)
    Iron <- percentileTable_metals(inputFile_metals(),'Iron',input$metalsSites_)
    Lead <- percentileTable_metals(inputFile_metals(),'Lead',input$metalsSites_)
    Manganese <- percentileTable_metals(inputFile_metals(),'Manganese',input$metalsSites_)
    Thallium <- percentileTable_metals(inputFile_metals(),'Thallium',input$metalsSites_)
    Nickel <- percentileTable_metals(inputFile_metals(),'Nickel',input$metalsSites_)
    Silver <- percentileTable_metals(inputFile_metals(),'Silver',input$metalsSites_)
    Zinc <- percentileTable_metals(inputFile_metals(),'Zinc',input$metalsSites_)
    Antimony <- percentileTable_metals(inputFile_metals(),'Antimony',input$metalsSites_)
    Aluminum <- percentileTable_metals(inputFile_metals(),'Aluminum',input$metalsSites_)
    Selenium <- percentileTable_metals(inputFile_metals(),'Selenium',input$metalsSites_)
    Hardness <- percentileTable_metals(inputFile_metals(),'Hardness',input$metalsSites_)
    final <- rbind(Calcium,Magnesium,Arsenic,Barium,Beryllium,Cadmium,Chromium,Copper,Iron,
                   Lead,Manganese,Thallium,Nickel,Silver,Zinc,Antimony,Aluminum,Selenium,Hardness)
    return(final)
  })
  
  output$colors_metals <- DT::renderDataTable({
    if(is.null(inputFile_metals()))
      return(NULL)
    datatable(percentilesDissolvedMetals(),options=list(pageLength=20))})
 
  # Choose Dissolved Metal to display
  output$dMetal <- renderUI({
    selectInput("dMetal_","Select Dissolved Metal to Plot",names(inputFile_metals()[5:23]))
  })
  output$dMetalplot_ <- renderUI({
    plotOutput("p_dMetal")
  })
  
  # Updating dissolved metals cdf plot
  output$p_dMetal <- renderPlot({
    if(is.null(inputFile_metals))
      return(NULL)
    parametercap <- toupper(input$dMetal_)
    cdfsubset <- subFunction(cdfdata,parametercap,"Virginia")
    pct1 <- cbind(percentilesDissolvedMetals(),metal=sub(" .*","",percentilesDissolvedMetals()$Dissolved_Metal))%>%
      filter(metal==input$dMetal_)
    pct <- subFunction2(cdfsubset,pct1[,3])
    p1 <- ggplot(cdfsubset, aes(x=Value,y=Estimate.P)) + geom_point() + labs(x=as.character(pct1[1,1]),y="Percentile") +
        ggtitle(paste("Virginia ",input$dMetal_," Percentile Graph")) + 
        theme(plot.title = element_text(hjust=0.5,face='bold',size=15)) +
        theme(axis.title = element_text(face='bold',size=12))
    p1+ geom_point(data=pct,color='orange',size=4)
    
  },height = 250,width=325)
 
  # In case need to test cdf output
  
  
  #output$test <- renderTable({
  #  if(is.null(inputFile_metals))
  #    return(NULL)
  #  parametercap <- toupper(input$dMetal_)
  #  cdfsubset <- subFunction(cdfdata,parametercap,"Virginia")
  #  pct1 <- mutate(percentilesDissolvedMetals(),metal=sub(" .*","",final$Dissolved_Metal))%>%
  #    filter(metal==input$dMetal_)
  #  pct <- subFunction2(cdfsubset,pct1[,3])
  #  return(pct[,3:8])
  #})
  
})