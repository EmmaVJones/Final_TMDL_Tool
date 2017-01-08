# Upload GIS data here to avoid uploading it twice (if it were in the global.R file)
#eco2 <- readOGR('C:/HardDriveBackup/R/ShinyApp_TMDLworkgroup','vaECOREGIONlevel3__proj84')
#supaB2 <- readOGR('C:/HardDriveBackup/R/ShinyApp_TMDLworkgroup','VAsuperbasins_proj84')



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
      formatStyle("Dsodium", backgroundColor = styleInterval(brksDNa, clrsDNa))
    })
  
  # Table with Yearly Stats and percentile lookup
  percentiles <- reactive({
  if(is.null(stats()))
    return(NULL)
  test <- percentileTable(stats(),"DO",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID))
  return(test)
  })

     
  
  output$DOtable <- DT::renderDataTable({percentiles()})
  
})

