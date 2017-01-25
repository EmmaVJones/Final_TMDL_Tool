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
  percentilesDissolvedMetals()})




# Dissolved Metals Lookup Functions
percentilesCalcium <- reactive({
  if(is.null(metalsCCU_results()))
    return(NULL)
  return(percentileTable_metals(inputFile_metals(),'Calcium',input$metalsSites_))})
percentilesMagnesium <- reactive({
  if(is.null(metalsCCU_results()))
    return(NULL)
  return(percentileTable_metals(inputFile_metals(),'Magnesium',input$metalsSites_))})
percentilesArsenic <- reactive({
  if(is.null(metalsCCU_results()))
    return(NULL)
  return(percentileTable_metals(inputFile_metals(),'Arsenic',input$metalsSites_))})
percentilesBarium <- reactive({
  if(is.null(metalsCCU_results()))
    return(NULL)
  return(percentileTable_metals(inputFile_metals(),'Barium',input$metalsSites_))})
percentilesBeryllium <- reactive({
  if(is.null(metalsCCU_results()))
    return(NULL)
  return(percentileTable_metals(inputFile_metals(),'Beryllium',input$metalsSites_))})
percentilesCadmium <- reactive({
  if(is.null(metalsCCU_results()))
    return(NULL)
  return(percentileTable_metals(inputFile_metals(),'Cadmium',input$metalsSites_))})
percentilesChromium <- reactive({
  if(is.null(metalsCCU_results()))
    return(NULL)
  return(percentileTable_metals(inputFile_metals(),'Chromium',input$metalsSites_))})
percentilesCopper <- reactive({
  if(is.null(metalsCCU_results()))
    return(NULL)
  return(percentileTable_metals(inputFile_metals(),'Copper',input$metalsSites_))})
percentilesIron <- reactive({
  if(is.null(metalsCCU_results()))
    return(NULL)
  return(percentileTable_metals(inputFile_metals(),'Iron',input$metalsSites_))})
percentilesLead <- reactive({
  if(is.null(metalsCCU_results()))
    return(NULL)
  return(percentileTable_metals(inputFile_metals(),'Lead',input$metalsSites_))})
percentilesManganese <- reactive({
  if(is.null(metalsCCU_results()))
    return(NULL)
  return(percentileTable_metals(inputFile_metals(),'Manganese',input$metalsSites_))})
percentilesThallium <- reactive({
  if(is.null(metalsCCU_results()))
    return(NULL)
  return(percentileTable_metals(inputFile_metals(),'Thallium',input$metalsSites_))})
percentilesNickel <- reactive({
  if(is.null(metalsCCU_results()))
    return(NULL)
  return(percentileTable_metals(inputFile_metals(),'Nickel',input$metalsSites_))})
percentilesSilver <- reactive({
  if(is.null(metalsCCU_results()))
    return(NULL)
  return(percentileTable_metals(inputFile_metals(),'Silver',input$metalsSites_))})
percentilesZinc <- reactive({
  if(is.null(metalsCCU_results()))
    return(NULL)
  return(percentileTable_metals(inputFile_metals(),'Zinc',input$metalsSites_))})
percentilesAntimony <- reactive({
  if(is.null(metalsCCU_results()))
    return(NULL)
  return(percentileTable_metals(inputFile_metals(),'Antimony',input$metalsSites_))})
percentilesAluminum <- reactive({
  if(is.null(metalsCCU_results()))
    return(NULL)
  return(percentileTable_metals(inputFile_metals(),'Aluminum',input$metalsSites_))})
percentilesSelenium <- reactive({
  if(is.null(metalsCCU_results()))
    return(NULL)
  return(percentileTable_metals(inputFile_metals(),'Selenium',input$metalsSites_))})
percentilesHardness <- reactive({
  if(is.null(metalsCCU_results()))
    return(NULL)
  return(percentileTable_metals(inputFile_metals(),'Hardness',input$metalsSites_))})

final <- rbind(Calcium,Magnesium,Arsenic,Barium,Beryllium,Cadmium,Chromium,Copper,Iron,
                 Lead,Manganese,Thallium,Nickel,Silver,Zinc,Antimony,Aluminum,Selenium,Hardness)

output$colors_metals <- DT::renderDataTable({
  if(is.null(inputFile_metals()))
    return(NULL)
  percentilesDissolvedMetals()})
