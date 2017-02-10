source('global.R')


shinyUI(fluidPage(theme="slate.css",
                  navbarPage("VDEQ Benthic Stressor Analysis Tool",
                             tabPanel("About",fluidRow(column(10,
                                                              h4("This app was created to assist in the identification of benthic stressors 
                                                                 to aquatic communities."),
                                                              h5("By uploading field parameters and chemistry data from a given sample site, 
                                                                 users may compare their dataset to 10 years of Virginia Probabilistic Monitoring
                                                                 data by major river basin, stream order, and ecoregion."),
                                                              br(),
                                                              h5(strong("Authors:")),
                                                              h5("Jason Hill, Water Quality Assessment Team Leader"),h5("Chip Sparks, Regional Biologist"),
                                                              h5("Mary Dail, Water Quality Assessment Scientist"),h5("Emma Jones, Water Quality Assessment Data Analyst"),
                                                              br(),
                                                              h5(strong("Background:")),
                                                              p("The DEQ assesses aquatic invertebrate communities in order to evaluate whether or 
                                                                not Virginia’s aquatic life use standard is being met. To meet both state and federal 
                                                                requirements, DEQ assesses water quality data, including chemical and biological data, 
                                                                every two years and subsequently lists those waterbodies not meeting Virginia’s water 
                                                                quality standards on the 303(d) list. The root cause of an aquatic community shift is 
                                                                rarely obvious even though changes in the composition and/or abundance of the benthic 
                                                                macroinvertebrate community are distinct. DEQ utilizes EPA’s recommended method, stressor 
                                                                analysis, to systematically characterize the cause of an aquatic community shift. The goal
                                                                of the stressor analysis process is to apply a weight-of-evidence approach to define a/the 
                                                                most probable stressor(s) that explain(s) the shift in the benthic macroinvertebrate community. 
                                                                Aquatic community stressors encompass a wide array of parameters that have varying degrees of 
                                                                synergistic interactions, further complicating the stressor analysis process. Recognizing these
                                                                challenges, stressor thresholds were developed utilizing ten years of data collected through 
                                                                DEQ’s Freshwater Probabilistic Monitoring Program.  Stressor thresholds are concentration/measured 
                                                                ranges linked to varying levels of stress to aquatic life that present context for stressor 
                                                                analyses reviewers and developers to evaluate water quality datasets and relate them to aquatic 
                                                                community outcomes. Statewide, ecoregion-, basin-, and stream order-specific context is presented 
                                                                in relation to the following common aquatic stressors: dissolved oxygen, pH, total phosphorus, 
                                                                total nitrogen, ionic strength (specific conductivity, TDS, and dissolved sulfate, chloride, 
                                                                sodium, and potassium), dissolved metals cumulative criterion unit, total habitat and relative 
                                                                bed stability.  Specifically, thresholds ranging from no stress to aquatic life to high probability 
                                                                of stress aquatic life were developed and integrated into a web-based application for better 
                                                                understanding stressors."),
                                                              hr(),
                                                              h4(strong("To begin, simply navigate to the 'Upload Data' tab and follow the on screen 
                                                                 instructions."))))),
                             tabPanel("Data Upload",
                                      column(4,wellPanel(
                                        h4("Instructions:"),
                                        p("Please upload site chemistry and field parameter data as a flat file (.csv). 
                                          All data uploaded to the app must be formatted correctly. If you are unsure whether 
                                          your data is in the correct format, please download the 'template.csv' file first to 
                                          check your data structure."),
                                        downloadButton('downloadTemplate',"Download template.csv"),
                                        fileInput('siteData','Upload Site (flat file)',accept='.csv',width='100%'))),
                                      column(8,tabsetPanel(
                                        tabPanel("User Data",
                                                 h3("User Uploaded Data"),
                                                 h5("Please fill in the Basin, Ecoregion, and Stream Order fields appropriately."),
                                                 column(3,selectInput("Basin",label=h4("Basin"),
                                                                      c(" "="NA","James Basin"="James Basin",
                                                                        "Roanoke Basin"="Roanoke Basin",
                                                                        "Potomac-Shenandoah"="Potomac-Shenandoah",
                                                                        "Rappahannock-York"="Rappahannock-York",
                                                                        "Chowan Basin"="Chowan",
                                                                        "Tennessee Basin"="Tennessee",
                                                                        "New Basin"="New"),selected=1)),
                                                 column(4,selectInput("Ecoregion",label=h4("Ecoregion"),
                                                                      c(" "="NA","Piedmont"="Piedmont",
                                                                        "Northern Piedmont"="Northern Piedmont",
                                                                        "Central Appalachian Ridges and Valleys"="Central Appalachian Ridges and Valleys",
                                                                        "Southeastern Plains"="Southeastern Plains",
                                                                        "Blue Ridge Mountains"="Blue Ridge Mountains",
                                                                        "Central Appalachians"="Central Appalachians"),selected=1)),
                                                 column(5,selectInput("StreamOrder",label=h4("Stream Order"),
                                                                      c(" "="NA","First Order"="First Order",
                                                                        "Second Order"="Second Order","Third Order"="Third Order",
                                                                        "Fourth Order"="Fourth Order","Fifth Order"="Fifth Order"),selected=1)),
                                                 DT::dataTableOutput('inputTable'),
                                                 hr(),
                                                 column(6,h3("Summary Statistics"),
                                                 tableOutput("summaryStats"))),
                                        tabPanel("Metals CCU Analysis, Single Site",
                                                 h3("User Input Metals Data"),
                                                 helpText("Please use this form to calculate MetalsCCU data for the User Data tab if you
                                                          have not done so already. Input StationID, Date, and associated metals data then
                                                          scroll down to view MetalsCCU result.",
                                                          span(strong("If you have more than one site to analyze for MetalsCCU, use the 
                                                                      Dissolved Metals Tab at the top of the navigation pane."))),
                                                 column(6,textInput("StationID",h5("StationID"), value = "")),
                                                 column(6,textInput("SampleDate",h5("Sample Date (MM-DD-YYYY)"), value = "")),
                                                 column(6,
                                                        numericInput("Hardness",h5("Hardness"), value = ""),
                                                        numericInput("Arsenic",h5("Arsenic"), value = ""),
                                                        numericInput("Chromium",h5("Chromium"), value = ""),
                                                        numericInput("Copper",h5("Copper"), value = "")),
                                                 column(6,
                                                        numericInput("Lead",h5("Lead"), value = ""),
                                                        numericInput("Nickel",h5("Nickel"), value = ""),
                                                        numericInput("Zinc",h5("Zinc"), value = "")),
                                                 column(12,h3('MetalsCCU Result'),DT::dataTableOutput('metalsTable'),
                                                        helpText('You can copy/paste this value into your spreadsheet for upload
                                                                 back to this app or use it for additional analyses.')))))),
                             tabPanel("Data Summary",
                                      column(12,tabsetPanel(
                                        tabPanel("Composite Table",br(),br(),
                                                 h4('Composite Table'),helpText('You can export the table below as a .csv, .xlsx, or .pdf by clicking the corresponding
                                                          button below. The Copy button copies all table data for you to put into any spreadsheet 
                                                          program. If you want the color background formatting, you need to manually select the 
                                                          table with your cursor to copy all associated formatting to a spreadsheet program.'),
                                                 DT::dataTableOutput('colors'),
                                                 column(3,DT::dataTableOutput('riskTableInfo')),br(),br(),
                                                 column(4,
                                                        wellPanel(h4('Report Output:'),
                                                                    helpText('Click below to save a .HTML version of all the tables and graphics associated with 
                                                                             the input station. You can save this to a .pdf after initial HTML conversion 
                                                                             (File -> Print -> Save as PDF).'),
                                                   downloadButton('report','Generate Report')))),
                                        # pH Summary
                                        tabPanel("pH Summary",br(),br(),h4('pH Summary'),
                                                 DT::dataTableOutput('pHtable_Site'),
                                                 DT::dataTableOutput('pHtable'),br(),
                                                 column(6,DT::dataTableOutput('riskTablepH')),
                                                 column(6,uiOutput("pHdataset"), uiOutput("pHplot_"),plotOutput('pHplot'))),
                                        # DO Summary
                                        tabPanel("DO Summary",br(),br(),h4('DO Summary'),
                                                 DT::dataTableOutput('DOtable_Site'),
                                                 DT::dataTableOutput('DOtable'),br(),
                                                 column(6,DT::dataTableOutput('riskTableDO')),
                                                 column(6,uiOutput("DOdataset"), uiOutput("DOplot_"),plotOutput('DOplot'))),
                                        # TN Summary
                                        tabPanel("TN Summary",br(),br(),h4('TN Summary'),
                                                 DT::dataTableOutput('TNtable_Site'),
                                                 DT::dataTableOutput('TNtable'),br(),
                                                 column(6,DT::dataTableOutput('riskTableTN')),
                                                 column(6,uiOutput("TNdataset"), uiOutput("TNplot_"),plotOutput('TNplot'))),
                                        navbarMenu("More",
                                                   # TP Summary
                                                   tabPanel("TP Summary",br(),br(),h4('TP Summary'),
                                                            DT::dataTableOutput('TPtable_Site'),
                                                            DT::dataTableOutput('TPtable'),br(),
                                                            column(6,DT::dataTableOutput('riskTableTP')),
                                                            column(6,uiOutput("TPdataset"), uiOutput("TPplot_"),plotOutput('TPplot'))),
                                                   # Total Habitat Summary
                                                   tabPanel("Total Habitat Summary",br(),br(),h4('Total Habitat Summary'),
                                                            DT::dataTableOutput('TotalHabitattable_Site'),
                                                            DT::dataTableOutput('TotalHabitattable'),br(),
                                                            column(6,DT::dataTableOutput('riskTableTotalHabitat')),
                                                            column(6,uiOutput("TotalHabitatdataset"), uiOutput("TotalHabitatplot_"),plotOutput('TotalHabitatplot'))),
                                                   # LRBS Summary
                                                   tabPanel("LRBS Summary",br(),br(),h4('LRBS Summary'),
                                                            DT::dataTableOutput('LRBStable_Site'),
                                                            DT::dataTableOutput('LRBStable'),br(),
                                                            column(6,DT::dataTableOutput('riskTableLRBS')),
                                                            column(6,uiOutput("LRBSdataset"), uiOutput("LRBSplot_"),plotOutput('LRBSplot'))),
                                                   # Metals CCU Summary
                                                   tabPanel("Metals CCU Summary",br(),br(),h4('Metals CCU Summary'),
                                                            DT::dataTableOutput('MetalsCCUtable_Site'),
                                                            DT::dataTableOutput('MetalsCCUtable'),br(),
                                                            column(6,DT::dataTableOutput('riskTableMetalsCCU')),
                                                            column(6,uiOutput("MetalsCCUdataset"), uiOutput("MetalsCCUplot_"),plotOutput('MetalsCCUplot'))),
                                                   # Specific Conductivity Summary
                                                   tabPanel("Specific Conductivity Summary",br(),br(),h4('Specific Conductivity Summary'),
                                                            DT::dataTableOutput('SpCondtable_Site'),
                                                            DT::dataTableOutput('SpCondtable'),br(),
                                                            column(6,DT::dataTableOutput('riskTableSpCond')),
                                                            column(6,uiOutput("SpConddataset"), uiOutput("SpCondplot_"),plotOutput('SpCondplot'))),
                                                   # TDS Summary
                                                   tabPanel("TDS Summary",br(),br(),h4('Total Dissolved Solids Summary'),
                                                            DT::dataTableOutput('TDStable_Site'),
                                                            DT::dataTableOutput('TDStable'),br(),
                                                            column(6,DT::dataTableOutput('riskTableTDS')),
                                                            column(6,uiOutput("TDSdataset"), uiOutput("TDSplot_"),plotOutput('TDSplot'))),
                                                   # Dissolved Sulfate Summary
                                                   tabPanel("Dissolved Sulfate Summary",br(),br(),h4('Dissolved Sulfate Summary'),
                                                            DT::dataTableOutput('DSulfatetable_Site'),
                                                            DT::dataTableOutput('DSulfatetable'),br(),
                                                            column(6,DT::dataTableOutput('riskTableDSulfate')),
                                                            column(6,uiOutput("DSulfatedataset"), uiOutput("DSulfateplot_"),plotOutput('DSulfateplot'))),
                                                   # Dissolved Chloride Summary
                                                   tabPanel("Dissolved Chloride Summary",br(),br(),h4('Dissolved Chloride Summary'),
                                                            DT::dataTableOutput('DChloridetable_Site'),
                                                            DT::dataTableOutput('DChloridetable'),br(),
                                                            column(6,DT::dataTableOutput('riskTableDChloride')),
                                                            column(6,uiOutput("DChloridedataset"), uiOutput("DChlorideplot_"),plotOutput('DChlorideplot'))),
                                                   # Dissolved Potassium Summary
                                                   tabPanel("Dissolved Potassium Summary",br(),br(),h4('Dissolved Potassium Summary'),
                                                            DT::dataTableOutput('DPotassiumtable_Site'),
                                                            DT::dataTableOutput('DPotassiumtable'),br(),
                                                            column(6,DT::dataTableOutput('riskTableDPotassium')),
                                                            column(6,uiOutput("DPotassiumdataset"), uiOutput("DPotassiumplot_"),plotOutput('DPotassiumplot'))),
                                                   # Dissolved Sodium Summary
                                                   tabPanel("Dissolved Sodium Summary",br(),br(),h4('Dissolved Sodium Summary'),
                                                            DT::dataTableOutput('DSodiumtable_Site'),
                                                            DT::dataTableOutput('DSodiumtable'),br(),
                                                            column(6,DT::dataTableOutput('riskTableDSodium')),
                                                            column(6,uiOutput("DSodiumdataset"), uiOutput("DSodiumplot_"),plotOutput('DSodiumplot'))))
                                                   ))),
                             tabPanel("Statewide Map",
                                      bootstrapPage(div(class="outer",
                                                        tags$style(type ="text/css",".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                                        leafletOutput('VAmap',width = '100%', height='100%'),
                                                        absolutePanel(top=60, right=50, class="panel panel-default",fixed=T,
                                                                      draggable = T, left = 50,bottom="auto",height="auto",width=900,
                                                                      p("The statewide map demonstrates water quality parameters as measured at 474 stations across Virginia. Users may display 
                                                                        basin and ecoregion layers to give additional context to parameter variation. Risk categories for each parameter are consistent 
                                                                        with breaks and color formatting utilized throughout the app.",span("(draggable)",style="color:red"))),
                                                        absolutePanel(top=140, right=10,class="panel panel-default",fixed=T,style = "overflow-y:scroll; max-height: 400px",
                                                                      draggable = T, left = "auto",bottom="auto",height=400,width=300,style = "overflow-x:scroll; max-width: 300px",
                                                                      h6(span("(draggable)"),style="color:red"),
                                                                      selectInput("parameterToPlot"
                                                                                  ,label = 'Choose a water quality parameter to display'
                                                                                  ,choices=c('','VSCI','pH','Dissolved Oxygen','Total Nitrogen','Total Phosphorus'
                                                                                             ,'Total Habitat','LRBS','Metals CCU','Specific Conductivity','Total Dissolved Solids'
                                                                                             ,'Dissolved Sulfate','Dissolved Chloride','Dissovled Potassium','Dissolved Sodium')
                                                                                  , selected=''),
                                                                      checkboxInput('showcdf','Show Statewide CDF Plot'),
                                                                      checkboxInput('eco','Show Ecoregions (Level III)',value=F),
                                                                      checkboxInput('basins','Show Virginia Super Basins',value=F),
                                                                      h5(strong("User Input Data")),
                                                                      h6(span("(Requires user to upload data)"),style="color:red"),
                                                                      checkboxInput('showUserSite','Show User Site',value=F),
                                                                      DT::dataTableOutput("colors2")),
                                                        absolutePanel(bottom=10,left=10,height=200, width=300,draggable = T,
                                                                      conditionalPanel(condition="input.showcdf == true",
                                                                                       plotOutput("Statewidecdf", height = 200,width=300)))))),
                             tabPanel("Dissolved Metals",
                                      column(3,wellPanel(
                                        h4("Instructions:"),
                                        p("Please upload dissolved metals data as a flat file (.csv). This section of the app allows
                                          for multiple sites to be uploaded. All data uploaded to the app must be formatted correctly. If you are unsure whether 
                                          your data is in the correct format, please download the 'template_metals.csv' file first to 
                                          check your data structure."),
                                        downloadButton('downloadTemplate_metals',"Download template_metals.csv"),
                                        fileInput('siteData_metals','Upload Dissolved Metals (flat file)',accept='.csv',width='100%'))),
                                      column(9,tabsetPanel(
                                        tabPanel("User Data",
                                                 h3("User Uploaded Data"),
                                                 h5("Please review data to ensure all fields are correct."),
                                                 DT::dataTableOutput('inputTable_metals'),
                                                 hr(),
                                                 column(6,h3("Metals CCU Analysis"),
                                                        DT::dataTableOutput("summary_MetalsCCU"),
                                                        helpText('You can copy/paste these values into your spreadsheet for upload
                                                                 back to this app or use it for additional analyses.'))),
                                        tabPanel("Data Summary",
                                                 fluidPage(
                                                 fluidRow(column(5,
                                                 h4("Dissolved Metals Statewide"),
                                                 uiOutput("metalsSitesUI"),
                                                 helpText("After uploading data from one or more sites on the previous tab ('User Data'), 
                                                          you will be able to scroll through sites to analyze dissolved metals against
                                                          statewide percentiles."),
                                                 br(),br(),br(),br(),br(),br(),br(),br(),
                                                 DT::dataTableOutput('colors_metals')),
                                                 column(5,br(),br(),
                                                        uiOutput('dMetal'),
                                                        uiOutput('dMetalplot_'),
                                                        plotOutput('dissolvedmetalscdf'))
                                                        
                                                 ))
                                                 
                                                 
                                                 
                                                 
                                                 ))))
                                      
                  ))
)
                                                                 