source('global.R')


shinyUI(fluidPage(theme="slate.css",
                  navbarPage("VDEQ Benthic Stressor Analysis Tool",
                             tabPanel("About",fluidRow(column(10,
                                                              h4("This app was created to assist in the identification of benthic stressors 
                                                                 to aquatic communities."),
                                                              h5("By uploading field parameters and chemistry data from a given sample site, 
                                                                 users may compare their dataset to 10 years of Virginia Probabilistic Monitoring
                                                                 data by major river basin, stream order, and ecoregion."),
                                                              h5(strong("Individual Credits")),
                                                              br(),
                                                              br(),
                                                              hr(),
                                                              h4("To begin, simply navigate to the 'Upload Data' tab and follow the on screen 
                                                                 instructions.")))),
                             tabPanel("Data Upload",
                                      sidebarPanel(
                                        h4("Instructions:"),
                                        p("Please upload site chemistry and field parameter data in either a flat file (.csv). 
                                          All data uploaded to the app must be formatted correctly. If you are unsure whether 
                                          your data is in the correct format, please download the 'template.csv' file first to 
                                          check your data structure."),
                                        downloadButton('downloadTemplate',"Download template.csv"),
                                        fileInput('siteData','Upload Sites (flat file)',accept='.csv',width='100%')),
                                      mainPanel(h3("User Data"),
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
                                                column(6,h3("Summary Statistics")),
                                                tableOutput("summaryStats"))),
                             tabPanel("Data Summary",
                                      column(12,tabsetPanel(
                                        tabPanel("Composite Table",br(),br(),h4('Composite Table'),DT::dataTableOutput('colors'),
                                                 column(6,DT::dataTableOutput('riskTableInfo'))),
                                        tabPanel("pH Summary",br(),br(),h4('pH Summary'),DT::dataTableOutput('pHtable'),column(6,DT::dataTableOutput('riskTablepH'))),
                                        tabPanel("DO Summary",br(),br(),h4('DO Summary'),
                                                 DT::dataTableOutput('DOtable'),verbatimTextOutput('test'),
                                                 column(6,DT::dataTableOutput('riskTableDO')),
                                                 column(6,h4('Statewide Dissolved Oxygen Percentile Graph'),plotOutput('DOplot'))),
                                        tabPanel("TN Summary",br(),br(),h4('TN Summary'),DT::dataTableOutput('TNtable'),column(6,DT::dataTableOutput('riskTableTN'))),
                                        navbarMenu("More",
                                                   tabPanel("TP Summary",br(),br(),h4('TP Summary'),DT::dataTableOutput('TPtable'),column(6,DT::dataTableOutput('riskTableTP'))),
                                                   tabPanel("Total Habitat Summary",br(),br(),h4('Total Habitat Summary'),DT::dataTableOutput('TotalHabitattable'),column(6,DT::dataTableOutput('riskTableTotalHabitat'))),
                                                   tabPanel("LRBS Summary",br(),br(),h4('LRBS Summary'),DT::dataTableOutput('LRBStable'),column(6,DT::dataTableOutput('riskTableLRBS'))),
                                                   tabPanel("Metals CCU Summary",br(),br(),h4('Metals CCU Summary'),DT::dataTableOutput('MetalsCCUtable'),column(6,DT::dataTableOutput('riskTableMetalsCCU'))),
                                                   tabPanel("Sp Conductivity Summary",br(),br(),h4('SP Conductivity Summary'),DT::dataTableOutput('SpCondtable'),column(6,DT::dataTableOutput('riskTableSpCond'))),
                                                   tabPanel("TDS Summary",br(),br(),h4('TDS Summary'),DT::dataTableOutput('TDStable'),column(6,DT::dataTableOutput('riskTableTDS'))),
                                                   tabPanel("Dissolved Sulfate Summary",br(),br(),h4('Dissolved Sulfate Summary'),DT::dataTableOutput('DSulfatetable'),column(6,DT::dataTableOutput('riskTableDSulfate'))),
                                                   tabPanel("Dissolved Chloride Summary",br(),br(),h4('Dissolved Chloride Summary'),DT::dataTableOutput('DChloridetable'),column(6,DT::dataTableOutput('riskTableDChloride'))),
                                                   tabPanel("Dissolved Potassium Summary",br(),br(),h4('Dissolved Potassium Summary'),DT::dataTableOutput('DPotassiumtable'),column(6,DT::dataTableOutput('riskTableDPotassium'))),
                                                   tabPanel("Dissolved Sodium Summary",br(),br(),h4('Dissolved Sodium Summary'),DT::dataTableOutput('DSodiumtable'),column(6,DT::dataTableOutput('riskTableDSodium'))))
                                        
                                      )))
                  ))
)
                                                                 