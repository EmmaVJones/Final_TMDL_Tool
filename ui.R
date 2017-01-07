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
                                                                     c(" "="NA","James Basin"="James",
                                                                       "Roanoke Basin"="Roanoke",
                                                                       "Potomac-Shenandoah"="PotShen",
                                                                       "Rappahannock-York"="RapYork",
                                                                       "Chowan Basin"="Chowan",
                                                                       "Tennessee Basin"="Tenn",
                                                                       "New Basin"="New"),selected=1)),
                                                column(4,selectInput("Ecoregion",label=h4("Ecoregion"),
                                                                     c(" "="NA","Piedmont"="Piedmont",
                                                                       "Northern Piedmont"="NorthernPiedmont",
                                                                       "Central Appalachian Ridges and Valleys"="CentralAppsRandV",
                                                                       "Southeastern Plains"="SEplains",
                                                                       "Blue Ridge Mountains"="BlueRidge",
                                                                       "Central Appalachians"="CentralApps"),selected=1)),
                                                column(5,selectInput("StreamOrder",label=h4("Stream Order"),
                                                                     c(" "="NA","First Order"="First",
                                                                       "Second Order"="Second","Third Order"="Third",
                                                                       "Fourth Order"="Fourth","Fifth Order"="Fifth"),selected=1)),
                                                DT::dataTableOutput('inputTable'),
                                                hr(),
                                                column(6,h3("Summary Statistics")),
                                                tableOutput("summaryStats"))),
                             tabPanel("Data Summary",
                                      mainPanel(tabsetPanel(
                                        tabPanel("Composite Table",DT::dataTableOutput('colors')),
                                        tabPanel("pH Summary"),
                                        tabPanel("DO Summary"),
                                        tabPanel("TN Summary"),
                                        navbarMenu("More",
                                                   tabPanel("TP Summary"),
                                                   tabPanel("Total Habitat Summary"),
                                                   tabPanel("LRBS Summary"),
                                                   tabPanel("Metals CCU Summary"),
                                                   tabPanel("Sp Conductivity Summary"),
                                                   tabPanel("TDS Summary"),
                                                   tabPanel("Dissolved Sulfate Summary"),
                                                   tabPanel("Dissolved Chloride Summary"),
                                                   tabPanel("Dissolved Potassium Summary"),
                                                   tabPanel("Dissolved Sodium Summary"))
                                        
                                      )))
                  ))
)
                                                                 