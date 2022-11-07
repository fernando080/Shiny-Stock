path = "env.R"
source(path)

ui <- fluidPage(tags$head(tags$style(HTML("table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
                              background-color: #9c4242 !important;
                              } "))),
  theme=shinythemes::shinytheme(theme = "superhero"),
  includeCSS("www/styles.css"),
  
  # titlePanel 
  titlePanel(strong("Inteligencia de Negocio con R: DSBI",
                    style="color:White;"),
             windowTitle = "Inteligencia de Negocio con R: DSBI"),
  
  # navlistPanel
  navlistPanel(widths=c(3,9),
               # tabPanel: Información
               tabPanel("Home",icon = icon("home"),
                        markdown('# <span style="color:#B3E2FF">**Shiny, R and Stock Market Analysis:**</span>'),
                        
                        div(class="center",
                            img(src="https://d31dn7nfpuwjnm.cloudfront.net/images/valoraciones/0037/2740/forex-vs-stock-market.jpg?1585583156", height = '200px', width = '500px')
                        ),
                        br(),br(),br(),
                        hr(),
                        h3("Objetive:",class="estiloh3"),
                        var_texto,
                        h3("Author:",class="estiloh3"),
                        strong("Fernando Cañizares Romero"),
                        h3("Summary of the Task:",class="estiloh3"),
                        var_texto_mitfm
               ),
               # tabPanel: Own Data -----
               tabPanel("Data Analysis",icon = icon("database"),
                        # fluidRow: selecciona variable
                        # fluidRow(
                        #   column(width=4,
                        #          selectInput("SelvariableOpenClose",
                        #                      strong("Select Open or Close", style = "font-size:20px;"),
                        #                      choices = names(datos)[c(var_selec[c(2,3)])],
                        #                      selected = names(datos)[var_selec[2]])
                        #         )
                        # ),br(),
                        fluidRow(
                          column(width = 5,
                                 fileInput(
                                   "file1",
                                   strong("Choose CSV File", style = "font-size:20px;"),
                                   multiple = FALSE,
                                   accept = ".csv",
                                   buttonLabel = "Browse...",
                                   placeholder = "Stock Data Here!"
                                 )),
                          
                          column(width = 4, offset = 1,
                                    selectInput('Iddataselect', 
                                                strong("Data Selection", style = "font-size:20px;"),
                                                choices = list.files('www/data/'))
                                      )
                                ),
                        fluidRow(
                              column(width = 2,
                                     checkboxInput('HaveHeader',  
                                                   strong('Data with Header', style = "font-size:15px;"), 
                                                   value = T)
                                  ),

                               column(width = 4,
                                      radioButtons("SepChar",  
                                                   strong("Sep Character?", style = "font-size:15px;"),
                                                   choices = c("Comma" = ",",
                                                     "Semicolon " = ";",
                                                     "Vertical bar" = "|",
                                                     "Dot" = "."),
                                                   inline=T)
                                  ),
                             
                             # Input: Numeric entry for number of obs to view ----
                             column(width = 4,
                             numericInput("obs",
                                          strong("Number of observations to view:", style = "font-size:15px;"),
                                          10)
                                    )
                                ),
                        # tabsetPanel: Unidimensional-----
                        tabsetPanel(
                              # tabPanel: Resumen Numérico ----
                              tabPanel("Exploratory Data Analysis",
                                       
                                       # Output: Verbatim text for data summary ----),
                                       verbatimTextOutput("summary", placeholder = TRUE),
                                       
                                       # Output: HTML table with requested number of observations ----
                                       DT::dataTableOutput("view")
                                       ),
                        
                              # tabPanel: histogramas ----
                              tabPanel("Graphs and Histograms Open/Close",
                                       fluidRow(
                                         column(width = 6, plotlyOutput("histoOpen")),
                                         column(width = 6, plotlyOutput("histoClose"))
                                       ),
                                       
                                       br(),br(),br(),
                                       
                                       plotlyOutput("plot1"),
                                       
                                       br(),br(),br(),
                                       
                                       fluidRow(
                                         column(width = 6, plotOutput("plotOpen")),
                                         column(width = 6, plotOutput("plotClose"))
                                       )
                                      ),
                              
                              # tabPanel: Model ----
                              tabPanel("Create the Model",
                                       br(), br(),
                                       
                                       fluidRow(
                                          column(width = 3, tags$h4("Augmented Dickey–Fuller Test Close p-value:")), 
                                          column(width = 3, verbatimTextOutput("ADFtest")), 
                                       ),
                                       br(),br(),br(),
                                       plotOutput("ADFplot"),
                                       verbatimTextOutput("Model"),
                                      
                                      br(),
                                      actionButton("autoArima", "Shall Auto-Arima Model?"),
                                      br(),
                                      verbatimTextOutput("resAutoAirima"),
                                      br(),
                                      tags$h4("Validate Your Model:"),
                                      br(),
                                      plotOutput("plotResAutoAirima"),
                                      br(),
                                      fluidRow(
                                        column(width = 2, tags$h4("Shapiro test p-value:")), 
                                        column(width = 3, verbatimTextOutput("Shapiro_test")),
                                        column(width = 2, tags$h4("Ljung test p-value:")), 
                                        column(width = 3, verbatimTextOutput("Ljung_test"))
                                      ),
                                      br(),
                                      plotOutput("plotTsdiag"),
                                      br(),
                                      plotOutput("plotResults"),
                                      br(),
                                      br()
                                 )
                              )
                          ),#,)
                          # tabPanel: Gráficos Unidimensionales ----
                          # continúa con más código R .... (cuidado con las comas y paréntesis)
               
               # tabPanel: Your Data -----
               tabPanel("Today's Winners, GDP, Locations",icon = icon("trophy"),
                          h3("Market Winners:",class="estiloh3"),
                          br(),
                          DT::dataTableOutput("WinWin"),
                          br(),
                          fluidRow(
                            column(width = 6, 
                                   h3("GDP per Country:",class="estiloh3"),
                            ),
                            column(width = 6, 
                                   h3("Visit Company Location:",class="estiloh3"),
                            )
                          ),
                          fluidRow(
                            column(width = 6, 
                                   br(),
                            ),
                            column(width = 2, 
                                   numericInput("long", label = h4("Longitude:"), value = 43.314582)
                            ),
                            column(width = 2, 
                                   numericInput("lat", label = h4("Latitude:"), value = -8.504916)
                            ),
                            column(width = 2, br(),br(),
                                   actionButton("recalc", "Show point")
                            )
                          ),
                        
                          
                          fluidRow(
                            column(width = 6, 
                                   leafletOutput("mymap")
                            ),
                            column(width = 6, 
                                   leafletOutput("mymap2")
                            )
                          ),
                          br(),
                          br()
                        ),
               
               # tabPanel: Nice Wheater to Invest? -----
               tabPanel("Nice Wheater to Invest?",icon = icon("sun"),
                        
                          # Page header
                          headerPanel('Invest Day?'),
                          
                          # Input values
                          sidebarPanel(
                            h3("Input parameters",class="estiloh3"),
                            
                            selectInput("outlook", label = "Outlook:", 
                                        choices = list("Sunny" = "sunny", "Overcast" = "overcast", "Rainy" = "rainy"), 
                                        selected = "Rainy"),
                            sliderInput("temperature", "Temperature:",
                                        min = 64, max = 86,
                                        value = 70),
                            sliderInput("humidity", "Humidity:",
                                        min = 65, max = 96,
                                        value = 90),
                            selectInput("windy", label = "Windy:", 
                                        choices = list("Yes" = "TRUE", "No" = "FALSE"), 
                                        selected = "TRUE"),
                            
                            actionButton("submitbutton", "Submit", class = "btn btn-primary")
                          ),
                          
                          mainPanel(
                            tags$label(h3('Status/Output')), # Status/Output Text Box
                            verbatimTextOutput('contents'),
                            tableOutput('tabledata') # Prediction results table
                            
                          )
                        
               ),
               tabPanel("Documentation",icon = icon("book"),
                        
                        # Markdown documentation
                        markdown(doc)
              )
                        
  )
)

# 
# ui <- fluidPage(
#   sidebarLayout(
#     sidebarPanel(
#       fileInput("file1", "Choose CSV File", accept = ".csv"),
#       checkboxInput("header", "Header", TRUE)
#     ),
#     mainPanel(
#       tableOutput("contents")
#     )
#   )
# )
# 
# server <- function(input, output) {
#   output$contents <- renderTable({
#     file <- input$file1
#     ext <- tools::file_ext(file$datapath)
#     
#     req(file)
#     validate(need(ext == "csv", "Please upload a csv file"))
#     
#     read.csv(file$datapath, header = input$header)
#   })
# }
# 
# shinyApp(ui, server)
# }