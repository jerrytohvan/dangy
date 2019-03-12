navbarPage("Dangy", id="nav",
           tabPanel("Background",
                    # Sidebar with a slider input for number of years
                    sidebarLayout(
                      sidebarPanel(
                        
                      ),
                      
                      # Show a plot of the generated distribution
                      mainPanel(
                        
                      )
                    )
           ),
           tabPanel("Exploratory Data Analysis",
                    # Sidebar with a slider input for number of years
                    sidebarLayout(

                        sidebarPanel(
                          sliderInput("yearSlider",
                                      "Year:",
                                      min = 1998,
                                      max = 2018,
                                      value = 1998)
                        ),
                      
                      # Show a plot of the generated distribution
                      mainPanel(
                        leafletOutput("mapPlot", height= 900),
                        leafletOutput("dataPoints")
                      )
                    )
           ),
           
           tabPanel("Further Analysis",
                    fluidRow(
                      column(3,
                             selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
                      ),
                      column(3,
                             conditionalPanel("input.states",
                                              selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
                             )
                      ),
                      column(3,
                             conditionalPanel("input.states",
                                              selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
                             )
                      )
                    ),
                    fluidRow(
                      column(1,
                             numericInput("minScore", "Min score", min=0, max=100, value=0)
                      ),
                      column(1,
                             numericInput("maxScore", "Max score", min=0, max=100, value=100)
                      )
                    ),
                    hr(),
                    DT::dataTableOutput("ziptable")
           ),
           
           tabPanel("Data Table",
                    navlistPanel(
                      "Header",
                      tabPanel("Exploratory Data Analysis",
                               
                               h3("Dengue Spatial Points Map"),
                               sidebarPanel(
                                 sliderInput("yearSlider",
                                             "Year:",
                                             min = 1998,
                                             max = 2018,
                                             value = 1998)
                               ),
                               leafletOutput("mapPlot", height= 900),
                               leafletOutput("dataPoints")
                      ),
                      tabPanel("Further Analysis",
                               h3("This is the second panel")
                      ),
                      tabPanel("Data Table",
                               h3("This is the third panel")
                      )
                    ),
                    # Sidebar with a slider input for number of years
                    sidebarLayout(
                      sidebarPanel(
                        
                      ),
                      
                      # Show a plot of the generated distribution
                      mainPanel(
                        
                      )
                    )
           )
           
           
)

