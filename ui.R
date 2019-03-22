navbarPage("Dangy", id="nav",
           tabPanel("Project Info",
                    # Sidebar with a slider input for number of years
                    sidebarLayout(
                      position = "left",
                      sidebarPanel(
                        width = 3,
                        img(src = "res/375px-DANGY_LOGO_FULL.png", style="display: block; margin-left: auto; margin-right: auto; width: 85%"),
                        h3("Content"),
                        tags$ol(
                          tags$a(href="#motivation", tags$li("Motivation")),
                          tags$a(href="#objective", tags$li("Objective"))
                        )
                      ),
                      
                      # Show a plot of the generated distribution
                      mainPanel(
                        tags$a(id="motivation",h3("Motivation")),
                        p("Dengue fever has for centuries been a prominent epidemic disease that plagued humanity. While normal dengue infections takes approximately a week to recover, complications like dengue hemorrhagic fever and dengue shock syndrome can be extremely severe, causing death. Today, even with our advanced healthcare and technology, there remains no proper cure or vaccine to combat the disease. This has allowed dengue to stay rampant in both developed and developing countries. Consider Taiwan, a country that has proven its evident economical growth and development, including its high level of healthcare services that means high international standards. Yet, according to the International Association for Medical Assistance to Traveller (IAMAT), health risks like air pollution and also mosquito transmitted diseases like dengue or chikungunya. In fact, IAMAT has listed dengue and chikungunya as its top general health risks on its website. "),
                        p("One of Taiwan's health catastrophe was the 2015 dengue outbreak. Up to 2016, there were 15,732 DF cases reported. Also,136 dengue hemorrhagic fever (DHF) cases, of which 20 resulted in death. Most of its cases were reported from kaohsiung, southern Taiwan. "),
                        p("A study conducted on 'Severe Dengue Fever Outbreak in Taiwan' has concluded that the the overall figure of its dengue outbreak have been associated with the outbreak in kaohsiung, southern Taiwan. The reason could be justified with the underground pipeline explosion combined with subsequent rainfall and higher temperature which enhances 'the mosquito breeding activity, facilitating DENV transmission' (Wang, 2016). "),
                        p("One should not underestimate even the smallest figure of a disease. Diseases could have a ripple-effect on its transmission thus resulting to an exponential cases. Maintenance and prevention should always be done in order to avoid any possibility. Especially when it comes to mosquito-spread diseases in Taiwan. As mentioned previously, vaccine or specific therapy on dengue has yet to be developed. Thus, finding a means to implement effective control measures is paramount. This led us to this project, where we aim to develop an analytical tool that can facilitate in the study of dengue fever and guide the implementation of control measures."),
                        
                        tags$a(id="objective",h3("Objective")),
                        p("This project aims to achieve 2 folds:"),
                        p("First, it aims to create an analytical solution that allows users to quickly analyze the outbreak of Dengue in Taiwan, facilitating the study of Dengue Fever. The tool will offer historical data of various types for users to work with, including: demographic spread, population density, weather and climate and dengue-prone locations such as water protection areas and industrial district."),
                        p("Second, it aims to provide an analysis discussing the possible reasons influencing the spread of dengue across the difference regions of Taiwan using the developed tool. Through identifying hotspots and studying the transmission of dengue over time, this project will help us better understand patterns and discover strategies on how to curb with epidemics in future & steps to prevent Dengue in Taiwan and similar states.")
                      )
                    )
           ),
           
           tabPanel("Exploratory Data Analysis",
              id="nav",
               div(class="outer",
                   
                   tags$head(
                     # Include our custom CSS
                     includeCSS("style.css"),
                     includeScript("gomap.js")
                   ),
                   
                   # If not using custom CSS, set height of leafletOutput to a number instead of percent
                   leafletOutput("dataPoints", width="100%", height="100%"),
                   
                   # Shiny versions prior to 0.11 should use class = "modal" instead.
                   absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                 draggable = TRUE, top = 60, left = "auto", right = 50, bottom = "auto",
                                 width = 450, 

                                 h2("Filters"),
                                 #sliderInput("yearSlider",
                                 #            "Year:",
                                 #            min = 1998,
                                 #            max = 2018,
                                 #            value = c(1998,1999)),
                                 selectInput("yearSlider", "Years:", selected = 1998,
                                             c(1998,1999,2000,2001,2002,2003,2004,
                                               2005,2006,2007,2008,2009,2010,2011,
                                               2012,2013,2014,2015,2016,2017,2018)),
                                 selectInput("filters", "Attributes:", selected = "age_group",
                                             c("Age" = "age_group",
                                               "Gender" = "gender",
                                               "Subzone" = "Living_county")),
                                #plotlyOutput(outputId = "barplot", height = 350)
                                #plotlyOutput(outputId = "mainplot", height = 350)
                                #plotlyOutput(outputId = "timeplot", height = 350)
                                bsCollapse(id = "collapseExample", open = "Case Count over Years",
                                           bsCollapsePanel("Case Count over Years", plotlyOutput(outputId = "mainplot", height = 350), style = "info"),
                                           bsCollapsePanel("Cases Count per Month", plotlyOutput(outputId = "timeplot", height = 350), style = "info"),
                                           bsCollapsePanel("Distribution of Attributes", plotlyOutput(outputId = "barplot", height = 350), style = "success")
                                )
                                
                   ),
                   #absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                   #              draggable = TRUE, top = "auto", left = 50, right = "auto", bottom = 50,
                   #              width = 300, height = 90,
                   #              h4("Methods of Visualisation:"),
                   #              selectInput("methods", label = NULL, selected = "feature_3",
                   #                          choices = c("Attribute Distribution" = "feature_3",
                   #                            "Cases against Time" = "feature_5"))
                   #),

                   
                   tags$div(id="cite",
                            'Data retrieved from ', tags$em('Dengue Daily Confirmed Cases Since 1998 '), ' by Taiwan\'s CDC Government.'
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
                    fluidPage(
                      titlePanel("Total Cases in Level 3 District"),
                      
                      # Create a new Row in the UI for selectInputs
                      fluidRow(
                        column(4,
                               selectInput("months",
                                           "Month(s):",
                                           c("All","January", "Febuary","March","April","May","June","July","August","September","November","December"))
                        )
                      ),
                      # Create a new row for the table.
                      DT::dataTableOutput("district_cases_table"),
                      titlePanel("Total Affected Countries/Cities"),
                      
                      # Create a new Row in the UI for selectInputs
                      fluidRow(
                        column(4,
                               selectInput("local",
                                           "Localisation Analysis Type:",
                                           c("Both","Taiwan Local", "International"))
                        )
                      ),
                      # Create a new row for the table.
                      DT::dataTableOutput("infected_countries")
           )
           
           )  
)

