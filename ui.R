  analysis_mode_options = c("1 Year","12 Weeks","14 Days")
  min_date = min(df_dengue2$Onset_day)
  max_date = max(df_dengue2$Onset_day)
  start_date = min_date
  years_options = year(df_dengue2$Onset_day) %>% unique
  kernel_options = c("gaussian","rectangular","triangular","epanechnikov","biweight","cosine","optcosine")
  region_options = c("All",sf_dengue$Living_county)
  break_bin = 10
  start_sigma = 0.5
  
  
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
                                   selectInput("analysis_mode2",
                                               "Date range by:",
                                               c("-","1 Year","12 Weeks","14 Days"), 
                                               selected = '-'
                                   ),
                                   conditionalPanel(
                                     condition = "input.analysis_mode2 == '1 Year'",
                                     selectInput("sptem_yearpick2",
                                                 h5("Select year:"),
                                                 years_options, 
                                                 selected = 1998
                                     )
                                   ),
                                   conditionalPanel(
                                     condition = "input.analysis_mode2 == '12 Weeks' | input.analysis_mode2 == '14 Days'",
                                     dateInput("sptem_datepick2", label = h5("Select start date:"),format = "dd/mm/yyyy", value = start_date, min=min_date, max= max_date)
                                   ),
                                   selectInput("filters", "Attributes:", selected = "age_group",
                                               c("Age" = "age_group",
                                                 "Gender" = "gender",
                                                 "Subzone" = "Living_county")),
                                  #plotlyOutput(outputId = "barplot", height = 350)
                                  #plotlyOutput(outputId = "mainplot", height = 350)
                                  #plotlyOutput(outputId = "timeplot", height = 350)
                                  bsCollapse(id = "collapseExample", open = "Case Count over Years",
                                             bsCollapsePanel("Case Count over Years", plotlyOutput(outputId = "mainplot", height = 350), style = "info"),
                                             bsCollapsePanel("Cases Count per Month", plotlyOutput(outputId = "monthplot", height = 350), style = "info"),
                                             bsCollapsePanel("Cases Count per Week", plotlyOutput(outputId = "weekplot", height = 350), style = "info"),
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
             
             tabPanel("Spatial Temporal Analysis",
                      shinyjs::useShinyjs(),
                      tags$style(HTML(".datepicker {z-index:99999 !important;}")),
                      sidebarLayout(
                        position = "right",
                        sidebarPanel(
                          width = 3,
                          h3("Parameters"),
                          selectInput("analysis_mode",
                                      "Date range by:",
                                      analysis_mode_options
                          ),
                          conditionalPanel(
                            condition = "input.analysis_mode == '1 Year'",
                            selectInput("sptem_yearpick",
                                        h5("Select year:"),
                                        years_options
                            )
                          ),
                          conditionalPanel(
                            condition = "input.analysis_mode == '12 Weeks' | input.analysis_mode == '14 Days'",
                            dateInput("sptem_datepick", label = h5("Select start date:"),format = "dd/mm/yyyy", value = start_date, min=min_date, max= max_date)
                          ),
                          selectInput("sptem_regionpick",
                                      h5("Select region:"),
                                      region_options
                          ),
                          numericInput("sptem_sigpick", h5("Select sigma:"), start_sigma, min = 0.05, max = 10, step = 0.1),
                          numericInput("sptem_binpick", h5("Select number of bins:"), break_bin, min = 5, max = 25, step = 1),
                          selectInput("sptem_kernelpick",
                                      h5("Select kernel:"),
                                      kernel_options
                          ),
                          actionButton("sptem_gen_btn", "Generate")
                          
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          imageOutput("sptem_gifplot")
                        )
                      )
             ),
             tabPanel("Dengue Spread Analysis",
                      fluidPage(
                      shinyjs::useShinyjs(),
                      tags$style(HTML(".datepicker {z-index:99999 !important;}")),
                      sidebarLayout(
                        position = "right",
                        sidebarPanel(
                          width = 4
                          ,
                          h3("Parameters"),
                          dateRangeInput("daterange3", "Date range:",
                                         start  = min_date,
                                         end    = max_date,
                                         min    = min_date,
                                         max    = max_date,
                                         format = "dd/mm/yyyy",
                                         separator = " - "),
                          actionButton("sttp_gen_btn", "Generate")
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          textOutput("validation_text"),
                          fluidRow(
                            column(6,
                                   imageOutput("sttp_gifplot")
                            ),
                            column(4,
                                   plotOutput("sttp_plot_2")
                            )
                                 
                                   
            
                          ),fluidRow(
                            column(6,
                                   plotOutput("sttp_plot_1")
                            ),
                            column(6,
                                   plotOutput("sttp_plot_3")
                            )
                            
                          )

                        )
                      )
                      )
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
  
