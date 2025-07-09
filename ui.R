library(shiny)
library(shinydashboard)
library(DT)  # For DT::dataTableOutput
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = "Environmental Data Dashboard"),
  
  # Setup Dashboard Sidebar.
  # Found icon codes, (https://rstudio.github.io/shinydashboard/appear
  #ance.html#icons)
  # Aim: Setting up left side bar for each dataset w/ labels to be used in
  #server code
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("About", tabName = "about", icon = icon("info-circle")),
                menuItem("Biological Data", tabName = "bio", icon = icon("dna")),
                menuItem("Habitat Data", tabName = "habitat", icon = icon("tree")),
                menuItem("Chemical Data", tabName = "chemical", icon = icon("flask")),
                menuItem("Modeling", tabName = "model", icon = icon("chart-line")),
                menuItem("Subset and Save", tabName = "dat", icon = icon("save"))
    )
  ),
  
  # Setup Dashboard Body.
  # Aim: Define each tab and what will be shown on each tab (name each tab)
  # Challenges: Formatting the user interface and making sure to keep track of all of the added code as
  # both of us work on the same app on different computers. Solved through our google doc trackers & communication
  
  # Dashboard Body (Setting up the tabs for our 3 dimensions of data and extra pages for later)
  dashboardBody(
    tabItems(
      tabItem(tabName = "about",
              fluidRow(
                column(12,
                       box(width = 12, title = h1(strong("Welcome to the LJEA Data Visualization Tool")), status = "primary", solidHeader = TRUE,
                           h3(strong("About the Data")),
                           p("(history of data)")
                       ))),
              
              fluidRow(
                
                column(6,
                       box(width = 12, title = h3(strong("Biological Data")), status = "primary",
                           h4("What indexes does the LJEA use to measure biological variables?"),
                           p("This tab displays data related to biological indicators of stream health, notably the NCBI index and bioclassification. The NCBI(North Carolina Biotic Index) is a numerical value used to assess water quality based on the macroinvertebrates present in the stream. Generally, lower NCBI scores indicate better stream quality. Bioclassifications are qualitative ratings (like Excellent, Good, Fair, Poor) that summarize overall stream health based on macroinvertebrate data. For the purpose of our app, we have turned these ratings into quantitative values between 1-5.")
                       )
                ),
                
                column(6,
                       box(width = 12, title = h3(strong("Chemical Data")), status = "primary",
                           h4("Where does the LJEA's chemical data come from?"),
                           p("This tab displays data on Lake James collected by the Environmental Quality Institute (EQI), a non-profit institute responsible for collecting chemical data on over 200 lake sites across Western North Carolina. Trained volunteers are responsible for collecting data for the Volunteer Water Information Network (VWIN) each month. The non-profit institute has been operating for over 35 years and conducts 20,000 tests annually, all for the purpose of maintaining regional water quality for North Carolina. ")))
                
              ),
              
              fluidRow(
                column(8,
                       box(width = 12, title = h3(strong("Habitat Assessment Data")), status = "primary",
                           h4("What do each of the categories of the habitat assessment represent?"),
                           p("This tab displays data collected through a habitat assessment form for the mountain or piedmont zones of Western North Carolina."),
                           p(""),
                           p(strong("Habitat Assessment Variables:")),
                           p(strong("channel.modification:"), " Degree to which the natural stream channel has been straightened/altered.
"),
                           p(strong("instream.habitat:"), " Variety and abundance of habitat features like logs, rocks, and vegetation."),
                           p(strong("bottom.substrate:"), " Composition of the stream bottom (e.g., sand, gravel, silt)."),
                           p(strong("pool.variety:"), " Number and diversity of pools that serve as fish habitat."),
                           p(strong("riffle.habitats:"), " Presence of shallow, fast-flowing riffles that oxygenate water."),
                           p(strong("bank.stability.and.vegetation:"), " Amount of sunlight reaching the stream surface.
"),
                           p(strong("light.penetration:"), " Stability of streambanks and presence of vegetation."),
                           p(strong("riparian.vegetative.zone.width:"), " Width of natural vegetation along the streambanks."),
                           p(strong("total:"), " Overall stream habitat quality score.")
                       )))),
      
      # Bio Tab; formatted with fluid row and defined user inputs
      tabItem(tabName = "bio",
              fluidRow(
                column(3,
                       box(width = 12, title = "Filters", solidHeader = TRUE, status = "primary",
                           selectizeInput("site_name", "Choose One or More Sites to View",
                                          choices = NULL, multiple = TRUE,
                                          options = list(placeholder = "Select 1 or more sites")),
                           selectInput("variable", "Choose Biological Variable:", choices = NULL),
                           selectInput("data_view", "Choose Data Display Type:",
                                       choices = c("All", "Averages", "Hurricane Helene")))
                ),
                
                column(9,
                       fluidRow(
                         column(12,
                                box(width = 12, title = "Biological Data Summary", status = "info", solidHeader = TRUE,
                                    conditionalPanel(condition = "input.data_view == 'All'",
                                                     plotOutput("bioLinePlot"))))
                       ),
                       
                       fluidRow(
                         column(12,
                                box(width = 12, title = "Biological Plot(s)",status = "info", solidHeader = TRUE,
                                    plotOutput("bioPlot"))
                         )),
                       fluidRow(
                         column(6,
                                box(width = 12, title = "Summary Table",status = "info", solidHeader = TRUE,
                                    conditionalPanel(condition = "input.data_view == 'All' || input.data_view == 'Averages'",
                                                     DT::dataTableOutput("bioSummary")))
                         ))))
      ),
      
      # Habitat tab; formatted (site column title is "name" not "site.name")
      tabItem(tabName = "habitat",
              fluidRow(
                column(3,
                       box(width = 12, title = "Filters", solidHeader = TRUE, status = "primary",
                           selectizeInput("site_name_hab", "Choose One or More Sites to View", choices = NULL, multiple = TRUE,options = list(placeholder = "Select 1 or more sites")),
                           selectInput("variable_hab", "Choose Habitat Variable:", choices = NULL),
                           selectInput("data_view_hab", "Choose Data Display Type:",
                                       choices = c("All", "Averages", "Hurricane Helene")))
                ),
                column(9,
                       fluidRow(
                         column(12,
                                box(width = 12, title = "Habitat Data Summary", status = "info", solidHeader = TRUE,
                                    conditionalPanel(condition = "input.data_view_hab == 'All'",
                                                     plotOutput("habLinePlot")))
                         )),
                       
                       fluidRow(
                         column(12,
                                box(width = 12, title = "Habitat Plot(s)", status = "info", solidHeader = TRUE,
                                    plotOutput("habitatPlot"))
                         )),
                       fluidRow(
                         column(12,
                                box(width = 12, title = "Correlation Matrix", status = "info", solidHeader = TRUE,
                                    plotOutput("habCorrPlot"))
                         )),
                       fluidRow(
                         column(6,
                                box(width = 12, title = "Summary Table",  status = "info", solidHeader = TRUE,
                                    conditionalPanel(condition = "input.data_view_hab == 'All' || input.data_view_hab == 'Averages'",
                                                     DT::dataTableOutput("habitatSummary")))
                         ))))
      ),
      
      # Chemical tab; nothing different from bio
      tabItem(tabName = "chemical",
              fluidRow(
                column(3,
                       box(width = 12, title = "General Filters", solidHeader = TRUE, status = "primary",
                           selectizeInput("site_name_chem", "Choose One or More Sites to View", choices = NULL, multiple = TRUE,options = list(placeholder = "Select 1 or more sites")),
                           selectInput("variable_chem", "Choose Chemical Variable:", choices = NULL),
                           selectInput("data_view_chem", "Choose Data Display Type:",
                                       choices = c("All", "Averages", "Hurricane Helene"))
                       ),
                       box(width = 12, title = "Scatterplot Filters", solidHeader = TRUE, status = "primary",
                           selectInput("x_var_chem", "Choose X Variable for Scatterplot:", choices = NULL),
                           selectInput("y_var_chem", "Choose Y Variable for Scatterplot:", choices = NULL),
                           checkboxInput("add_regression", "Add Linear Regression Line", value = FALSE)
                           
                       ),
                       box(width = 12, title = "Chemical Variable Key", status = "warning", solidHeader = TRUE,
                           p(strong("NH3.N:"), " Ammonia"),
                           p(strong("NO3.N:"), " Nitrate"),
                           p(strong("Total.P:"), " Total Phosphorus"),
                           p(strong("PO4:"), " Orthophosphate"),
                           p(strong("Turb:"), " Turbidity"),
                           p(strong("TSS:"), " Total Suspended Solids"),
                           p(strong("Cond:"), " Conductivity"),
                           p(strong("Alk:"), " Alkalinity"),
                           p(strong("pH:"), " Acidity level")
                       )
                       
                       
                ),
                
                column(9,
                       fluidRow(
                         column(12,
                                box(width = 12, title = "Chemical Data Summary", status = "info", solidHeader = TRUE,
                                    conditionalPanel(condition = "input.data_view_chem == 'All'",
                                                     plotOutput("chemLinePlot")))
                         )),
                       
                       fluidRow(
                         column(12,
                                box(width = 12, title = "Chemical Data Plot(s)",status = "info", solidHeader = TRUE,
                                    plotOutput("chemPlot"))
                         )),
                       
                       fluidRow(
                         column(12,
                                box(width = 12, title = "Scatterplot of Selected Variables", status = "info", solidHeader = TRUE,
                                    plotOutput("chemScatterPlot"))
                         )),
                       
                       fluidRow(
                         column(12,
                                box(width = 12, title = "Correlation Matrix", status = "info", solidHeader = TRUE,
                                    plotOutput("chemCorrPlot"))
                         )),
                       
                       fluidRow(
                         column(6,
                                box(width = 12, title = "Summary Table",status = "info", solidHeader = TRUE,
                                    conditionalPanel(condition = "input.data_view_chem == 'All' || input.data_view_chem == 'Averages'",
                                                     DT::dataTableOutput("chemSummary")))
                         ))))
      ),
      
      tabItem(tabName = "model",
              h3("Modeling Page Placeholder")
      ),
      
      tabItem(tabName = "data",
              h3("Data Subset and Download Placeholder") # from github repository
      )
    )
  )
)