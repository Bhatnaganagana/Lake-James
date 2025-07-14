library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(zoo)
library(plotly)
library(corrplot)

# Later on in the code, we make plots that show the pristine and regional chemical value averages for different chemicals. 
# The values are taken from the vwin data summary sheet. The pristine average tells you the high water quality reading average. 
# These act as a comparison to inform what the chemical value means. 

pristine_avgs <- c(
  "chem_NH3.N"   = 0.04,  
  "chem_NO3.N"   = 0.2,   
  "chem_Total.P" = NA,    # No pristine average available
  "chem_PO4"     = 0.04,  
  "chem_Turb"    = 2.1,   
  "chem_TSS"     = 2.8,   
  "chem_Cond"    = 19.7,  
  "chem_Alk"     = 9.5,   
  "chem_pH"      = 7.0    
)

regional_avgs <- c(
  "chem_NH3.N"   = 0.08,  
  "chem_NO3.N"   = 0.5,   
  "chem_Total.P" = NA,    # No regional average available
  "chem_PO4"     = 0.08,  
  "chem_Turb"    = 6.0,   
  "chem_TSS"     = 7.9,   
  "chem_Cond"    = 77.7,  
  "chem_Alk"     = 22.2,  
  "chem_pH"      = 7.2    
)

# Beginning our Shiny Server Code: 
server <- function(input, output, session) {
  
  # 1. READING IN the non-edited/cleaned data sets
 # We use reactive() here to make each of the plots update as the link/google spreadsheet updates
  getBioData <- reactive({
# define the URL and set it to bio_data
    url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTw2TDKoVxuXm22xo-vn7IYnR8obaC5OllS0bO6_uv2boVeZNwZzb_vypuDEEMkyIExb3RiXaWIVnCF/pub?output=csv"
    bio_data <- read.csv(url)

    # This makes it possible to graph time series data as.yearmon (taking the columns year and month from bio_data can be done with “$”)
    bio_data$date <- as.yearmon(paste(bio_data$year, bio_data$month), "%Y %m") # Converted to time-series

    # Here we rename the columns so that those specific renamed columns only can be converted to long format. Wide format is what we are currently in (bio_data). It is when basically you have one site andfor example 5 different columns of data values on that one site. Long format is when instead of 5 columns, we have 5 rows, each row has that one site, the variable it is measuring, and the value. 
    # Rather than cleaning the data, we resorted to focusing on only columns of interest by marking them with bio_, or chem_, or hab_. This allowed us to make visualizations and consistently display those data columns without error (even if na pops up).
    bio_data <- bio_data %>% # Renaming columns
      rename_with(.cols = c(ncbi, bioclassification),
                  .fn = ~ paste0("bio_", .x))
    
    bio_long <- bio_data %>% # Converting to long format
      tidyr::pivot_longer(cols = starts_with("bio_"), names_to = "Variable", values_to = "Value")
    
    return(bio_long)
    
  })
  
  getHabitatData <- reactive({
    # Everything is the same for the habitat data other than the fact that we rename different columns for longer format
    url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTT6cKJASYl74Kw3-R90FMZcZ3qw08Iy4oABDLEgfPpVJGKvGleq6BV3GEdOpCL4rkTUTN55rRV0n1I/pub?output=csv"
    hab_data <- read.csv(url)
    
    hab_data$date <- as.yearmon(paste(hab_data$year, hab_data$month), "%Y %m")
    
    hab_data <- hab_data %>%
      rename_with(
        .cols = c(`channel.modification`, `instream.habitat`, `bottom.substrate`, `pool.variety`, `riffle.habitats`, `bank.stability.and.vegetation`, `light.penetration`, `riparian.vegetative.zone.width`, `total`),
        .fn = ~ paste0("hab_", .x))
    
    hab_long <- hab_data %>%
      tidyr::pivot_longer(cols = starts_with("hab_"), names_to = "Variable", values_to = "Value")
    
    
    return(hab_long)
  })
  
  getChemicalData <- reactive({
    url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQ33Gy0sgu-QtTTJBOo1anC42maeb3a-Dx6Th61z2ZOPCpH-IhRxi_feh8JoJamE2andgEUybZw93J8/pub?gid=0&single=true&output=csv"
    chem_data <- read.csv(url)
    #chem_data <- read.csv("C:/Users/tuong/OneDrive/Desktop/25vwin.csv")
    
    chem_data$date <- as.yearmon(paste(chem_data$year, chem_data$month), "%Y %m")
    
    chem_data <- chem_data %>%
      rename_with(.cols = c(`NH3.N`,`NO3.N`,`Total.P`,`PO4`, `Turb`, `TSS`, `Cond`, `Alk`, `pH`), .fn = ~ paste0("chem_", .x))
    
    chem_long <- chem_data %>%
      tidyr::pivot_longer(cols = starts_with("chem_"), names_to = "Variable", values_to = "Value")
    
    return(chem_long)
  })
  
  # 2. UPDATE based on user input on each tab
  # Showing the choices based on each individual dataset
  
  
  observeEvent(input$tabs, {
    if (input$tabs == "bio") {
      bio_data <- getBioData() # Example of getting reactive data
      # So, updateSelectInput takes the current session the user is on once they select the tab
      # (if input$tabs == “bio”) Then, it looks at the name of the input which for bio is site_name if you remember from the UI.
      # Lastly, it looks at what to replace the NULL with, choices are all of the unique site names from the dataset.
      updateSelectInput(session, "site_name", choices = unique(bio_data$site.name))
      updateSelectInput(session, "variable", choices = unique(bio_data$Variable))
    }
  })
  
  observeEvent(input$tabs, {
    if (input$tabs == "habitat") {
      hab_data <- getHabitatData()
      updateSelectInput(session, "site_name_hab", choices = unique(hab_data$name))
      updateSelectInput(session, "variable_hab", choices = unique(hab_data$Variable))
    }
  })
  
  observeEvent(input$tabs, {
    if (input$tabs == "chemical") {
      chem_data <- getChemicalData()
      updateSelectInput(session, "site_name_chem", choices = unique(chem_data$site.name))
      updateSelectInput(session, "variable_chem", choices = unique(chem_data$Variable))
      # This x_var and y_var relates to the scatterplot later on that is only feasible in this project for the chemical data
      updateSelectInput(session, "x_var_chem", choices = unique(chem_data$Variable))
      updateSelectInput(session, "y_var_chem", choices = unique(chem_data$Variable))
    }
  })
  
  
  # 3. FILTER based on the dataset and user input!
  
  # - Filtered Biological Data by site
  
  filteredBioData <- reactive({
    req(input$site_name, input$variable, input$data_view) # This req means that the code requires input first from the user and the ui.

    # This takes the reactive data and then filters it. Here we use site.name %in% input$site_name to say 
    # that we want to allow the user to select as many sites as they want and all of those sites will be filtered out. 
    bio_data <- getBioData() %>%
      filter(site.name %in% input$site_name, Variable == input$variable)

    # If the plots are just time plots and box and whisker plots, it is okay to keep it filtered just 
    # by site name and chosen variables
    if (input$data_view == "All") {
      return(bio_data)

    # Here, we filter it and take the average to use for average plots
    } else if (input$data_view == "Averages") {
      avg_data <- bio_data %>%
        group_by(site.name, Variable) %>%
          summarise(
            mean = mean(Value, na.rm = TRUE),
            sd = sd(Value, na.rm = TRUE),
            .groups = "drop"
          )
      return(avg_data)
    }
  })
  
  
  # - Filtered Habitat Data by site & variable
  filteredHabitatData <- reactive({
    req(input$site_name_hab, input$variable_hab, input$data_view_hab)
    
    hab_data <- getHabitatData() %>%
      filter(name %in% input$site_name_hab, Variable == input$variable_hab)
    
    if (input$data_view_hab == "All") {
      return(hab_data)
      
    } else if (input$data_view_hab == "Averages") {
      avg_data <- hab_data %>%
        group_by(name, Variable) %>%
        summarise(
          mean = mean(Value, na.rm = TRUE),
          sd = sd(Value, na.rm = TRUE),
          .groups = "drop"
        )
      return(avg_data)
    }
  })
  
  
  # - Filtered Chemical Data by site & variable
  filteredChemicalData <- reactive({
    req(input$site_name_chem, input$variable_chem, input$data_view_chem)
    
    chem_data <- getChemicalData() %>%
      filter(site.name %in% input$site_name_chem, Variable == input$variable_chem)
    
    if (input$data_view_chem == "All") {
      return(chem_data)
      
    } else if (input$data_view_chem == "Averages") {
      avg_data <- chem_data %>%
        group_by(site.name, Variable) %>%
        summarise(
          mean = mean(Value, na.rm = TRUE),
          sd = sd(Value, na.rm = TRUE),
          .groups = "drop"
        )
      return(avg_data)
    }
  })
  
  
  # 4. Visualizing the impact of Hurricane Helene
  
   # Making a before & after hurricane helene data split for the data that is also reactive
# This requires a bit more data manipulation than the other plots
  bioMedianSplit <- reactive({
    req(input$site_name, input$variable)

    # Here we use as.yearmon to define the date of HH
    cutoff_date <- as.yearmon("2024-09")

    # Once again using %in% to allow for as many sites as the user wants
    bio_data <- getBioData() %>%
      filter(site.name %in% input$site_name, Variable == input$variable) %>%
    # mutate is the function that allows us to define the different periods
      mutate(
        period = ifelse(date < cutoff_date, "Before Sept 2024", "After Sept 2024"),
        period = factor(period, levels = c("Before Sept 2024", "After Sept 2024"))  
      ) %>% # Fixing the order of the barplot (so before is first (left) and then after is on the right)
      group_by(site.name, period) %>%
      summarise(median_value = median(Value, na.rm = TRUE), .groups = "drop") # using the median rather than mean
    # beacuse of skewed data
    
    return(bio_data)
  })
  
  # Splitting up Habitat data before vs after HH
  habMedianSplit <- reactive({
    req(input$site_name_hab, input$variable_hab)
    
    cutoff_date <- as.yearmon("2024-09")
    
    hab_data <- getHabitatData() %>%
      filter(name %in% input$site_name_hab, Variable == input$variable_hab) %>%
      mutate(period = ifelse(date < cutoff_date, "Before Sept 2024", "After Sept 2024"),
             period = factor(period, levels = c("Before Sept 2024", "After Sept 2024"))  
      ) %>%
      group_by(name, period) %>%
      summarise(median_value = median(Value, na.rm = TRUE), .groups = "drop")
    
    return(hab_data)
  })
  
  # Splitting up Chemical data before and after HH
  
  chemMedianSplit <- reactive({
    req(input$site_name_chem, input$variable_chem)
    
    cutoff_date <- as.yearmon("2024-09")
    
    chem_data <- getChemicalData() %>%
      filter(site.name %in% input$site_name_chem, Variable == input$variable_chem) %>%
      mutate(period = ifelse(date < cutoff_date, "Before Sept 2024", "After Sept 2024"),
             period = factor(period, levels = c("Before Sept 2024", "After Sept 2024"))  
      ) %>%
      group_by(site.name, period) %>%
      summarise(median_value = median(Value, na.rm = TRUE), .groups = "drop")
    
    return(chem_data)
  })
  
  # 4.9) Correlation Matrices for Habitat & Chemical data
  # Correlation matrix for the habitat data 
  habCorr <- reactive({
    # reads in the original dataset
    url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vT4PEi7EUwv1ED1bD8Dv1ZOKthQn_K7IOGASXlM7Srcp9SvBp_EBIJ4XvLUQUt7qTml-PE4R2f1eJWs/pub?output=csv"
    hab_data <- read.csv(url)
    # Creates the matrix using only the relevant columns from the dataset 
    correlations <- cor(hab_data[, c("channel.modification", "instream.habitat", "bottom.substrate", "pool.variety", "riffle.habitats", "bank.stability.and.vegetation", "light.penetration", "riparian.vegetative.zone.width", "total")], use = "pairwise.complete.obs")
    return(correlations)
  })

  # Correlation matrix for the chemical data
  chemCorr <- reactive({
    chem_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQ33Gy0sgu-QtTTJBOo1anC42maeb3a-Dx6Th61z2ZOPCpH-IhRxi_feh8JoJamE2andgEUybZw93J8/pub?gid=0&single=true&output=csv")
    correlations <- cor(chem_data[, c("NH3.N", "NO3.N", "Total.P", "PO4", "Turb", "TSS", "Cond", "pH")], use = "pairwise.complete.obs")
    return(correlations)
  })

   # 5. Code for Plots (CHANGE based on user input)
  
  
  # Creating color codes for the plots that match up with the LJEA’s colors, I wanted each variable to have a unique color

  
  bio_colors <- c("bio_ncbi" = "#20B2AA", "bio_bioclassification" = "#FFDAB9")
  
  hab_colors <- c(
    "hab_channel.modification" = "#AFEEEE", 
    "hab_instream.habitat" = "#FBCEB1",
    "hab_bottom.substrate" = "#FFDAB9", 
    "hab_pool.variety" = "#B0E0E6",
    "hab_riffle.habitats" = "#E0FFFF", 
    "hab_bank.stability.and.vegetation" = "#FFE4B5",
    "hab_light.penetration" = "#87CEFA", 
    "hab_riparian.vegetative.zone.width" = "#F5DEB3",
    "hab_total" = "#ADD8E6"
  )
  chem_colors <- c(
    "chem_NH3.N" = "#FFB6C1", "chem_NO3.N" = "#B0E0E6", "chem_Total.P" = "#FFE4B5",
    "chem_PO4" = "#AFEEEE", "chem_Turb" = "#FFDAB9", "chem_TSS" = "#E6E6FA",
    "chem_Cond" = "#F08080", "chem_Alk" = "#E0FFFF", "chem_pH" = "#87CEFA"
  )
  
  #my_colors <- c("#20B2AA", "#FFDAB9", "#87CEFA", "#FBCEB1", "#AFEEEE")
  
  # BIOLOGICAL data PLOTS
  # Bio time series plot (line plot)
  output$bioLinePlot <- renderPlot({
    data <- filteredBioData() # This takes the filtered data done earlier with time-series capability

    # color = site.name means each site will have a unique color
    ggplot(data, aes(x = date, y = Value, color = site.name, group = site.name)) +
      geom_line() +
      geom_point() +
      scale_color_viridis_d() + # This color scale is easier to see for those with visual impairments
      geom_vline(xintercept = 2024.667, color = "#20B2AA")+ # This is the line of the date of HH
      annotate(
        "text",
        x = 2024.667,               # same as vertical line
        y = max(data$Value, na.rm = TRUE)/2,  # hoping for a halfway 
        label = "Hurricane Helene",
        color = "#20B2AA",
        angle = 90,                # vertical text
        vjust = -0.5,              # tweak vertical positioning
        hjust = 0                  # align text to line
      ) +
      labs(title = paste("Time Series for Biological Data at Selected Site(s)"),
           x = "Date", y = "Value", color = "Site") +
      theme_minimal()+
      theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))
  })
  
  # bio general data distribution plot (3 different plots nested based on user input)
  output$bioPlot <- renderPlot({
    data <- filteredBioData()

    # Displays a boxplot if the user input is “All”
    if (input$data_view == "All") {
      # Boxplot for distribution of all values
      ggplot(data, aes(x = site.name, y = Value, fill = Variable)) + # x axis - sites user inputted; y-axis numeric values of variable chosen; fill sets the variable to the one chosen
        geom_boxplot() +  # uses geom_boxplot
        scale_fill_manual(values = bio_colors)+
        labs(title = "Biological Data Distribution by Site",
             x = "Site Name", y = "Value") +
        theme_minimal()+
        theme(
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 1) # Adds a black border around the panel
        )
      
    } else if (input$data_view == "Averages") {
      # Bar chart for average value at the selected site
      ggplot(data, aes(x = site.name, y = mean, fill = Variable)) +
        geom_col(position = "dodge") +
        geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                      position = position_dodge(0.9), width = 0.2) +
        scale_fill_manual(values = bio_colors) + # sets colors to the codes defined earlier
        labs(title = "Average Biological Data by Site",
             x = "Site Name", y = "Average Value") +
        theme_minimal()+
        theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))
        
    } else if (input$data_view == "Hurricane Helene") {
      data2 <- bioMedianSplit()  # Taking the reactive data that split the data to before hh and after
      
      # The median comparison plot for visualizing impact of HH
      ggplot(data2, aes(x = site.name, y = median_value, fill = period)) +
        geom_col(position = "dodge") + # maintains grouped position of plots
        scale_fill_manual(values = c("Before Sept 2024" = "#FFE4B5",  # Unique colors here too 
                                     "After Sept 2024" = "#20B2AA")) +  
        labs(title = "Median Biological Values Before vs After Hurricane Helene",
             x = "Site Name", y = "Median Value", fill = "Period") +
        theme_minimal()+ # fill = “Period” changes the color based on period
        theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))
    }
    
  })
  
  # Biological Tab Output Summary
  # We ended up commenting this out for the actual app because there were issues
  
  output$bioSummary <- DT::renderDataTable({
    data <- filteredBioData()
    DT::datatable(summary(data), caption = "Biological Data Summary")
  })
  
  
  # HABITAT Tab PLOTS
  
  
  # Time series habitat plot
  output$habLinePlot <- renderPlot({
    data <- filteredHabitatData()
    
    ggplot(data, aes(x = date, y = Value, color = name, group = name)) +
      geom_line() +
      geom_point() +
      scale_color_viridis_d() +
      geom_vline(xintercept = 2024.667, color = "#20B2AA")+
      annotate(
        "text",
        x = 2024.667,               # same as vertical line
        y = max(data$Value, na.rm = TRUE)/2,  # hoping for a halfway 
        label = "Hurricane Helene",
        color = "#20B2AA",
        angle = 90,                # vertical text
        vjust = -0.5,              # tweak vertical positioning
        hjust = 0                  # align text to line
      ) +
      labs(title = paste("Time Series for Habitat Assessment Data at Selected Site(s)"),
           x = "Date", y = "Value", color = "Site") +
      theme_minimal()+
      theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))
  })
  
  
  # Data distribution plot for habitat data
  output$habitatPlot <- renderPlot({
    data <- filteredHabitatData()
    
    if (input$data_view_hab == "All") {
      # Boxplot for distribution of all values
      ggplot(data, aes(x = name, y = Value, fill = Variable)) +
        geom_boxplot() +
        scale_fill_manual(values = hab_colors)+
        labs(title = "Habitat Data Distribution by Site (Boxplot)",
             x = "Site Name", y = "Value") +
        theme_minimal()+
        theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))
      
    } else if (input$data_view_hab == "Averages") {
      # Bar chart for average value
      ggplot(data, aes(x = name, y = mean, fill = Variable)) +
        geom_col(position = "dodge") +
        geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                      position = position_dodge(0.9), width = 0.2) +
        scale_fill_manual(values = hab_colors)+
        labs(title = "Average Habitat Assessment Data by Site",
             x = "Site Name", y = "Average Value") +
        theme_minimal()+
        theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))
    } else if (input$data_view_hab == "Hurricane Helene") {
      data2 <- habMedianSplit()
      
      ggplot(data2, aes(x = name, y = median_value, fill = period)) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = c("Before Sept 2024" = "#FFE4B5",   
                                     "After Sept 2024" = "#20B2AA")) +  
        labs(title = "Median Habitat Assessment Data Values Before vs After Hurricane Helene",
             x = "Site Name", y = "Median Value", fill = "Period") +
        theme_minimal()+
        theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))
    }
    
  })
  
  output$habitatSummary <- DT::renderDataTable({
    data <- filteredHabitatData()
    DT::datatable(summary(data), caption = "Habitat Data Summary")
  })


  # Renders the Habitat Correlation plot
  output$habCorrPlot <- renderPlot({
    # Simply uses the corrplot utility in the library to create the plot based on the matrix
    corrplot(habCorr())
  })
  
  
  # CHEMICAL Tab Outputs/PLOTS
  
  
 # Rendering chemical scatterplot for selected variables(optional regression line) :D
  output$chemScatterPlot <- renderPlot({
    # Req ensures both X and Y input variables are selected before proceeding    
    req(input$x_var_chem, input$y_var_chem)
      
    # Reads in the original wide data format for scatterplot use
    url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQ33Gy0sgu-QtTTJBOo1anC42maeb3a-Dx6Th61z2ZOPCpH-IhRxi_feh8JoJamE2andgEUybZw93J8/pub?gid=0&single=true&output=csv"
    
    chem_data <- read.csv(url)
    
    chem_data <- chem_data %>%
      
      rename_with(.cols = c(`NH3.N`,`NO3.N`,`Total.P`,`PO4`, `Turb`, `TSS`, `Cond`, `Alk`, `pH`), .fn = ~ paste0("chem_", .x))
    
    # Preparing data for the scatterplot:
    # - Keep only rows where both selected variables are NOT NA
    # - Select only the columns for X and Y axes (renamed to x_value, y_value)
    scatter_data <- chem_data %>%
      filter(!is.na(.data[[input$x_var_chem]]), !is.na(.data[[input$y_var_chem]])) %>%
      select(x_value = all_of(input$x_var_chem), y_value = all_of(input$y_var_chem))
    
    #  Creates the base scatterplot using ggplot2
    p <- ggplot(scatter_data, aes(x = x_value, y = y_value)) +
      geom_point() + #add scatter points
      labs(
        x = input$x_var_chem, #using labels for the y and x axis
        y = input$y_var_chem,
        title = "Chemical Values Scatterplot"
      ) +
      theme_minimal()+ # Clean, minimal background
      theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))

     # If the checkbox to add regression is selected, overlay a linear regression line 
    if (input$add_regression) {
      # This is to make a linear model taken from old labs. coef(of the regression)[2] and [1] give you the slope and intercept.
      fit <- lm(y_value ~ x_value, data = scatter_data)
      slope <- coef(fit)[2]
      intercept <- coef(fit)[1]
      equation <- paste0("y = ", round(slope, 3), "x + ", round(intercept, 3)) # This develops the equation as y=mx+b format and rounds the slope
      
      # Add regression line and equation label; added it to the left-top corner because a majority of the regressions are positive in direction
      p <- p + # method = "lm" fits a linear model
        geom_smooth(method = "lm", se = TRUE, color = "#20B2AA") + # This adds just the line 
        annotate("text", #this annotates the line with the equation and sets it on the plot
                 x = min(scatter_data$x_value, na.rm = TRUE), # This aligns it so that the text is where the minimum value on the plot is (left-side)
                 y = max(scatter_data$y_value, na.rm = TRUE), # This aligns it so that it is as low or as high as the max value (upper side)
                 label = equation,
                 hjust = 0, #space from the left
                 vjust = 1, #space from the top that is adjusted
                 size = 6,
                 color = "#20B2AA") # sets a custom line color for consistency
    }
    
    p
  })
  
  output$chemLinePlot <- renderPlot({
    data <- filteredChemicalData()
    
    # Store base plot in `p`
    p <- ggplot(data, aes(x = date, y = Value, color = site.name, group = site.name)) +
      geom_line() +
      geom_point() +
      scale_color_viridis_d() +
      geom_vline(xintercept = 2024.667, color = "#20B2AA") +
      annotate( # using annotate to add text to plots
        "text",
        x = 2024.667,               # same as vertical line
        y = max(data$Value, na.rm = TRUE)/2,  # hoping for halfway of the plot (using max) so the text doesn't look funny 
        label = "Hurricane Helene",
        color = "#20B2AA", #setting a standard color code
        angle = 90,                # vertical text
        vjust = -0.5,              # tweak vertical positioning
        hjust = 0                  # align text to line
      ) +
      labs(
        title = "Time Series for Chemical Data at Selected Site(s)",
        x = "Date", y = "Value", color = "Site"
      ) +
      theme_minimal() +
      theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))
    
    # Add pristine avg line if the input from the user matches a variable defined in the pristine averages global key. 
    # Also use !NA because some chemical data variables don't have a value for pristine avg
    if (input$show_avg && input$variable_chem %in% names(pristine_avgs)) {
      pristine_value <- pristine_avgs[[input$variable_chem]]
      regional_value <- regional_avgs[[input$variable_chem]]
      
      if (!is.na(pristine_value)) { # used geom_hline and defined the line to have no slope (y-int at pristine_value) and then all y's at that point
        p <- p + geom_hline(yintercept = pristine_value, linetype = "dashed", color = "#F08080") + # Set a pastelish color code that matches with LJEA branding
          annotate("text", x = min(data$date), y = pristine_value, label = "Pristine Avg", vjust = -1, hjust = 0, color = "#F08080", size = 3)
      } # the -1 makes it so that the text lays on top of the value 
      
      if (!is.na(regional_value)) {
        p <- p + geom_hline(yintercept = regional_value, linetype = "dotdash", color = "darkblue") +
          annotate("text", x = min(data$date), y = regional_value, label = "Regional Avg", vjust = 1.5, hjust = 0, color = "darkblue", size = 3)
      } # the 1.5 makes it so the text lays below the line value 
    }
    
    # Return final plot
    p
  })
  
  
  
  output$chemPlot <- renderPlot({
    data <- filteredChemicalData()
    
    if (input$data_view_chem == "All") {
      # Boxplot for distribution of all values
      ggplot(data, aes(x = site.name, y = Value, fill = Variable)) +
        geom_boxplot() +
        scale_fill_manual(values = chem_colors)+
        labs(title = "Chemical Data Distribution by Site (Boxplot)",
             x = "Site Name", y = "Value") +
        theme_minimal()+
        theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))
      
    } else if (input$data_view_chem == "Averages") {
      # Bar chart for average value
      ggplot(data, aes(x = site.name, y = mean, fill = Variable)) +
        geom_col(position = "dodge") +
        geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                      position = position_dodge(0.9), width = 0.2) +
        scale_fill_manual(values = chem_colors)+
        labs(title = "Average Chemical Data by Site",
             x = "Site Name", y = "Average Value") +
        theme_minimal()+
        theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))
    } else if (input$data_view_chem == "Hurricane Helene") {
      data2 <- chemMedianSplit()
      
      ggplot(data2, aes(x = site.name, y = median_value, fill = period)) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = c("Before Sept 2024" = "#FFE4B5",   
                                     "After Sept 2024" = "#20B2AA")) +  
        labs(title = "Median Chemical Data Values Before vs After Hurricane Helene",
             x = "Site Name", y = "Median Value", fill = "Period") +
        theme_minimal()+
        theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))
    }
  })
  
  output$chemSummary <- DT::renderDataTable({
    data <- filteredChemicalData()
    DT::datatable(summary(data), caption = "Chemical Data Summary")
  })
  
  output$chemCorrPlot <- renderPlot({
    corrplot(chemCorr())
  })
}

