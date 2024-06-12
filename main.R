library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(reshape2)

# Load the dataset
planets <- read.csv("data/hwc.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Planetary Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "Select X-axis variable:", choices = names(planets)),
      selectInput("yvar", "Select Y-axis variable:", choices = names(planets)),
      selectInput("detection", "Select Detection Method:",
                  choices = unique(planets$P_DETECTION), multiple = TRUE),
      selectInput("facility", "Select Discovery Facility:",
                  choices = unique(planets$P_DISCOVERY_FACILITY), multiple = TRUE),
      sliderInput("yearRange", "Select Year Range:",
                  min = min(planets$P_YEAR), max = max(planets$P_YEAR),
                  value = range(planets$P_YEAR), step = 1),
      sliderInput("massRange", "Select Mass Range:",
                  min = min(planets$P_MASS, na.rm = TRUE), max = max(planets$P_MASS, na.rm = TRUE),
                  value = range(planets$P_MASS, na.rm = TRUE), step = 1),
      sliderInput("maxPlanets", "Max Number of Planets to Display:",
                  min = 10, max = 10000, value = 1000, step = 100),
      checkboxInput("habitable", "Show Habitable Planets Only", value = FALSE),
      checkboxInput("errorBars", "Include Error Bars", value = FALSE),
      selectInput("plotType", "Select Plot Type:",
                  choices = c("Scatter Plot", "Histogram", "Density Plot", "Box Plot"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("mainPlot")),
        tabPanel("Summary Statistics", verbatimTextOutput("summaryStats")),
        tabPanel("Data Table", DTOutput("dataTable")),
        tabPanel("Correlation Matrix", plotOutput("corrMatrix"))
      )
    )
  ),
  fluidRow(
    p("Data Source: Exoplanet Archive"),
    p("Developed by: Your Name")
  )
)

# Define server logic
server <- function(input, output) {

  # Reactive dataset based on filters
  filteredData <- reactive({
    data <- planets
    if (!is.null(input$detection)) {
      data <- data[data$P_DETECTION %in% input$detection, ]
    }
    if (!is.null(input$facility)) {
      data <- data[data$P_DISCOVERY_FACILITY %in% input$facility, ]
    }
    data <- data[data$P_YEAR >= input$yearRange[1] & data$P_YEAR <= input$yearRange[2], ]
    data <- data[data$P_MASS >= input$massRange[1] & data$P_MASS <= input$massRange[2], ]
    if (input$habitable) {
      data <- data[data$P_HABITABLE == 1, ]
    }

    # Limit the number of planets
    if(nrow(data) > input$maxPlanets) {
      set.seed(123)
      data <- data %>% sample_n(input$maxPlanets)
    }

    return(data)
  })

  # Main plot
  output$mainPlot <- renderPlot({
    data <- filteredData()

    if (input$plotType == "Scatter Plot") {
      p <- ggplot(data, aes_string(x = input$xvar, y = input$yvar)) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "lm", color = "blue", se = FALSE) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      if (input$errorBars && !is.null(data$P_MASS_ERROR_MIN) && !is.null(data$P_MASS_ERROR_MAX)) {
        p <- p + geom_errorbar(aes(ymin = P_MASS - P_MASS_ERROR_MIN, ymax = P_MASS + P_MASS_ERROR_MAX), width = 0.2)
      }
      # Calculate and add correlation
      if (is.numeric(data[[input$xvar]]) && is.numeric(data[[input$yvar]])) {
        corr <- cor(data[[input$xvar]], data[[input$yvar]], use = "complete.obs")
        p <- p + annotate("text", x = Inf, y = Inf, label = paste("Correlation:", round(corr, 2)),
                          hjust = 1.1, vjust = 2, size = 5, color = "red")
      }
    } else if (input$plotType == "Histogram") {
      if (is.numeric(data[[input$xvar]])) {
        p <- ggplot(data, aes_string(x = input$xvar)) +
          geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
          theme_minimal()
      } else {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Please select a numeric variable for the x-axis", size = 5) +
          theme_void()
      }
    } else if (input$plotType == "Density Plot") {
      if (is.numeric(data[[input$xvar]])) {
        p <- ggplot(data, aes_string(x = input$xvar)) +
          geom_density(fill = "blue", alpha = 0.5) +
          theme_minimal()
      } else {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Please select a numeric variable for the x-axis", size = 5) +
          theme_void()
      }
    } else if (input$plotType == "Box Plot") {
      if (is.numeric(data[[input$yvar]])) {
        p <- ggplot(data, aes_string(x = input$xvar, y = input$yvar)) +
          geom_boxplot(fill = "blue", alpha = 0.7) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Please select a numeric variable for the y-axis", size = 5) +
          theme_void()
      }
    }

    print(p)
  })

  # Summary statistics
  output$summaryStats <- renderPrint({
    data <- filteredData()
    summary(data)
  })

  # Data table
  output$dataTable <- renderDT({
    data <- filteredData()
    datatable(data)
  })

  # Correlation matrix
  output$corrMatrix <- renderPlot({
    data <- filteredData()
    numericData <- data[sapply(data, is.numeric)]
    numericData <- na.omit(numericData)  # Remove rows with NA values
    if (input$habitable) {
      numericData <- numericData[data$P_HABITABLE == 2, ]
    }
    corr <- cor(numericData, use = "complete.obs")
    ggplot(melt(corr), aes(Var1, Var2, fill = value)) +
      geom_tile() +
      geom_text(aes(label = round(value, 2)), size = 3) +  # Add correlation coefficients
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
