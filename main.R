library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(reshape2)
library(httr)
library(jsonlite)

fetch_planets_data <- function() {
  res <- GET("http://localhost:8000/planets")
  if (status_code(res) == 200) {
    data <- content(res, as = "text")
    data <- fromJSON(data)
    return(data)
  } else {
    stop("Failed to fetch data from the API")
  }
}

ui <- fluidPage(
  titlePanel("Planetary Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "Select X-axis variable:", choices = NULL),
      selectInput("yvar", "Select Y-axis variable:", choices = NULL),
      selectInput("detection", "Select Detection Method:", choices = NULL, multiple = TRUE),
      selectInput("facility", "Select Discovery Facility:", choices = NULL, multiple = TRUE),
      sliderInput("yearRange", "Select Year Range:", min = 0, max = 0, value = c(0, 0), step = 1),
      sliderInput("massRange", "Select Mass Range:", min = 0, max = 0, value = c(0, 0), step = 1),
      sliderInput("maxPlanets", "Max Number of Planets to Display:", min = 10, max = 10000, value = 1000, step = 100),
      checkboxInput("habitable", "Show Habitable Planets Only", value = FALSE),
      checkboxInput("errorBars", "Include Error Bars", value = FALSE),
      selectInput("plotType", "Select Plot Type:", choices = c("Scatter Plot", "Histogram", "Density Plot", "Box Plot")),
      actionButton("getData", "Predict planet")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("mainPlot")),
        tabPanel("Summary Statistics", verbatimTextOutput("summaryStats")),
        tabPanel("Data Table", DTOutput("dataTable")),
        tabPanel("Correlation Matrix", plotOutput("corrMatrix")),
        tabPanel("API Data", verbatimTextOutput("apiData"))
      )
    )
  ),
  fluidRow(
    p("Data Source: Exoplanet Archive"),
    p("Developed by: Your Name")
  )
)

server <- function(input, output, session) {
  
  planets <- reactiveVal()
  
  observe({
    tryCatch({
      print("Fetching data from API...")
      data <- fetch_planets_data()
      print("Data fetched successfully")
      planets(data)
      
      updateSelectInput(session, "xvar", choices = names(data))
      updateSelectInput(session, "yvar", choices = names(data))
      updateSelectInput(session, "detection", choices = unique(data$P_DETECTION))
      updateSelectInput(session, "facility", choices = unique(data$P_DISCOVERY_FACILITY))
      updateSliderInput(session, "yearRange", min = min(data$P_YEAR), max = max(data$P_YEAR), value = range(data$P_YEAR))
      updateSliderInput(session, "massRange", min = min(data$P_MASS, na.rm = TRUE), max = max(data$P_MASS, na.rm = TRUE), value = range(data$P_MASS, na.rm = TRUE))
      
    }, error = function(e) {
      print(paste("Error fetching data: ", e$message))
      showModal(modalDialog(
        title = "Error",
        paste("Failed to fetch data from the API:", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
    })
  })
  
  filteredData <- reactive({
    data <- planets()
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
    
    if(nrow(data) > input$maxPlanets) {
      set.seed(123)
      data <- data %>% sample_n(input$maxPlanets)
    }
    
    return(data)
  })
  
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
  
  output$summaryStats <- renderPrint({
    data <- filteredData()
    summary(data)
  })
  
  output$dataTable <- renderDT({
    data <- filteredData()
    datatable(data)
  })
  
  # Matrice de correlation
  output$corrMatrix <- renderPlot({
    data <- filteredData()
    numericData <- data[sapply(data, is.numeric)]
    numericData <- na.omit(numericData)
    if (input$habitable) {
      numericData <- numericData[data$P_HABITABLE == 1, ]
    }
    corr <- cor(numericData, use = "complete.obs")
    ggplot(melt(corr), aes(Var1, Var2, fill = value)) +
      geom_tile() +
      geom_text(aes(label = round(value, 2)), size = 3) +  # Coefficient de corrélation
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
      theme_minimal()
  })
  
  apiData <- eventReactive(input$getData, {

    req <- tryCatch({
    response <- GET("http://localhost:8000/prediction")
    stop_for_status(response)
    data <- content(response, "parsed")

    prob_non_habitable <- data[[1]]$`0`
    prob_habitable <- data[[1]]$`1`
    

    paste("The probability of the planet being habitable is ",prob_non_habitable, "and the probability of the planet being non-habitable is ", prob_habitable)
  }, error = function(e) {
    paste("Not enough data to make a prediction")
  })
  req
})
  
  output$apiData <- renderText({
    apiData()
  })
}

runApp(shinyApp(ui = ui, server = server))
