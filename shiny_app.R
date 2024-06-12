library(shiny)
library(ggplot2)
library(httr)
library(jsonlite)

# Function to fetch data from the API
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

# Fetch the data once at startup and store it in a reactive value
planets_data <- reactiveVal()

observe({
  tryCatch({
    print("Fetching data from API...")  # Debugging statement
    data <- fetch_planets_data()
    print("Data fetched successfully")  # Debugging statement
    planets_data(data)
  }, error = function(e) {
    print(paste("Error fetching data: ", e$message))  # Debugging statement
    showModal(modalDialog(
      title = "Error",
      paste("Failed to fetch data from the API:", e$message),
      easyClose = TRUE,
      footer = NULL
    ))
  })
})

# User Interface
ui <- fluidPage(
  titlePanel("Comparaison des propriétés des planètes"),

  sidebarLayout(
    sidebarPanel(
      uiOutput("property_selector"),
      hr(),
      helpText("Sélectionnez une propriété pour comparer les planètes.")
    ),

    mainPanel(
      plotOutput("propertyPlot")
    )
  )
)

# Server
server <- function(input, output, session) {
  output$property_selector <- renderUI({
    req(planets_data())
    print("Rendering property selector UI...")  # Debugging statement
    selectInput("property", "Choisissez une propriété:",
                choices = names(planets_data()))
  })

  output$propertyPlot <- renderPlot({
    req(planets_data())
    req(input$property)
    print(paste("Rendering plot for property:", input$property))  # Debugging statement

    ggplot(planets_data(), aes_string(x = "P_NAME", y = input$property)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(title = paste("Comparaison de", input$property, "entre les planètes"),
           x = "Planète",
           y = input$property)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
