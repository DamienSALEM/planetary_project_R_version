# install.packages("shiny")
# install.packages("ggplot2")
# install.packages("data.table")
library(shiny)
library(ggplot2)
library(data.table)

# Lire le fichier CSV avec data.table
planets <- fread("api/flaskapp/data/hwc.csv", encoding = "UTF-8")

# Convertir les colonnes de caractères en UTF-8 explicitement
convert_to_utf8 <- function(df) {
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      df[[col]] <- iconv(df[[col]], from = "latin1", to = "UTF-8", sub = "")
      # Convertir les colonnes de caractères en facteurs
      df[[col]] <- as.factor(df[[col]])
    }
  }
  return(df)
}

planets <- convert_to_utf8(planets)

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Comparaison des propriétés des planètes"),

  sidebarLayout(
    sidebarPanel(
      selectInput("property", "Choisissez une propriété:", 
                  choices = colnames(planets)),
      hr(),
      helpText("Sélectionnez une propriété pour comparer les planètes.")
    ),

    mainPanel(
      plotOutput("propertyPlot")
    )
  )
)

# Serveur
server <- function(input, output) {
  output$propertyPlot <- renderPlot({
    ggplot(planets, aes_string(x = "P_NAME", y = input$property)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(title = paste("Comparaison de", input$property, "entre les planètes"), 
           x = "Planète", 
           y = input$property)
  })
}

# Exécuter l'application
runApp(shinyApp(ui = ui, server = server))