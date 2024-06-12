# install.packages("httr")
# install.packages("jsonlite")
# install.packages("data.table")
# install.packages("caret")
# install.packages("sparklyr")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("purrr")
# install.packages("stringr")

library(httr)
library(jsonlite)
library(data.table)
library(caret) # ou tout autre package pour charger et utiliser le modèle

source('api/test_prob/planets.r') 

# Configuration du chemin du modèle
model_path <- "api/bestmodel/"

# Fonction pour envoyer une requête au service de scan
probe_request <- function() {
  res <- GET(paste0("http://localhost:5000/lastscan"))
  content_text <- content(res, "text", encoding = "UTF-8")
  
  # Remplacer NaN par null
  content_text <- gsub("NaN", "null", content_text)
  
  # Convertir en liste
  data <- fromJSON(content_text)
  return(data)
}

# Fonction pour utiliser le modèle de prédiction
prediction_model <- function(input_data) {
  # Chargement du modèle
  model <- readRDS(model_path)
  
  # Prétraitement des données
  preprocessed_data <- use_planets(input_data) # Assurez-vous que use_planets est défini dans R
  
  # Conversion des données en matrice
  preprocessed_matrix <- as.matrix(preprocessed_data)
  
  # Prédictions
  predictions <- predict(model, newdata = preprocessed_matrix, type = "prob")
  
  print(predictions)
  return(predictions)
}

# Boucle principale
tryCatch({
  # x <- 0
  # while (x < 20) {
  #   # pred <- prediction_model(res)
  #   x <- x + 1
  # }
  
  res <- probe_request()
  print(res)
}, error = function(e) {
  cat("Erreur : ", conditionMessage(e), "\n")
})
