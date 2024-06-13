

library(httr)
library(jsonlite)
library(data.table)
library(caret) # ou tout autre package pour charger et utiliser le modèle

source('planets.r') 

# Configuration du chemin du modèle
model_path <- "model_planet.rds"

# Fonction pour envoyer une requête au service de scan
probe_request <- function() {
  res <- GET(paste0("http://localhost:8000/planet"))
  content_text <- content(res, "text", encoding = "UTF-8")
  
  # Remplacer NaN par null
  content_text <- gsub("NaN", "null", content_text)
  
  # Convertir en liste
  data <- fromJSON(content_text)
  preprocessed_data <- use_planets(data)
  return(preprocessed_data)
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
  res <- probe_request()
  pred <- prediction_model(res)
  print(pred)
}, error = function(e) {
  cat("Erreur : ", conditionMessage(e), "\n")
})
