# plumber.R

library(mongolite)
library(dotenv)
library(httr)
library(jsonlite)
library(data.table)
library(caret)
library(randomForest)
source('C:/code/repo/IPSSI/s10Y2/planetary_project_R_version/planets.r')

# Load environment variables from .env file
dotenv::load_dot_env(file = "C:/code/repo/IPSSI/s10Y2/.env")

# Get the MongoDB URL from the environment variables
mongo_url <- Sys.getenv("url")
collection_name <- "planets"

# Create a connection to MongoDB
mongo_conn <- mongo(collection = collection_name, url = mongo_url)

model_path <- "model_planet.rds"

probe_request <- function() {
  pipeline <- '[{ "$sample": { "size": 1 } }]'
  res <- mongo_conn$aggregate(pipeline)
  print(res)

  content_text <- gsub("NaN", "null", toJSON(res))

  data <- fromJSON(content_text)
  data <- as.data.frame(data)

  cols_to_keep <- c("P_HABITABLE", "P_HABZONE_OPT", "P_ESI", "P_HABZONE_CON",
                  "P_OMEGA_ERROR_MAX", "S_LOG_LUM_ERROR_MAX", "S_LOG_G",
                  "P_ECCENTRICITY_ERROR_MAX", "P_TEMP_SURF", "P_TEMP_EQUIL",
                  "S_LOG_LUM", "S_MASS", "S_DISTANCE", "P_TYPE")


  for (col in cols_to_keep) {
      if (!col %in% names(data)) {
          data[[col]] <- NA
      }
  }
  print(data)

  # res <- res[, cols_to_keep]
  return(res)
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


# Define an endpoint to get data
#* @get /planets
function() {
  data <- mongo_conn$find("{}")
  return(data)
}

#* @get /prediction
function(){
  res <- probe_request()
  # pred <- prediction_model(res)
  return(res)
}