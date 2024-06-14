# plumber.R

library(mongolite)
library(dotenv)
library(httr)
library(jsonlite)
library(data.table)
library(caret)
library(randomForest)
source('planets.r')

dotenv::load_dot_env(file = ".env")

mongo_url <- Sys.getenv("url")
collection_name <- "planets"

mongo_conn <- mongo(collection = collection_name, url = mongo_url)

model_path <- "model_planet.rds"

probe_request <- function() {
  pipeline <- '[{ "$sample": { "size": 1 } }]'
  res <- mongo_conn$aggregate(pipeline)

  data <- res

  cols_to_keep <- c("P_HABZONE_OPT", "P_ESI", "P_HABZONE_CON",
                   "S_LOG_LUM_ERROR_MAX", "S_LOG_G", "P_TEMP_SURF", "P_TEMP_EQUIL",
                  "S_LOG_LUM", "S_MASS", "S_DISTANCE", "P_TYPE")

 for (col in cols_to_keep) {
    if (!col %in% names(data)) {
        data[[col]] <- NA
    }
}
  data <- data[, cols_to_keep]

  model <- readRDS(model_path)
  
  preprocessed_data <- use_planets(data)

  
  preprocessed_matrix <- as.matrix(data)
  
  predictions <- predict(model, newdata = data, type = "prob")
  numeric_prediction <- as.numeric(as.character(predictions))
  cat(numeric_prediction)
  

  return(predictions)

}

#* @get /planets
function() {
  data <- mongo_conn$find("{}")
  return(data)
}

#* @get /prediction
function(){
  res <- probe_request()
  return(res)
}