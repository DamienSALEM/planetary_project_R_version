# plumber.R

library(mongolite)
library(dotenv)

# Load environment variables from .env file
dotenv::load_dot_env(file = "E:/dev/R/planetary_project_R_version/.env")

# Get the MongoDB URL from the environment variables
mongo_url <- Sys.getenv("url")
collection_name <- "planets"

# Create a connection to MongoDB
mongo_conn <- mongo(collection = collection_name, url = mongo_url)

# Define an endpoint to get data
#* @get /planets
function() {
  data <- mongo_conn$find("{}")
  return(data)
}
