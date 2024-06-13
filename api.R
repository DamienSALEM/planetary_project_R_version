library(plumber)

# Create a plumber router by referencing the plumber.R file
r <- plumb("plumber.R")

# Run the API
r$run(port = 8000)
