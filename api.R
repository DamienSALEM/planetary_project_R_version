library(plumber)

# Create a plumber router by referencing the plumber.R file
r <- plumb("C:/code/repo/IPSSI/s10Y2/planetary_project_R_version/plumber.R")

# Run the API
r$run(port = 8000)
