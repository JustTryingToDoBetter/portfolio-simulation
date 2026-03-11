# Compatibility entrypoint for shiny::runApp("app").
# The main dashboard app is defined at the project root in app.R.
source("../app.R", chdir = TRUE)
