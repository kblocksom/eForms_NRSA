library(shiny)

port <- Sys.getenv('PORT')

shiny::runApp('app.R', host = '0.0.0.0', port = as.numeric(port))
