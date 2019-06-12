library(shiny)

port <- Sys.getenv('PORT')

shiny::runApp('eForms_NRSA', host = '0.0.0.0', port = as.numeric(port))
