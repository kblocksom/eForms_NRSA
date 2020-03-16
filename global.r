library(shiny)
library(shinyjs)
# library(dplyr)
library(magrittr)
library(purrr)
# library(Hmisc)
library(RJSONIO)
library(stringr)
library(writexl)
library(zip)
library(shinyBS)

source('eForms_NRSA/functions/eFormsParseJSON_basic.r')
source('eForms_NRSA/functions/eFormsParseJSONtext.r') 
source('eForms_NRSA/functions/eFormsOrganizeData_byTable.r')
source('eForms_NRSA/functions/karenParseEVJ.R')


metadata <- readRDS("eForms_NRSA/data/metadata.rds")