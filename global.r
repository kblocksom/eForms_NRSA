library(shiny, warn.conflicts = FALSE)
library(shinyjs, warn.conflicts = FALSE)
#library(dplyr)
library(magrittr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
#library(Hmisc)
library(RJSONIO)
library(stringr)
library(writexl)
library(zip, warn.conflicts = FALSE)
library(shinyBS)

source('eForms_NRSA/functions/eFormsParseJSON_basic.r')
source('eForms_NRSA/functions/eFormsParseJSONtext.r') 
source('eForms_NRSA/functions/eFormsOrganizeData_byTable.r')
source('eForms_NRSA/functions/karenParseEVJ.R')


metadata <- readRDS("data/metadata.rds")

