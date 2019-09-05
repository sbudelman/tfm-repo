library(DT)
library(shiny)
library(shinyjs)
library(shinythemes)
library(shiny.i18n)
library(writexl)

source("./code/functions.R")
source("./code/loadData.R")
source("helpers.R")

# Translations https://appsilon.com/internationalization-of-shiny-apps-i18n/
translator <- Translator$new(translation_csvs_path = "translations")
