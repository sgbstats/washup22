library(shiny)
library(tidyverse)
library(shinyWidgets)
library(ggsankey)
library(htmltools)
library(flextable)

load("Data/Data.RDa")

source("Server.R", local = TRUE)
source("ui.R", local = TRUE)

shinyApp(ui = ui, server = server)
