
library(rlang)
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(lubridate)
library(readxl)
library(ggplot2)
library(ggsankey)
library(htmltools)
library(flextable)
library(htmlwidgets)


`%notin%`=Negate(`%in%`)
load("Data/Data.RDa")

yearreg=unique((Data %>% arrange(regcycle))$regcycle) %>% as.character()
source("Server.R")
source("ui.R")
# Run the application 
shinyApp(ui = ui, server = server)
