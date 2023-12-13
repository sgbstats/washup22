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

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: #ff0000;
        font-weight: bold;
      }
    "))
  ),
  tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),
  tabsetPanel(
    tabPanel("Basic Stats", fluid=T,
             sidebarLayout(
               sidebarPanel(
                 radioButtons("year1", "Year", choices = c("2023"="L23", "2022"="L22"), selected = "L23", inline = T),
                 checkboxGroupInput("ward1", "Ward", choices = c("Didsbury East", "Didsbury West", "Ancoats & Beswick" ), selected =  c("Didsbury East", "Didsbury West", "Ancoats & Beswick"), inline = T),
                 radioButtons("group1", "Level", choices = c("Ward", "Polling District"), selected = "Ward", inline = T),
                 pickerInput("regcycle1", "Year registered", 
                             choices = yearreg, selected = yearreg,                 
                             options = list(`actions-box` = TRUE),
                             multiple = T),
                 
                 checkboxInput("prevvoters1", "Previous voters only"),
                 conditionalPanel(condition="input.prevvoters1==1 && input.year1=='L23'",
                                  checkboxGroupInput("votehistory1_23", "Voting history (any of)", choices = c("Local 22"="L22","Local 21"="L21", "Local 19"="L19", "Local 18"="L18", "General 19"="G19", "By-election 22"="B22"), selected = "L22", inline=T)
                 ),
                 conditionalPanel(condition="input.prevvoters1==1 && input.year1=='L22'",
                                  checkboxGroupInput("votehistory1_22", "Voting history (any of)", choices = c("Local 21"="L21", "Local 19"="L19", "Local 18"="L18", "General 19"="G19", "By-election 22"="B22"), selected = "L21", inline=T)
                 ),
                 
                 width=4
               ),
               mainPanel(
                 uiOutput("table1")
               )
             )
    ),
    tabPanel("Turnout", fluid=T,
             sidebarLayout(
               sidebarPanel(
                 radioButtons("year2", "Year", choices = c("2023"="L23", "2022"="L22"), selected = "L23", inline = T),
                 checkboxGroupInput("ward2", "Ward", choices = c("Didsbury East", "Didsbury West", "Ancoats & Beswick" ), selected =  "Didsbury West", inline = T),
                 radioButtons("group2", "Level", choices = c("Ward"="WardName",
                                                             "Polling District"="PollingDistrictCode",
                                                             "Shuttle"="Shuttle2", "Vote Type"="PV2", "Year registered"="regcycle"), selected = "PollingDistrictCode", inline = T),
                 checkboxGroupInput("voters2", "Voter type", choices = c("In Person"=F, "Postal"=T), selected = c(T,F), inline = T),
                 checkboxGroupInput("shuttle2", "Shuttleworth", choices = c("Shuttle"=T, "Non-Shuttle"=F), selected = c(T,F), inline = T),
                 pickerInput("regcycle2", "Year registered", 
                             choices = yearreg, selected = yearreg,                 
                             options = list(`actions-box` = TRUE),
                             multiple = T),
                 checkboxInput("prevvoters2", "Previous voters only"),
                 conditionalPanel(condition="input.prevvoters2==1 && input.year2=='L23'",
                                  checkboxGroupInput("votehistory2_23", "Voting history (any of)", choices = c("Local 22"="L22","Local 21"="L21", "Local 19"="L19", "Local 18"="L18", "General 19"="G19", "By-election 22"="B22"), selected = "L22", inline=T)
                 ),
                 conditionalPanel(condition="input.prevvoters2==1 && input.year2=='L22'",
                                  checkboxGroupInput("votehistory2_22", "Voting history (any of)", choices = c("Local 21"="L21", "Local 19"="L19", "Local 18"="L18", "General 19"="G19", "By-election 22"="B22"), selected = "L21", inline=T)
                 ),
                 checkboxInput("av", "Display overall ward turnout"),
                 width=4
               ),
               mainPanel(
                 plotOutput("distPlot2", width = "100%")
               )
             )
    ),
    tabPanel("Phantom Shuttle", fluid=T,
             sidebarLayout(
               sidebarPanel(
                 radioButtons("year3", "Year", choices = c("2023"="L23", "2022"="L22"), selected = "L23", inline = T),
                 checkboxGroupInput("ward3", "Ward", choices = c("Didsbury East", "Didsbury West", "Ancoats & Beswick" ), selected =  "Didsbury West", inline = T),
                 radioButtons("Box3", "Level", choices = c("PVs in Boxes", "PVs separate", "Ward-wide"), selected = "PVs in Boxes", inline = T),
                 radioButtons("n_pc", "Count or percentage", choices = c("Count", "Percentage Phantom", "Absolute Phantom"), selected = "Count"),
                 width=4,
                 tags$p("Faint part is phantom LD voters: the number of votes for us in the box minus those on the shuttle that voted (who are in block colour)"),
                 tags$p("For percentage phantom, it is calculated as the 100%-(number in the shuttle/number of votes we got). A negative score denotes that more people on the shuttle voted than than we received votes (bad).")
               ),
               mainPanel(
                 plotOutput("distPlot3")
               )
             )
    ),
    tabPanel("Turnover", fluid=T,
             sidebarLayout(
               sidebarPanel(
                 radioButtons("year4", "Year", choices = c("2023"="L23", "2022"="L22"), selected = "L23", inline = T),
                 radioButtons("abs_pc", "Axis", choices = c("Percentage"="pc", "Absolute"="abs"), selected = "pc", inline = T),
                 checkboxGroupInput("ward4", "Ward", choices = c("Didsbury East", "Didsbury West", "Ancoats & Beswick" ), selected =  "Didsbury West", inline = T),
                 radioButtons("group4", "Level", choices = c("Ward"="WardName",
                                                             "Polling District"="PollingDistrictCode",
                                                             "Shuttle"="Shuttle2", "Vote Type"="PV2"), selected = "WardName", inline = T),
                 checkboxGroupInput("voted4", "Voted?", choices = c("Yes", "No"), selected = c("Yes"), inline = T),
                 checkboxGroupInput("voters4", "Voter type", choices = c("In Person", "Postal"), selected = c("In Person", "Postal"), inline = T),
                 checkboxGroupInput("shuttle4", "Shuttleworth", choices = c("Shuttle", "Non-Shuttle"), selected = c("Shuttle", "Non-Shuttle"), inline = T),
                 pickerInput("regcycle4", "Year registered", 
                             choices = yearreg, selected = yearreg,                 
                             options = list(`actions-box` = TRUE),
                             multiple = T),
                 tags$p("UK: unknown registration date, most likely prior to 2012."),
                 width=4
               ),
               mainPanel(
                 plotOutput("distPlot4")
               )
             )
    ),
    tabPanel("Voting History", fluid=T,
             sidebarLayout(
               sidebarPanel(
                 radioButtons("year5", "Year", choices = c("2023"="L23", "2022"="L22"), selected = "L23", inline = T),
                 conditionalPanel(condition = "input.year5=='L23'",
                                  checkboxInput("leftreg5", "Include those that left the register")),
                 checkboxGroupInput("ward5", "Ward", choices = c("Didsbury East", "Didsbury West", "Ancoats & Beswick" ), selected =  "Didsbury West", inline = T),
                 conditionalPanel(condition = "input.year5=='L22'",
                                  radioButtons("election5_22", "Election to compare", choices = c("Local 2021"="L21", "General 2019"="G19", "Local 2019"="L19", "Local 2018"="L18", "Local By-election 2022"="B22", "Any Local 18-21"="PrevL22"), selected = "L21", inline = T)),
                 conditionalPanel(condition = "input.year5=='L23'",
                                  radioButtons("election5_23", "Election to compare", choices = c("Local 2022"="L22","Local 2021"="L21", "General 2019"="G19", "Local 2019"="L19",  "Local By-election 2022"="B22", "Any Local 19-29"="PrevL23"), selected = "L22", inline = T)),
                 
                 checkboxGroupInput("voters5", "Voter type", choices = c("In Person", "Postal"), selected = c("In Person", "Postal"), inline = T),
                 checkboxGroupInput("shuttle5", "Shuttleworth", choices = c("Shuttle", "Non-Shuttle"), selected = c("Shuttle", "Non-Shuttle"), inline = T),
                 pickerInput("regcycle5", "Year registered", 
                             choices = yearreg, selected = yearreg,                 
                             options = list(`actions-box` = TRUE),
                             multiple = T),
                 width=4
               ),
               mainPanel(
                 plotOutput("distPlot5")
               )
             )
    ),
    
  )
  
)