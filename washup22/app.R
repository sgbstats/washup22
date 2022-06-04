
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(lubridate)
library(readxl)
library(ggplot2)
library(ggsankey)
library(htmltools)
library(flextable)


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
  tabsetPanel(
    tabPanel("Basic Stats", fluid=T,
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("ward0", "Ward", choices = c("Didsbury East", "Didsbury West"), selected =  c("Didsbury East", "Didsbury West"), inline = T),
                 radioButtons("group0", "Level", choices = c("Ward", "Polling District"), selected = "Ward", inline = T),
                 width=4
               ),
               mainPanel(
                 uiOutput("table0")
               )
             )
    ),
    tabPanel("Turnout", fluid=T,
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("ward", "Ward", choices = c("Didsbury East", "Didsbury West"), selected =  "Didsbury West", inline = T),
                 radioButtons("group", "Level", choices = c("Ward", "Polling District", "Shuttle", "Vote Type", "Year registered"), selected = "Polling District", inline = T),
                 checkboxGroupInput("voters", "Voter type", choices = c("In Person", "Postal"), selected = c("In Person", "Postal"), inline = T),
                 checkboxGroupInput("shuttle", "Shuttleworth", choices = c("Shuttle", "Non-Shuttle"), selected = c("Shuttle", "Non-Shuttle"), inline = T),
                 pickerInput("regcycle", "Year registered", 
                             choices = yearreg, selected = yearreg,                 
                             options = list(`actions-box` = TRUE),
                             multiple = T),
                 width=4
               ),
               mainPanel(
                 plotOutput("distPlot1")
               )
             )
    ),
    tabPanel("Phantom Shuttle", fluid=T,
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("ward2", "Ward", choices = c("Didsbury East", "Didsbury West"), selected =  "Didsbury West", inline = T),
                 radioButtons("Box", "Postal voters", choices = c("In Boxes", "Separate"), selected = "In Boxes", inline = T),
                 width=4
               ),
               mainPanel(
                 plotOutput("distPlot2"),
                 tags$ul("Faint part is phantom LD voters: the number of votes in the box minus those on the shuttle that voted")
               )
             )
    ),
    tabPanel("Turnover", fluid=T,
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("ward3", "Ward", choices = c("Didsbury East", "Didsbury West"), selected =  "Didsbury West", inline = T),
                 radioButtons("group3", "Level", choices = c("Ward", "Polling District", "Shuttle", "Vote Type"), selected = "Ward", inline = T),
                 checkboxGroupInput("voted3", "Voted?", choices = c("Yes", "No"), selected = c("Yes"), inline = T),
                 checkboxGroupInput("voters3", "Voter type", choices = c("In Person", "Postal"), selected = c("In Person", "Postal"), inline = T),
                 checkboxGroupInput("shuttle3", "Shuttleworth", choices = c("Shuttle", "Non-Shuttle"), selected = c("Shuttle", "Non-Shuttle"), inline = T),
                 pickerInput("regcycle3", "Year registered", 
                             choices = yearreg, selected = yearreg,                 
                             options = list(`actions-box` = TRUE),
                             multiple = T),
                 width=4
               ),
               mainPanel(
                 plotOutput("distPlot3")
               )
             )
    ),
    tabPanel("Voting History", fluid=T,
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("ward4", "Ward", choices = c("Didsbury East", "Didsbury West"), selected =  "Didsbury West", inline = T),
                 radioButtons("election4", "Election", choices = c("Local 2021", "General 2019", "Local 2019", "Local 2018", "Any Local 18-21"), selected = "Local 2021", inline = T),
                 checkboxGroupInput("voters4", "Voter type", choices = c("In Person", "Postal"), selected = c("In Person", "Postal"), inline = T),
                 checkboxGroupInput("shuttle4", "Shuttleworth", choices = c("Shuttle", "Non-Shuttle"), selected = c("Shuttle", "Non-Shuttle"), inline = T),
                 pickerInput("regcycle4", "Year registered", 
                             choices = yearreg, selected = yearreg,                 
                             options = list(`actions-box` = TRUE),
                             multiple = T),
                 width=4
               ),
               mainPanel(
                 plotOutput("distPlot4")
               )
             )
    ),
    
  )
  
)

server <- function(input, output) {
  
  #tab 1
  output$table0=renderUI({
    if(input$group0=="Ward")
    {
      Data9=Data %>% filter(WardName %in% input$ward0) %>%  rename("foo"="WardName")
    }else
    {
      Data9=Data  %>% filter(WardName %in% input$ward0) %>% rename("foo"="PollingDistrictCode")
    }
    Data9 %>% 
      select(foo, PV, Shuttle, L22) %>% 
      pivot_longer(cols = -c("foo"), names_to = "param", values_to = "aval") %>% 
      group_by(foo, param) %>% 
      summarise(n=sum(aval), pc=mean(aval)) %>% 
      mutate(n_pc=paste(n, " (", sprintf("%.1f", 100*pc), "%)", sep="")) %>% 
      select(-n, -pc) %>% 
      mutate(param=case_when(param=="L22"~"Voted 2022",
                             param=="PV"~"Postal Voters",
                             param=="Shuttle"~"Shuttleworth")) %>% 
      pivot_wider(names_from = "param", values_from = "n_pc") %>% 
      flextable() %>% 
      set_header_labels(foo="") %>% 
      autofit() %>%
      htmltools_value()
  })
  
  #tab 2
  output$distPlot1 <- renderPlot({
    Data2=Data %>% filter(
      WardName%in%input$ward,
      regcycle%in%input$regcycle,
      Shuttle2%in%input$shuttle,
      PV2%in%input$voters,
    )
    
    #picking the grouping variable
    if(input$group=="Ward")
    {
      Data3=Data2 %>% rename("foo"="WardName") 
    }else if(input$group=="Polling District")
    {
      Data3=Data2 %>% rename("foo"="PollingDistrictCode") 
    }else if(input$group=="Shuttle")
    {
      Data3=Data2 %>% rename("foo"="Shuttle2") 
    }else if(input$group=="Vote Type")
    {
      Data3=Data2 %>% rename("foo"="PV2") 
    }else if(input$group=="Year registered")
    {
      Data3=Data2 %>% rename("foo"="regcycle") 
    }
    
    
    Data3 %>% group_by(foo)%>% 
      summarise(V=mean(L22), x=n()) %>% 
      ggplot(aes(x=foo, y=V, fill=foo))+
      geom_bar(stat="identity")+
      xlab(input$group)+
      ylab("Turnout")+
      scale_y_continuous(labels = scales::percent)+
      theme_bw()+
      theme(legend.position="none")
    
    
  })
  #tab 3
  output$distPlot2 <- renderPlot({
    
    Data4=Data %>% filter(WardName%in%input$ward2,
                          L22, Shuttle) %>% 
      mutate(PD2=case_when(WardName=="Didsbury West" &PV~ "DW Postal",
                           WardName=="Didsbury East" &PV~ "DE Postal",
                           T~PollingDistrictCode))
    
    #calculates whether to count the postals separate
    if(input$Box=="In Boxes")
    {
      box2=boxes %>% filter(Ward%in%input$ward2)  
      Data5=Data4 %>% rename("foo"="PollingDistrictCode")%>% count(foo)
    }else if(input$Box=="Separate")
    {
      box2=boxes %>% filter(Ward%in%input$ward2) %>% 
        dplyr::select(Box, Ward, ipvotesest) %>% 
        rename("total"="ipvotesest") %>% 
        rbind.data.frame(boxes%>% filter(Ward%in%input$ward2) %>% 
                           group_by( Ward) %>% 
                           summarise(total=sum(pvvotesest)) %>% 
                           mutate(Box=if_else(Ward=="Didsbury West", "DW Postal", "DE Postal")) %>% 
                           ungroup()) %>% 
        ungroup()
      Data5=Data4 %>% rename("foo"="PD2") %>% count(foo)
    }
    
    box2 %>% ggplot(aes(x=Box, y=total, fill=Box))+
      geom_bar(stat = "identity", alpha=0.5)+
      geom_bar(data=Data5, aes(x=foo, y=n, fill=foo),stat="identity")+
      xlab("Box")+
      ylab("LD votes")+
      theme_bw()+
      theme(legend.position="none")
    
  })
  
  # Tab 4
  
  output$distPlot3<-renderPlot({
    Data6= Data %>% mutate(voted=if_else(L22, "Yes", "No"))%>% 
      filter(WardName%in%input$ward4,
             regcycle%in%input$regcycle4,
             Shuttle2%in%input$shuttle4,
             PV2%in%input$voters4,
             voted %in% input$voted3)
    
    if(input$group3=="Ward")
    {
      Data7=Data6 %>% rename("foo"="WardName") 
    }else if(input$group3=="Polling District")
    {
      Data7=Data6 %>% rename("foo"="PollingDistrictCode") 
    }else if(input$group3=="Shuttle")
    {
      Data7=Data6 %>% rename("foo"="Shuttle2") 
    }else if(input$group3=="Vote Type")
    {
      Data7=Data6 %>% rename("foo"="PV2") 
    }else if(input$group3=="Year registered")
    {
      Data7=Data6 %>% rename("foo"="regcycle") 
    }
    
    Data7 %>% count(foo, regcycle) %>% 
      merge(Data7 %>% count(foo), by="foo") %>% 
      mutate(n_pc=100*n.x/n.y) %>% 
      ggplot(aes(x=regcycle, fill=foo, y=n_pc))+
      geom_bar(position = "dodge", stat = "identity")+
      xlab("Cycle of registration")+
      ylab("%")+
      scale_x_discrete(guide = guide_axis(angle = -45))+
      labs(fill=input$group3)+
      theme_bw()
  })
  
  #tab 5
  output$distPlot4<-renderPlot({
    Data8= Data %>% 
      filter(WardName%in%input$ward4,
             regcycle%in%input$regcycle4,
             Shuttle2%in%input$shuttle4,
             PV2%in%input$voters4) %>% 
      mutate(L22=if_else(L22, "Voted", "Didn't Vote"),
             L21=case_when(regcycle2==2022~"Not on register",
                           L21~"Voted",
                           !L21~"Didn't Vote"),
             L19=case_when(regcycle2>2019~"Not on register",
                           L19~"Voted",
                           !L19~"Didn't Vote"),
             L18=case_when(regcycle2>2018~"Not on register",
                           L18~"Voted",
                           !L18~"Didn't Vote"),
             PrevL=case_when(regcycle2==2022~"Not on register",
                             PrevL~"Voted",
                             !PrevL~"Didn't Vote"),
             G19=case_when(G19~"Voted",
                           regcycle2>2019~"Not on register",
                           !G19~"Didn't Vote"))
    
    if(input$election4=="Local 2021")
    {
      sankey=Data8 %>% rename("foo"="L21")
    }else if(input$election4=="Local 2019")
    {
      sankey=Data8 %>% rename("foo"="L19")
    }else if(input$election4=="General 2019")
    {
      sankey=Data8 %>% rename("foo"="G19")
    }else if(input$election4=="Any Local 18-21")
    {
      sankey=Data8 %>% rename("foo"="PrevL")
    }else if(input$election4=="Local 2018")
    {
      sankey=Data8 %>% rename("foo"="L18")
    }
    
    
    #does the stuff for the sankey diagram
    df=sankey %>% make_long(foo, L22) 
    
    df %>%   ggplot(aes(x = x, 
                        next_x = next_x, 
                        node = node, 
                        next_node = next_node,
                        fill = factor(node),
                        label = node)) +
      geom_sankey(flow.alpha = 0.75, node.color = 1) +
      geom_sankey_label(size = 3.5, color = 1, fill = "white") +
      theme_sankey(base_size = 12)+
      scale_fill_manual(values = c("#E41A1C", "#F6CB2F", "#4DAF4A"))+
      scale_x_discrete(name=c("foo", "L22"), labels=c(input$election4, "Local 2022"), position="top")+
      theme(legend.position = "none", axis.title.x = element_blank())
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
