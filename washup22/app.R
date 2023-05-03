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
                 checkboxGroupInput("ward0", "Ward", choices = c("Didsbury East", "Didsbury West", "Ancoats & Beswick" ), selected =  c("Didsbury East", "Didsbury West", "Ancoats & Beswick"), inline = T),
                 radioButtons("group0", "Level", choices = c("Ward", "Polling District"), selected = "Ward", inline = T),
                 pickerInput("regcycle0", "Year registered", 
                             choices = yearreg, selected = yearreg,                 
                             options = list(`actions-box` = TRUE),
                             multiple = T),
                 
                 checkboxInput("prevvoters", "Previous voters only"),
                 conditionalPanel(condition="input.prevvoters==1",
                                  checkboxGroupInput("votehistory1", "Voting history (any of)", choices = c("Local 21"="L21", "Local 19"="L19", "Local 18"="L18", "General 19"="G19", "By election 22"="B22"), selected = "L21", inline=T)
                 )
                 ,
                 conditionalPanel(condition="input.prevvoters==2"),
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
                 checkboxGroupInput("ward", "Ward", choices = c("Didsbury East", "Didsbury West", "Ancoats & Beswick" ), selected =  "Didsbury West", inline = T),
                 radioButtons("group", "Level", choices = c("Ward"="WardName",
                                                            "Polling District"="PollingDistrictCode",
                                                            "Shuttle"="Shuttle2", "Vote Type"="PV2", "Year registered"="regcycle"), selected = "PollingDistrictCode", inline = T),
                 checkboxGroupInput("voters", "Voter type", choices = c("In Person"=F, "Postal"=T), selected = c(T,F), inline = T),
                 checkboxGroupInput("shuttle", "Shuttleworth", choices = c("Shuttle"=T, "Non-Shuttle"=F), selected = c(T,F), inline = T),
                 pickerInput("regcycle", "Year registered", 
                             choices = yearreg, selected = yearreg,                 
                             options = list(`actions-box` = TRUE),
                             multiple = T),
                 checkboxInput("prevvoters2", "Previous voters only"),
                 conditionalPanel(condition="input.prevvoters2==1",
                                  checkboxGroupInput("votehistory2", "Voting history (any of)", choices = c("Local 21"="L21", "Local 19"="L19", "Local 18"="L18", "General 19"="G19", "By election 22"="B22"), selected = "L21", inline=T)
                 )
                 ,
                 conditionalPanel(condition="input.prevvoters2==2"),
                 checkboxInput("av", "Display overall ward turnout"),
                 width=4
               ),
               mainPanel(
                 plotOutput("distPlot1", width = "100%")
               )
             )
    ),
    tabPanel("Phantom Shuttle", fluid=T,
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("ward2", "Ward", choices = c("Didsbury East", "Didsbury West", "Ancoats & Beswick" ), selected =  "Didsbury West", inline = T),
                 radioButtons("Box", "Level", choices = c("PVs in Boxes", "PVs separate", "Ward-wide"), selected = "PVs in Boxes", inline = T),
                 radioButtons("n_pc", "Count or percentage", choices = c("Count", "Percentage Phantom", "Absolute Phantom"), selected = "Count"),
                 width=4,
                 tags$p("Faint part is phantom LD voters: the number of votes for us in the box minus those on the shuttle that voted (who are in block colour)"),
                 tags$p("For percentage phantom, it is calculated as the 100%-(number in the shuttle/number of votes we got). A negative score denotes that more people on the shuttle voted than than we received votes (bad).")
               ),
               mainPanel(
                 plotOutput("distPlot2")
               )
             )
    ),
    tabPanel("Turnover", fluid=T,
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("ward3", "Ward", choices = c("Didsbury East", "Didsbury West", "Ancoats & Beswick" ), selected =  "Didsbury West", inline = T),
                 radioButtons("group3", "Level", choices = c("Ward"="WardName",
                                                             "Polling District"="PollingDistrictCode",
                                                             "Shuttle"="Shuttle2", "Vote Type"="PV2", "Year registered"="regcycle"), selected = "WardName", inline = T),
                 checkboxGroupInput("voted3", "Voted?", choices = c("Yes", "No"), selected = c("Yes"), inline = T),
                 checkboxGroupInput("voters3", "Voter type", choices = c("In Person", "Postal"), selected = c("In Person", "Postal"), inline = T),
                 checkboxGroupInput("shuttle3", "Shuttleworth", choices = c("Shuttle", "Non-Shuttle"), selected = c("Shuttle", "Non-Shuttle"), inline = T),
                 pickerInput("regcycle3", "Year registered", 
                             choices = yearreg, selected = yearreg,                 
                             options = list(`actions-box` = TRUE),
                             multiple = T),
                 tags$p("UK: unknown registration date, most likely prior to 2012."),
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
                 checkboxGroupInput("ward4", "Ward", choices = c("Didsbury East", "Didsbury West", "Ancoats & Beswick" ), selected =  "Didsbury West", inline = T),
                 radioButtons("election4", "Election to compare", choices = c("Local 2021", "General 2019", "Local 2019", "Local 2018", "Local By-election 2022", "Any Local 18-21"), selected = "Local 2021", inline = T),
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
  
  DWcols=monochromeR::generate_palette(
    "#4DAF4A", 
    "go_darker",
    n_colours = 8,
    view_palette = F
  )
  names(DWcols)=c("Didsbury West", paste("4DW", LETTERS[1:6], sep=""), "DW Postal")
  
  DEcols=monochromeR::generate_palette(
    "#007AC0", 
    "go_darker",
    n_colours = 11,
    view_palette = F
  )
  
  names(DEcols)=c("Didsbury East", paste("4DE", LETTERS[c(1:8,10)], sep=""), "DE Postal")
  
  ABcols=monochromeR::generate_palette(
    "#F6CB2F", 
    "go_darker",
    n_colours = 7,
    view_palette = F
  )
  names(ABcols)=c("Ancoats & Beswick", paste("2AB", LETTERS[1:5], sep=""), "AB Postal")
  
  pv=c("#4DAF4A", "#E41A1C")
  names(pv)=c("Postal", "In Person")
  shuttle=c("#4DAF4A", "#E41A1C")
  
 regcols=monochromeR::generate_palette(
    "#660099", 
    "go_lighter",
    n_colours = 22,
    view_palette = F
  )
 names(regcols)=c("UK", 2002:2022)
  names(shuttle)=c("Shuttle", "Non-Shuttle")
  # cols= c("Didsbury West"="#4DAF4A",
  #         "Didsbury East"="#007AC0",
  #         "Ancoats & Beswick"="#FF7F00",
  #         "4DWA"="#64B962",
  #         "4DWB"="#7CC47A",
  #         "4DWC"="#94CF92",
  #         "4DWD"="#ABD9AA",
  #         "4DWE"="#C3E4C2",
  #         "4DWF"="#DBEFDA")
  
  cols=c(ABcols, DWcols, DEcols, pv, shuttle, regcols)
  #tab 1 basic stats
  output$table0=renderUI({
    if(input$group0=="Ward")
    {
      Data9=Data %>% filter(WardName %in% input$ward0) %>%  rename("foo"="WardName")
    }else
    {
      Data9=Data  %>% filter(WardName %in% input$ward0) %>% rename("foo"="PollingDistrictCode")
    }
    if(input$prevvoters){
      vh=Data %>% select(`Voter File VANID`, input$votehistory1) %>% 
        pivot_longer(cols=-c(`Voter File VANID`), names_to = "election", values_to = "vote") %>% 
        filter(vote)
    }else
    {
      vh=Data %>% select(`Voter File VANID`)
    }
    
    
    Data9 %>% 
      filter(regcycle%in%input$regcycle0,
             `Voter File VANID` %in% vh$`Voter File VANID`) %>% 
      select(foo, PV, Shuttle, L22) %>% 
      pivot_longer(cols = -c("foo"), names_to = "param", values_to = "aval") %>% 
      group_by(foo, param) %>% 
      summarise(n=sum(aval), pc=mean(aval)) %>% 
      mutate(n_pc=paste(n, " (", sprintf("%.1f", 100*pc), "%)", sep="")) %>% 
      select(-n, -pc) %>% 
      mutate(param=case_when(param=="L22"~"2022 Turnout",
                             param=="PV"~"Total Postal Voters",
                             param=="Shuttle"~"Total Shuttleworth")) %>% 
      pivot_wider(names_from = "param", values_from = "n_pc") %>% 
      flextable() %>% 
      set_header_labels(foo="") %>% 
      autofit() %>%
      font(fontname = "Arial", part="all") %>% 
      htmltools_value()
  })
  
  #tab 2 turnout
  output$distPlot1 <- renderPlot({
    
    if(input$prevvoters2){
      vh=Data %>% select(`Voter File VANID`, input$votehistory2) %>% 
        pivot_longer(cols=-c(`Voter File VANID`), names_to = "election", values_to = "vote") %>% 
        filter(vote)
    }else
    {
      vh=Data %>% select(`Voter File VANID`)
    }
    
    
    Data3=Data %>% filter(
      WardName%in%input$ward,
      regcycle%in%input$regcycle,
      Shuttle%in%input$shuttle,
      PV%in%input$voters,
      `Voter File VANID` %in% vh$`Voter File VANID`
    ) %>% 
      rename("foo"=input$group)%>%
      arrange(foo)
    
    
    
    #picking the grouping variable
    # if(input$group=="Ward")
    # {
    #   Data3=Data2 %>% rename("foo"="WardName") %>% arrange(foo)
    # }else if(input$group=="Polling District")
    # {
    #   Data3=Data2 %>% rename("foo"="PollingDistrictCode") %>% arrange(foo)
    # }else if(input$group=="Shuttle")
    # {
    #   Data3=Data2 %>% rename("foo"="Shuttle2") %>% arrange(foo)
    # }else if(input$group=="Vote Type")
    # {
    #   Data3=Data2 %>% rename("foo"="PV2") 
    # }else if(input$group=="Year registered")
    # {
    #   Data3=Data2 %>% rename("foo"="regcycle") %>% arrange(foo)
    # }
    
    wardturnout=Data %>% 
      filter(WardName%in%input$ward)%>%
      group_by(WardName)%>% 
      summarise(V=mean(L22), x=n()) %>% 
      mutate(WardName=paste(WardName, "turnout"), v2=V+0.01, foo=Data3$foo[1])
    
    g= Data3 %>% group_by(foo)%>% 
      summarise(V=mean(L22), x=n()) %>% 
      ggplot(aes(x=foo, y=V, fill=foo))+
      geom_bar(stat="identity")+
      xlab("")+
      ylab("Turnout")+
      scale_y_continuous(labels = scales::percent)+
      theme_bw()+
      theme(legend.position="none",
            axis.text = element_text(color = "black", size=10),
            axis.title = element_text(color = "black", size=13),)+
      scale_fill_manual(values = cols)
    
    if(input$av)
    {
      g+geom_hline(yintercept = wardturnout$V, size=0.75)+
        geom_text(data=wardturnout, aes(x=foo, y=v2, label=WardName),hjust = 0)
    }else
    {
      g
    }
    
    
  }, height = reactive(0.8*input$dimension[2]))
  
  #tab 3 phantom shuttle
  output$distPlot2 <- renderPlot({
    
    Data4=Data %>% filter(WardName%in%input$ward2,
                          L22, Shuttle) %>% 
      mutate(PD2=case_when(WardName=="Didsbury West" &PV~ "DW Postal",
                           WardName=="Didsbury East" &PV~ "DE Postal",
                           WardName=="Ancoats & Beswick" &PV~ "AB Postal",
                           T~PollingDistrictCode))
    
    #calculates whether to count the postals separate
    if(input$Box=="PVs in Boxes")
    {
      box2=boxes %>% filter(Ward%in%input$ward2)  
      Data5=Data4 %>% rename("foo"="PollingDistrictCode")%>% count(foo)
    }else if(input$Box=="Ward-wide")
    {
      box2=boxes %>% filter(Ward%in%input$ward2) %>% mutate(Box=Ward)  %>% group_by(Ward, Box) %>% summarise(ipvotesest=sum(ipvotesest),
                                                                                                             pvvotesest=sum(pvvotesest),
                                                                                                             total=sum(total) )
      Data5=Data4 %>% rename("foo"="WardName")%>% count(foo)
    }else if(input$Box=="PVs separate")
    {
      box2=boxes %>% filter(Ward%in%input$ward2) %>% 
        dplyr::select(Box, Ward, ipvotesest) %>% 
        rename("total"="ipvotesest") %>% 
        rbind.data.frame(boxes%>% filter(Ward%in%input$ward2) %>% 
                           group_by( Ward) %>% 
                           summarise(total=sum(pvvotesest)) %>% 
                           mutate(Box=case_when(Ward=="Didsbury West"~"DW Postal", 
                                                Ward=="Didsbury East"~"DE Postal",
                                                Ward=="Ancoats & Beswick"~"AB Postal")) %>% 
                           ungroup()) %>% 
        ungroup()
      Data5=Data4 %>% rename("foo"="PD2") %>% count(foo)
    }
    
    
    if(input$n_pc=="Count")
    {
      box2 %>% ggplot(aes(x=Box, y=total, fill=Box))+
        geom_bar(stat = "identity", alpha=0.5)+
        geom_bar(data=Data5, aes(x=foo, y=n, fill=foo),stat="identity")+
        xlab("Box")+
        ylab("LD votes")+
        theme_bw()+
        theme(legend.position="none",
              axis.text = element_text(color = "black", size=10),
              axis.title = element_text(color = "black", size=13),)+
        scale_fill_manual(values = cols)
    }else if(input$n_pc=="Percentage Phantom")
    {
      box3=merge(box2, Data5, by.x="Box", by.y="foo" ) %>%
        mutate(diff=1-n/total)
      
      box3 %>% ggplot(aes(x=Box, y=diff, fill=Box))+
        geom_bar(stat = "identity", alpha=0.5)+
        xlab("Box")+
        ylab("% phantom shuttle")+
        theme_bw()+
        theme(legend.position="none",
              axis.text = element_text(color = "black", size=10),
              axis.title = element_text(color = "black", size=13),)+
        scale_y_continuous(labels = scales::percent)+
        scale_fill_manual(values = cols)
    }else if(input$n_pc=="Absolute Phantom")
      {
      
      box4=merge(Data5, box2, by.y="Box", by.x="foo") %>% 
        mutate(diff=total-n)
      
        box4 %>% ggplot(aes(x=foo, y=diff, fill=foo))+
          geom_bar(stat = "identity", alpha=0.5)+
          # geom_bar(data=Data5, aes(x=foo, y=n, fill=foo),stat="identity")+
          xlab("Box")+
          ylab("Phantom LD votes")+
          theme_bw()+
          theme(legend.position="none",
                axis.text = element_text(color = "black", size=10),
                axis.title = element_text(color = "black", size=13),)+
          scale_fill_manual(values = cols)
      }
    
    
  }, height = reactive(0.8*input$dimension[2]))
  
  # Tab 4 Turnover
  
  output$distPlot3<-renderPlot({
    Data7= Data %>% mutate(voted=if_else(L22, "Yes", "No"))%>% 
      filter(WardName%in%input$ward3,
             regcycle%in%input$regcycle3,
             Shuttle2%in%input$shuttle3,
             PV2%in%input$voters3,
             voted %in% input$voted3) %>% 
      rename(foo=input$group3)
    
    # if(input$group3=="Ward")
    # {
    #   Data7=Data6 %>% rename("foo"="WardName") 
    # }else if(input$group3=="Polling District")
    # {
    #   Data7=Data6 %>% rename("foo"="PollingDistrictCode") 
    # }else if(input$group3=="Shuttle")
    # {
    #   Data7=Data6 %>% rename("foo"="Shuttle2") 
    # }else if(input$group3=="Vote Type")
    # {
    #   Data7=Data6 %>% rename("foo"="PV2") 
    # }else if(input$group3=="Year registered")
    # {
    #   Data7=Data6 %>% rename("foo"="regcycle") 
    # }
    
    Data7 %>% count(foo, regcycle) %>% 
      merge(Data7 %>% count(foo), by="foo") %>% 
      mutate(n_pc=100*n.x/n.y) %>% 
      ggplot(aes(x=regcycle, fill=foo, y=n_pc))+
      geom_bar(position = "dodge", stat = "identity")+
      xlab("Cycle of registration")+
      ylab("%")+
      scale_x_discrete(guide = guide_axis(angle = -45))+
      labs(fill=input$group3)+
      theme_bw()+
      theme(
        axis.text = element_text(color = "black", size=10),
        axis.title = element_text(color = "black", size=13),)+
      scale_fill_manual(values = cols)
    
  },  height = reactive(0.8*input$dimension[2]))
  
  #tab 5 voting history
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
             B22=case_when(regcycle2>2022~"Not on register",
                           B22~"Voted",
                           !B22&WardName=="Ancoats & Beswick"~"Didn't Vote",
                           T~"No Election"),
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
    }else if(input$election4=="Local By-election 2022")
    {
      sankey=Data8 %>% rename("foo"="B22")
    }
    
    
    #does the stuff for the sankey diagram
    df=sankey %>% make_long(foo, L22) 
    
    df_nr <- 
      df %>% 
      filter(!is.na(node)) %>% 
      group_by(x, node)%>% 
      summarise(count = n())
    
    df <- 
      df %>% 
      left_join(df_nr) %>% 
      mutate(count2=paste(node, "\n", count, sep=""))
    
    df %>%   ggplot(aes(x = x, 
                        next_x = next_x, 
                        node = node, 
                        next_node = next_node,
                        fill = factor(node),
                        label = count2)) +
      geom_sankey(flow.alpha = 0.75, node.color = 1) +
      geom_sankey_label(size = 3.5, color = 1, fill = "white") +
      # geom_sankey_text(aes(label = count), size = 3.5, vjust = 2,color = 1, fill = "white", check_overlap = TRUE) +
      theme_sankey(base_size = 12)+
      scale_fill_manual(values = c("Didn't Vote"="#E41A1C","Not on register"="#F6CB2F","Voted"="#4DAF4A"))+
      scale_x_discrete(name=c("foo", "L22"), labels=c(input$election4, "Local 2022"), position="top")+
      theme(legend.position = "none", axis.title.x = element_blank(),
            axis.text.x = element_text(color = "grey20", size = 20, hjust = .5, vjust = .5, face = "bold"))
  }, height = reactive(0.8*input$dimension[2]))
}

# Run the application 
shinyApp(ui = ui, server = server)
