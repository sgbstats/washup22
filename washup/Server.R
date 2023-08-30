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
  
  cols=c(ABcols, DWcols, DEcols, pv, shuttle, regcols)
  #tab 1 basic stats
  
  output$table1=renderUI({
    
    if(input$group1=="Ward")
    {
      Data1_1=Data %>% filter(WardName %in% input$ward1) %>%  rename("foo"="WardName")
    }else
    {
      Data1_1=Data  %>% filter(WardName %in% input$ward1) %>% rename("foo"="PollingDistrictCode")
    }
    
    if(input$prevvoters1 && input$year1=="L22"){
      vh=Data %>% select(`Voter File VANID`, input$votehistory1_22) %>% 
        pivot_longer(cols=-c(`Voter File VANID`), names_to = "election", values_to = "vote") %>% 
        filter(vote)
    }else if(input$prevvoters1 && input$year1=="L23"){
      vh=Data %>% select(`Voter File VANID`, input$votehistory1_23) %>% 
        pivot_longer(cols=-c(`Voter File VANID`), names_to = "election", values_to = "vote") %>% 
        filter(vote)
    }else
    {
      vh=Data %>% select(`Voter File VANID`)
    }
    
    if(input$year1=="L22")
    {
      Data1_2=Data1_1 %>% 
        filter(regcycle%in%input$regcycle1,
               `Voter File VANID` %in% vh$`Voter File VANID`,
               moved!="IN") %>% 
        select(foo, PV, Shuttle22, L22) %>% 
        pivot_longer(cols = -c("foo"), names_to = "param", values_to = "aval") %>% 
        group_by(foo, param) %>% 
        summarise(n=sum(aval), pc=mean(aval)) %>% 
        mutate(n_pc=paste(n, " (", sprintf("%.1f", 100*pc), "%)", sep="")) %>% 
        select(-n, -pc) %>% 
        mutate(param=case_when(param=="L22"~"2022 Turnout",
                               param=="PV"~"Total Postal Voters",
                               param=="Shuttle22"~"Total Shuttleworth")) 
      
    }else if(input$year1=="L23")
    {
      Data1_2=Data1_1 %>% 
        filter(regcycle%in%input$regcycle1,
               `Voter File VANID` %in% vh$`Voter File VANID`,
               moved!="OUT") %>% 
        select(foo, PV, Shuttle23, L23) %>% 
        pivot_longer(cols = -c("foo"), names_to = "param", values_to = "aval") %>% 
        group_by(foo, param) %>% 
        summarise(n=sum(aval), pc=mean(aval)) %>% 
        mutate(n_pc=paste(n, " (", sprintf("%.1f", 100*pc), "%)", sep="")) %>% 
        select(-n, -pc) %>% 
        mutate(param=case_when(param=="L23"~"2023 Turnout",
                               param=="PV"~"Total Postal Voters",
                               param=="Shuttle23"~"Total Shuttleworth")) 
    }
    
    Data1_2 %>% pivot_wider(names_from = "param", values_from = "n_pc") %>% 
      flextable() %>% 
      set_header_labels(foo="") %>% 
      autofit() %>%
      font(fontname = "Arial", part="all") %>% 
      htmltools_value()
    
  })
  
  #tab 2 turnout
  output$distPlot2 <- renderPlot({
    
    if(input$prevvoters2 && input$year2=="L22"){
      vh=Data %>% select(`Voter File VANID`, input$votehistory2_22) %>% 
        pivot_longer(cols=-c(`Voter File VANID`), names_to = "election", values_to = "vote") %>% 
        filter(vote)
    }else if(input$prevvoters2 && input$year2=="L23"){
      vh=Data %>% select(`Voter File VANID`, input$votehistory2_23) %>% 
        pivot_longer(cols=-c(`Voter File VANID`), names_to = "election", values_to = "vote") %>% 
        filter(vote)
    }else
    {
      vh=Data %>% select(`Voter File VANID`)
    }
    if(input$year2=="L22")
    {
      movedstatus=c("OUT", "STAY")
    }else if(input$year2=="L23")
    {
      movedstatus=c("IN", "STAY")
    }
    
    Data2_1=Data %>% 
      rename("Shuttle"=if_else(input$year2=="L22", "Shuttle22", "Shuttle23")) %>% 
      filter(WardName%in%input$ward2,
             regcycle%in%input$regcycle2,
             Shuttle%in%input$shuttle2,
             PV%in%input$voters2,
             `Voter File VANID` %in% vh$`Voter File VANID`,
             moved %in% movedstatus) %>% 
      rename("foo"=input$group2,
             "foo2"=input$year2)%>%
      arrange(foo)
    
    
    wardturnout=Data %>% 
      filter(WardName%in%input$ward2,
             moved %in% movedstatus)%>%
      rename("foo2"=input$year2) %>% 
      group_by(WardName)%>% 
      summarise(V=mean(foo2), x=n()) %>% 
      mutate(WardName=paste(WardName, "turnout"), v2=V+0.01, foo=Data2_1$foo[1])
    
    g=Data2_1 %>% group_by(foo)%>% 
      summarise(V=mean(foo2), x=n()) %>% 
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
  output$distPlot3 <- renderPlot({
    
    if(input$year3=="L22")
    {
      Data3_1=Data %>% filter(WardName%in%input$ward3,
                            L22, Shuttle22) %>% 
        mutate(PD2=case_when(WardName=="Didsbury West" &PV~ "DW Postal",
                             WardName=="Didsbury East" &PV~ "DE Postal",
                             WardName=="Ancoats & Beswick" &PV~ "AB Postal",
                             T~PollingDistrictCode))
      boxes=boxes22
    }else if(input$year3=="L23")
    {
      Data3_1=Data %>% filter(WardName%in%input$ward3,
                            L23, Shuttle23) %>% 
        mutate(PollingDistrictCode=gsub("4DEB", "4DEA", PollingDistrictCode),
               PD2=case_when(WardName=="Didsbury West" &PV~ "DW Postal",
                             WardName=="Didsbury East" &PV~ "DE Postal",
                             WardName=="Ancoats & Beswick" &PV~ "AB Postal",
                             PollingDistrictCode=="4DEB"~"4DEA",
                             T~PollingDistrictCode))
      boxes=boxes23
    }
    
    
    #calculates whether to count the postals separate
    if(input$Box3=="PVs in Boxes")
    {
      box2=boxes %>% filter(Ward%in%input$ward3)  
      Data3_2=Data3_1 %>% rename("foo"="PollingDistrictCode")%>% count(foo)
    }else if(input$Box3=="Ward-wide")
    {
      box2=boxes %>% 
        filter(Ward%in%input$ward3) %>% 
        mutate(Box=Ward)  %>% 
        group_by(Ward, Box) %>%
        summarise(ipvotesest=sum(ipvotesest),
                  pvvotesest=sum(pvvotesest),
                  total=sum(total) )
      Data3_2=Data3_1 %>% rename("foo"="WardName")%>% count(foo)
    }else if(input$Box3=="PVs separate")
    {
      box2=boxes %>% filter(Ward%in%input$ward3) %>% 
        dplyr::select(Box, Ward, ipvotesest) %>% 
        rename("total"="ipvotesest") %>% 
        rbind.data.frame(boxes%>% filter(Ward%in%input$ward3) %>% 
                           group_by( Ward) %>% 
                           summarise(total=sum(pvvotesest, na.rm=T)) %>% 
                           mutate(Box=case_when(Ward=="Didsbury West"~"DW Postal", 
                                                Ward=="Didsbury East"~"DE Postal",
                                                Ward=="Ancoats & Beswick"~"AB Postal")) %>% 
                           ungroup()) %>% 
        ungroup()
      Data3_2=Data3_1 %>% rename("foo"="PD2") %>% count(foo)
    }
    
    
    if(input$n_pc=="Count")
    {
      box2 %>% ggplot(aes(x=Box, y=total, fill=Box))+
        geom_bar(stat = "identity", alpha=0.5)+
        geom_bar(data=Data3_2, aes(x=foo, y=n, fill=foo),stat="identity")+
        xlab("Box")+
        ylab("LD votes")+
        theme_bw()+
        theme(legend.position="none",
              axis.text = element_text(color = "black", size=10),
              axis.title = element_text(color = "black", size=13),)+
        scale_fill_manual(values = cols)
    }else if(input$n_pc=="Percentage Phantom")
    {
      box3=merge(box2, Data3_2, by.x="Box", by.y="foo" ) %>%
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
      
      box4=merge(Data3_2, box2, by.y="Box", by.x="foo") %>% 
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
  
  output$distPlot4<-renderPlot({
    if(input$year4=="L22")
    {
      movedstatus=c("OUT", "STAY")
    }else if(input$year4=="L23")
    {
      movedstatus=c("IN", "STAY")
    }
    Data4_1= Data %>%
      rename("foo2"=input$year4) %>% 
      mutate(voted=if_else(foo2, "Yes", "No")
      )%>% 
      rename("Shuttle2"=gsub("L", "Shuttle2_", input$year4)) %>% 
      filter(WardName%in%input$ward4,
             regcycle%in%input$regcycle4,
             Shuttle2%in%input$shuttle4,
             PV2%in%input$voters4,
             voted %in% input$voted4,
             moved %in% movedstatus) %>% 
      rename("foo"=input$group4)
    
    Data4_1 %>% count(foo, regcycle) %>% 
      merge(Data4_1 %>% count(foo), by="foo") %>% 
      mutate(n_pc=100*n.x/n.y) %>% 
      ggplot(aes(x=regcycle, fill=foo, y=n_pc))+
      geom_bar(position = "dodge", stat = "identity")+
      xlab("Cycle of registration")+
      ylab("%")+
      scale_x_discrete(guide = guide_axis(angle = -45))+
      labs(fill=input$group4)+
      theme_bw()+
      theme(
        axis.text = element_text(color = "black", size=10),
        axis.title = element_text(color = "black", size=13),)+
      scale_fill_manual(values = cols)
    
  },  height = reactive(0.8*input$dimension[2]))
  
  #tab 5 voting history
  output$distPlot5<-renderPlot({
    movedstatus=ifelse(input$leftreg5, "FOO", "OUT")
    if(input$year5=="L22")
    {
      election="Local 2022"
      Data5_1=Data %>% filter(Shuttle2_22%in%input$shuttle5, moved!="IN")
    }else if(input$year5=="L23")
    {
      Data5_1=Data %>% filter(Shuttle2_23%in%input$shuttle5, moved!=movedstatus)
      election="Local 2023"
    }
    Data5_2= Data5_1 %>%
      filter(
        WardName%in%input$ward5,
             regcycle%in%input$regcycle5,
             PV2%in%input$voters5
        ) %>%
      mutate(L23=case_when(regcycle2>2023~"Not on register",
                           moved=="OUT"~"Left Register",
                           L23~"Voted",
                           !L23~"Didn't Vote"),
             L22=case_when(regcycle2>2022~"Not on register",
                           L22~"Voted",
                           !L22~"Didn't Vote"),
             L21=case_when(regcycle2>2021~"Not on register",
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
             PrevL22=case_when(regcycle2>2021~"Not on register",
                             PrevL22~"Voted",
                             !PrevL22~"Didn't Vote"),
             PrevL23=case_when(regcycle2>2022~"Not on register",
                               PrevL23~"Voted",
                               !PrevL23~"Didn't Vote"),
             G19=case_when(G19~"Voted",
                           regcycle2>2019~"Not on register",
                           !G19~"Didn't Vote")) %>%
      rename("foo2"=input$year5)
    
    electionnames=c("Local 2022"="L22","Local 2021"="L21", "General 2019"="G19", "Local 2019"="L19", "Local 2018"="L18", "Local By-election 2022"="B22", "Any Local 18-21"="PrevL22","Any Local 19-29"="PrevL23")
    if(input$year5=="L22")
    {
      sankey=Data5_2 %>% rename("foo"=input$election5_22)
      comparename=names(electionnames[electionnames==input$election5_22])
    }else if(input$year5=="L23")
    {
      sankey=Data5_2 %>% rename("foo"=input$election5_23)
      comparename=names(electionnames[electionnames==input$election5_23])
    }

   
     #does the stuff for the sankey diagram
    df=sankey %>% make_long(foo, foo2)

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
      scale_fill_manual(values = c("Didn't Vote"="#E41A1C","Not on register"="#F6CB2F","Voted"="#4DAF4A", "Left Register"="#F6CB2F"))+
      scale_x_discrete(name=c("foo", "foo2"), labels=c(comparename, election), position="top")+
      theme(legend.position = "none", axis.title.x = element_blank(),
            axis.text.x = element_text(color = "grey20", size = 20, hjust = .5, vjust = .5, face = "bold"))
  }, height = reactive(0.8*input$dimension[2]))
}