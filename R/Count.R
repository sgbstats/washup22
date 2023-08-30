setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(readxl)
`%notin%`=Negate(`%in%`)

Tally=readxl::read_excel("Count.xlsx", sheet="Tally")
Box=readxl::read_excel("Count.xlsx", sheet="Boxes")
PVs=readxl::read_excel("Count.xlsx", sheet="PVs") 
final=readxl::read_excel("Count.xlsx", sheet="final") 

T1=Tally %>% pivot_longer(cols = -c("Box"), names_to = "Party", values_to = "Votes") %>%
  group_by(Box, Party) %>% 
  summarise(Votes=sum(Votes,na.rm=T)) %>% 
  group_by(Box) %>% 
  mutate(pcv=Votes/sum(Votes,na.rm=T)) %>% 
  ungroup() 

B1=Box %>% 
  filter(Box %in% Tally$Box) %>% 
  mutate(Ward=substr(Box,1,2)) %>% 
  group_by(Ward) %>% 
  mutate(pct=Total/sum(Total)) %>% 
  ungroup()

B1 %>% dplyr::select(Ward, Box)

overall=T1 %>% dplyr::select(Box, Party, pcv)%>%
  merge(B1 %>% dplyr::select(Box, Ward, pct), by="Box") %>% 
  mutate(pc=pcv*pct) %>% 
  group_by(Ward, Party) %>% 
  summarise(Result=100*sum(pc))

correction=overall %>% merge(final, by=c("Ward", "Party")) %>% 
  mutate(cor=Res-Result)


####
T1 %>% mutate(Boxpc=round(100*pcv, 1)) %>% 
  dplyr::select(Box, Party, Boxpc) %>% 
  mutate(Ward=substr(Box,1,2)) %>% 
  merge(correction %>% select(Ward, Party, cor), by=c("Ward", "Party")) %>% 
  mutate(Boxpc=Boxpc+cor) %>% 
  select(-cor) %>% 
  pivot_wider(names_from = "Party", values_from = "Boxpc") %>% 
  dplyr::select(Box, LD, Lab, Grn, Con, Oth) %>% 
  mutate_if(is.numeric, round) %>% 
  arrange(Box) %>% 
  write.csv(file = "Boxest.csv", na="")




T1 %>% mutate(Boxpc=round(100*pcv, 1)) %>% 
  filter(Box %notin% c("DWP", "ABP", "WTP", "DEP")) %>% 
  dplyr::select(Box, Party, pcv) %>% 
  merge(Box, by="Box") %>% 
  mutate(ipvotesest=pcv*Total) %>% 
  select(Box, Party, ipvotesest) %>% 
  merge(PVs%>% mutate(Ward=substr(Box,1,2)) %>% 
          merge(T1 %>% filter(grepl("P", Box)) %>% 
                  mutate(Ward=substr(Box,1,2)), by="Ward") %>% 
          ungroup() %>% 
          mutate(pvvotesest=Votes.x*pcv) %>% 
          select(Box.x, Party, pvvotesest), by.x=c("Box", "Party"), by.y=c("Box.x", "Party")) %>% 
  mutate(votesest=ipvotesest+pvvotesest) %>% 
  group_by(Box) %>% 
  mutate(pc=round(100*votesest/sum(votesest),1)) %>% 
  mutate_at(vars(c("ipvotesest", "pvvotesest", "votesest")), round, digits=0) %>% 
  write.csv("Postalsupdate.csv")

