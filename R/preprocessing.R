library(tidyverse)
library(readxl)

streets=read_xlsx("Data/streets.xlsx", sheet= "Address to street")
walks=read_xlsx("Data/streets.xlsx", sheet= "Walks")
pools=read_xlsx("Data/Pools.xlsx")
boxes0=read_xlsx("Data/boxes.xlsx") %>% mutate(total=ipvotesest+pvvotesest, Box=paste("4", Box, sep=""))

# This was moved to a file not in the repo for security concerns
regdata <- read_excel("C:\\Users\\mbbx4sb5\\OneDrive\\LD\\Side projects\\Shiny washup\\regdata.xlsx")

regdata2 = regdata 
# %>% merge(streets, by=c("AddressLine1", "AddressLine2", "AddressLine3", "City","PostalCode")) %>%
#   select(-c("AddressLine1", "AddressLine2", "AddressLine3", "City","PostalCode")) %>% 
# merge(walks, by="Walk")
#this was removed as a feature when I was thinking of doing streets.

#merging in the data for each of the pools
Data=regdata2 %>% 
  mutate(Shuttle=`Voter File VANID`%in%pools$Shuttle,
         L22=`Voter File VANID`%in%pools$L22,
         L21=`Voter File VANID`%in%pools$L21,
         L19=`Voter File VANID`%in%pools$L19,
         L18=`Voter File VANID`%in%pools$L18,
         G19=`Voter File VANID`%in%pools$G19,
         PV=`Voter File VANID`%in%pools$PV,
         PrevL=L18|L19|L21) %>% 
  mutate(year=factor(if_else(!is.na(DateReg),as.character(lubridate::year(DateReg)),"UK"),
                     c("UK", as.character(seq(from=2001, to=2022, by=1))), ordered=T),
         regcycle=factor(if_else(!is.na(DateReg),as.character(lubridate::year(DateReg+months(6))),"UK"),
                         c("UK", as.character(seq(from=2001, to=2022, by=1))), ordered=T),
         regcycle2=as.character(regcycle) %>% as.numeric(),
         PollingDistrictCode=if_else(PollingDistrictCode=="44899", "4DEC", PollingDistrictCode)) %>% 
  select(-DateReg, -year, -contains("Address"), -PostalCode, -City)%>% 
  mutate(PV2=if_else(PV, "Postal", "In Person"),
         Shuttle2=if_else(Shuttle, "Shuttle", "Non-Shuttle"))


DEcount=read.csv("Data/Boxest.csv") %>% select(Box, LD) %>% mutate(LD=LD/100, Box=paste("4", Box, sep="")) %>% filter(grepl("DE", Box)) 

#this is lazy because I had the box estimates from the counts already done but not the numbers in each box for East so wanted to go back an make the file match although in hindsight now that I have the data I can do the estimates like I did for west. 
DEIP=Data %>% filter(WardName=="Didsbury East") %>% 
  filter(L22, !PV) %>% 
  count(PollingDistrictCode) %>% 
  merge(DEcount, by.x = "PollingDistrictCode", by.y="Box") %>%
  mutate(ipvotesest=n*LD) %>% 
  select(PollingDistrictCode, ipvotesest)

DEPV=Data %>% filter(WardName=="Didsbury East") %>% 
  filter(L22, PV) %>%
  count(PollingDistrictCode) %>%
  mutate(pvvotesest=n*DEcount[10,2]) %>% 
  select(-n)

boxes=boxes0 %>% rbind.data.frame(DEIP %>% merge(DEPV, by="PollingDistrictCode") %>% 
                                    rename("Box"="PollingDistrictCode")%>%
                                    mutate(total=ipvotesest+pvvotesest, Party="LD")) %>%
  mutate(Ward=case_when(grepl("DW", Box)~"Didsbury West",
                        grepl("DE", Box)~"Didsbury East")) %>% 
  filter(Party=="LD") %>% 
  select(-Party)

save(Data, boxes, file="washup22/Data/Data.RDa")

