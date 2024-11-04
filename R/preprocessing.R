library(tidyverse)
library(readxl)
`%notin%` <-Negate(`%in%`)
streets=read_xlsx("Data/streets.xlsx", sheet= "Address to street")
walks=read_xlsx("Data/streets.xlsx", sheet= "Walks")
pools=read_xlsx("Data/Pools.xlsx")
boxes0=read_xlsx("Data/Boxes.xlsx") %>% mutate(total=ipvotesest+pvvotesest, Box=case_when(grepl("DW", Box)~paste("4", Box, sep=""),
                                                                                          grepl("AB", Box)~paste("2", Box, sep="")))

# This was moved to a file not in the repo for security concerns
regdata22 <- readxl::read_excel("C:\\Users\\mbbx4sb5\\OneDrive - Personal\\OneDrive\\LD\\Side projects\\Shiny washup\\regdata.xlsx")
regdata23 <- readxl::read_excel("C:\\Users\\mbbx4sb5\\OneDrive - Personal\\OneDrive\\LD\\Side projects\\Shiny washup\\regdata23.xlsx")
regdata24 <- readxl::read_excel("C:\\Users\\mbbx4sb5\\OneDrive - Personal\\OneDrive\\LD\\Side projects\\Shiny washup\\regdata224.xlsx")


regdata = regdata22 %>% 
  select(-contains("Address"), -PostalCode, -City) %>% 
  rbind.data.frame(regdata23 %>% filter(`Voter File VANID`%notin%regdata22$`Voter File VANID`, DateReg<as.Date("2023-06-02")) %>% 
                     select(-ElectorNumberWithSuffix)) %>% 
  rbind.data.frame(regdata24 %>% filter(`Voter File VANID`%notin%regdata22$`Voter File VANID`,
                                        `Voter File VANID`%notin%regdata23$`Voter File VANID`,
                                        DateReg<as.Date("2024-06-02"))) %>% 
  mutate(regstatus=paste0(if_else(`Voter File VANID`%in%regdata22$`Voter File VANID`,"L22", ""),
                          if_else(`Voter File VANID`%in%regdata23$`Voter File VANID`,"L23", ""),
                          if_else(`Voter File VANID`%in%regdata24$`Voter File VANID`,"L24", "")))
# %>% merge(streets, by=c("AddressLine1", "AddressLine2", "AddressLine3", "City","PostalCode")) %>%
#   select(-c("AddressLine1", "AddressLine2", "AddressLine3", "City","PostalCode")) %>% 
# merge(walks, by="Walk")
#this was removed as a feature when I was thinking of doing streets.

#merging in the data for each of the pools
Data=regdata %>% 
  mutate(Shuttle22=`Voter File VANID`%in%pools$Shuttle22,
         Shuttle23=`Voter File VANID`%in%pools$Shuttle23,
         Shuttle24=`Voter File VANID`%in%pools$Shuttle24,
         L24=`Voter File VANID`%in%pools$L24,
         L23=`Voter File VANID`%in%pools$L23,
         L22=`Voter File VANID`%in%pools$L22,
         L21=`Voter File VANID`%in%pools$L21,
         L19=`Voter File VANID`%in%pools$L19,
         L18=`Voter File VANID`%in%pools$L18,
         G19=`Voter File VANID`%in%pools$G19,
         B22=`Voter File VANID`%in%pools$B22,
         PV=`Voter File VANID`%in%pools$PV,
         PrevL22=L18|L19|L21|B22,
         PrevL23=L23|L19|L21|B22,
         PrevL24=L24|L23|L21|B22) %>% 
  mutate(DateReg=if_else(lubridate::year(DateReg)<2001, NA_Date_, DateReg),
         year=factor(if_else(!is.na(DateReg),as.character(lubridate::year(DateReg)),"UK"),
                     c("UK", as.character(seq(from=2001, to=2024, by=1))), ordered=T),
         regcycle=factor(if_else(!is.na(DateReg),as.character(lubridate::year(DateReg+months(6))),"UK"),
                         c("UK", as.character(seq(from=2001, to=2024, by=1))), ordered=T),
         regcycle2=as.character(regcycle) %>% as.numeric(),
         PollingDistrictCode=if_else(PollingDistrictCode%in%c("44899", "45264"), "4DEC", PollingDistrictCode)) %>% 
  select(-DateReg, -year, -contains("Address"))%>% 
  mutate(PV2=if_else(PV, "Postal", "In Person"),
         Shuttle2_22=if_else(Shuttle22, "Shuttle", "Non-Shuttle"),
         Shuttle2_23=if_else(Shuttle23, "Shuttle", "Non-Shuttle"),
         Shuttle2_24=if_else(Shuttle24, "Shuttle", "Non-Shuttle"))


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

boxes22=boxes0 %>% rbind.data.frame(DEIP %>% merge(DEPV, by="PollingDistrictCode") %>% 
                                    rename("Box"="PollingDistrictCode")%>%
                                    mutate(total=ipvotesest+pvvotesest, Party="LD")) %>%
  mutate(Ward=case_when(grepl("DW", Box)~"Didsbury West",
                        grepl("DE", Box)~"Didsbury East",
                        grepl("AB", Box)~"Ancoats & Beswick")) %>% 
  filter(Party=="LD") %>% 
  select(-Party)

boxes23=read.csv("Data/Postalsupdate.csv") %>% 
  select(-pc, -X) %>% 
  filter(!grepl("WT", Box)) %>% 
  mutate(Ward=case_when(grepl("DW", Box)~"Didsbury West",
                        grepl("DE", Box)~"Didsbury East",
                        grepl("AB", Box)~"Ancoats & Beswick"),
         Box=case_when(grepl("DW|DE", Box)~paste("4", Box, sep=""),
                       grepl("AB", Box)~paste("2", Box, sep=""))) %>% 
  filter(Party=="LD") %>% 
  rename("total"="votesest")

boxsize24=read_excel("Data/count24.xlsx", sheet="Boxes")
boxes24_1=read.csv("Data/Box_count24.csv") %>% 
  select(-X, -Votes) %>% 
  filter(Party=="LD") %>% 
  merge(boxsize24) %>% 
  mutate(votes=pcv*Total/100) %>% 
  mutate(Ward=case_when(grepl("DW", Box)~"Didsbury West",
                        grepl("DE", Box)~"Didsbury East",
                        grepl("AB", Box)~"Ancoats & Beswick"),
         Box=case_when(grepl("DW|DE", Box)~paste("4", Box, sep=""),
                       grepl("AB", Box)~paste("2", Box, sep="")))


boxes24_2=boxes24_1 %>% 
          filter(grepl("P", Box)) %>%
  merge(Data %>% filter(PV) %>% count(WardName, PollingDistrictCode) %>% 
          mutate(pc=n/sum(n),.by="WardName"), by.x="Ward", by.y="WardName") %>% 
  mutate(pvvotesest=votes*pc) %>% 
  select(PollingDistrictCode, pvvotesest, Ward) 


boxes24=boxes24_1 %>% 
  rename(ipvotesest=votes) %>% 
  filter(!grepl("P", Box)) %>% 
  merge(boxes24_2, by.x=c("Ward", "Box"), by.y=c("Ward","PollingDistrictCode" )) %>% 
  mutate(total=ipvotesest+pvvotesest) %>% 
  select("Box",        "Party" ,     "ipvotesest","pvvotesest", "total"  ,    "Ward" ,)


save(Data, boxes22, boxes23,boxes24,  file="washup/Data/Data.RDa")



