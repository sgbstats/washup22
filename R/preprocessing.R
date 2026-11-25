library(tidyverse)
library(readxl)
streets <- read_xlsx("Data/streets.xlsx", sheet = "Address to street")
walks <- read_xlsx("Data/streets.xlsx", sheet = "Walks")
pools <- read_xlsx("Data/Pools.xlsx")
boxes0 <- read_xlsx("Data/Boxes.xlsx") |>
  mutate(
    total = ipvotesest + pvvotesest,
    Box = case_when(
      grepl("DW", Box) ~ paste("4", Box, sep = ""),
      grepl("AB", Box) ~ paste("2", Box, sep = "")
    )
  )

# This was moved to a file not in the repo for security concerns
regdata22 <- readxl::read_excel(
  "H:\\My Drive\\Side projects\\Shiny washup\\regdata.xlsx"
)
regdata23 <- readxl::read_excel(
  "H:\\My Drive\\Side projects\\Shiny washup\\regdata23.xlsx"
)
regdata24 <- readxl::read_excel(
  "H:\\My Drive\\Side projects\\Shiny washup\\regdata224.xlsx"
)
regdata26 <- readxl::read_excel(
  "H:\\My Drive\\Side projects\\Shiny washup\\regdata26.xlsx"
)

regdata <- regdata22 |>
  select(-contains("Address"), -PostalCode, -City) |>
  rbind.data.frame(
    regdata23 |>
      filter(
        !`Voter File VANID` %in% regdata22$`Voter File VANID`,
        DateReg < as.Date("2023-06-02")
      ) |>
      select(-ElectorNumberWithSuffix)
  ) |>
  rbind.data.frame(
    regdata24 |>
      filter(
        !`Voter File VANID` %in% regdata22$`Voter File VANID`,
        !`Voter File VANID` %in% regdata23$`Voter File VANID`,
        DateReg < as.Date("2024-06-02")
      )
  ) |>
  rbind.data.frame(
    regdata26 |>
      filter(
        !`Voter File VANID` %in% regdata22$`Voter File VANID`,
        !`Voter File VANID` %in% regdata23$`Voter File VANID`,
        !`Voter File VANID` %in% regdata24$`Voter File VANID`,
        is.na(DateReg) | DateReg < as.Date("2026-06-02")
      ) |>
      select(-ElectorNumberWithSuffix)
  ) |>
  mutate(
    regstatus = paste0(
      if_else(`Voter File VANID` %in% regdata22$`Voter File VANID`, "L22", ""),
      if_else(`Voter File VANID` %in% regdata23$`Voter File VANID`, "L23", ""),
      if_else(`Voter File VANID` %in% regdata24$`Voter File VANID`, "L24", ""),
      if_else(`Voter File VANID` %in% regdata26$`Voter File VANID`, "L26", "")
    )
  )
# |> merge(streets, by=c("AddressLine1", "AddressLine2", "AddressLine3", "City","PostalCode")) |>
#   select(-c("AddressLine1", "AddressLine2", "AddressLine3", "City","PostalCode")) |>
# merge(walks, by="Walk")
#this was removed as a feature when I was thinking of doing streets.

#merging in the data for each of the pools
Data <- regdata |>
  mutate(
    Shuttle22 = `Voter File VANID` %in% pools$Shuttle22,
    Shuttle23 = `Voter File VANID` %in% pools$Shuttle23,
    Shuttle24 = `Voter File VANID` %in% pools$Shuttle24,
    Shuttle26 = `Voter File VANID` %in% pools$Shuttle26,
    L24 = `Voter File VANID` %in% pools$L24,
    L23 = `Voter File VANID` %in% pools$L23,
    L26 = `Voter File VANID` %in% pools$L26,
    L22 = `Voter File VANID` %in% pools$L22,
    L21 = `Voter File VANID` %in% pools$L21,
    L19 = `Voter File VANID` %in% pools$L19,
    L18 = `Voter File VANID` %in% pools$L18,
    G19 = `Voter File VANID` %in% pools$G19,
    B22 = `Voter File VANID` %in% pools$B22,
    PV = `Voter File VANID` %in% pools$PV,
    PV26 = `Voter File VANID` %in% pools$PV26,
    PrevL22 = L18 | L19 | L21 | B22,
    PrevL23 = L23 | L19 | L21 | B22,
    PrevL24 = L24 | L23 | L21 | B22,
    PrevL26 = L26 | L24 | L23 | L21 | B22,
    G24 = `Voter File VANID` %in% pools$G24
  ) |>
  mutate(
    DateReg = if_else(lubridate::year(DateReg) < 2001, NA_Date_, DateReg),
    year = factor(
      if_else(!is.na(DateReg), as.character(lubridate::year(DateReg)), "UK"),
      c("UK", as.character(seq(from = 2001, to = 2027, by = 1))),
      ordered = T
    ),
    regcycle = factor(
      if_else(
        !is.na(DateReg),
        as.character(lubridate::year(DateReg + months(6))),
        "UK"
      ),
      c("UK", as.character(seq(from = 2001, to = 2027, by = 1))),
      ordered = T
    ),
    regcycle2 = as.character(regcycle) |> as.numeric(),
    PollingDistrictCode = if_else(
      PollingDistrictCode %in% c("44899", "45264"),
      "4DEC",
      PollingDistrictCode
    )
  ) |>
  select(-DateReg, -year, -contains("Address")) |>
  mutate(
    PV2 = if_else(PV, "Postal", "In Person"),
    Shuttle2_22 = if_else(Shuttle22, "Shuttle", "Non-Shuttle"),
    Shuttle2_23 = if_else(Shuttle23, "Shuttle", "Non-Shuttle"),
    Shuttle2_24 = if_else(Shuttle24, "Shuttle", "Non-Shuttle"),
    Shuttle2_26 = if_else(Shuttle26, "Shuttle", "Non-Shuttle"),
    PV2_26 = if_else(PV26, "Postal", "In Person")
  )


DEcount <- read.csv("Data/Boxest.csv") |>
  select(Box, LD) |>
  mutate(LD = LD / 100, Box = paste("4", Box, sep = "")) |>
  filter(grepl("DE", Box))

#this is lazy because I had the box estimates from the counts already done but not the numbers in each box for East so wanted to go back an make the file match although in hindsight now that I have the data I can do the estimates like I did for west.
DEIP <- Data |>
  filter(WardName == "Didsbury East") |>
  filter(L22, !PV) |>
  count(PollingDistrictCode) |>
  merge(DEcount, by.x = "PollingDistrictCode", by.y = "Box") |>
  mutate(ipvotesest = n * LD) |>
  select(PollingDistrictCode, ipvotesest)

DEPV <- Data |>
  filter(WardName == "Didsbury East") |>
  filter(L22, PV) |>
  count(PollingDistrictCode) |>
  mutate(pvvotesest = n * DEcount[10, 2]) |>
  select(-n)

boxes22 <- boxes0 |>
  rbind.data.frame(
    DEIP |>
      merge(DEPV, by = "PollingDistrictCode") |>
      rename("Box" = "PollingDistrictCode") |>
      mutate(total = ipvotesest + pvvotesest, Party = "LD")
  ) |>
  mutate(
    Ward = case_when(
      grepl("DW", Box) ~ "Didsbury West",
      grepl("DE", Box) ~ "Didsbury East",
      grepl("AB", Box) ~ "Ancoats & Beswick"
    )
  ) |>
  filter(Party == "LD") |>
  select(-Party)

boxes23 <- read.csv("Data/Postalsupdate.csv") |>
  select(-pc, -X) |>
  filter(!grepl("WT", Box)) |>
  mutate(
    Ward = case_when(
      grepl("DW", Box) ~ "Didsbury West",
      grepl("DE", Box) ~ "Didsbury East",
      grepl("AB", Box) ~ "Ancoats & Beswick"
    ),
    Box = case_when(
      grepl("DW|DE", Box) ~ paste("4", Box, sep = ""),
      grepl("AB", Box) ~ paste("2", Box, sep = "")
    )
  ) |>
  filter(Party == "LD") |>
  rename("total" = "votesest")

boxsize24 <- read_excel("Data/count24.xlsx", sheet = "Boxes")
boxes24_1 <- read.csv("Data/Box_count24.csv") |>
  select(-X, -Votes) |>
  filter(Party == "LD") |>
  merge(boxsize24) |>
  mutate(votes = pcv * Total / 100) |>
  mutate(
    Ward = case_when(
      grepl("DW", Box) ~ "Didsbury West",
      grepl("DE", Box) ~ "Didsbury East",
      grepl("AB", Box) ~ "Ancoats & Beswick"
    ),
    Box = case_when(
      grepl("DW|DE", Box) ~ paste("4", Box, sep = ""),
      grepl("AB", Box) ~ paste("2", Box, sep = "")
    )
  )


boxes24_2 <- boxes24_1 |>
  filter(grepl("P", Box)) |>
  merge(
    Data |>
      filter(PV) |>
      count(WardName, PollingDistrictCode) |>
      mutate(pc = n / sum(n), .by = "WardName"),
    by.x = "Ward",
    by.y = "WardName"
  ) |>
  mutate(pvvotesest = votes * pc) |>
  select(PollingDistrictCode, pvvotesest, Ward)


boxes24 <- boxes24_1 |>
  rename(ipvotesest = votes) |>
  filter(!grepl("P", Box)) |>
  merge(
    boxes24_2,
    by.x = c("Ward", "Box"),
    by.y = c("Ward", "PollingDistrictCode")
  ) |>
  mutate(total = ipvotesest + pvvotesest) |>
  select("Box", "Party", "ipvotesest", "pvvotesest", "total", "Ward", )


ward_lookup <- c(
  dw = "Didsbury West",
  de = "Didsbury East",
  ab = "Ancoats & Beswick",
  wt = "Withington"
)

# Rows are individual ballot boxes (several per polling district); "pv" rows are postal votes
boxes26_1 <- read_excel(
  "H:\\My Drive\\Side projects\\Shiny washup\\votesandpds26.xlsx"
) |>
  # Typo in source: "adpv" is the Ancoats & Beswick postal vote row
  mutate(pd = if_else(pd == "adpv", "abpv", pd)) |>
  pivot_longer(-pd, names_to = "Party", values_to = "votes") |>
  mutate(
    Party = recode_values(
      Party,
      "ld" ~ "LD",
      "lab" ~ "Lab",
      "con" ~ "Con",
      "grn" ~ "Grn",
      "ref" ~ "Ref"
    ),
    ward_code = substr(pd, 1, 2),
    Ward = unname(ward_lookup[ward_code]),
    is_pv = grepl("pv$", pd)
  ) |>
  # Drops wards not in the voter data (dg, py, om)
  filter(!is.na(Ward))

# Postal votes allocated to boxes by share of each ward's 2026 postal voters
pvshare26 <- Data |>
  filter(PV26) |>
  count(WardName, Box = PollingDistrictCode) |>
  mutate(pc = n / sum(n), .by = WardName)

boxes26_pv <- boxes26_1 |>
  filter(is_pv) |>
  summarise(pvtotal = sum(votes), .by = c(Ward, Party)) |>
  inner_join(
    pvshare26,
    by = c("Ward" = "WardName"),
    relationship = "many-to-many"
  ) |>
  mutate(pvvotesest = pvtotal * pc) |>
  select(Ward, Box, Party, pvvotesest)

boxes26 <- boxes26_1 |>
  filter(!is_pv) |>
  mutate(Box = paste0(if_else(ward_code == "ab", "2", "4"), toupper(pd))) |>
  summarise(ipvotesest = sum(votes), .by = c(Ward, Box, Party)) |>
  left_join(boxes26_pv, by = c("Ward", "Box", "Party")) |>
  mutate(
    pvvotesest = coalesce(pvvotesest, 0),
    total = ipvotesest + pvvotesest
  ) |>
  select(Box, Party, ipvotesest, pvvotesest, total, Ward)

save(Data, boxes22, boxes23, boxes24, boxes26, file = "washup/Data/Data.RDa")
