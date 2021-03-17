# Tick Pathogen Data

library(tidyverse)
library(readxl)

tbo <- read_excel("data/TBO_SERDP_Results_202008.xlsx",
                  sheet = "2017-18 info & TBA hits")
names(tbo)
names(tbo)[2] <- "Installation"

tbo <- tbo %>%
      select(`sample ID`:`Coxiella endosymbiont`, `number of TBAs per tick`)
names(tbo)

tbo <- tbo %>%
      rename(
            sample_ID = `sample ID`, plot_ID = `plot ID`,
            collection_method = `collection method`, ng_ul_DNA = `ng/ul DNA`,
            DNA_method = `DNA extract method`,
            tbo1 = `Tick-borne Agent...12`, tbo2 = `Tick-borne Agent...13`,
            tbo3 = `Tick-borne Agent...14`, tbo4 = `Tick-borne Agent...15`,
            tbo5 = `Tick-borne Agent...16`,
            tbo6 = `Rickettsia amblyommatis...17`,
            tbo7 = `Francisella sp....18`, tbo8 = `Coxiella endosymbiont`,
            tbo_per_tick = `number of TBAs per tick`
      )
tbo %>% summary()
tbo <- filter(tbo, !is.na(date))
unique(tbo$collection_method)
xtabs(~collection_method, data = tbo)
tbo %>% group_by(collection_method) %>%
      summarise(tbos = sum(tbo_per_tick),
                ticks = length(plot_ID))

tbo_long <- tbo %>%
      select(Installation, plot_ID, date, sample_ID, collection_method, tbo1:tbo8) %>%
      pivot_longer(cols = c(tbo1:tbo8),
                   names_to = "tboid", values_to = "tbo",
                   values_drop_na = TRUE)
sort(unique(tbo_long$tbo))

## Correct tbo names
tbo_long$tbo[tbo_long$tbo=="Ehrlichia muris subsp. euclariensis" |
            tbo_long$tbo=="Ehrlichia muris subsp. euclairensis"] <- "Ehrlichia muris subsp. eauclairensis"
tbo_long$tbo[tbo_long$tbo=="Ricettsia bellii"] <- "Rickettsia bellii"
tbo_long$tbo[tbo_long$tbo=="Theileria sp"] <- "Theileria sp."


pathogenecity <- read_excel("data/TBO_pathogenicity.xlsx")
p <- pathogenecity %>%
      rename(
            Pathogen_Name = `PATHOGEN NAME`,
            Domestic_Animals = `Domestic Animals`,
            Human_Endo = `Human/Endosymbiont`,
            Human_Animal = `Human/Animal`
      ) %>%
      select(Pathogen_Name:Unknown) %>%
      mutate(Human = ifelse(Human=="+", "Yes","No"),
             Wildlife = ifelse(Wildlife=="+", "Yes","No"),
             Domestic_Animals = ifelse(Domestic_Animals=="+", "Yes","No"),
             Endosymbiont = ifelse(Endosymbiont=="+", "Yes","No"),
             Human_Endo = ifelse(Human_Endo=="+", "Yes","No"),
             Human_Animal = ifelse(Human_Animal=="+", "Yes","No"),
             Unknown = ifelse(Unknown=="+", "Yes","No"))
p$Pathogen_Name[p$Pathogen_Name=="Coxiella endosymbiont of Amblyomma americanum"] <- "Coxiella endo. of A. americanum"

d <- left_join(tbo_long, p, by = c("tbo"="Pathogen_Name"))

d2 <- d %>%
      filter(Human=="Yes" | Human_Endo == "Yes" | Human_Animal == "Yes")
sid <- unique(d2$sample_ID)
human_strict <- d %>%
      filter(Human=="Yes")
sid2 <- unique(human_strict$sample_ID)



tbo2 <- tbo %>%
      select(sample_ID:life_stage, tbo_per_tick) %>%
      mutate(human_path = ifelse(sample_ID%in%sid, "Yes","No"))

tbo_plot_agg <- tbo2 %>%
      mutate(year = lubridate::year(date)) %>%
      group_by(Installation, site, plot_ID, year, human_path) %>%
      summarise(Pathogen_abundance = sum(tbo_per_tick),
                Tick_abundance = length(plot_ID)) %>%
      mutate(tbo_per_tick = Pathogen_abundance/Tick_abundance)
summary(tbo_plot_agg)
write_csv(tbo_plot_agg, "data/tbo_ticks_plot_year.csv")

tbo_ticks_traps <- tbo2 %>%
      filter(collection_method=="trap") %>%
      mutate(year = lubridate::year(date)) %>%
      group_by(Installation, site, plot_ID, year, human_path) %>%
      summarise(Pathogen_abundance = sum(tbo_per_tick),
                Tick_abundance = length(plot_ID)) %>%
      mutate(tbo_per_tick = Pathogen_abundance/Tick_abundance)
write_csv(tbo_ticks_traps, "data/tbo_ticks_onTraps.csv")


tbo_human_strict <- tbo %>%
      select(sample_ID:life_stage, tbo_per_tick) %>%
      mutate(human_path = ifelse(sample_ID%in%sid2, "Yes","No"))

tbo_human_strict_traps <- tbo_human_strict %>%
      filter(collection_method=="trap") %>%
      mutate(year = lubridate::year(date)) %>%
      group_by(Installation, site, plot_ID, year, human_path) %>%
      summarise(Pathogen_abundance = sum(tbo_per_tick),
                Tick_abundance = length(plot_ID)) %>%
      mutate(tbo_per_tick = Pathogen_abundance/Tick_abundance)
write_csv(tbo_human_strict_traps, "data/tbo_human_strict_onTraps.csv")

p <- p %>%
      pivot_longer(
            cols = c(Human:Unknown),
            names_to = "Pathogen_type",
            values_to = "foo",
            values_drop_na = TRUE) %>%
      select(-foo)

summary(d)

d2 <- d %>% filter(collection_method=="Whalen",
             Human=="Yes"|Human_Animal=="Yes"|Human_Endo=="Yes")
d2 %>% select(tbo)
xtabs(~Installation, data = d2)

d3 <- d %>% filter(tbo=="Ehrlichia muris subsp. eauclairensis")
unique(d3$Installation)