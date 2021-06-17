# Tick Pathogen Data

library(tidyverse)
library(readxl)
library(rlang)

tbo <- read_excel("/projectnb/dietzelab/mccabete/SERDP_shiny/serdp_data/TBO_SERDP_Results_202008.xlsx",
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


pathogenecity <- read_excel("/projectnb/dietzelab/mccabete/SERDP_shiny/serdp_data/TBO_pathogenicity.xlsx")
p <- pathogenecity %>%
  rename(
    Pathogen_Name = `PATHOGEN NAME`,
    Domestic_Animals = `Domestic Animals`,
    Human_Endo = `Human/Endosymbiont`,
    Human_Animal = `Human/Animal`, 
    Vertebrate_Host = `VERTEBRATE TICK HOST(S)`,
    Disease = `DISEASE`
  ) %>%
  mutate(Human = ifelse(Human=="+", "Yes","No"),
         Wildlife = ifelse(Wildlife=="+", "Yes","No"),
         Domestic_Animals = ifelse(Domestic_Animals=="+", "Yes","No"),
         Endosymbiont = ifelse(Endosymbiont=="+", "Yes","No"),
         Human_Endo = ifelse(Human_Endo=="+", "Yes","No"),
         Human_Animal = ifelse(Human_Animal=="+", "Yes","No"),
         Unknown = ifelse(Unknown=="+", "Yes","No"))

p$Pathogen_Name[p$Pathogen_Name=="Coxiella endosymbiont of Amblyomma americanum"] <- "Coxiella endo. of A. americanum"

d <- left_join(tbo_long, p, by = c("tbo"="Pathogen_Name"))

d <- select(d, -c("TICK HOSTS, DOD SERDP STUDY", "DISEASE HOST", "TICK HOST(S)", "plot_ID":"tboid" ))

d$animal_infection <- rep(NA, length(d$Installation))
for (i in seq_along(d$Installation)){
  tmp <- c("")
  if(!is.na(d$Human[i])){tmp <- paste(tmp, "Human")}
  if(!is.na(d$Wildlife[i])){tmp <- paste(tmp, "Wildlife") }
  if(!is.na(d$Domestic_Animals[i])){tmp <- paste(tmp, "Domestic Animals")}
  #if(!is.na(d$Endosymbiont[i])){tmp <- paste(tmp, "Endosymbiont")} #not sure if I should include this? 
  if(!is.na(d$Human_Endo[i])){tmp <- paste(tmp, "Human/Endosymbiont")} # Not sure how different from "Human" catagory
  if(!is.na(d$Human_Animal[i])){tmp <- paste(tmp, "Human/Animals")}
  if(!is.na(d$Unknown[i])){tmp <- paste(tmp, "Unknown")}
  if(tmp == ""){tmp <- NA}
  
  d$animal_infection[i] <- tmp
  
}
d <- select(d, -c("Human":"Unknown"))
names(d) <- c("Installation", "Organism/Pathogen", "Vertebrate_Host", "Disease", "Disease_infection_population")
d <- unique(d)
  
write_csv(d, "/projectnb/dietzelab/mccabete/SERDP_shiny/code/www/pathogenicity_by_installation.csv")

# d2 <- d %>%
#   filter(Human=="Yes" | Human_Endo == "Yes" | Human_Animal == "Yes")
# sid <- unique(d2$sample_ID)
# human_strict <- d %>%
#   filter(Human=="Yes")
# sid2 <- unique(human_strict$sample_ID)


