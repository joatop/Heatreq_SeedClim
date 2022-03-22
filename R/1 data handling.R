library(tidyr)
library(dplyr)
library(plyr)
library(stringr)
library(tidyverse)
library("DBI")# also needs RSQLite installed

#### data handling ####
### load seedclim community data
## Download community data from SeedClim database

# make database connection
con <- dbConnect(RSQLite::SQLite(), "input/seedclim.sqlite")

# get taxonomy table
taxa <- tbl(con, "taxon") %>%
  collect()

# tells you whats in the data base
DBI::dbListTables(con)

# Assemble community data set for the turf
comdat <- tbl(con, "turf_community") %>%
  collect() %>%
  left_join(tbl(con, "taxon"),by="species", copy=TRUE) %>%
  left_join(tbl(con, "turfs"), by="turfID", copy=TRUE) %>%
  left_join(tbl(con, "plots"), by=c("destinationPlotID"="plotID"), copy=TRUE) %>% 
  left_join(tbl(con, "blocks"), by= "blockID", copy=TRUE, suffix = c("", "_block")) %>%
  left_join(tbl(con, "sites"), by="siteID", copy=TRUE) %>%
  left_join(tbl(con, "turf_environment"), by = c("turfID", "year"), copy = TRUE) %>% 
  select(siteID, blockID, turfID, latitude, longitude, annual_precipitation_gridded, summer_temperature_gridded, year,
         TTtreat, species, species_name, family, cover, total_vascular, total_bryophytes, total_lichen, vegetation_height, moss_height) %>%
  collect() %>% 
  filter(TTtreat %in% list("TTC","TT1","TT2")) %>%
  filter(year %in% list(2009,2019))

# take out unidentified species
comdat <- filter(comdat, !(species %in% c("NID.seedling", "NID.gram","Åkerplante","NID.herb")))

# disconnect the database connection
dbDisconnect()

### load the indicator data
ind_swe <- read.table(file="C:/Users/joachim.topper/OneDrive - NINA/work/R projects/projects/økol tilst indikatorverdier/input/indicators/ind_swe.txt", sep = '\t', header=T, quote = '')
summary(ind_swe)

## indicator data handling
names(ind_swe)[1] <- 'species'
ind_swe$species <- as.factor(ind_swe$species)
summary(ind_swe$species)
ind_swe <- ind_swe[!is.na(ind_swe$species),]
# make Hieracium sections into into a species
ind_swe <- ind_swe %>% 
  mutate(species=str_replace(species,"Hieracium sect. Alpina", "Hieracium alpinum")) %>%
  mutate(species=str_replace(species,"Hieracium sect. Vulgata", "Hieracium vulgatum"))
# simplify the wording in species to match with the species data
ind_swe[,'species'] <- word(ind_swe[,'species'], 1,2)
ind_swe[duplicated(ind_swe[,'species']),"species"]
ind_swe.dup <- ind_swe[duplicated(ind_swe[,'species']),"species"]
ind_swe[ind_swe$species %in% ind_swe.dup,]
ind_swe <- ind_swe[!duplicated(ind_swe[,'species']),]
ind_swe[ind_swe$species %in% ind_swe.dup,]
ind_swe$species <- as.factor(ind_swe$species)
ind_swe[duplicated(ind_swe[,'species']),"species"]
summary(ind_swe$species)


### merge community and indicator data
comind <- merge(x=comdat, y= ind_swe[,c(1,11)],by.x="species_name", by.y="species", all.x=T)

# checking which species didn't find a match
unique(comind[is.na(comind$Heat_requirement),'species_name'])
unique(comind[!is.na(comind$Heat_requirement),'species_name'])

# fix species name issues
ind_swe <- ind_swe %>% 
  mutate(species=str_replace(species,"Aconitum lycoctonum", "Aconitum septentrionale")) %>% 
  mutate(species=str_replace(species,"Carex simpliciuscula", "Kobresia simpliciuscula")) 
comdat <- comdat %>% 
  mutate(species_name=str_replace(species_name,"Arctous alpinus", "Arctous alpina")) %>%
  mutate(species_name=str_replace(species_name,"Hieracium pilosella", "Pilosella officinarum")) %>%
  mutate(species_name=str_replace(species_name,"Empetrum hermaphroditum", "Empetrum nigrum")) %>%
  mutate(species_name=str_replace(species_name,"Hieracium vulgatum", "Hieracium umbellatum")) %>%
  mutate(species_name=str_replace(species_name,"Leontodon autumnalis", "Scorzoneroides autumnalis")) %>%
  mutate(species_name=str_replace(species_name,"Loiseleuria procumbens", "Kalmia procumbens")) %>%
  mutate(species_name=str_replace(species_name,"Omalotheca supina", "Gnaphalium supinum")) %>%
  mutate(species_name=str_replace(species_name,"Omalotheca norvegica", "Gnaphalium norvegicum")) %>%
  mutate(species_name=str_replace(species_name,"Omalotheca sylvatica", "Gnaphalium sylvaticum")) %>%
  mutate(species_name=str_replace(species_name,"Trientalis europaea", "Lysimachia europaea"))
 
#get rid of the ? in Crepis paludosa?
comdat <- comdat %>%
  mutate(species_name=gsub("\\?", "", species_name))

# merge & check again
comind <- merge(x=comdat, y= ind_swe[,c(1,11)],by.x="species_name", by.y="species", all.x=T)
# checking which species didn't find a match
unique(comind[is.na(comind$Heat_requirement),'species_name'])
unique(comind[!is.na(comind$Heat_requirement),'species_name'])

summary(comind)
dim(comind) # 261 of 3934 observations are without indicator values for heat requirement
comind <- comind[!is.na(comind$Heat_requirement),]
# replace space with dot in turfID
comind <- comind %>%
  mutate(turfID=gsub(" ", ".", comind$turfID))
# merge turfID & year
comind$turfYr <- paste(comind$turfID,comind$year,sep="_")

colnames(comind)

# add original altitudinal classes
comind <- comind %>%
  mutate(alt.orig=case_when(
    TTtreat %in% list("TTC","TT1") & siteID %in% c('Ovstedalen', 'Arhelleren', 'Vikesland', 'Fauske') ~ 'boreal',
    TTtreat %in% list("TTC","TT1") & siteID %in% c('Veskre', 'Rambera', 'Hogsete', 'Alrust') ~ 'sub-alpine',
    TTtreat %in% list("TTC","TT1") & siteID %in% c('Skjelingahaugen', 'Gudmedalen', 'Lavisdalen', 'Ulvehaugen') ~ 'alpine',
    TTtreat %in% list("TT2") & siteID %in% c('Ovstedalen', 'Arhelleren', 'Vikesland', 'Fauske') ~ 'sub-alpine',
    TTtreat %in% list("TT2") & siteID %in% c('Veskre', 'Rambera', 'Hogsete', 'Alrust') ~ 'alpine',
  ))

# listing all naturally alpine species
sp.alp <- unique(
  comind %>% 
    filter(alt.orig %in% list("alpine")) %>%
    filter(!(year %in% list(2019) & TTtreat %in% list("TT2"))) %>%
    select(species_name)
)[,1]
# adding a variable indicating whether or not a species is natural in the alpine
comind <- comind %>%
  mutate(species.alp=case_when(
    species_name %in% sp.alp ~ 'yes',
    !(species_name %in% sp.alp) ~ 'no'
  ))
      
# listing all naturally sub-alpine or alpine species
sp.subalp <- unique(
  comind %>% 
    filter(alt.orig %in% list("alpine","sub-alpine")) %>%
    filter(!(year %in% list(2019) & TTtreat %in% list("TT2"))) %>%
    select(species_name)
)[,1]
# adding a variable indicating whether or not a species is natural in the sub-alpine or alpine
comind <- comind %>%
  mutate(species.subalp=case_when(
    species_name %in% sp.subalp ~ 'yes',
    !(species_name %in% sp.subalp) ~ 'no'
  ))            


comind %>%
  filter(alt.orig %in% list("alpine")) %>%
  filter(TTtreat %in% list("TTC")) %>%
  filter(Heat_requirement>6) %>%
  select(species_name,cover,year,Heat_requirement)
  