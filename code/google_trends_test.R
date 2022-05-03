#### set-up ####

# load packages
library(gtrendsR)
library(tidyverse)
library(countrycode)

# import data
load("data/combi_20220126.RData") # GLONAF
countries2 <- as_tibble(countries) # from gtrendsR package


#### edit data ####

# save data as tibble
dat <- as_tibble(combi)

# extract regions
# select country-level code
regions <- dat %>%
  select(region_code, region_name) %>%
  unique() %>%
  mutate(country_ISO = str_sub(region_code, 1, 3))

# identify problem codes (warnings in code below)
regions %>%
  filter(country_ISO %in% c("ANT", "BOR", "GAM", "HIS", "LAN", "NAY", "NGU", "SOC", "STM", "TMP", "VGI"))
# these need to be resolved (many are multiple countries)
# ones with only one country match are resolved below

# convert GLONAF codes from ISO-3 to ISO-2
regions2 <- regions %>%
  mutate(country_code = countrycode(country_ISO, origin = 'iso3c', destination = 'iso2c'),
         country_code = case_when(country_ISO == "ANT" & 
                                    region_name == "Netherlands Antilles" ~ "AN",
                                  country_ISO == "GAM" & 
                                    region_name == "Gambia" ~ "GM",
                                  country_ISO == "TMP" & 
                                    region_name == "East Timor" ~ "TP",
                                  TRUE ~ country_code))

# check for matching
regions2 %>%
  anti_join(countries2 %>%
              select(country_code)) %>%
  select(country_ISO, region_name, country_code)
# should return 0

# identify species to test functions
species <- tibble(taxon_corrected = c("Carpobrotus edulis", "Acacia dealbata", "Bromus arvensis", "Eichhornia crassipes"))

# add country info
# filter for test species
dat2 <- dat %>%
  left_join(regions2 %>%
              select(region_code, country_code) %>%
              unique()) %>%
  inner_join(species) %>%
  select(taxon_corrected, country_code) %>% # remove regions (smaller scale than country)
  unique() %>%
  filter(!is.na(country_code))

# check for all species
n_distinct(dat2$taxon_corrected)


#### data export structure ####

# add directory and file name
dat3 <- dat2 %>%
  expand_grid(gsource = c("web", "news", "images", "froogle", "youtube")) %>%
  mutate(dirname = paste0("data/",
                          gsource,
                          "/",
                          str_replace(taxon_corrected, " ", "-")),
         filename = paste0(dirname,
                           "/",
                           str_replace(country_code, " ", "-"),
                           ".csv"))

# directory names
dir_names <- unique(dat3$dirname)

# make directory for each species
sapply(dir_names, 
       function(x) if(!dir.exists(x)) {dir.create(x, recursive =  T)})


#### five species ####

# gtrends wrapper function
gtrends_export <- function(taxon_corrected, country_code, gsource, filename, ...){
  
  # extract Google trends
  dat_temp <- gtrends(taxon_corrected, time = "all", geo = country_code, gprop = gsource)
  
  if(!is.null(dat_temp$interest_over_time)){
    
    # format data
    dat_temp2 <- as_tibble(dat_temp$interest_over_time)
    
    # export
    write_csv(dat_temp2, filename)
    
  }
  
}

# apply function across dataset
pmap(dat3, gtrends_export) # partially ran

