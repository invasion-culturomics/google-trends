#### set-up ####

# load packages
library(gtrendsR)
library(tidyverse)
library(countrycode)

# import data
dat <- read_csv("data/Taxon_x_List_GloNAF_vanKleunenetal2018Ecology.csv")
regions <- read_csv("data/Region_GloNAF_vanKleunenetal2018Ecology.csv")
countries2 <- as_tibble(countries) # from gtrendsR package


#### edit data ####

# convert GLONAF codes from ISO-3 to ISO-2
regions2 <- regions %>%
  mutate(country_code = countrycode(country_ISO, origin = 'iso3c', destination = 'iso2c'),
         country_code = case_when(country_ISO == "ANT" & 
                                    country == "Netherlands Antilles" ~ "AN", # addresses warning
                                  TRUE ~ country_code))

# check for matching
regions2 %>%
  anti_join(countries2 %>%
              select(country_code)) %>%
  select(country_ISO, country, country_code)
# should return 0

# identify species to test functions
species <- tibble(standardized_name = c("Carpobrotus edulis", "Acacia dealbata", "Bromus arvensis", "Eichhornia crassipes"))

# add country info
# filter for test species
dat2 <- dat %>%
  left_join(regions2 %>%
              select(region_id, country_code) %>%
              unique()) %>%
  inner_join(species) %>%
  select(standardized_name, country_code) %>% # remove regions (smaller scale than country)
  unique()


#### data export structure ####

# add directory and file name
dat3 <- dat2 %>%
  expand_grid(gsource = c("web", "news", "images", "froogle", "youtube")) %>%
  mutate(dirname = paste0("data/",
                          gsource,
                          "/",
                          str_replace(standardized_name, " ", "-")),
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
gtrends_export <- function(standardized_name, country_code, gsource, filename, ...){
  
  # extract Google trends
  dat_temp <- gtrends(standardized_name, time = "all", geo = country_code, gprop = gsource)
  
  if(!is.null(dat_temp$interest_over_time)){
    
    # format data
    dat_temp2 <- as_tibble(dat_temp$interest_over_time)
    
    # export
    write_csv(dat_temp2, filename)
    
  }
  
}

# apply function across dataset
pmap(dat3, gtrends_export) # partially ran

