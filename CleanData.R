library(tidyverse)
library(here)

# Reading in the file
infile <- here("FinalData.csv")
data <- read_csv(infile, col_types = 
                      cols_only(
                           specimen_no = col_number(),
                           measurement_type = col_factor(),
                           average = col_number(),
                           specimen_part = col_factor(),
                           accepted_name = col_character(),
                           class = col_factor(),
                           order = col_factor(),
                           family = col_factor(),
                           genus = col_character(),
                           lng = col_double(),
                           lat = col_double(),
                           nation = col_factor(),
                           state = col_factor(),
                           county = col_character(),
                           max_ma = col_number(),
                           min_ma = col_number()),
                 na = c("", "NA"))

# Pivoting the table to include each measurement type as a separate column, renaming the columns,
# getting rid of certain columns, filtering data from US and data with shell measurements
data %>% 
     pivot_wider(names_from = measurement_type, values_from = average) %>% 
     rename(
          ScientificName = accepted_name,
          Class = class,
          Order = order,
          Family = family,
          Genus = genus,
          Longitude = lng,
          Latitude = lat,
          State = state,
          County = county,
          Height = height,
          Width = width,
          Length = length,
          Diameter = diameter,
          Inflation = inflation) %>% 
     filter(nation == "US" & specimen_part == "shell") %>%
     select(-c(specimen_no, specimen_part, nation)) -> data

# Updating factors in several taxonomic fields
data %>% 
     mutate(
          Class = fct_collapse(Class,
                             NULL = c("", "NO_CLASS_SPECIFIED")),
          Order = fct_collapse(Order,
                               NULL = c("", "NO_ORDER_SPECIFIED")),
          Family = fct_collapse(Family,
                                NULL = c("", "NO_FAMILY_SPECIFIED")),
          Genus = ifelse(str_detect(Genus, "\\("), str_replace(Genus, "(\\s\\(.*\\))", ""), Genus),
          Genus = ifelse(Genus == "", NA, Genus),
          ScientificName = ifelse(str_detect(ScientificName, "\\("), str_replace(ScientificName, "(\\s\\(.*\\))", ""), ScientificName),
          ScientificName = ifelse(str_detect(ScientificName, "\\s"), ScientificName, str_c(ScientificName, " sp."))) -> data

# Updating locational data and measurement data, filtering out data with no know scientific name
data %>% 
     mutate(
          Longitude = round(Longitude, digits = 2),
          Latitude = round(Latitude, digits = 2),
          County = ifelse(County == "Crook County", "Crook", County),
          Height = round(Height, digits = 2),
          Length = round(Length, digits = 2),
          Width = round(Width, digits = 2),
          Inflation = round(Inflation, digits = 2),
          Diameter = round(Diameter, digits = 2)) %>% 
     filter(!is.na(ScientificName)) -> data

# Fixing age field
data %>%
     rename(MinRelAge = min_ma,
            MaxRelAge = max_ma) %>% 
     mutate(MinRelAge = round(MinRelAge, 2),
            MaxRelAge = round(MaxRelAge, 2)) %>% 
     rowwise() %>% 
     mutate(AvgRelativeAge = round(mean(c(MinRelAge, MaxRelAge)), digits = 2)) -> data

# Undoing `rowwise()`
data %>% 
     ungroup() -> data