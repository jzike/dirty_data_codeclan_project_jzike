# Libraries ----
library(tidyverse)
library(readxl)

# Data ----
candy_2015 <- read_xlsx("raw_data/boing-boing-candy-2015.xlsx")
candy_2016 <- read_xlsx("raw_data/boing-boing-candy-2016.xlsx")
candy_2017 <- read_xlsx("raw_data/boing-boing-candy-2017.xlsx")


# Data exploration ----
names(candy_2015)
names(candy_2016)
names(candy_2017)

# Drop columns not related to candy or variables needed in analysis questions----
# Variable required for analysis - year, age, gender, country, candy, rating

candy_2015_extract <- candy_2015 %>% 
  select(Timestamp, 
         `How old are you?`, 
         `Are you going actually going trick or treating yourself?`,
         starts_with("["))

candy_2016_extract <- candy_2016 %>% 
  select(Timestamp,
         `Are you going actually going trick or treating yourself?`,
         `Your gender:`,
         `How old are you?`,
         `Which country do you live in?`,
         starts_with("[") & ends_with("]"))

candy_2017_extract <- candy_2017 %>% 
  select(`Internal ID`,
         starts_with("Q1:"),
         starts_with("Q2"),
         starts_with("Q3"),
         starts_with("Q4"),
         starts_with("Q6"))

# Format for consistency between datasets ----
# Rename going out, age, gender, country for consistency between datasets
candy_2015_extract <- candy_2015_extract %>% 
  rename(
    "year" = "Timestamp",
    "age" = "How old are you?",
    "going_out" = "Are you going actually going trick or treating yourself?")

candy_2016_extract <- candy_2016_extract %>% 
  rename(
    "year" = "Timestamp",
    "age" = "How old are you?",
    "going_out" = "Are you going actually going trick or treating yourself?",
    "country" = "Which country do you live in?",
    "gender" = "Your gender:"
  )

candy_2017_extract <- candy_2017_extract %>% 
  rename(
    "going_out" = "Q1: GOING OUT?",
    "gender" = "Q2: GENDER",
    "age" = "Q3: AGE",
    "country" = "Q4: COUNTRY"
  )

# Add columns to datasets
# All datasets need a participant ID column (to identify distinct raters when tables are merged) and a year column (to identify years when tables are merged). The 2015 dataset needs gender and country columns populated with NAs to merge correctly with the other tables 

#Add year columns
candy_2015_extract <- candy_2015_extract %>% 
  mutate(year = 2015)

candy_2016_extract <- candy_2016_extract %>% 
  mutate(year = 2016)

candy_2017_extract <- candy_2017_extract %>% 
  mutate(year = 2017, .after = `Internal ID`)

#Add gender and country columns to candy_2015

candy_2015_extract <- candy_2015_extract %>% 
  mutate(gender = NA, country = NA, .after = year)



# Add participant ID column

candy_2017_extract <- candy_2017_extract %>% 
  mutate(participant_id = paste0("2017","-", row_number()),
         .before = 'Internal ID')

candy_2016_extract <- candy_2016_extract %>% 
  mutate(participant_id = paste0("2016","-", row_number()),
         .before = 'year')

candy_2015_extract <- candy_2015_extract %>% 
  mutate(participant_id = paste0("2015","-", row_number()),
         .before = 'year')

# Prepare for binding rows ----
## Convert all datasets into long format ----

# Pivot longer to put candy in one column and rating in another

candy_2017_long <- candy_2017_extract %>% 
  pivot_longer(cols = starts_with("Q6"), 
               names_to = "candy", 
               values_to = "rating")

candy_2016_long <- candy_2016_extract %>% 
  pivot_longer(cols = starts_with("["), 
               names_to = "candy", 
               values_to = "rating")

candy_2015_long <- candy_2015_extract %>% 
  pivot_longer(cols = starts_with("["), 
               names_to = "candy", 
               values_to = "rating")

## Drop rows that are primarily NAs----
# If a row doesn't have a value in age, going out and rating, it should be dropped
# This is because these rows are useless for our analysis questions, which are interested in connections between age and trick or treating and ratings. Keeping all these NAs will just increase the size of the table without adding useable data.

candy_2017_long <- candy_2017_long %>% 
  filter(!is.na(rating) | (!is.na(age) & !is.na(going_out)))

candy_2016_long <- candy_2016_long %>% 
  filter(!is.na(rating) | (!is.na(age) & !is.na(going_out)))

candy_2015_long <- candy_2015_long %>% 
  filter(!is.na(rating) | (!is.na(age) & !is.na(going_out)))

## Put columns in correct order before binding rows----

candy_2015_long <- candy_2015_long %>% 
  select(participant_id, year, age, going_out, gender, country, candy, rating)

candy_2016_long <- candy_2016_long %>% 
  select(participant_id, year, age, going_out, gender, country, candy, rating)

candy_2017_long <- candy_2017_long %>% 
  select(participant_id, year, age, going_out, gender, country, candy, rating)

#Bind rows----





#Round age in all datasets
## Age is a character variable
## Change values without digits to "NA" and then use na_if to change them to NA


#candy_2016_ <- candy_2016_ %>% 
 # mutate(
  #  age = 
   #   if_else(str_detect(age, "[:digit:]", negate = TRUE) == TRUE, "NA", age)
  #) 

#candy_2016_ <- candy_2016_ %>% 
 # mutate(age = na_if(age, "NA"))



#Change age into numeric variable



