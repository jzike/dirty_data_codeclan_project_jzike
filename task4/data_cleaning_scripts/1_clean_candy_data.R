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

candy_clean <- bind_rows(candy_2015_long, candy_2016_long, candy_2017_long)



# Clean columns ----
# Main columns that need cleaning - age, gender, country, candy

## Age ----
# Age is in character format and needs to be in numeric format
### 1) Deal with problematic values that don't contain numbers----

#Identifies strings without numbers and changes them to NA
candy_clean <- candy_clean %>% 
 mutate(
  age = 
   if_else(str_detect(age, "[:digit:]", negate = TRUE) == TRUE, 
           NA_character_, age)
) 



### 2) Deal with confusing values----
#These values need to be changed individually - they contain an age, but also something that will make it difficult to extract the correct age (e.g. the age of their child)

candy_clean <- candy_clean %>% 
  mutate(
    age = 
      if_else(age == "45, but the 8-year-old Huntress and bediapered Unicorn give me political cover and social respectability.  However, I WILL eat more than they do combined.", "45", age))

candy_clean <- candy_clean %>% 
  mutate(
    age = 
      if_else(age == "5 months", NA_character_ , age))#A 5 month old can't take a survey, this could be the parent, but we can't logically guess how old they are, so needs to change to NA

candy_clean <- candy_clean %>% 
  mutate(
    age = 
      if_else(age == "50, taking a 13 year old.", "50" , age))

candy_clean <- candy_clean %>% 
  mutate(
    age = 
      if_else(age == "49 11/12ths", "49" , age))

candy_clean <- candy_clean %>% 
  mutate(
    age = 
      if_else(age == "45-55", "50" , age))#This age range can't be converted to numeric, instead I've chosen the number in the middle of the range.


### 3) Remove characters from ages----
#Now that I have dealt with confusing values, I can remove characters from the values without having confusing values in some cells.

candy_clean <- candy_clean %>% 
  mutate(
    age = str_remove_all(age, "[:alpha:]")
  )

### 4) Remove all punctuation except "." and blank spaces----
#We can't remove "." yet because it is a decimal point in a large part of the data
candy_clean <- candy_clean %>% 
  mutate(
    age = str_remove_all(age, "[-＋,>^'+!-():[:blank:]]")
  ) 

### 5) Extract everything before the "." ----
candy_clean <- candy_clean %>% 
  mutate(
    age = str_extract(age, "[^.]+"))

### 6) Change age to numeric variable ----
#Changing age to numeric loses 2 distinct age values. One of these is the problematic "７１＋" which didn't respond to any stringr manipulation. Not sure what the other one was, but it is likely that it was another problematic value. At most, we have lost age data for 2 participants, which is not very significant considering the size of the dataset
candy_clean <- candy_clean %>% 
  mutate(age = as.numeric(age)) 


### 7) Fix outliers ----
#The candy dataset still has some problematic numbers that are making the Mean and Max values "infinite". We need to work with the mean for the analysis questions, so this is a problem. The oldest person in the world lived to be 122, so I have chosen to replace all values over 122 with the median value.

candy_clean <- candy_clean %>% 
  mutate(age = if_else(age > 122, median(age), age))
  
## Gender ----


## Country ----



