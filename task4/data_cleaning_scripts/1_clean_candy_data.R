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

### 1) Combine unknown genders into one value----

unknown_genders <- c("Other", "I'd rather not say")

candy_clean <- candy_clean %>% 
  mutate(
    gender = 
      if_else(gender %in% unknown_genders, "Unknown", gender))

## Country ----


### 1) Change country into title format ----
#To combine some values that are separate due to differences in case.

candy_clean <- candy_clean %>% 
  mutate(
    country = str_to_title(country)
  )

### 2) Combine same country, different spelling values for analysis countries ----

#Assign all distinct values that are USA to a vector
america_pseudos <- c(
  "Usa",
  "Us",
  "United States Of America",
  "United States",
  "Ussa",
  "U.s.a.",
  "Murica",
  "Usa!",
  "Usa (I Think But It's An Election Year So Who Can Really Tell)",
  "U.s.",
  "America",
  "Units States",
  "Usa Usa Usa",
  "The Best One - Usa",
  "Usa! Usa! Usa!",
  "The Yoo Ess Of Aaayyyyyy",
  "Usa!!!!!!",
  "Usa! Usa!",
  "United Sates",
  "Sub-Canadian North America... 'Merica",
  "Trumpistan",
  "Merica",
  "United Stetes",
  "Usa Usa Usa Usa",
  "United  States Of America",
  "United State",
  "United Staes",
  "Usausausa",
  "Us Of A",
  "Unites States",
  "The United States",
  "North Carolina",
  "Unied States",
  "U S",
  "The United States Of America",
  "Unite States",
  "Usa? Hard To Tell Anymore..",
  "'Merica",
  "Usas",
  "Pittsburgh",
  "New York",
  "California",
  "I Pretend To Be From Canada, But I Am Really From The United States.",
  "United Stated",
  "Ahem....Amerca",
  "United Statss",
  "Murrika",
  "Usaa",
  "Alaska",
  "U S A",
  "United Statea",
  "United Ststes",
  "Usa Usa Usa!!!!",
  "New Jersey"
  )

#Change names of all countries in USA vector to "USA"
candy_clean <- candy_clean %>% 
  mutate(
    country = 
      if_else(country %in% america_pseudos, "USA", country))

#Assign all UK values to a vector
uk_pseudos <- c(
  "Uk",
  "England",
  "United Kingdom",
  "United Kindom",
  "U.k.",
  "Scotland"
)

#Change all UK values to "UK"
candy_clean <- candy_clean %>% 
  mutate(
    country = 
      if_else(country %in% uk_pseudos, "UK", country))

#Assign all Canada values to a vector
canada_pseudos <- c(
  "Canada",
  "Can",
  "Canae",
  "Canada`",
  "Soviet Canuckistan"
)

#Change all Canada values to "Canada"
candy_clean <- candy_clean %>% 
  mutate(
    country = 
      if_else(country %in% canada_pseudos, "Canada", country))

### 3) Change other country names to "other country" ----
#Note that I have interpreted analysis question 8 to mean that I only need four country categories for the analysis - US, Canada, UK and other countries

#Assign analysis countries to a vector
analysis_countries <- c("USA",
                        "UK",
                        "Canada")

#Change all country names not in analysis_countries to "Other country"
candy_clean <- candy_clean %>% 
  mutate(
    country = 
      if_else(country %in% analysis_countries, country, "Other country"))


## Candy ----


### 1) Remove square brackets from candy names----

candy_clean <- candy_clean %>% 
  mutate(candy = if_else(grepl("]", candy), str_sub(candy, 2, nchar(candy) - 1), candy ))

### 2) Remove "Q6 | " from candy names----

candy_clean <- candy_clean %>% 
  mutate(candy = if_else(grepl("Q6", candy), str_sub(candy, 5, nchar(candy)), candy )) 

### 3) Trim any white space before/after values in candy ----

candy_clean <- candy_clean %>% 
  mutate(candy = str_trim(candy, side = "both"))


### 4) Rename duplicate values in candy ----
#Sometimes the candy names differ slightly between datasets and should be combined.

#Change Dark Chocolate Hershey to Hershey's Dark Chocolate so that 2015 dataset matches the others

candy_clean <- candy_clean %>% 
  mutate(
    candy = 
      if_else(candy == "Dark Chocolate Hershey", 
              "Hershey's Dark Chocolate", 
              candy)) 

#Change JoyJoy (Mit Iodine) to JoyJoy (Mit Iodine!) so that 2015 dataset matches the others

candy_clean <- candy_clean %>% 
  mutate(
    candy = 
      if_else(candy == "JoyJoy (Mit Iodine)", 
              "JoyJoy (Mit Iodine!)", 
              candy)) 

#Change Sweetums to Sweetums (a friend to diabetes) so that 2015 dataset matches the others

candy_clean <- candy_clean %>% 
  mutate(
    candy = 
      if_else(candy == "Sweetums", 
              "Sweetums (a friend to diabetes)", 
              candy)) 

#Change "Anonymous brown globs that come in black and orange wrappers	(a.k.a. Mary Janes)" to "Mary Janes" so that 2017 dataset matches the others. The other datasets have separate candies for "Anonymous brown globs..." and "Mary Janes", but the 2017 dataset doesn't have a separate Mary Janes candy and states that the Anonymous brown globs are Mary Janes

candy_clean <- candy_clean %>% 
  mutate(
    candy = 
      if_else(candy == 
      "Anonymous brown globs that come in black and orange wrappers	(a.k.a. Mary Janes)", 
              "Mary Janes", 
              candy)) 


