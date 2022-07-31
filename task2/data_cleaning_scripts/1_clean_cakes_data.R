# Libraries
library(tidyverse)

# Read in data
cake_ingredients <- read_csv("raw_data/cake-ingredients-1961.csv")
cake_codes <- read_csv("raw_data/cake_ingredient_code.csv")
 

# Pivot data longer - move ingredients to a column and amounts to a separate column

cake_ingredients_long <- cake_ingredients %>% 
  pivot_longer(
    cols = AE:ZH,
    names_to = "ingredient_code",
    values_to = "amount"
  )

# Join with cake codes to get ingredient names and type of measure for each

cakes_joined <- cake_ingredients_long %>% 
  left_join(cake_codes, by = c("ingredient_code" = "code")) %>% 
  janitor::clean_names()

# Find and deal with missing values

## Check for missing values in measure
cakes_joined %>% 
  select(ingredient, measure) %>% 
  filter(is.na(measure))

## Drop rows where amount = NA, NA in amount means the ingredient isn't in that cake



## Change NAs to "cup" in measure, only ingredients called "Sour cream cup" are missing an amount, meaning that the measure has been mistakenly added to the ingredient column