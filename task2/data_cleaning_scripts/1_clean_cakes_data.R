# 1. Libraries ----

library(tidyverse)

# 2. Read in data ----

cake_ingredients <- read_csv("raw_data/cake-ingredients-1961.csv")
cake_codes <- read_csv("raw_data/cake_ingredient_code.csv")
 
# 3. Tidy data ----
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

# 4. Find and deal with missing values ----

## Check for missing values in measure
cakes_joined %>% 
  select(ingredient, measure) %>% 
  filter(is.na(measure))

## Drop rows where amount = NA, NA in amount means the ingredient isn't in that cake

cakes_joined <- cakes_joined  %>% 
  filter(!is.na(amount))

## Change NAs to "cup" in measure, only ingredients called "Sour cream cup" are missing an amount, meaning that the measure has been mistakenly added to the ingredient column

cakes_imputed <- cakes_joined %>% 
  mutate(measure = coalesce(measure, "cup"))

# 5. Write clean data csv ----

write_csv(cakes_imputed, "clean_data/tidy_cakes.csv")
