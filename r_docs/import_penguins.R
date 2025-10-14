## ============================================================================
## Data Wrangling Practice with Palmer Penguins
## Based on Chapter 7: Data Wrangling concepts
## ============================================================================
# Check if pacman is installed, install if not
if (!require("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
### Because you need more penguins in your life
### For details check out: https://tinyurl.com/isspenguins
pacman::p_load(palmerpenguins)

# Load required packages
pacman::p_load(tidyverse, janitor, skimr, lubridate, naniar, palmerpenguins)

## ============================================================================
## 1. IMPORT YOUR DATA -----
## ============================================================================

# The penguins data is already loaded from the package
penguins
penguins_raw


# But let's practice writing and reading data
penguins_raw %>%
  # Change date variables to character to practice kubridate functions
  mutate(`Date Egg` = str_replace_all(`Date Egg`, "-", " ")) %>%
  # Write the raw data to CSV
  write_csv("data/penguins_raw.csv")

# note that this is a relative path:
# "data/" specifies the subfolder and "penguins_raw.csv" is the file name

# Now practice importing it back
# TASK: Method 1: Using read_csv() directly
# Replace the first _____ with the name of the sub-folder
# Replace the second _____ with the file name (including the .csv extension)
penguins_imported <- read_csv("_____/_____")

# Method 2: You can also click on the file in the Files pane and select
# "Import Dataset" to see the import options

# ⚠️ COMMON ERROR: If you get "cannot open file":
# - Check that the "data" folder exists in your project directory
# - Make sure you saved the file in the previous step
# - Check your spelling of the file name

## ============================================================================
## 2. INSPECTING YOUR DATA -----
## ============================================================================

### Use glimpse() to find out about the datasets
glimpse(penguins)           # the simplified dataset from the package
glimpse(penguins_raw)       # the raw dataset from the package
glimpse(penguins_imported)  # the dataset we just imported
### What's the difference between them?

## Let's work with the penguins_imported dataset and call it dat
# TASK: Use the assignment operator <- to create dat from penguins_imported
dat <- _____

# Get a comprehensive summary
skim(dat)

# What do you notice about the distributions of the data?

# Look at the first few rows
head(dat)

# Check the dimensions (rows x columns)
dim(dat)

# Get column names
names(dat)
# we'll be using this a lot to check after changing some of the data

# Look at a specific variable using $
dat$Species

# Find the unique Species names:
dat$Species %>%
  unique()

# OR
dat %>%
  distinct(Species)

# TASK: How many observations are in the dataset?
# HINT: Look at the first number from dim()
# EXPECTED: 344 observations

# TASK: How many variables are in the dataset?
# HINT: Look at the second number from dim()
# EXPECTED: 17 variables

# TASK: What data types do you see?
# HINT: Look at the output from glimpse() - the <chr>, <dbl> abbreviations
# EXPECTED: character (chr), double/numeric (dbl)

# TASK: What does the %>% do?
# Try running these two versions and compare:
# Version 1 (without pipe):
head(dat)

# Version 2 (with pipe):
dat %>% head()

# They do the same thing! The pipe passes the data to the next function.
# The pipe takes what's on the left and passes it as the first argument
# to the function on the right.

## CHECKPOINT 1: Before continuing, verify:
# ✓ dat contains 344 rows and 17 columns
# ✓ You can see the data structure with glimpse(dat)
# ✓ You understand what the pipe operator %>% does
# ✓ Column names have spaces and mixed capitalization (we'll fix this next!)

## ============================================================================
## 3. CLEANING YOUR COLUMN NAMES -----
## ============================================================================

# Look at the current column names - are they consistent and easy to use?
names(dat)

# Problems with current names:
# - Spaces in names (requires backticks: `Date Egg`)
# - Inconsistent capitalization (studyName vs Species)
# - Long names that are hard to type

# TASK: Clean the column names using janitor
# HINT: The function is clean_names()
penguins_clean <- dat %>%
  janitor::_____()

# Compare the old and new names
names(dat)
names(penguins_clean)

# What changed?
# - Spaces became underscores: "Date Egg" -> "date_egg"
# - Everything is lowercase: "Species" -> "species"
# - Consistent naming style throughout

# You can also manually rename specific columns
penguins_clean <- penguins_clean %>%
  rename(study = studyname,
         sample_id = sample_number)

glimpse(penguins_clean)

# TASK: What improvements did clean_names() make?

# TASK: Rename at least two more columns to names you prefer
# Example: You might want shorter names for the measurement variables
penguins_clean <- penguins_clean %>%
  rename(
    bill_length = culmen_length_mm,
    bill_depth = culmen_depth_mm
  )

# Let's verify our cleaning worked:
penguins_clean %>%
  select(1:5) %>%  # first 5 columns
  head()

## CHECKPOINT 2: Before continuing, verify:
# ✓ All column names are lowercase
# ✓ No spaces in column names (all use underscores)
# ✓ You renamed at least 2 columns
# ✓ glimpse(penguins_clean) shows your changes

## ============================================================================
## 4. DATA TYPES: What are they and how can you change them? -----
## ============================================================================

# Check current data types
glimpse(penguins_clean)

# Data types we see:
# <chr> = character (text)
# <dbl> = double (numbers with decimals)

# Let's look at the 'species' variable - it should be a factor
# because it has a fixed set of categories
class(penguins_clean$species)

# Currently it's character, but we want factor for categorical data

# TASK: Create a new variable called species_short which is a factor type
# Replace the first _______ with the new variable name: species_short
# Replace the second _______ with the current variable name: species
penguins_clean <- penguins_clean %>%
  mutate(_______ = as_factor(_______))

# Convert other character variables to factors
# Note: Make sure you use the correct column names in case you changed any!
names(penguins_clean)  # Check your column names

penguins_clean <- penguins_clean %>%
  mutate(
    island = as_factor(island),
    sex = as_factor(sex),
    region = as_factor(region)
  )

# Check the conversion
glimpse(penguins_clean)

# Look at the factor levels
levels(penguins_clean$species_short)
levels(penguins_clean$island)
levels(penguins_clean$sex)

# Convert clutch_completion to factor
penguins_clean <- penguins_clean %>%
  mutate(clutch_completion = as_factor(clutch_completion))

# TASK: Check the data types again. Which variables are factors now?

# TASK: Convert 'study' to a factor
penguins_clean <- penguins_clean %>%
  mutate(study = as_factor(study))

# TASK: Why might 'sample_id' NOT be converted to a factor?

# TASK: Convert 'clutch_completion' from a factor to a logical (TRUE/FALSE)
# HINT: The values are "Yes" and "No" - think about TRUE/FALSE
penguins_clean <- penguins_clean %>%
  mutate(clutch_completion_logical = case_when(
    clutch_completion == "Yes" ~ TRUE,
    clutch_completion == "No" ~ FALSE,
    TRUE ~ NA  # for any other values, use NA
  ))

# Check the conversion
penguins_clean %>%
  select(clutch_completion, clutch_completion_logical) %>%
  head(10)

## CHECKPOINT 3: Before continuing, verify:
# ✓ species_short is a factor with 3 levels
# ✓ island, sex, region, study are all factors
# ✓ You can explain the difference between factor and character
# ✓ You understand why sample_id doesn't need to be a factor

# ⚠️ COMMON ERROR: If as_factor() doesn't work:
# - Make sure you loaded the tidyverse or forcats package
# - Check that you spelled the variable name correctly
# - Ensure the variable exists in penguins_clean

## ============================================================================
## 5. HANDLING FACTORS -----
## ============================================================================

### 5.1 Recoding Factors ###

# The sex variable uses "MALE" and "FEMALE" - let's recode for consistency
count(penguins_clean, sex)

penguins_clean <- penguins_clean %>%
  mutate(sex = fct_recode(sex,
                          "Male" = "MALE",
                          "Female" = "FEMALE"))

count(penguins_clean, sex)

# Let's recode species names to be more readable
# First, let's see what we're working with:
unique(penguins_clean$species_short)

penguins_clean <- penguins_clean %>%
  mutate(species_short = fct_recode(species_short,
                                    "Adelie" = "Adelie Penguin (Pygoscelis adeliae)",
                                    "Chinstrap" = "Chinstrap penguin (Pygoscelis antarctica)",
                                    "Gentoo" = "Gentoo penguin (Pygoscelis papua)"))

count(penguins_clean, species_short)

# TASK: Create a new variable called 'study_short' that recodes the study names
# to something shorter (e.g., "P07" instead of the full name "PAL0708")
# HINT: First check what the study names are:
unique(penguins_clean$study)

# The studies are: PAL0708, PAL0809, PAL0910
# Let's create shorter codes: P07, P08, P09

penguins_clean <- penguins_clean %>%
  mutate(study_short = fct_recode(study,
                                  "___" = "_____",
                                  "___" = "_____",
                                  "___" = "_____"))

# Verify it worked:
count(penguins_clean, study, study_short)

### 5.2 Reordering Factor Levels ###

# Check current order of species
levels(penguins_clean$species_short)

# The order is alphabetical by default, but we might want to order by frequency

# Reorder by frequency
penguins_clean %>%
  count(species_short) %>%
  ggplot(aes(x = fct_infreq(species_short), y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Penguins by Species (ordered by frequency)",
       x = "Species", y = "Count")

# Reorder manually - maybe we want a specific order for our analysis
penguins_clean <- penguins_clean %>%
  mutate(species_short = fct_relevel(species_short,
                                     "Adelie", "Gentoo", "Chinstrap"))

levels(penguins_clean$species_short)

# Reverse the order - useful if you want to flip your plot
penguins_clean %>%
  count(species_short) %>%
  ggplot(aes(x = fct_rev(species_short), y = n)) +
  geom_col(fill = "coral") +
  labs(title = "Penguins by Species (reversed order)",
       x = "Species", y = "Count")

# TASK: Reorder the island factor alphabetically
# HINT: fct_relevel() with alphabetical order
penguins_clean <- penguins_clean %>%
  mutate(island = fct_relevel(island, "___", "___", "___"))

# Verify:
levels(penguins_clean$island)

# TASK: Create a bar plot showing the count of penguins by island
# HINT: Use the same structure as the species plot above


### 5.3 Removing Factor Levels ###

# Let's create a subset and see unused levels
penguins_subset <- penguins_clean %>%
  dplyr::filter(island != "Torgersen")

# Check if all factor levels are still there
levels(penguins_subset$island)
# Notice: "Torgersen" is still listed as a level, even though we filtered it out!

# TASK: What's the difference between the following?
count(penguins_subset, island)
count(penguins_subset, island, .drop = FALSE)

# Remove unused factor levels
penguins_subset <- penguins_subset %>%
  mutate(island = fct_drop(island))

# Check again:
levels(penguins_subset$island)
# Now "Torgersen" is gone from the levels!

# Or use droplevels() for all factors at once
# Let's create a new subset to demonstrate
penguins_subset2 <- penguins_clean %>%
  dplyr::filter(island != "Torgersen") %>%
  droplevels()

levels(penguins_subset2$island)
levels(penguins_subset2$species_short)

# TASK: Create a subset with only "Gentoo" penguins and remove unused levels


## CHECKPOINT 4: Before continuing, verify:
# ✓ sex shows "Male" and "Female" (not "MALE"/"FEMALE")
# ✓ species_short shows clean names (Adelie, Chinstrap, Gentoo)
# ✓ You created study_short with abbreviated names
# ✓ You understand the difference between fct_relevel(), fct_infreq(), and fct_rev()
# ✓ You can remove unused factor levels with fct_drop() or droplevels()

## ============================================================================
## 6. HANDLING DATES, TIMES, AND DURATIONS -----
## ============================================================================

# The penguins dataset has a date_egg variable
glimpse(penguins_clean$date_egg)

# Check if it's already a date type
class(penguins_clean$date_egg)
# It's character because we converted it earlier for import practice

# If it's character, convert it to date
# The format appears to be YYYY-MM-DD

# TASK: Which lubridate function should we use? Replace the ____
# HINT: y = year, m = month, d = day. What order are they in the date?
penguins_clean <- penguins_clean %>%
  mutate(date_egg = ___(date_egg))

# Verify the conversion
class(penguins_clean$date_egg)
glimpse(penguins_clean$date_egg)

# Extract components from dates
# TASK: Which lubridate functions should we use? Replace the ___
# HINT: Think about what each variable name suggests (year, month, day)
penguins_clean <- penguins_clean %>%
  mutate(
    year = ___(date_egg),
    month = ___(date_egg),
    day = ___(date_egg),
    month_name = ___(date_egg, label = TRUE)
  )

# Look at the new variables
penguins_clean %>%
  select(date_egg, year, month, day, month_name) %>%
  head(10)

# Calculate duration between dates
# Let's see how the egg-laying dates are distributed over time
penguins_clean %>%
  dplyr::filter(!is.na(date_egg)) %>%
  ggplot(aes(x = date_egg)) +
  geom_histogram(binwidth = 7, fill = "steelblue", alpha = 0.7) +
  labs(title = "Distribution of Egg-Laying Dates",
       x = "Date", y = "Count") +
  theme_minimal()

# Calculate days since first observation
penguins_clean <- penguins_clean %>%
  mutate(days_since_first = as.numeric(date_egg - min(date_egg, na.rm = TRUE)))

# Look at some examples
penguins_clean %>%
  select(date_egg, days_since_first) %>%
  arrange(date_egg) %>%
  head(10)

# Look at breeding season by month
penguins_clean %>%
  dplyr::filter(!is.na(month_name)) %>%
  count(month_name, species_short) %>%
  ggplot(aes(x = month_name, y = n, fill = species_short)) +
  geom_col(position = "dodge") +
  labs(title = "Egg-laying by Month and Species",
       x = "Month", y = "Count", fill = "Species") +
  theme_minimal()

# TASK: Extract the week of the year from date_egg
# HINT: lubridate has a week() function
penguins_clean <- penguins_clean %>%
  mutate(week_of_year = week(date_egg))

# TASK: Which week had the most egg-laying activity?
penguins_clean %>%
  dplyr::filter(!is.na(week_of_year)) %>%
  count(week_of_year) %>%
  arrange(desc(n)) %>%
  head(5)

# TASK: Create a variable that shows the day of the week (Mon, Tue, etc.)
# HINT: lubridate has a wday() function with a label parameter
penguins_clean <- penguins_clean %>%
  mutate(day_of_week = wday(date_egg, label = TRUE, abbr = FALSE))

# Verify:
penguins_clean %>%
  select(date_egg, day_of_week, week_of_year) %>%
  head(10)

## CHECKPOINT 5: Before continuing, verify:
# ✓ date_egg is now a Date type (not character)
# ✓ You extracted year, month, day, and month_name
# ✓ You can calculate durations (days_since_first)
# ✓ You created week_of_year and day_of_week variables
# ✓ Your plots show egg-laying patterns over time

# ⚠️ COMMON ERROR: If dates don't convert properly:
# - Check the format with head(penguins_clean$date_egg)
# - Make sure the format matches your lubridate function (ymd, mdy, dmy, etc.)
# - If you get NA values, the date format might be inconsistent

## ============================================================================
## BONUS: Preview of Missing Data -----
## ============================================================================

# Visualize missing data patterns
vis_miss(penguins_clean)

# Which variables have missing data?
miss_var_summary(penguins_clean)

# TASK: Which variable has the most missing data?

# TASK: How many complete cases are there?
# Complete cases = rows with NO missing data in ANY variable
n_complete(penguins_clean)

# What percentage of data is complete?
penguins_clean %>%
  summarise(
    total_rows = n(),
    complete_rows = n_complete(penguins_clean),
    pct_complete = (complete_rows / total_rows) * 100
  )

# Visualize missingness by species
gg_miss_fct(penguins_clean, fct = species_short)

## ============================================================================
## SAVE YOUR CLEAN DATA -----
## ============================================================================

# Write the cleaned data to the data folder
write_csv(penguins_clean, "data/penguins_clean.csv")

# You can now use this cleaned dataset for analysis!

# Let's verify the file was created:
list.files("data")

## CHECKPOINT 6: Final verification!
# ✓ You have a file called "penguins_clean.csv" in your data folder
# ✓ The file contains your cleaned data with:
#   - Clean column names (lowercase, underscores)
#   - Proper data types (factors for categories, dates for dates)
#   - Recoded factors (readable labels)
#   - Extracted date components
# ✓ You understand the missing data patterns

## ============================================================================
## ADDITIONAL PRACTICE CHALLENGES -----
## ============================================================================

# 1. Create a new variable 'size_category' that classifies penguins as:
#    - "Small" if body_mass_g < 3700
#    - "Medium" if body_mass_g between 3700 and 4750
#    - "Large" if body_mass_g > 4750


# 2. Calculate the body mass in kilograms (divide by 1000)


# 3. Extract the season from date_egg (Nov-Jan = Summer, Feb-Apr = Fall,
#    May-Jul = Winter, Aug-Oct = Spring in Antarctica)


# 4. Count how many penguins were observed in each year


# 5. Create a plot showing body mass distribution by species using factors
#    ordered by median body mass



## ============================================================================
## The next sections review code from variable_associations.R file -----
## ============================================================================

pacman::p_load(tidyverse, ggpubr, vcd, broom, moderndive, sjPlot, gapminder)

# Now that we have clean data, we can do meaningful analysis!

## ============================================================================
## PLOTTING -----
## ============================================================================

### Univariate data (one variable)

# Categorical counts - use geom_bar() or geom_col()
# geom_bar() counts automatically, geom_col() uses pre-counted data


# Continuous distributions - use geom_histogram() or geom_density()


### Bivariate (two variables)

# Continuous vs continuous - use geom_point() for scatterplots


# Categorical vs continuous - use geom_boxplot() or faceted histograms


# Categorical vs categorical - use mosaic plots (from vcd package)


### Trivariate data (three variables)
# How can you plot trivariate data?
# HINT: Use color, size, facets, or combinations


## ============================================================================
## Statistical tests -----
## ============================================================================

### Correlations - which type of bivariates can you test?
# HINT: Continuous vs continuous


### Chi-square test - which type of bivariates can you test?
# HINT: Categorical vs categorical


## ============================================================================
## Modelling -----
## ============================================================================

### Linear models for a selection of variables


### Any surprising results??


## ============================================================================
## ============================================================================
## ANSWER KEY - FOR SELF-CHECK
## ============================================================================
## ============================================================================

# Scroll down for answers...
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

## SECTION 1: IMPORT YOUR DATA -----

# ANSWER:
penguins_imported <- read_csv("data/penguins_raw.csv")


## SECTION 2: INSPECTING YOUR DATA -----

# ANSWER: Create dat
dat <- penguins_imported

# ANSWER: How many observations?
# 344 observations (rows)

# ANSWER: How many variables?
# 17 variables (columns)

# ANSWER: What data types?
# character (chr) and double/numeric (dbl)

# ANSWER: What does %>% do?
# The pipe operator passes the result from the left side as the first 
# argument to the function on the right side. It makes code more readable
# by showing the flow of data transformations.


## SECTION 3: CLEANING COLUMN NAMES -----

# ANSWER:
penguins_clean <- dat %>%
  janitor::clean_names()

# ANSWER: What improvements did clean_names() make?
# - Converted all names to lowercase
# - Replaced spaces with underscores
# - Removed special characters
# - Made names consistent and easier to type


## SECTION 4: DATA TYPES -----

# ANSWER: Create species_short
penguins_clean <- penguins_clean %>%
  mutate(species_short = as_factor(species))

# ANSWER: Which variables are factors now?
# species_short, island, sex, region, clutch_completion, study

# ANSWER: Why might sample_id NOT be converted to a factor?
# sample_id is a unique identifier for each penguin. Even though it's a number,
# it's not meant for mathematical operations or categorization. Each ID is unique
# (not a category with repeated values), so it doesn't benefit from being a factor.
# It could stay as numeric or character - it's just a label.


## SECTION 5: HANDLING FACTORS -----

# ANSWER: Create study_short
penguins_clean <- penguins_clean %>%
  mutate(study_short = fct_recode(study,
                                  "P07" = "PAL0708",
                                  "P08" = "PAL0809",
                                  "P09" = "PAL0910"))

# ANSWER: Reorder island alphabetically
penguins_clean <- penguins_clean %>%
  mutate(island = fct_relevel(island, "Biscoe", "Dream", "Torgersen"))

# ANSWER: Create bar plot of penguins by island
penguins_clean %>%
  count(island) %>%
  ggplot(aes(x = island, y = n)) +
  geom_col(fill = "darkgreen") +
  labs(title = "Penguins by Island",
       x = "Island", y = "Count")

# ANSWER: What's the difference between count() with and without .drop = FALSE?
# Without .drop = FALSE: Only shows islands with data (Biscoe, Dream)
# With .drop = FALSE: Shows ALL factor levels, even unused ones (includes Torgersen with 0 count)

# ANSWER: Create subset with only Gentoo penguins
penguins_gentoo <- penguins_clean %>%
  dplyr::filter(species_short == "Gentoo") %>%
  droplevels()


## SECTION 6: HANDLING DATES -----

# ANSWER: Convert date_egg to date type
penguins_clean <- penguins_clean %>%
  mutate(date_egg = ymd(date_egg))

# ANSWER: Extract date components
penguins_clean <- penguins_clean %>%
  mutate(
    year = year(date_egg),
    month = month(date_egg),
    day = day(date_egg),
    month_name = month(date_egg, label = TRUE)
  )

# ANSWER: Extract week of year
penguins_clean <- penguins_clean %>%
  mutate(week_of_year = week(date_egg))

# ANSWER: Which week had the most egg-laying activity?
# Week 45 (early November) had the most activity
penguins_clean %>%
  dplyr::filter(!is.na(week_of_year)) %>%
  count(week_of_year) %>%
  arrange(desc(n))

# ANSWER: Create day_of_week variable
penguins_clean <- penguins_clean %>%
  mutate(day_of_week = wday(date_egg, label = TRUE, abbr = FALSE))


## BONUS: MISSING DATA -----

# ANSWER: Which variable has the most missing data?
# sex has the most missing data (~3% or about 11 observations)

# ANSWER: How many complete cases?
# Use n_complete(penguins_clean) - typically around 330-334 complete cases


## ADDITIONAL PRACTICE CHALLENGES -----

# ANSWER 1: Create size_category
penguins_clean <- penguins_clean %>%
  mutate(size_category = case_when(
    body_mass_g < 3700 ~ "Small",
    body_mass_g >= 3700 & body_mass_g <= 4750 ~ "Medium",
    body_mass_g > 4750 ~ "Large",
    TRUE ~ NA_character_
  ),
  size_category = fct_relevel(size_category, "Small", "Medium", "Large"))

# Visualize:
penguins_clean %>%
  dplyr::filter(!is.na(size_category)) %>%
  ggplot(aes(x = size_category, fill = species_short)) +
  geom_bar(position = "dodge") +
  labs(title = "Penguin Size Categories by Species",
       x = "Size Category", y = "Count", fill = "Species")

# ANSWER 2: Calculate body mass in kg
penguins_clean <- penguins_clean %>%
  mutate(body_mass_kg = body_mass_g / 1000)

# ANSWER 3: Extract season
penguins_clean <- penguins_clean %>%
  mutate(
    season = case_when(
      month %in% c(11, 12, 1) ~ "Summer",
      month %in% c(2, 3, 4) ~ "Fall",
      month %in% c(5, 6, 7) ~ "Winter",
      month %in% c(8, 9, 10) ~ "Spring",
      TRUE ~ NA_character_
    ),
    season = fct_relevel(season, "Spring", "Summer", "Fall", "Winter")
  )

# Visualize:
penguins_clean %>%
  dplyr::filter(!is.na(season)) %>%
  count(season, species_short) %>%
  ggplot(aes(x = season, y = n, fill = species_short)) +
  geom_col(position = "dodge") +
  labs(title = "Egg-laying Season by Species",
       x = "Season", y = "Count", fill = "Species")

# ANSWER 4: Count penguins per year
penguins_clean %>%
  count(year) %>%
  ggplot(aes(x = factor(year), y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Penguins Observed per Year",
       x = "Year", y = "Count")

# ANSWER 5: Body mass by species (ordered by median)
penguins_clean %>%
  dplyr::filter(!is.na(body_mass_g)) %>%
  mutate(species_short = fct_reorder(species_short, body_mass_g, median)) %>%
  ggplot(aes(x = species_short, y = body_mass_g, fill = species_short)) +
  geom_boxplot() +
  labs(title = "Body Mass Distribution by Species (ordered by median)",
       x = "Species", y = "Body Mass (g)") +
  theme_minimal() +
  theme(legend.position = "none")


## DISCUSSION QUESTIONS - ANSWERS -----

# 1. Why is it important to clean data before analysis?
# ANSWER:
