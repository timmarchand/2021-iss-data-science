# `tidyr` in action: Exploring Educational Statistics

## Introduction ----
# This code file uses a real data set to demonstrate how reshaping your data with tidyr is an important part 
# of the data exploration process. The data in this example was downloaded from the World Bank Data Explorer,
# which is a data collection of hundreds of indicators (measures) of different economic and social development factors. 
# In particular, this example considers educational indicators that capture a relevant signal of a country’s 
# level of (or investment in) education—for example, government expenditure on education, literacy rates, 
# school enrollment rates, and dozens of other measures of educational attainment. The imperfections of this data set 
# (unnecessary rows at the top of the .csv file, a substantial amount of missing data, long column names with special 
#   characters) are representative of the challenges involved in working with real data sets. 

# Load the necessary libraries
library(readr) # for reading in files
library(tidyr) # for data wrangling
library(dplyr) # for data wrangling
library(ggplot2) # for plotting
library(ggrepel) # for plotting
library(scales) # for plotting
library(ggpubr) # for one stat test

# After having downloaded the data, you will need to load it into your R environment:

# Load data
wb_data <- read_csv(
  "https://tinyurl.com/iss-world-bank-tidyr",
)
# Check the column names
names(wb_data)

# Tidy them up with clean_names()
wb_data <- janitor::clean_names(wb_data)

# Visually compare expenditures for 1990 and 2014

# Begin by filtering the rows for the indicator of interest
indicator <- "Government expenditure on education, total (% of GDP)"
expenditure_plot_data <- wb_data %>%
  filter(indicator_name == indicator)

# that the expenditure (relative to gross domestic product) is fairly correlated between the two time points: 
# countries that spent more in 1990 also spent more in 2014 
# (specifically, the correlation—calculated in R using the cor() function—is .64).

# Plot the expenditure in 1990 against 2014 using the `ggplot2` package
expenditure_chart <- ggplot(data = expenditure_plot_data) +
  geom_text_repel(
    mapping = aes(x = year_1990 / 100, y = year_2014 / 100, label = country_code )
  ) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  labs(
    title = indicator, x = "Expenditure 1990",
    y = "Expenditure 2014"
  )

expenditure_chart

# The plot reveals that the expenditure (relative to gross domestic product) is fairly correlated between 
# the two time points: countries that spent more in 1990 also spent more in 2014 
# (specifically, the correlation calculated by spearman method = .52).

 ggplot(data = expenditure_plot_data, mapping = aes(x = year_1990 / 100, y = year_2014 / 100)) +
  geom_point() +
   geom_smooth(method = "lm", se = FALSE) +
   ggpubr::stat_cor(method ="spearman") +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  labs(
    title = indicator, x = "Expenditure 1990",
    y = "Expenditure 2014"
  )


# However, if you want to extend your analysis to visually compare how the expenditure across all 
# years varies for a given country, you would need to reshape the data. Instead of having each 
# observation be an indicator for a country, you want each observation to be an indicator for a country 
# for a year—thereby having all of the values for all of the years in a single column and making the 
# data long(er) format.
# 
# To do this, you can pivot_longer() the year columns :

# Pivot longer to create a new column for the `year`
long_year_data <- wb_data %>%
  pivot_longer(cols = starts_with("year"),
               names_to = "year", # `year` will be the new key column
               names_prefix = "year_", # specifying the prefix means it will be dropped
               values_to = "value") # `value` will be the new value column

# This pivot_longer statement creates a year column, so each observation (row) 
# represents the value of an indicator in a particular country in a given year. 
# The expenditure for each year is stored in the value column created (coincidentally, 
# this column is given the name "value").


# Filter the rows for the indicator and country of interest
indicator <- "Government expenditure on education, total (% of GDP)"
spain_plot_data <- long_year_data %>%
  filter(
    indicator_name == indicator,
    country_code == "ESP" # Spain
  ) %>%
  mutate(year = as.numeric(year)) # change to numerical value

# The resulting chart uses the available data to show a timeline of the fluctuations in 
# government expenditures on education in Spain. This produces a more complete picture of the history of 
# educational investment, and draws attention to major changes as well as the absence of data in particular years.

# This structure will now allow you to compare fluctuations in an indicator’s value over time (across all years):
# Show the educational expenditure over time
chart_title <- paste(indicator, " in Spain")
spain_chart <- ggplot(data = spain_plot_data) +
  geom_line(mapping = aes(x = year, y = value / 100)) +
  scale_y_continuous(labels = percent) +
  labs(title = chart_title, x = "Year", y = "Percent of GDP Expenditure")
spain_chart

# The resulting chart uses the available data to show a timeline of the fluctuations in 
# government expenditures on education in Spain. This produces a more complete picture of the history of 
# educational investment, and draws attention to major changes as well as the absence of data in particular years.

# You may also want to compare two indicators to each other. For example, you may want to assess 
# the relationship between each country’s literacy rate (a first indicator) and its unemployment rate 
# (a second indicator). To do this, you would need to reshape the data again so that each observation 
# is a particular country and each column is an indicator. Since indicators are currently in one column, 
# you need to spread them out using the pivot_wider() function:

# Reshape the data to create columns for each indicator
wide_data <- long_year_data %>%
  select(-indicator_code) %>% # do not include the `indicator_code` column
  pivot_wider(names_from = indicator_name, # new column names are `indicator_name` values
    values_from = value # populate new columns with values from `value`
  )

wide_data %>% slice(10)

# Prepare data and filter for year of interest
x_var <- "Literacy rate, adult female (% of females ages 15 and above)"
y_var <- "Unemployment, female (% of female labor force) (modeled ILO estimate)"
lit_plot_data <- wide_data %>%
  mutate(
    lit_percent_2014 = wide_data[[x_var]] / 100,
    employ_percent_2014 = wide_data[[y_var]] / 100
  ) %>%
  filter(year == "2014")

# Show the literacy vs. employment rates
lit_chart <- ggplot(data = lit_plot_data) +
  geom_point(mapping = aes(x = lit_percent_2014, y = employ_percent_2014)) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  labs(
    x = x_var,
    y = "Unemployment, female (% of female labor force)",
    title = "Female Literacy Rate versus Female Unemployment Rate"
  )

