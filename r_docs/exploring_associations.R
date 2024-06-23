## Associations between variables set up ----

## OPTIONAL clear memory
 rm(list=ls(all=TRUE))

## load libraries
# library(tidyverse)
# library(ggpubr) # for adding stats to plots
# library(infer) # for stats tests on tidy data
# library(vcd) # for chi-squared and mosaic plot
# library(broom) # for creating tidy tables from regression models
# library(moderndive) # more easy to read tables from regression models
# library(sjPlot) # ADVANCED: for plotting of effects
# library(gapminder) # for data

## Efficeint alternative - use pacman!
pacman::p_load(tidyverse, ggpubr, infer, vcd, broom, moderndive, sjPlot, gapminder)

## load data ----
dat <- read_csv("data/random_qs.csv")

## If you get an error then uncomment the following

# dat <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQul_g9GtsSj-30sQ2hOE_rixxy8gH6AdhawcT5AoRJodJdGZnBgqtwgDAmfab0CJ1AjmFCrKmVw5MA/pub?gid=1799592268&single=true&output=csv")
# 
# dat <- dat %>%
#   set_names(c("timestamp","height",  "month", "date",  "visit1", ">14", 
#              "guess1", "visit2",   "<96",    "guess2",   "data_sci"))
# 
# cols <- c("timestamp", "height", "birthday", "visit", "guess", "data_sci", 
# "odd", "birthday_24")
# 
# dat <- dat %>% 
#   mutate(visit = coalesce(visit1,visit2),
#          visit = str_detect(visit, "Yes"),
#          guess = coalesce(guess1,guess2),
#          data_sci = str_detect(data_sci,"Yes"),
#          timestamp = lubridate::mdy_hms(timestamp),
#          date = parse_number(date),
#          odd = date %%2 == 1) %>% 
#   unite("birthday",  c(month, date), sep = " ") %>% 
#   mutate(birthday_24 = str_c(birthday, " 2024") %>% mdy()) %>% 
#   select(all_of(cols))
# 
# write_csv(dat, file = "data/random_qs.csv")

### Was there something funny about the height data?
dat %>% 
  arrange(height)

## Two options

# filter out outliers
dat %>% 
  filter(height>7) %>% 
  arrange(height)

## mutate if less than 100, then filter 

dat %>% 
  mutate(height = ifelse(height < 100, height*100, height)) %>% 
  mutate(height = na_if(height, height > 200)) 

## make a new variable
df <- dat %>% 
  mutate(height = ifelse(height < 100, height*100, height)) %>% 
  mutate(height = na_if(height, 700)) 

## get the data from online csv
resume <- read_csv("https://tinyurl.com/iss-racial-disc-web")

## check with glimpse
glimpse(resume)

## call is numeric, so let's create another column to make it a factor

## get gapminder data from package
gapminder <-  gapminder::gapminder

####################################################################
####################################################################


## CONTINUOUS vs CATEGORICAL:  odd birthday-guess data----
## compare values with box plots or faceted histograms
## assess association with regression lines
## statistical tests from the t-test family (another form of the linear regression)


# check the data
glimpse(df)

## Try the boxplot again
ggplot(df) +
  aes(x = "", y = guess, fill = odd) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "Birthday date", y = "Guess for how many countries in Africa", title = "Boxplot of African guesses by odd/even birthday") +
  theme_minimal() +
  stat_compare_means(method = "t.test")



## Do these plots suggest a difference in the groups?
## Are the differences significant?

## use the t.test or anova functions

## t.test {Base R} ----
## requires us to work on vectors

## but first we need to split the df into two groups using dplyr
df_odd <- df %>% filter(odd)
df_even <- df %>% filter(!odd)

## Extract vectors from dataframes with $
## Try the t.test function
t.test(df_even$guess,df_odd$guess)
## as we saw before p-value very small!!
## this gives us the mean values of our two groups (mean of x vs mean of y)
## the 95% confidence intervals shows what the difference could be in 95% of similar samples


## You can feed the result into tidy() to get a table of the results
t.test(df_even$guess,df_odd$guess) %>% 
  tidy()

## estimate = difference in means
## estimate 1 = mean of group 1
## estimate 2 = mean of group 2
## statistic = t score
## parameter = degrees of freedom

## t_test using {infer} on the dataframe ----
## The infer package allows us to work on the dataframe directly

df %>% 
  infer::t_test(formula = guess ~ odd)

## statistic = t score
## t_df = degrees of freedom
## alternative = hypothesis chosen (Options are "two-sided" (default), "greater", or "less")
## lower_ci = lower confidence interval
## upper_ci = upper confidence interval

##  t-test from regression ----

## Using regression: any t-test or anova result can be found with a linear regression model
## use the function lm(dependent/outcome variable ~ independent/predictor, data = df)
cat_model <- lm(guess ~ odd, data = df)
summary(cat_model)
## (Intercept) estimate = predicted value for the base category (i.e mean for even guess of guess)
## birthdayodd estimate = predicted change in value for going from even to odd
## Pr(>|t|) = probability, or p-value
## Multiple R-squared = correlation coefficient
## Adjusted R-squared = adjusted in case of multiple variables

## From moderndive:

get_regression_summaries(cat_model)
## r_squared = correlation coefficient
## adj_r_squared = adjusted r squared, adjusted for multiple variables
## statistic = F-statistic
## nobs = number of observations

get_regression_table(cat_model)
## (Intercept) estimate = predicted value for the base category (i.e mean for even guess)
## numberodd estimate = predicted change in value for going from even to odd
## statistic = t value
## lower_ci = lower confidence interval
## upper_ci = upper confidence interval


get_regression_points(cat_model)

## guess_hat shows the predicted values for the model (the means)
## residuals shows the difference between the fitted (predicted) values and the real (guess) values



## CONTINUOUS vs MULTI-CATEGORICAL:  real world data----
## compare values with box plots or faceted histograms
## assess association with regression lines
## statistical tests from the t-test family (another form of the linear regression)

## Examine whether there is a difference associated between continent and life expectancy in 2007

## filter for 2007
gapminder2007 <- gapminder %>%
  filter(year == 2007) %>%
  select(country, lifeExp, continent, gdpPercap)

## take a look at the data
glimpse(gapminder2007)

## make a faceted histogram plot by continent
ggplot(gapminder2007, aes(x = lifeExp)) +
  geom_histogram(binwidth = 5, color = "white") +
  labs(x = "Life expectancy",
       y = "Number of countries",
       title = "Histogram of distribution of worldwide life expectancies") +
  facet_wrap(~ continent, nrow = 2)

## make a boxplot plot of lifeExp  by continent
ggplot(gapminder2007, aes(x = continent, y = lifeExp)) +
  geom_boxplot() +
  labs(x = "Continent", y = "Life expectancy",
       title = "Life expectancy by continent")


## as with t-test, can add Anova stat to the plot
ggplot(gapminder2007, aes(x = continent, y = lifeExp)) +
  geom_boxplot() +
  labs(x = "Continent", y = "Life expectancy",
       title = "Life expectancy by continent") +
  stat_compare_means(method = "anova")

## use dplyr to get a table of means and medians
lifeExp_by_continent <- gapminder2007 %>%
  group_by(continent) %>%
  summarize(median = median(lifeExp),
            mean = mean(lifeExp))

lifeExp_by_continent

## Regression modelling also possible for multiple categories
lifeExp_model <- lm(lifeExp ~ continent, data = gapminder2007)
summary(lifeExp_model)
get_regression_summaries(lifeExp_model)
get_regression_table(lifeExp_model)
get_regression_points(lifeExp_model, ID = "country")


### ADVANCED - plotting effects of a linear model with a categorical predictors ----
## (OKAY TO SKIP, or paste for your own project)
## EFFECTS PLOTS show you the nature of the statistical test
## variable names have been replaced

plot_model(lifeExp_model, type = "pred", terms = c("continent")) 

## categorical vs categorical ----
## compare joint frequencies in contingencies tables
## statistical test from the Chi-squared family
## plot joint frequencies with bar plots or mosaic plots

## Use {Base R} table()
table(df$odd)
table(df$visit)


## contingency tables, put two vectors together
table(df$odd, df$visit) # first value for rows, second value for columns

## easily get the chi-squared value using summary()
summary(table(df$odd, df$visit))
# p-value > 0.8 - of course, no significant difference
# Chi-squared approximation may be incorrect message - because 'yes' column very small

# use fisher exact test as alternative
fisher.test(df$odd,df$visit) # p-value = 1!!


## Chisq_test with {infer} ----
df %>% 
  chisq_test(formula = odd ~ visit)


## plotting bar chart with ggplot
ggplot(df) +
  aes(x = visit, fill = odd) +
  geom_bar() +
  scale_fill_viridis_d(option = "cividis") + # for a fancy colour scheme
  coord_flip() + # change x and y axis
  theme_minimal()

## plotting with a mosaic plot from vcd package

##the mosaic plot
mosaic(~ odd + visit,
       direction = c("v", "h"),
       data = df,
       shade = TRUE)


## Real world example - racial discrimination in the labour market ----
# Does racial discrimination exist in the labor market? Or, should racial disparities
# in the unemployment rate be attributed to other factors such as racial gaps in
# educational attainment? To answer this question, two social scientists conducted
# the following experiment (Bertrand & Mullainathan, 2004)**. In response to newspaper ads, the researchers sent out
# résumés of fictitious job candidates to potential employers. They varied only the names
# of job applicants, while leaving the other information in the résumés unchanged.

# **Bertrand, M., & Mullainathan, S. (2004).
# Are Emily and Greg more employable than Lakisha and Jamal?
# A field experiment on labor market discrimination.
# American economic review, 94(4), 991-1013.

# https://www.aeaweb.org/articles?id=10.1257/0002828042002561


## get the data from online csv
resume <- read_csv("https://tinyurl.com/iss-racial-disc-web")

## check with glimpse
glimpse(resume)



### Base R and table()

## as before, you can get a contingency table with table and two column vectors
(table(resume$race,resume$call))
## or a table of percentages, rounded down
round(prop.table(table(resume$race,resume$call))*100,1) # rounded to one decimal place


## easily get the chi-squared value using summary()
summary((table(resume$race,resume$call)))# p-value < 0.001


### Tidy alternative with {infer}

resume %>% 
  infer::chisq_test(call ~ race)

#' Check the error message
#' Error: The response variable of `call` is not appropriate since 
#' the response variable is expected to be categorical.

resume %>%
  mutate(call = as.factor(call)) %>% 
  infer::chisq_test(call ~ race)


## plotting bar chart with ggplot
resume %>%  
  ggplot(aes(x = race, fill = call)) +
  geom_bar() +
  scale_fill_viridis_d(option = "cividis") +
  coord_flip() +
  theme_minimal()

## Not the best plot ever!

## check the problem with the variables
class(resume$race)
class(resume$call)
## remember call is saved as a numeric variable!

## change to logical and try again
## plotting bar chart with ggplot
resume %>% 
  mutate(call = as.logical(call)) %>% 
  ggplot(aes(x = race, fill = call)) +
  geom_bar() +
  scale_fill_viridis_d(option = "cividis") +
  coord_flip() +
  theme_minimal() 

## plotting with a mosaic plot from vcd package

mosaic(~ race + call,
       direction = c("v", "h"),
       data = resume,
       shade = TRUE)

## this mosaic plot is now shaded to show which group
## is significantly over-represented (in blue) and
## which group is significantly under-represented (in red)

## Using linear regression on categorical - categorical data ----
 glimpse(resume)

## call variable has been coded into dummy 1 and 0 variables
## This means we can also use lm to calculate chi.squared!

cat_cat_model <- lm(call ~ race, resume)

## use summary
summary(cat_cat_model) 

## or use moderndive functions:
get_regression_summaries(cat_cat_model)
# statistic almost the same as chi.square test result
# p_value = significant!

get_regression_table(cat_cat_model)
# intercept estimate = call mean for base variable (black names)
# race: white estimate = added value to base (white names)

####################################################################
####################################################################


## DIY: use dplyr verbs to create a new df with new columns: ----

## black_male, black_female, white_male and white_female applicants
## find out if there is evidence for racial discrimination  for both sexes

## create res_2 variable by
## taking resume df
## group by race and sex variables
## mutate a new column called applicant using paste() to combine the character strings
## ungroup
## select only new applicant and call variables

####################################################################
####################################################################

## CONTINUOUS vs CONTINUOUS  africa-guess data ----
## compare values with scatter plots
## assess association with regression lines
## statistical tests from the R-squared family (or the linear model of regression)

## Let's revisit the df example of height X guess

ggplot(df) +
  aes(x = height, y = guess) +
  geom_point() +
  geom_smooth(method = "lm")  +
  ggpubr::stat_cor(method = "spearman") + # this works without specifying method
  theme_minimal()

## simple correlations without plotting
## working on vectors -- use $ to extrat vectors from df

## to get other statistics, you can use cor.test
## and broom::tidy() to make it easy to read
cor.test(df$height,df$guess, method = "pearson") %>% 
  tidy()

## estimate = correlation
## statistic = t score
## parameter = degrees of freedom
## conf.low = lower 95% confidence level
## conf.high = upper 95% confidence level

## in this case the confidence levels go from -0.XXX to 0.XXX
## if it crosses over 0, there's a good chance there is no relationship at all
## as evidenced by the high p.value

## note that for many correlation tests, the variables are assumed to be distributed 
## normally -- always good practice to check that first

## QUESTION - how can you check variables have normal distribution? ----


## linear regression models for two continuous variables ----
cont_model <- lm(formula = guess ~ height, data = df)
summary(cont_model)
get_regression_summaries(cont_model)
get_regression_table(cont_model) ## note the estimates for intercept and height
get_regression_points(cont_model)


## Our original plot, but now with expanded x axis
ggplot(df) +
  aes(x = height, y = guess) +
  geom_point() +
  geom_smooth(method = "lm")  +
 # geom_abline(mapping=aes(slope=0.26, intercept=1)) +
   xlim(0,200) +
  ggpubr::stat_cor(method="pearson") +
  theme_minimal()

## uncomment the geom_abline
## using the intercept value as the estimate for intercept
## and the the slope value as the estimate for height

####################################################################
####################################################################


## CONTINUOUS vs CONTINUOUS real world example ----
## compare values with scatter plots
## assess association with regression lines
## statistical tests from the R-squared family (or the linear model of regression)

## simple correlations without plotting
## use $ to extract columns as vectors
gap <- gapminder
## this give the the R value, or slope of the line
cor(x=gapminder$gdpPercap, y=gapminder$lifeExp)

## you can choose a method depending on the nature of the data
cor(x=gapminder$gdpPercap, y=gapminder$lifeExp, method = "kendall")

## to get other statistics, you can use cor.test
cor.test(gapminder$gdpPercap,gapminder$lifeExp, method = "pearson")

## you can produce a nice table using the broom::tidy function
cor.test(gapminder$gdpPercap,gapminder$lifeExp, method = "pearson") %>% 
broom::tidy(res)

## Plotting the two continuous variables
gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggpubr::stat_cor()+ # use default correlation setting
theme_minimal()

## Does the line look like a good fit??

# check the distributions of gdpPercap and lifeExp

ggplot(gapminder) +
  aes(x = lifeExp) +
  geom_histogram() +
  theme_minimal()

ggplot(gapminder) +
  aes(x = gdpPercap) +
  geom_histogram() +
  theme_minimal()

## gdpPercap very far from normal
## try some kind of transformation

# log transform gdpPercap
ggplot(gapminder) +
  aes(x = log2(gdpPercap)) +
  geom_histogram() +
  theme_minimal()

# distribution looks better

gapminder %>%
  ggplot(aes(x = log(gdpPercap), y = lifeExp)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggpubr::stat_cor() +
  theme_minimal()
## much nicer regression line and bigger R value! (0.81 vs 0.58)



## let's make two models - one without log transformed variables

gdp_life_model01 <- lm(lifeExp ~ gdpPercap, data = gapminder)

# details
summary(gdp_life_model01)
get_regression_summaries(gdp_life_model01)
get_regression_table(gdp_life_model01)
get_regression_points(gdp_life_model01)

## second one with log transformation
## first do that using dplyr

gapminder_log <- gapminder %>%
  mutate(log_gdp = log(gdpPercap))

names(gapminder_log)

## fit the model
gdp_life_model02 <- lm(lifeExp ~ log_gdp, data = gapminder_log)

# details
summary(gdp_life_model02)
get_regression_summaries(gdp_life_model02)
get_regression_table(gdp_life_model02)
get_regression_points(gdp_life_model02)

####################################################################
####################################################################




### PACKAGE SUMMARY moderndive ----

## Use moderndive functions to see the results of linear regression models

## First run a linear regression model
# continuous ~ categorical 
cat_model <- lm(guess ~ birthday, data = df)
# continuous ~ multi categorical 
multi_cat_model <- lm(gdpPercap ~ continent, data = gapminder2007)
# categorical ~ categorical
cat_cat_model <- lm(call ~ race, resume)
# continuous ~ continuous
cont_model <- lm(formula = guess ~ height, data = df)
# continuous ~ (log-transformed) continuou
gdp_log_model <- lm(lifeExp ~ log_gdp, data = gapminder_log)


## Get the summaries for the model
get_regression_summaries(cat_model)
get_regression_summaries(multi_cat_model)
get_regression_summaries(cat_cat_model)
get_regression_summaries(cont_model)
get_regression_summaries(gdp_log_model)

## HOW BIG IS THE CORRELATION? (closer to 1 = greater correlation)
## r_squared = correlation coefficient
## adj_r_squared = adjusted r squared, adjusted for multiple variables

## HOW GOOD IS THE MODEL? these 3 show the answer (lower is better)
## mse = mean squared error
## rmse = root mean squared error - used to show average difference in residuals
## sigma = standard error of the residuals = rmse / df

## WHAT ARE THE STATS?
## statistic = F-statistic
## p_value = probability (is it significant? e.g. < 0.05)
## df = degrees of freedom
## nobs = number of observations

get_regression_table(cat_model)
get_regression_table(mulit_cat_model)
get_regression_table(cat_cat_model)
get_regression_table(cont_model)
get_regression_table(gdp_life_model02)
## term 1 estimate = intercept = predicted value for the base category 
##               --> base value for continuous = 0
##.              --> base value for categorical might be alphabetical unless set!

## term 2 estimate  = predicted change in value for going from base to term 2
##               --> if continuous  = one unit in continuous variable
##               --> if categorical = going from base to new category
## statistic = test statistic used
## lower_ci = lower confidence interval 
## upper_ci = upper confidence interval

get_regression_points(cat_model)
get_regression_table(mulit_cat_model)
get_regression_points(cat_cat_model)
get_regression_points(cont_model)
get_regression_points(gdp_life_model02, ID = "country")

## _hat shows the predicted values for the model (the means)
## residuals shows the difference between predicted values and real values

### PACKAGE SUMMARY infer ----

## use infer to apply statistical tests on dataframes

## t test
df %>% 
  infer::t_test(formula = guess ~ odd)

## chi_squared test
resume %>% 
  mutate(call = factor(call)) %>% # change call to a factor (categorical) variable
  infer::chisq_test(call ~ race)

### PACKAGE SUMMARY broom ----

## use broom for "tidy" output from statistical tests

## on a t test
t.test(df_even$guess,df_odd$guess) %>% 
  broom::tidy()

## on a cor.test
cor.test(df$height,df$guess, method = "pearson") %>%  
  broom::tidy()

## on a linear model
lm(guess ~ birthday, data = df) %>% 
  broom::tidy()

## on an Anova
aov(formula = gdpPercap ~ continent, data = gapminder2007) %>% 
  broom::tidy()

### PACKAGE summary vcd ----
## use vcd for chi squared test and mosaic plots

## visualise with a mosaic plot
vcd::mosaic(~ race + call,
       direction = c("v", "h"),
       data = resume,
       shade = TRUE)

### PACKAGE summary sjPlot ----

## visualise the effects of variables in your model

## First run a linear regression model
# continuous ~ categorical 
cat_model <- df %>% 
  mutate(birthday_odd = ifelse(odd, "odd","even")) %>% 
   lm(guess ~ birthday_odd, data = .)
# continuous ~ multi categorical 
multi_cat_model <- lm(gdpPercap ~ continent, data = gapminder2007)
# categorical ~ categorical
cat_cat_model <- lm(call ~ race, resume)
# continuous ~ continuous
cont_model <- lm(formula = guess ~ height, data = df)
# continuous ~ (log-transformed) continuou
gdp_log_model <- lm(lifeExp ~ log_gdp, data = gapminder_log)


# define model, choose prediction type, define which terms to visualise
plot_model(cat_model, type = "pred", , terms = "birthday_odd") 
plot_model(multi_cat_model, type = "pred",  terms = "continent") 
plot_model(cat_cat_model, type = "pred", terms = "race")
plot_model(cont_model, type = "pred", , terms = "height") 
plot_model(gdp_log_model, type = "pred", , terms = "log_gdp") 


### PACKAGE summary of ggpur ----

## add statistical test results to ggplots

## compare mean values with stat_compare_means()

## comparing two categorical variables
ggplot(df) +
  aes(x = "", y = guess, fill = odd) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "Birthday date", y = "Guess for how many countries in Africa", title = "Boxplot of African guesses by odd/even birthday") +
  theme_minimal() +
  ggpubr::stat_compare_means(method = "t.test") # specify test with method

ggplot(df) +
  aes(x = "", y = guess, fill = odd) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "Birthday date", y = "Guess for how many countries in Africa", title = "Boxplot of African guesses by odd/even birthday") +
  theme_minimal() +
  ggpubr::stat_compare_means() # default uses non-parametric method (safer)


## comparing multiple categorical variables
ggplot(gapminder2007, aes(x = continent, y = lifeExp)) +
  geom_boxplot() +
  labs(x = "Continent", y = "Life expectancy",
       title = "Life expectancy by continent") +
  stat_compare_means(method = "anova") # specify test with method

## comparing multiple categorical variables
ggplot(gapminder2007, aes(x = continent, y = lifeExp)) +
  geom_boxplot() +
  labs(x = "Continent", y = "Life expectancy",
       title = "Life expectancy by continent") +
  stat_compare_means() # default uses non-parametric method (safer)
