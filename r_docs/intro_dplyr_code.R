## Clean the environment, Install packages and libraries ----

## Clear all data
rm(list=ls(all=TRUE))

### dplyr package
# install.packages("dplyr") # once per machine
# install.packages("nycflights13") # once per machine
library(ggplot2) # for plotting
library(dplyr)  # in each relevant script
library(nycflights13) # in each relevant script

### install the 'pscl' package to use the 'presidentialElections' data frame
install.packages("pscl")  # once per machine
library(pscl)  # in each relevant script

# Look at the dataset
View(presidentialElections)

## Select ----

# Select 'year' and 'demVote' (= percentage of vote won by the Democrat)
# from the 'presidentialElections' df

votes <- select(presidentialElections,year,demVote) 
# GENERAL PATTERN
# select(df,variable_name1, variable_name2)
# Note there are no quotation marks!

## BASE R equivalent code: (OKAY TO SKIP)
## Extract columns by name
## subset [rows,columns] when column is either "year" or "demVote" 
votes <- presidentialElections[,c("year","demVote")]

## QUESTION: complete the following code to make a plot showing percentage of votes cast for the Democrat ----
ggplot(data = ____, aes(x=____,y=____))+
  geom_point()


## Advanced 1
# add an alpha argument to show overplotting
# change the color of the points

## Advanced 2
# make a new  votes df from presidentialElections to include the names of states
# use geom_text to add state names as text on the plot 

## Advanced 3
# Run the code below.
ggplot(votes) +
  aes(x = year, y = demVote) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_hline(yintercept=50) +
  facet_wrap(~state)

# Which line of code separated all the states into individual plots?
# Answer: 
# What do you think geom_hline(yintercept=50) did?
# Answer: 



## Select can also be vectors of column names without using c()

# select columns 'state' through 'year'
select(presidentialElections,state:year)
# select all columns except for 'south'
select(presidentialElections, -south)

## ADVANCED NOTE 
## select() on a single column will return a dataframe, not a vector
## If you want a specific column as a vector, use in combination with pull()

# Filter ----

## filter() allows you to choose and extract rows from your dataframe

# Filter only rows from the 2008 election
votes_2008 <- filter(presidentialElections, year == 2008)

## GENERAL PATTERN
## filter(df, column_name condition)
## Note the double == for equal to!
## Opposite is != which means NOT equal to


# Extract the row(s) for Colorado in 2008
# Note I've put them on separate lines for readability
votes_colorado_2008 <- filter(
  presidentialElections,
  year == 2008,
  state == "Colorado"
)


## BASE R equivalent code: (OKAY TO SKIP)
## Extract columns by name
## subset [rows,]  all the rows when the column $year ==2008
votes_2008 <- presidentialElections[presidentialElections$year == 2008,] 

## QUESTION: complete the following code to make a plot showing only states with majority votes cast for the Democrat ----
dem_majority <- filter(presidentialElections, ____ > __)

ggplot(data = _____, aes(x=_____,y=______, label=_____))+
  geom_text()



## Advanced 1
# You can add labels to your x and y axes using another layer:
# labs(x = 'x_axis_label', y = 'y_axis_label')
# Add the labels 'Election Year' and 'Democratic Votes (%)' as a layer
# Don't forget the plus + sign at the end of the geom_text line!

ggplot(data = dem_votes, aes(x=year,y=demVote, label=state)) +
  geom_text() 



## Advanced 2
# What happens when you add the argument check_overlap = TRUE to geom_text
# geom_text(check_overlap= TRUE)?


## Advanced 3
# The final line in a ggplot is a good place to set the theme
# Add theme_minimal() or theme_void() or theme_dark(). What happens?
# Don't forget the plus + sign at the end of the geom_text line!
ggplot(data = dem_votes, aes(x=year,y=demVote, label=state))+
  geom_text(check_overlap= TRUE) +
  labs(x = 'Election Year', y = 'Democratic Votes (%)') 

## EXTRA FILTERING with the flights dataset ----
## Add comments to explain what is happening!

# What is being filtered here?
#Answer:
alaska_flights <- flights %>%
  filter(carrier == "AS")

# What is being filtered here?
#Answer:
portland_flights <- flights %>%
  filter(dest == "PDX")


# What do '&' and '|' do ?
#Answer:
btv_sea_flights_fall1 <- flights %>%
  filter(origin == "JFK" & (dest == "BTV" | dest == "SEA") & month >= 10)

# Do '&' ',' function in the same way ?
#Answer:
btv_sea_flights_fall2 <- flights %>%
  filter(origin == "JFK", (dest == "BTV" | dest == "SEA"), month >= 10)
glimpse(btv_sea_flights_fall1)
glimpse(btv_sea_flights_fall2)

# What does '!' do?
#Answer:
not_BTV_SEA1 <- flights %>%
  filter(!(dest == "BTV" | dest == "SEA"))

# Is the position of the '(' and '!' important? compare below:
#Answer:
not_BTV_SEA2 <-  flights %>% filter(!dest == "BTV" | dest == "SEA")

View(not_BTV_SEA1)
View(not_BTV_SEA2)

# Here are a lot of '|' OR and == equal to operators  
many_airports1 <- flights %>%
  filter(dest == "SEA" | dest == "SFO" | dest == "PDX" |
         dest == "BTV" | dest == "BDL")

# Use %in% operator and a vector instead! Are the results the same?
#Answer:
many_airports2 <- flights %>%
  filter(dest %in% c("SEA", "SFO", "PDX", "BTV", "BDL"))
glimpse(many_airports1)
glimpse(many_airports2)

## Filter operators ----
# ==  tests for equality:
# > corresponds to “greater than”
# < corresponds to “less than”
# >= corresponds to “greater than or equal to”
# <= corresponds to “less than or equal to”
# != corresponds to “not equal to.” The ! is used in many programming languages to indicate “not.”

# You can combine multiple criteria using operators that make comparisons:
#   | corresponds to “or”
# & corresponds to “and”

## Mutate ----

## Mutate allows you to create additional columns for your df

## Add an 'other_parties_vote' column that is a percentage of votes
## for other parties
## Also add an 'abs_vote_difference' column of absolute difference
## between percentages

mutate(
  presidentialElections,
  other_parties_vote = 100 - demVote, # other parties is 100% - Democrat%
  abs_vote_difference = abs(demVote - other_parties_vote) # using the abs() function
)

## Note: mutate doesn't change the df itself, only returns the change
## You need to overwrite your df name to save the changes

presidentialElections <- mutate(
  presidentialElections,
  other_parties_vote = 100 - demVote, # other parties is 100% - Democrat%
  abs_vote_difference = abs(demVote - other_parties_vote) # using the abs() function
)

## EXTRA Mutate with flights dataset ----

# Remember the temperatures in the weather df were in Farenheit
#Question: what does the new column show?
# Answer: 
weather <- weather %>% 
  mutate(temp_in_C = (temp - 32) / 1.8)

#Question: what does the new column in the flights df show?
# Answer: 
flights <- flights %>% 
  mutate(gain = dep_delay - arr_delay)

## Arrange ----

## The arrange () function allows you to sort the rows of your df by some column value
## E.g. sort presidentialElections df by year, then within each year sort by percentage
## vote for democrat

# Arrange rows in decreasing order by 'year', then by 'demVote' within each year

presidentialElections <- arrange(presidentialElections, -year, demVote)
presidentialElections <- arrange(presidentialElections, desc(year), demVote) # same as above

# By default, arrange() will sort by increasing order
# Use minus sign (-) or desc() function for descending order

## Summarise ----

## summarise() (or US spelling summarize()) generates a new df containing a summary of 
## a column, computing a single value from a function on the column values

# Compute summary statistics 'mean' for the 'presidentialElections' df

average_votes <- summarise(
  presidentialElections,
  mean_dem_vote = mean(demVote),
  mean_other_parties = mean(other_parties_vote)
)
average_votes

# summarise(df, new_column_name1 = function(column),
#                new_column_name2 = function(column))

## You can use for any summary function:
## mean()
## median()
## max()
## min()
## sd() for standard deviations
## n() for frequencies
## sum() for totals

## QUESTIONS with summarise  ----



# Why does this code not show the mean?
# Answer:
summarise(weather,
            mean = mean(temp), 
            std_dev = sd(temp))

# na.rm = TRUE is an argument of both mean() and sd(), removing all NA values
summarise(weather,
           mean = mean(temp, na.rm = TRUE), 
          std_dev = sd(temp, na.rm = TRUE))

# you can also use na.omit() function with the pipe operator %>%  as we will see later...

## ADVANCED (OKAY TO SKIP) Your own functions ----

## You can create your own function using the function() {} code
## We'll look at making functions later on

## A function that returns furthest from 50
furthest_from_50 <- function(vec){
  # subtract 50 from each value
  adjusted_values <- vec - 50
  
  # return the element with the largest absolute difference from 50
  vec[abs(adjusted_values) == max(abs(adjusted_values))]
}

# summarise the df, generating a column 'biggest_landslide'
# that stores the value furthest from 50%

summarise(
  presidentialElections,
  biggest_landslide = furthest_from_50(demVote)
)



## Combining dplyr functions ----

## more complex analyses often requires combining functions
## Taking the result of one function, and putting it into another
## Three appraoches
## A Use intermediary variables - like stepping stones
## B Use nested functions - like Excel functions
## C Use the pipe operator - like humans read and think

## Question - which state had the highest percentage of votes for Obama in 2008?
## Simple question, but requires some steps:
## Filter the df to only observations from 2008
## Of the percentages in 2008, filter down to the highest percentage
## Select the name of the state

## A. Use a sequence of steps

## 1. Filter down to only 2008 votes
votes_2008 <- filter(presidentialElections, year == 2008)
votes_2008
## 2. Filter down to the state with the highest demVote
most_dem_votes <- filter(votes_2008, demVote == max(demVote))
most_dem_votes
## 3. Select the name of the state
most_dem_state <- select(most_dem_votes,state)
most_dem_state
## Pros - easy to read and understand later
## Cons - you create a lot of new variables which soon clutter up your Environment

## B. Use nested functions

most_dem_state <- select(filter(filter(presidentialElections, year == 2008), demVote == max(demVote)), state)
## So many parantheses!

## Let's break it down
most_dem_state <- select( # 3. select the name of the state
  filter( # 2. Filter down to the state with the highest demVote
    filter( # 1. Filter down to only 2008 votes
      presidentialElections, # Arguments for step 1 'filter'
      year == 2008
    ),
    demVote == max(demVote) # second argument for step 2 'filter'
  ),
  state # # second argument for step 3 'select'
)


## Pros - no need to create redundant variables
## Cons - not easy to read or understand later; easy to miscalculate the order

## C. The Pipe Operator ----

## The pipe operator is written as %>%,
## It has a shortcut of control-shift-m / command-shift-m in RStudio
## It takes the result of one function, and passes it as the first argument to the next

## Same question as above using the pipe operator:

presidentialElections %>% # start with the df
  filter(year == 2008) %>% # 1. Filter down to only 2008 votes
  filter(demVote == max(demVote)) %>% # 2. Filter down to the state with the highest demVote
  select(state) # 3. select the name of the state

## I often check if the pipe chain works first
## then just add the new object name afterwards

most_dem_state <- presidentialElections %>% # start with the df
  filter(year == 2008) %>% # 1. Filter down to only 2008 votes
  filter(demVote == max(demVote)) %>% # 2. Filter down to the state with the highest demVote
  select(state) # 3. select the name of the state

## Pros: Easy to read and understand later; no redundant variables
## Cons: Don't forget to put the pipe at the end of a line!


## QUESTION: compare these two: ----
weather %>%   
  summarise(mean = mean(temp, na.rm = TRUE)) %>% 
  summarise(std_dev = sd(temp, na.rm = TRUE))

weather %>%   
  summarise(mean = mean(temp, na.rm = TRUE), 
  std_dev = sd(temp, na.rm = TRUE))

## Why does the first one produce error message?
#Answer: 



## group_by ----

## summarise is most powerful when used in combination with another function - group_by()
## group_by() lets you group rows together so that you can summarise the grouped data for
## things like mean(), sd(), sum(), n() etc

# Group observations by state
group_by(presidentialElections, state)

# Group observations by state using the pipe
grouped <- presidentaElections %>% group_by(state)

## Returns a grouped_df Type object in the Environment pane

## The grouped df does not look too special, but now you can apply other dplyr verbs
## onto the grouped date

# Compute average percentages across the years by state
state_voting_summary <- presidentialElections %>%
  group_by(state) %>%
  summarise(
    mean_dem_vote = mean(demVote),
    mean_other_parties = mean(other_parties_vote)
  )

# QUESTIONS with the weather and flights dfs ----

# What's the difference between these three?
summary_temp1 <- weather %>% na.omit %>% 
  summarise(mean = mean(temp), std_dev = sd(temp))

summary_temp2 <- weather %>% 
  group_by(month) %>% 
  summarise(mean = mean(temp, na.rm = TRUE), 
            std_dev = sd(temp, na.rm = TRUE))

summary_temp3 <- weather %>% na.omit %>% 
  group_by(month) %>% 
  summarise(mean = mean(temp), 
            std_dev = sd(temp))

summary_temp1
summary_temp2
summary_temp3
# Answer:

## What does this code summarise?
by_origin <- flights %>% 
  group_by(origin) %>% 
  summarise(count = n())

by_origin
# Answer:

## What is the effect of putting two variables in the group_by()?
by_origin_monthly <- flights %>% 
  group_by(origin, month) %>% 
  summarise(count = n())

by_origin_monthly
# Answer:

## Why is this incorrect? What does it show?
by_origin_monthly_incorrect <- flights %>% 
  group_by(origin) %>% 
  group_by(month) %>% 
  summarise(count = n())

by_origin_monthly_incorrect
# Answer:

## Adding a gain variable to show time gained during flight
flights <- flights %>% 
  mutate(gain = dep_delay - arr_delay)

# Create a full summary of gain
gain_summary <- flights %>% 
  summarise(
    min = min(gain, na.rm = TRUE),
    q1 = quantile(gain, 0.25, na.rm = TRUE), # using quantile() to define IQR first quarter
    median = quantile(gain, 0.5, na.rm = TRUE),
    q3 = quantile(gain, 0.75, na.rm = TRUE), # using quantile() to define IQR third quarter
    max = max(gain, na.rm = TRUE),
    mean = mean(gain, na.rm = TRUE),
    sd = sd(gain, na.rm = TRUE),
    missing = sum(is.na(gain))
  )
gain_summary


## make a histogram of the gain variable - does it look normal?
ggplot(data = _____, mapping = aes(x = ____)) +
  geom_() 
# warning message matches are missing variables data

## -----------------------------------------------------------------------------
flights <- flights %>% 
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours
  )

## CHALLENGE Make a new df called freq_dest ----
## take the flights dataset
## group by destination
## summarise the number of flights with n(), call the variable num_flights


## Arrange the freq_dest df so that num_flights is in descending order


## Joining dataframes ----
# Create first example data frame
profs1 <- tibble(teacher = c("KI","LM"),                      
                    hobby = c("windsurfing", "cinema"))
# Create second example data frame
profs2 <- tibble(teacher = c("LM","TO"),                      
                    drink = c("tea", "sake"))

profs1
profs2

# left_join keeps all the rows in the left df, adds NAs if necessary to columns from right df
left_join(profs1, profs2, by = "teacher") 


# inner_join keeps only rows shared between the dfs
inner_join(profs1, profs2, by = "teacher")  

# it is the same as doing left_join then na.omit
left_join(profs1, profs2, by = "teacher") %>% na.omit

# full_join keeps all columns and rows, adds NAs if necessary to all columns
full_join(profs1, profs2, by = "teacher") 

# Create third example data frame
profs3 <-  tibble(prof = c("YN","LM","TO"),                     
                  reading = c("history", "art","sci-fi"))

# no shared column names!
colnames(profs1)
colnames(profs3)

# use by = c("colname1" = "colname2") to join dfs with different column names
full_join(profs3, profs1, by = c("prof" = "teacher"))


# Look at airlines from the nycflights13 library

View(airlines)

# the airlines df works like a glossary, defining the carrier code as airline names

# Create a new df with the airline names attached
flights_joined <- flights %>%
  inner_join(airlines, by = "carrier")
View(flights)
View(flights_joined)

# In airports df, faa shows the code for airports, the next column gives the names
airports

# No faa column name in the flights df, but the dest variable uses the same code
# Join the flights and airports df to get the airport names 
# using by = c('colname1' = 'colname2')
flights_with_airport_names <- flights %>%
inner_join(airports, by = c("dest" = "faa"))

flights_with_airport_names


## CHALLENGE - join airports with freq_dest df to find out the airport names ----
## use select to end up with only a 3 column df with dest, name and num_flights


