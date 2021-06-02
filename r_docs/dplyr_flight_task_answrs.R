library(dplyr)
library(nycflights13)
library(ggplot2)


## CHALLENGE - join airports with freq_dest df to find out the airport names ----
## use select to end up with only a 3 column df with dest, name and num_flights
freq_dest <- flights %>%
  group_by(dest) %>%
  summarise(num_flights = n()) %>%
  arrange(desc(num_flights)) %>%
  left_join(airports, by = c("dest" = "faa")) %>% 
  select(dest, name, num_flights)
freq_dest


## SEAT AVAILABILITY CHALLENGE ----

# Extending this idea, let’s say an airline had 2 flights using a plane with 10 seats that flew 
# 500 miles and 3 flights using a plane with 20 seats that flew 1000 miles, 
# 
# the available seat miles would be:  
# #   
# #  (2 × 10 × 500) + (3 × 20 × 1000)   = 70,000 seat miles.
# #
#  Using the datasets included in the nycflights13 package, compute the available seat miles 
#   for each airline sorted in descending order. 
#  After completing all the necessary data wrangling steps, the resulting data frame should have 16 rows 
# (one for each airline) and 2 columns (airline name and available seat miles). 

### variables you need:
### airline names, seats on each plane, distance of each flight, airplane id:


## first glimpse at the data, identify the important variables
glimpse(flights) # tailnum for plane id, carrier for airline id, distance for distance in miles
glimpse(planes) # tailnum for plane id, seats for number of seats NOTE year shows year of manufacture!!
glimpse(airlines) # carrier for airline id and name for airline name


## next join the dfs, then select columns of interest
## join planes and flights first to match up seats with distance
## join using planes first with inner_join so that year is not matched up
step_1 <- inner_join(planes,flights) %>% select(tailnum,distance,seats, carrier)
## alternative, specify the matching variable names
step_1 <- left_join(planes,flights, by = "tailnum") %>% select(tailnum,distance,seats, carrier)
step_1

## join new df with airlinesto match up carrier with airline names
step_2 <- left_join(step_1,airlines, by = "carrier") %>% select(name,distance,seats)
step_2

## mutate the df to add a new column seat_miles
step_3 <- step_2 %>% mutate(seat_miles = distance * seats) %>% select(name,seat_miles)
step_3

## summarise the total seat_miles for each airline (group_by name)
step_4 <- step_3 %>% group_by(name) %>% 
  summarise(seat_availability = sum(seat_miles, na.rm = TRUE))
step_4

## tidy up by arranging in descending order, rename name to airline
step_5 <- step_4  %>% arrange(desc(seat_availability)) %>% 
  rename(airline = name)
step_5


### This can all be done without new objects and piped into ggplot

left_join(planes,flights, by = "tailnum") %>% 
  left_join(airlines, by = "carrier") %>% 
  mutate(seat_miles = distance * seats) %>%
  group_by(name) %>% 
  summarise(seat_availability = sum(seat_miles, na.rm = TRUE)) %>% 
  arrange(desc(seat_availability)) %>% 
  rename(airline = name) %>%
  select(airline,seat_availability) # %>% 
# ggplot(aes(x = reorder(airline,seat_availability), y = seat_availability)) +
#   geom_col() +
#   coord_flip()+
#   theme_minimal()
  
  
  

  
  
  
