library(tidyverse)
theme_set(theme_light())

summarized <- crossing(people = seq(2, 100, 2),
                       trial = 1:10000) %>%
  mutate(birthday = map(people, ~ sample(365, ., replace = TRUE)),
         multiple = map_lgl(birthday, ~ any(duplicated(.)))) %>%
  group_by(people) %>%
  summarize(chance = mean(multiple))

# Visualizing the probability
ggplot(summarized, aes(people, chance)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "Probability two have the same birthday")


## create group size from 2 to 100, even numbers only
people = seq(2, 100, 2)
## create 10000 random trials
trial = 1:10000


## cross each over into new df
trials <- crossing(trial,people)

## calculate chance

## take the large df
chance_summary <- trials %>%
## add birthday list column with a random sample of days
  mutate(birthday = map(people, ~ sample(365, ., replace = TRUE)),
## find out if any are duplicated
         multiple = map_lgl(birthday, ~ any(duplicated(.)))) %>%
 ## group by size of group
   group_by(people) %>%  ## use mean of logic vector to calculate probablilty
  summarize(chance = mean(multiple))


## Visualizing the probability -------------------------------------------


ggplot(summarized, aes(people, chance)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "Probability two have the same birthday")





# Use stats::pbirthday ----------------------------------------------------
probabilities <- tibble(people = people,
                        probability = map_dbl(people,stats::pbirthday),
                         chance = chance_summary$chance,
                        difference = probability - chance)




# Estimating pi -----------------------------------------------------------

# set the seed for reproducibility
set.seed(314)
# number of simulations
n=1000000
# generate the x and y co-ordinates from uniform
# distribution taking values from -1 to 1
x<-runif(n, min=-1, max=1)
y<-runif(n, min=-1, max=1)
# Distrance of the points from the center (0,0)
z<-sqrt(x^2+y^2)

# the area within the circle is all the z
# which are smaller than the radius^2
# in our case radius=1
4*sum((z<=1))/length(z)

InOut<-as.factor(ifelse(z<=1, "In", "Out"))
plot(x,y, col=InOut, main="Estimate PI with Monte Carlo")

