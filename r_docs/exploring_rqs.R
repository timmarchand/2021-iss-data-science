## Exploring random questions data

## OPTIONAL clear memory
 rm(list=ls(all=TRUE))

## load libraries and data ----
# library(tidyverse)
# library(gridExtra) # to put plots together

pacman::p_load(tidyverse, gridExtra)

## load data from folder


dat <- read_csv("data/random_qs.csv")
glimpse(dat)

## add case numbers
rq_all<- dat %>%
            mutate(case = row_number(), .before = timestamp)
rq_class <- dat %>%
            filter(data_sci) %>% 
            mutate(case = row_number(), .before = timestamp)


## data on Japanese heights for comparisons
jpn_ht <-  read_csv("https://tinyurl.com/iss-jpn-height-web")
glimpse(jpn_ht)



## Density plot for height from Japan data ----
p1 <- ggplot(jpn_ht) +
  aes(x = height) +
  geom_density(adjust = 1, fill = "steelblue") +
  labs(x = "Height in cm", y = "Probability Density",
       title ="Density plot of heights in Japan",
       subtitle = str_c("n = ", nrow(jpn_ht))) +
  theme_minimal()
p1

## The two points in the plot means the data look bimodial
## This means there may be 2 separate populations..

## Let's check by plotting 2 density plots
## Using fill to separate the sexes
p2 <- ggplot(jpn_ht) +
  aes(x = height, fill = sex) +
  geom_density(adjust = 1, alpha = 0.5) +
  labs(x = "Height in cm", y = "Probability Density") +
  scale_fill_hue() +
  theme(legend.position="bottom")
p2

## put the plots together
grid.arrange(p1,p2)

## This strongly suggests we should consider sex as a factor
## when collecting data for height (no surprise there)

## A boxplot confirms this suspicion
ggplot(jpn_ht) +
  aes(x = "", y = height, fill = sex) +
  geom_boxplot() +
  scale_fill_hue() +
  labs( y = "Height in cm", title = "Boxplot of heights in Japan") +
  theme(axis.title.x = element_blank())


## Explore the class height variables with a box plot
## The plot suggests there may be a difference in heights depending on
## if you are born on an even or odd date

### Code chunk 1 ----
ggplot(rq_class) +
  aes(x = "", y = height, fill = odd) +
  geom_boxplot() +
  scale_fill_hue() +
 labs(x = "Height in cm", y = "Probability Density",
        title = "Density plot of height",
       subtitle = str_c("n = ", nrow(rq_class))) +
  theme_minimal()
## But is the n value enough to make that judgment?


### Code chunk 2 ----
## Explore the class height variables with a density plot
ggplot(rq_class) +
  aes(x = height) +
  geom_density(adjust = 0.5, fill = "#0c4c8a") +
 labs(x = "Height in cm", y = "Probability Density",
        title = "Density plot of height",
       subtitle = str_c("n = ", nrow(rq_class))) +
  theme_minimal()
## Is the height variable bimodial? A combination of two separate groups?

## Let's check by separating the number variable as fill
## Also add "rugs" just to show where the data lies

### Code chunk 3 ----
ggplot(rq_class) +
  aes(x = height, fill = odd) +
  geom_density(adjust = 0.5,alpha = 0.5) +
  geom_rug(aes(colour=odd)) +
  labs(x = "Height in cm", y = "Probability Density",
        title = "Density plot of height by odd/even birthday",
       subtitle = str_c("n = ", nrow(rq_class)))  +
  theme_minimal()

## The density plot shows both variables are far from normal (as might be expected)
# It will be difficult to get a reliable statistical measure from this

## Now copy, paste and tweak the above for rq_all data
### Code chunk 1 copy

### Code chunk 2 copy

### Code chunk 3 copy

## Do you notice anything strange??

ggplot(rq_all) +
  aes(x = height) +
  geom_density(adjust = 1, fill = "#0c4c8a") +
  labs(x = "Height in cm", y = "Probability Density",
       title = "Density plot of height",
       subtitle = str_c("n = ", nrow(rq_all))) +
  theme_minimal()

## Something strange going on here
## Which cases are the outliers?
## Use filter to find out!


## What to do with these?
## case 91 could probably be fixed by multiplying by 100
## case 26 should probably be dropped

## For now, let's just filter out both

rq_filtered <- rq_all %>%
  filter(height > 100)



## Explore the class height variables with a density plot
rq_filtered %>%
ggplot() +
  aes(x = height, fill = odd) +
  geom_density(adjust = 1,alpha = 0.5) +
  geom_rug(aes(colour=odd)) +
 labs(x = "Height in cm", y = "Probability Density",
       title = "Density plot of height by odd/even birthday",
       subtitle = str_c("n = ", nrow(rq_filtered))) +
  theme_minimal()

## And with a boxplot
### Code chunk 4 ----
rq_filtered %>%
  ggplot(aes(x = "", y = height, fill = odd)) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "Birthday", y = "Height in cm") +
  labs(x = "Height in cm", y = "Probability Density",
       title = "Boxplot of height by odd/even birthday",
       subtitle = str_c("n = ", nrow(rq_filtered))) +
  theme_minimal()

## Given the small size of n
## I'd guess that these two "populations" are not
## significantly different

### Now let's do the same by looking at the comparing the 
### the guess vs odd/even birthday variable on rq_all

## Remember to copy, paste and tweak
## First the density plot (Code chunk 3)
rq_all %>%
ggplot() +
  aes(x = guess, fill = odd) +
  geom_density(adjust = 1,alpha = 0.5) +
  geom_rug(aes(colour=odd)) +
 labs(x = "Guess", y = "Probability Density",
       title = "Density plot of guess by odd/even birthday",
       subtitle = str_c("n = ", nrow(rq_all))) +
  theme_minimal()

## Now the boxplot (Code chunk 4)



## Do you think the two populations look categorically different?
## If so, can you think of any reason why this could be so?
