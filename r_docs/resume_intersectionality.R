library(tidyverse)
library(infer) # for stats tests on tidy data
library(vcd) # for chi-squared and mosaic plot

# you can load the data directly here
resume <- read_csv("https://tinyurl.com/iss-racial-disc-web")

glimpse(resume)

## tidyr::unite ----

resume_unite <- resume %>%
  unite("category", race:sex)

## stringr::str_c ----

resume_str <- resume %>%
  mutate(category = str_c(race,sex))

## Notice the differences between the two approaches

## Statistical tests ----
new_resume <- resume_str %>%
    mutate(call = factor(call))

new_resume %>%
chisq_test(formula = category ~ call)


## plotting bar chart with ggplot ----
new_resume %>%
  ggplot(aes(x = category, fill = call)) +
  geom_bar() +
  scale_fill_viridis_d(option = "cividis") + # for a fancy colour scheme
  coord_flip() + # change x and y axis
  theme_minimal()

new_resume %>%
  mutate(call = as.character(call)) %>%
  ## plotting bar chart with ggplot
  ggplot(aes(x = category, fill = call)) +
  geom_bar() +
  scale_fill_viridis_d(option = "cividis") + # for a fancy colour scheme
  coord_flip() + # change x and y axis
  facet_wrap(~sex, scale = "free_y") +
  theme_minimal()


## mosaic plot - the easiest way ----
##the mosaic plot
## note that we can use the original dataframe
vcd::mosaic(~ sex + race + call ,
       direction = c("v", "h"),
       data = resume,
       shade = TRUE)


## Or the new one with the combimed category
vcd::mosaic(~  call + category ,
       direction = c("v", "h"),
       data = new_resume,
       shade = TRUE)

# Which one do you think is better?





