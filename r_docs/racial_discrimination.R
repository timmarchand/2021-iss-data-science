#' ---
#' title: "Untitled"
#' author: "Tim Marchand"
#' date: "10/7/2020"
#' output: html_document
#' editor_options:
#'   chunk_output_type: console
#' ---
#' 
#' # Section 2.1: Racial Discrimination in the Labor Market
## -------------------------------------------------------------------
resume <- read_csv("data/resume.csv")

dim(resume)
head(resume)
summary(resume)

race.call.tab <- table(race = resume$race, call = resume$call) 
race.call.tab


chisq.test(resume$race,resume$call)

prop.table(race.call.tab)

addmargins(race.call.tab)

## overall callback rate: total callbacks divided by the sample size
sum(race.call.tab[, 2]) / nrow(resume)

## callback rates for each race
race.call.tab[1, 2] / sum(race.call.tab[1, ]) # black
race.call.tab[2, 2] / sum(race.call.tab[2, ]) # white

race.call.tab[1, ]  # the first row
race.call.tab[, 2]  # the second column

mean(resume$call)
 ## mosaic plots ----

fly
ggplot(data = fly) +
  geom_mosaic(aes(x = product(RudeToRecline), fill=RudeToRecline), na.rm=TRUE) +
  labs(x="Is it rude recline? ", title='f(RudeToRecline)') 

ggplot(data = fly) +
  geom_mosaic(aes(x = product(DoYouRecline, RudeToRecline), fill=DoYouRecline), na.rm=TRUE) + 
  labs(x = "Is it rude recline? ", title='f(DoYouRecline | RudeToRecline) f(RudeToRecline)')

ggplot(data = resume) +
  geom_mosaic(aes(x = product(race, call), fill=call), na.rm=TRUE) + 
  labs(x = "Is it rude recline? ", title='f(DoYouRecline | RudeToRecline) f(RudeToRecline)')


data(Titanic)
titanic <- as.data.frame(Titanic)
titanic$Survived <- factor(titanic$Survived, levels=c("Yes", "No"))

library(ggmosaic)
ggplot(data=titanic) +
  ggmosaic::geom_mosaic(aes(weight=Freq, x=product(Class), fill=Survived))

ggplot(data = resume) +
  geom_mosaic(aes(weight=call,x =product(race), fill=call))
# good practice: use the 'dependent' variable (or most important variable)
# as fill variable

#' 
#' 
#' # Section 2.2: Subsetting the Data in R
## -------------------------------------------------------------------
class(TRUE)

as.integer(TRUE)
as.integer(FALSE)

x <- c(TRUE, FALSE, TRUE) # a vector with logical values

mean(x) # proportion of TRUEs
sum(x) # number of TRUEs

FALSE & TRUE
TRUE & TRUE
TRUE | FALSE
FALSE | FALSE
TRUE & FALSE & TRUE

(TRUE | FALSE) & FALSE # the parentheses evaluate to TRUE
TRUE | (FALSE & FALSE) # the parentheses evaluate to FALSE

TF1 <- c(TRUE, FALSE, FALSE)
TF2 <- c(TRUE, FALSE, TRUE)
TF1 | TF2
TF1 & TF2

#' 
#' ## Section 2.2.2: Relational Operators
#' 
## -------------------------------------------------------------------
4 > 3

"Hello" == "hello"  # R is case-sensitive
"Hello" != "hello"

x <- c(3, 2, 1, -2, -1)
x >= 2
x != 1

## logical conjunction of two vectors with logical values
(x > 0) & (x <= 2)
## logical disjunction of two vectors with logical values
(x > 2) | (x <= -1)

x.int <- (x > 0) & (x <= 2) # logical vector
x.int

mean(x.int) # proportion of TRUEs
sum(x.int)  # number of TRUEs

#' 
#' ## Section 2.2.3: Subsetting
#' 
## -------------------------------------------------------------------
## callback rate for black-sounding names
mean(resume$call[resume$race == "black"]) 

## dplyr
library(dplyr)
resume %>% filter(race == "black") %>% summarise(mean = mean(call))


## race of first 5 observations
resume$race[1:5]  

resume %>% select(race) %>% slice(1:5)

## comparison of first 5 observations
(resume$race == "black")[1:5] 

dim(resume) # dimension of original data frame

## subset blacks only
resumeB1 <- resume[resume$race == "black", ]
dim(resumeB1) # this data.frame has fewer rows than the original data.frame
mean(resumeB1$call) # callback rate for blacks

resumeB2 <- resume %>% filter(race == "black")
dim(resumeB2) # this data.frame has fewer rows than the original data.frame
mean(resumeB2$call) # callback rate for blacks



## keep "call" and "firstname" variables 
## also keep observations with black female-sounding names
resumeBf1 <- subset(resume, select = c("call", "firstname"),
                   subset = (race == "black" & sex == "female"))
head(resumeBf1)

resumeBf2 <- resume %>% filter(race == "black" & sex == "female") %>% 
                        select(call,firstname)

head(resumeBf2)

applicant <- resume %>% group_by(race,sex) %>% summarise(call_rate = sum(call) / length(call)) %>% 
  mutate(applicant = paste(race,sex,sep ="_")) %>% ungroup %>% 
  select(applicant,call_rate)

applicant2 <- resume %>% group_by(race,sex) %>%  
  mutate(applicant = paste(race,sex,sep ="_")) %>% ungroup %>% 
  select(applicant,call)

chisq.test(applicant2)

table(applicant2$applicant,applicant2$call)


## ## an alternative syntax with the same results
## resumeBf <- resume[resume$race == "black" & resume$sex == "female",
##                    c("call", "firstname")]
## black male
resumeBm <- subset(resume, subset = (race == "black") & (sex == "male"))
## white female
resumeWf <- subset(resume, subset = (race == "white") & (sex == "female"))
## white male
resumeWm <- subset(resume, subset = (race == "white") & (sex == "male"))
## racial gaps
mean(resumeWf$call) - mean(resumeBf$call) # among females
mean(resumeWm$call) - mean(resumeBm$call) # among males

#' 
#' ## Section 2.2.4: Simple Conditional Statements
#' 
## -------------------------------------------------------------------
resume$BlackFemale <- ifelse(resume$race == "black" & 
                                 resume$sex == "female", 1, 0)
table(race = resume$race, sex = resume$sex,
      BlackFemale = resume$BlackFemale)

#' 
#' ## Section 2.2.5: Factor Variables
#' 
## -------------------------------------------------------------------
resume$type <- NA
resume$type[resume$race == "black" & resume$sex == "female"] <- "BlackFemale"
resume$type[resume$race == "black" & resume$sex == "male"] <- "BlackMale"
resume$type[resume$race == "white" & resume$sex == "female"] <- "WhiteFemale"
resume$type[resume$race == "white" & resume$sex == "male"] <- "WhiteMale"

## check object class
class(resume$type)

## coerce new character variable into a factor variable
resume$type <- as.factor(resume$type)
## list all levels of a factor variable
levels(resume$type)

## obtain the number of observations for each level
table(resume$type)
tapply(resume$call, resume$type, mean)

## turn first name into a factor variable 
resume$firstname <- as.factor(resume$firstname)
## compute callback rate for each first name
callback.name <- tapply(resume$call, resume$firstname, mean)
## sort the result in the increasing order
sort(callback.name)

#' 
#' # Section 2.3: Causal Effects and the Counterfactual
## -------------------------------------------------------------------
resume[1, ]

#' 
#' # Section 2.4: Randomized Controlled Trials
#' 
#' ## Section 2.4.1: The Role of Randomization
#' 
#' ## Section 2.4.2: Social Pressure and Voter Turnout
#' 
## -------------------------------------------------------------------
social <- read.csv("social.csv") # load the data

summary(social) # summarize the data

## turnout for each group
tapply(social$primary2006, social$messages, mean)

## turnout for control group
mean(social$primary2006[social$messages == "Control"])

## subtract control group turnout from each group
tapply(social$primary2006, social$messages, mean) -
    mean(social$primary2006[social$messages == "Control"])

social$age <- 2006 - social$yearofbirth # create age variable
tapply(social$age, social$messages, mean)
tapply(social$primary2004, social$messages, mean) 
tapply(social$hhsize, social$messages, mean) 

#' 
#' # Section 2.5: Observational Studies
#' 
#' ## Section 2.5.1: Minimum Wage and Unemployment
#' 
## -------------------------------------------------------------------
minwage <- read.csv("minwage.csv") # load the data

dim(minwage) # dimension of data
summary(minwage) # summary of data

## subsetting the data into two states
minwageNJ <- subset(minwage, subset = (location != "PA"))
minwagePA <- subset(minwage, subset = (location == "PA"))

## proportion of restaurants whose wage is less than $5.05
mean(minwageNJ$wageBefore < 5.05) # NJ before 
mean(minwageNJ$wageAfter < 5.05)  # NJ after 
mean(minwagePA$wageBefore < 5.05) # PA before 
mean(minwagePA$wageAfter < 5.05)  # PA after 

## create a variable for proportion of full-time employees in NJ and PA
minwageNJ$fullPropAfter <- minwageNJ$fullAfter / 
    (minwageNJ$fullAfter + minwageNJ$partAfter)
minwagePA$fullPropAfter <- minwagePA$fullAfter / 
    (minwagePA$fullAfter + minwagePA$partAfter)

## compute the difference in means
mean(minwageNJ$fullPropAfter) - mean(minwagePA$fullPropAfter)

#' 
#' ## Section 2.5.2: Confounding Bias
#' 
## -------------------------------------------------------------------
prop.table(table(minwageNJ$chain))
prop.table(table(minwagePA$chain))

## subset Burger King only
minwageNJ.bk <- subset(minwageNJ, subset = (chain == "burgerking"))
minwagePA.bk <- subset(minwagePA, subset = (chain == "burgerking"))

## comparison of full-time employment rates
mean(minwageNJ.bk$fullPropAfter) - mean(minwagePA.bk$fullPropAfter)

minwageNJ.bk.subset <- 
    subset(minwageNJ.bk, subset = ((location != "shoreNJ") & 
                                       (location != "centralNJ")))

mean(minwageNJ.bk.subset$fullPropAfter) - mean(minwagePA.bk$fullPropAfter)

#' 
#' ## Section 2.5.3: Before-and-After and Difference-in-Differences Designs
#' 
## -------------------------------------------------------------------
## full-time employment proportion in the previous period for NJ
minwageNJ$fullPropBefore <- minwageNJ$fullBefore / 
    (minwageNJ$fullBefore + minwageNJ$partBefore)

## mean difference between before and after the minimum wage increase
NJdiff <- mean(minwageNJ$fullPropAfter) - mean(minwageNJ$fullPropBefore)
NJdiff

## full-time employment proportion in the previous period for PA
minwagePA$fullPropBefore <- minwagePA$fullBefore / 
    (minwagePA$fullBefore + minwagePA$partBefore)
## mean difference between before and after for PA
PAdiff <- mean(minwagePA$fullPropAfter) - mean(minwagePA$fullPropBefore)
## difference-in-differences
NJdiff - PAdiff

## full-time employment proportion in the previous period for PA
minwagePA$fullPropBefore <- minwagePA$fullBefore / 
    (minwagePA$fullBefore + minwagePA$partBefore)
## mean difference between before and after for PA
PAdiff <- mean(minwagePA$fullPropAfter) - mean(minwagePA$fullPropBefore)
## difference-in-differences
NJdiff - PAdiff

#' 
#' # Section 2.6: Descriptive Statistics for a Single Variable
#' 
#' ## Section 2.6.1: Quantiles
#' 
## -------------------------------------------------------------------
## cross-section comparison between NJ and PA
median(minwageNJ$fullPropAfter) - median(minwagePA$fullPropAfter)
## before and after comparison
NJdiff.med <- median(minwageNJ$fullPropAfter) - 
    median(minwageNJ$fullPropBefore)
NJdiff.med
## median difference-in-differences
PAdiff.med <- median(minwagePA$fullPropAfter) - 
    median(minwagePA$fullPropBefore)
NJdiff.med - PAdiff.med

## summary shows quartiles as well as minimum, maximum, and mean
summary(minwageNJ$wageBefore)
summary(minwageNJ$wageAfter)
## interquartile range 
IQR(minwageNJ$wageBefore)
IQR(minwageNJ$wageAfter)

## deciles (10 groups)
quantile(minwageNJ$wageBefore, probs = seq(from = 0, to = 1, by = 0.1))
quantile(minwageNJ$wageAfter, probs = seq(from = 0, to = 1, by = 0.1))

#' 
#' ## 2.6.2: Standard Deviation
#' 
## -------------------------------------------------------------------
sqrt(mean((minwageNJ$fullPropAfter - minwageNJ$fullPropBefore)^2))
mean(minwageNJ$fullPropAfter - minwageNJ$fullPropBefore)

## standard deviation
sd(minwageNJ$fullPropBefore)
sd(minwageNJ$fullPropAfter)
## variance
var(minwageNJ$fullPropBefore)
var(minwageNJ$fullPropAfter)


resume%>%
  group_by(race,sex)%>%
  summarize(n=n())%>%
  mutate(prop=n/sum(n))   # our new proportion variable



resume%>%
  group_by(race, sex)%>%
  summarize(n=n())

resume%>%
  group_by(race,sex)%>%
  summarize(n=n())%>%
  mutate(prop=n/sum(n))   # our new proportion variable

resume%>%
  group_by(race, sex)%>%
  summarise(n=n())%>%
  tidyr::spread(race, n) # spread the variable you want as column names

resume%>%
  group_by(race, sex)%>%
  summarise(mean_call=mean(call))%>%
  tidyr::spread(sex, mean_call) # # spread the variable you want as column names

resume%>%
  group_by(race, sex)%>%
  summarize(n=n())%>%
  mutate(prop=n/sum(n))%>%
  select(-n) %>%   #drop the frequency value
  tidyr::spread(sex, prop)


xtab <- with(resume, table(race,call))
chisq.test(xtab)
chisq.test(xtab)$expected
mode(resume$call)

resumef <- mutate(resume, call = factor(call))

disc_mdl <- glm(call ~ race*sex, data = resumef, family = 'binomial')
broom::tidy(disc_mdl)
