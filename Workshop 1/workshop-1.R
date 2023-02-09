# This is a script 
# You can write code here, save it, and run it when necessary 

#set working directory
setwd("~/Documents/workshop-1")

# you will only need to install packages once 
install.packages("tidyverse")
install.packages("kableExtra")
install.packages("stargazer")
install.packages("knitr")
install.packages("rdrobust")

#load in libraries 
library(tidyverse)
library(kableExtra)
library(stargazer)
library(knitr)
library(rdrobust)

### some R programming basics 
# object assignment 
# (strings, numbers, dataframes, lists, etc.)
this_is_an_object <- "object"

# this is how you inspect an object 
this_is_an_object

# another assignment 
x <- 2^3 

# another inspection 
x

# read in and inspect MLDA data 
mlda <- read_csv("mlda.csv")
mlda
View(mlda)

# some quick data exploration 
mean(mlda$student)
mean(mlda$drinks_alcohol)

# add a new variable for age in years and a heavy drinker variable if they drink more than 75% of days
mlda <- mutate(mlda, 
               age_years = 21 + days_21/365, 
               heavy_drinker = if_else(perc_days_drink > 75, 1, 0))

# filter to just underage drinkers 
underage <- filter(mlda, days_21 < 0 & drinks_alcohol == 1)

# demographic characteristics of underage drinkers 
summ_stats_underage <- summarise(underage, 
                                 min_age = min(age_years),
                                 mean_age = mean(age_years), 
                                 median_age = median(age_years),
                                 mean_heavy_drinker = mean(heavy_drinker), 
                                 mean_white = mean(white), 
                                 mean_black = mean(black), 
                                 mean_hispanic = mean(hispanic))

summ_stats_underage

# the pipe 
x <- paste("A", "String")
print(x)

print(paste("A", "String"))

paste("A", "String") %>% 
  print(.)

## apply the pipe 
summ_stats <- mutate(mlda, 
                     age_years = 21 + days_21/365, 
                     heavy_drinker = if_else(perc_days_drink > 75, 1, 0)) %>% 
  filter(., days_21 < 0 & drinks_alcohol == 1) %>%  
  summarise(.,min_age = min(age_years),
            mean_age = mean(age_years), 
            median_age = median(age_years),
            mean_perc_days_drink = mean(perc_days_drink), 
            median_perc_days_drink = median(perc_days_drink)) 

summ_stats

# now let's look at summary stats of heavy drinkers vs non heavy drinkers: 

# demographic characteristics of heavy drinkers vs non-heavy drinkers
summ_stats_heavy <- mlda %>% 
  filter(drinks_alcohol ==1) %>%
  group_by(heavy_drinker) %>%
  summarise(min_age = min(age_years),
            mean_age = mean(age_years), 
            median_age = median(age_years),
            mean_student = mean(student),
            n = n())
summ_stats_heavy

## summary stats for student drinkers 
summ_stats2 <- mutate(mlda, 
                      student_drinker = if_else(student==1 & drinks_alcohol == 1, 1,0)) %>%
  group_by(student_drinker) %>%  
  summarise(mean_male = mean(male), 
            mean_white = mean(white), 
            mean_black = mean(black),
            mean_hispanic = mean(hispanic)) 
summ_stats2


# age comparison of drinkers vs non drinkers 

viz <- mlda %>% 
  mutate(drinks_alcohol_string = if_else(drinks_alcohol == 1, "Drinks", "Does Not Drink")) %>%
  ggplot(aes(x=drinks_alcohol_string, y=age_years)) +
  geom_boxplot()
viz

# lpm of years in age on likelihood of drinking alcohol
lpm1 <- lm(formula = drinks_alcohol ~ age_years, data = mlda )
summary(lpm1)

## adding age cubed and age squared variables and running new models 
mlda2 <- mlda %>% 
  mutate(age_sq = age_years^2, age_cu = age_years^3) 

lpm2 <- lm(formula = drinks_alcohol ~ age_years + age_sq + age_cu, data = mlda2)
lpm3 <- lm(formula = drinks_alcohol ~ age_years + age_sq + age_cu + student + male, data=mlda2)

summary(lpm2)
summary(lpm3)

# RDD plot 
library(rdrobust)
rdplot(mlda$drinks_alcohol,mlda$age_years,c=21)






