####################################
#### Applied Regression script #####
####################################

## install.packages
library(haven)
library(dplyr)
library(ggplot2)
library(estimatr)
##Setting working directory and load the ESS data
setwd("C:/Users/joura/Desktop/Marburg Uni/R and Inequality")
dtall <- read_dta("ESS11e04_2.dta")

## Variables
## gndr = gender; 
## agea = age; 
## edlvdsk = Highest level of Education;
## cntry = Country;
## eisced - Highest level of Education;
## gincdif - Government should reduce differences in income levels;

## Creating a dataframe with our country "Slovakia"
dt <- dtall |> 
  filter(cntry == "SK") |> 
  select(gndr, agea, edlvdsk, eisced, gincdif)
unique(dt$gndr)

## Let's create more meaningful column. 
##Redi we just need to reverse scale, so we extract it from 6 (number of mean.value + 1)

dt <- dt |> mutate(age = agea,
                   gender = if_else(gndr == 1, "Male", "Female"),
                   redi = 6 - gincdif,
                   uni = case_when(
                     eisced == 0 ~ NA,
                     eisced == 55 ~ NA,
                     eisced <= 5 ~ 0,
                     eisced > 5 ~ 1))
##Checking column
table(dt$gender, dt$gndr)
table(dt$uni, dt$eisced)
table(dt$redi, dt$gincdif)

prop.table(table(dt$gender))

## Support for redi by group BY gender and age 
dt |> 
  group_by(gender) |> 
  summarise(mean_redi = mean(redi, na.rm = TRUE)) |> 
  ggplot(aes(x=gender, y=mean_redi)) + geom_col()

redi_age <- dt |> 
  group_by(age) |> 
  summarise(mean_redi = mean(redi, na.rm = TRUE))

redi_age |> ggplot(aes(x=age, y=mean_redi)) + geom_point()

lm_robust(redi ~ age, data=dt)

ggplot(data=redi_age, aes(x=age, y=mean_redi)) +geom_point() + geom_smooth(method='lm', se = FALSE)

## average age and average support for redistribution in Slovakia
cat("Average support for redi in Slovakia: ", mean(dt$redi, na.rm = TRUE))
cat("Average age in Slovakia (in dataset): ", mean(dt$age, na.rm = TRUE))
    