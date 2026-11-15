####################################
#### Applied Regression script #####
####################################

## install.packages
library(haven)
library(dplyr)
library(ggplot2)
library(estimatr)
library(modelsummary)
library(rstudioapi)

##Setting working directory and load the ESS data
setwd("C:/Users/joura/Desktop/Marburg Uni/R and Inequality")
dtall <- read_dta("ESS11e04_2.dta")
ipssdf <- read_dta("ZA7600_v3-0-0.dta")

## Variables ESS
## gndr = gender; 
## agea = age; 
## edlvdsk = Highest level of Education;
## cntry = Country;
## eisced - Highest level of Education;
## gincdif - Government should reduce differences in income levels;
## hinctnta - 

## Creating a dataframe with our country "Slovakia"
dt <- dtall |> 
  filter(cntry == "SK") |> 
  select(gndr, agea, edlvdsk, eisced, gincdif, hinctnta)
unique(dt$hinctnta)

## Let's create more meaningful column. 
##Redi we just need to reverse scale, so we extract it from 6 (number of mean.value + 1)

dt <- dt |> mutate(age = agea,
                   gender = if_else(gndr == 1, "Male", "Female"),
                   redi = 6 - gincdif,
                   uni = case_when(
                     eisced == 0 ~ NA,
                     eisced == 55 ~ NA,
                     eisced <= 5 ~ 0,
                     eisced > 5 ~ 1),
                   income = hinctnta)
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

##Variables IPSS 
## Country. In our case there is no Slovakia, so instead we've chosen Czech Republic (203)
## V22. Q4b Questions about redi. It it responsible of government to reduce the differences in incomes
## v29. How would you describes taxes in the country
## CZ_ETHN1 - Nationalities of Czech Rep. to catch Slovaks

unique(ipssdf$country)
unique(ipssdf$v29)

Czipss <- ipssdf |> 
  filter(country == 203) |> 
  select(v22, v29, CZ_ETHN1)

unique(Czipss$v29)

Czipss <- Czipss |> mutate(
                   redi = case_when(
                     v22 <= 0 ~ NA,
                     v22 == v22 ~ 6 - v22),
                   taxes = case_when(
                     v29 <= 0 ~ NA,
                     v29 == v29 ~ 6 - v29
                   ))

Czipss <- Czipss |> mutate(top_taxes = if_else(v29 <= 0, NA, v29),
                           redi2 = if_else(v22 <= 0, NA, 6 - v22))
                   
                   
table(Czipss$top_taxes, Czipss$v29)
table(Czipss$redi2, Czipss$v22)

mean(Czipss$redi, na.rm = TRUE)
mean(Czipss$top_taxes, na.rm = TRUE)

## Now let's select all Slovak in Czech republic and check average value for them
CZSlovak <- Czipss |> 
  filter(CZ_ETHN1 == 4) |> 
  select(redi, taxes, top_taxes)

mean(CZSlovak$redi, na.rm = TRUE)
mean(CZSlovak$top_taxes, na.rm = TRUE)

## Using a categorical variable __ gender
lm_robust(redi ~ gender, data = dt)
dt |> group_by(gender) |> summarise(mean_redi = mean(redi, na.rm=T))

rgg <- lm_robust(redi ~ gender, data = dt)
modelsummary(rgg, stars = T, gof_map = "nobs")

rig<-lm_robust(redi~income,data=dt)
modelsummary(rig,stars=T, gof_map="nobs")

rag<-lm_robust(redi~age,data=dt)
modelsummary(rag,stars=T, gof_map="nobs")

redi_income <- dt |> 
  group_by(income) |> 
  summarise(mean_redi = mean(redi, na.rm = TRUE))

redi_income |> ggplot(aes(x=income, y=mean_redi)) + geom_point()

lm_robust(redi ~ income, data=dt)

ggplot(data=redi_income, aes(x=income, y=mean_redi)) +geom_point() + geom_smooth(method='lm', se = FALSE)
