#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("C:/Users/Surface/Desktop/STA304 PronlemSet 3")
raw_data <- read_dta("usa_00003 (1).dta.gz")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

names(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data %>% 
  select(perwt,
         stateicp,
         sex, 
         age, 
         race,
         citizen,
         educd,
         hhincome,
         empstat,
         region,
         empstatd)
         

# Change variable "age" type
reduced_data$age <- as.numeric(reduced_data$age)

#Filtrate for the people who are eligible to vote 
filter_data <- reduced_data %>% filter(age>=18 &
                                       (citizen=="naturalized citizen"|citizen=="born abroad of american parents"))
# Adjust and ignore "NA" 
filter_data$hhincome <- ifelse(filter_data$hhincome==9999999,NaN,filter_data$hhincome)
filter_data %>% na.omit()

# Remove redundant dataset
rm(raw_data,reduced_data)


# Mapping census data to survey data

## Mapping and creating a new age group column: 
filter_data <- filter_data %>% 
  mutate(agegroup=case_when(age<20 ~ "less than 20",
                            age>=20 & age<30 ~ "20~30",
                            age>=30 & age<40 ~ "30~40",
                            age>=40 & age<50 ~ "40~50",
                            age>=50 & age<60 ~ "50~60",
                            age>=60 & age<70 ~ "60~70",
                            age>=70 & age<80 ~ "70~80",
                            age>= 80 ~ "80 or above 80"))

## Mapping and creating a new education level column:
filter_data <- filter_data %>%  
                  mutate(education_level=case_when(educd == "no schooling completed"~ "Low education level",
                                                   educd == "nursery school, preschool" ~ "Low education level",
                                                   educd == "kindergarten" ~ "Low education level",
                                                   educd == "grade 1" ~ "Low education level",
                                                   educd == "grade 2" ~ "Low education level",
                                                   educd == "grade 3" ~ "Low education level",
                                                   educd == "grade 4" ~ "Low education level",
                                                   educd == "grade 1, 2, 3, or 4" ~ "Low education level",
                                                   educd == "grade 5, 6, 7, or 8" ~ "Low education level",
                                                   educd == "grade 5" ~ "Low education level",
                                                   educd == "grade 6" ~ "Low education level",
                                                   educd == "grade 7" ~ "Low education level",
                                                   educd == "grade 8" ~ "Low education level",
                                                   educd == "grade 7 or 8" ~ "Low education level",
                                                   educd == "grade 9" ~ "Middle education level",
                                                   educd == "grade 10" ~ "Middle education level",
                                                   educd == "grade 11" ~ "Middle education level",
                                                   educd == "grade 12" ~ "Middle education level",
                                                   educd == "12th grade, no diploma" ~ "Middle education level",
                                                   educd == "high school graduate or ged" ~ "Middle education level",
                                                   educd == "regular high school diploma" ~ "Middle education level",
                                                   educd == "ged or alternative credential" ~ "Middle education level",
                                                   educd == "some college, but less than one year" ~ "Middle education level",
                                                   educd == "1 year of college" ~ "Middle education level",
                                                   educd == "2 years of college" ~ "High education level",
                                                   educd == "associate's degree, type not specified" ~ "High education level",
                                                   educd == "associate's degree, occupational program" ~ "High education level",
                                                   educd == "associate's degree, academic program" ~ "High education level",
                                                   educd == "3 years of college" ~ "High education level",
                                                   educd == "4 years of college" ~ "High education level",
                                                   educd == "bachelor's degree" ~ "High education level",
                                                   educd == "5+ years of college" ~ "High education level",
                                                   educd == "6 years of college (6+ in 1960-1970)" ~ "High education level",
                                                   educd == "7 years of college" ~  "High education level",
                                                   educd == "8+ years of college" ~ "High education level",
                                                   educd == "master's degree" ~ "Very high education level",
                                                   educd == "professional degree beyond a bachelor's degree" ~ "Very high education level",
                                                   educd == "doctoral degree" ~ "Very high education level"))

## Change the state variable name "stateip" to "state_full_name":                                                
filter_data <- rename(filter_data, state_full_name=stateicp)


## Mapping and creating a new household income column:
filter_data <- filter_data %>%  
                  mutate(household_income=case_when(hhincome<14999 ~ "Less than $14,999",
                                                    hhincome>=14999 & hhincome<=19999 ~ "$15,000 to $19,999",
                                                    hhincome>19999 & hhincome<=24999  ~ "$20,000 to $24,999",
                                                    hhincome>24999 & hhincome<=29999 ~ "$25,000 to $29,999",
                                                    hhincome>29999 & hhincome<=34999 ~ "$30,000 to $34,999",
                                                    hhincome>34999 & hhincome<=39999 ~ "$35,000 to $39,999",
                                                    hhincome>39999 & hhincome<=44999 ~ "$40,000 to $44,999",
                                                    hhincome>44999 & hhincome<=49999 ~ "$45,000 to $49,999",
                                                    hhincome>49999 & hhincome<=54999 ~ "$50,000 to $54,999",
                                                    hhincome>54999 & hhincome<=59999 ~ "$55,000 to $59,999",
                                                    hhincome>59999 & hhincome<=64999 ~ "$60,000 to $64,999",
                                                    hhincome>64999 & hhincome<=69999 ~ "$65,000 to $69,999",
                                                    hhincome>69999 & hhincome<=74999 ~ "$70,000 to $74,999",
                                                    hhincome>74999 & hhincome<=79999 ~ "$75,000 to $79,999",
                                                    hhincome>79999 & hhincome<=84999 ~ "$80,000 to $84,999",
                                                    hhincome>84999 & hhincome<=89999 ~ "$85,000 to $89,999",
                                                    hhincome>89999 & hhincome<=94999 ~ "$90,000 to $94,999",
                                                    hhincome>94999 & hhincome<=99999 ~ "$95,000 to $99,999",
                                                    hhincome>99999 & hhincome<=124999 ~ "$100,000 to $124,999",
                                                    hhincome>124999 & hhincome<=149999 ~ "$125,000 to $149,999",
                                                    hhincome>149999 & hhincome<=174999 ~ "$150,000 to $174,999",
                                                    hhincome>174999 & hhincome<=199999 ~ "$175,000 to $199,999",
                                                    hhincome>199999 & hhincome<= 249999 ~ "$200,000 to $249,999",
                                                    hhincome>249999 ~ "$250,000 and above"))



## Mapping and creating a new race group column:
filter_data <- filter_data %>%  
                  mutate(race_group=case_when(race == "other race, nec" ~ "Other race",
                                              race == "two major races" ~ "Other race",
                                              race == "three or more major races" ~ "Other race",
                                              race == "chinese" ~ "Chinese",
                                              race == "japanese" ~ "Japanese",
                                              race == "american indian or alaska native" ~ "American Indian or Alaska Native",
                                              race == "other asian or pacific islander" ~ "Other Asian or Pacific Islander",
                                              race == "white" ~ "White",
                                              race == "black/african american/negro" ~ "Black"))




## Mapping and creating a new employment status column:
filter_data <- filter_data %>%  
  mutate(employment_status=case_when(empstatd == "at work" ~ "Employed",
                                     empstatd == "at work, public emerg" ~ "Employed",
                                     empstatd == "has job, not working" ~ "Employed",
                                     empstatd == "unemployed" ~ "Unemployed",
                                     empstatd == "unemp, exper worker" ~ "Unemployed",
                                     empstatd == "unemp, new worker" ~ "Unemployed",
                                     empstatd == "nilf, school" ~ "Students",
                                     empstatd == "nilf, housework" ~ "Homemaker",
                                     empstatd == "armed forces" ~ "Others",
                                     empstatd == "armed forces--at work" ~ "Others",
                                     empstatd == "armed forces--not at work but with job" ~ "Others",
                                     empstatd == "not in labor force" ~ "Others",
                                     empstatd == "nilf, unable to work" ~ "Others",
                                     empstatd == "nilf, other" ~ "Others"))




                                                    
# Transforming the cleaned dataset to a csv file:
write_csv(filter_data, "census_data_1.csv")

