#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("C:/Users/Surface/Desktop/STA304 PronlemSet 3/ns20200625")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(registration,
         vote_intention,
         vote_2020,
         employment,
         gender,
         race_ethnicity,
         household_income,
         education,
         state,
         age,
         census_region)

# Adjusted data type
reduced_data$age %>% as.numeric()

# Filtrate for the people who are both registered and intended to vote
filter_data_s <- reduced_data %>%
               filter(registration=="Registered"&
                      vote_intention!="No, I am not eligible to vote"&
                      vote_intention!="No, I will not vote but I am eligible"&
                      vote_intention!="Not sure"&
                     (vote_2020=="Donald Trump"|vote_2020=="Joe Biden"))
                    
  
# Ignore "NA"
filter_data_s %>% na.omit()

# Remove redundant dataset
rm(raw_data, reduced_data)




# Mapping survey data to census data:

## Mapping and creating a new age group column:
filter_data_s <- filter_data_s %>% 
                   mutate(agegroup=case_when(age<20 ~ "less than 20",
                                             age>=20 & age<30 ~ "20~30",
                                             age>=30 & age<40 ~ "30~40",
                                             age>=40 & age<50 ~ "40~50",
                                             age>=50 & age<60 ~ "50~60",
                                             age>=60 & age<70 ~ "60~70",
                                             age>=70 & age<80 ~ "70~80",
                                             age>= 80 ~ "80 or above 80"))
               


## Changing gender variable all to lower case:
filter_data_s$gender <- ifelse(filter_data_s$gender=="Male","male","female")

## Mapping variable name "gender" to "sex":
filter_data_s <- rename(filter_data_s,sex=gender)


## Mapping and creating a new education level column:
filter_data_s <- filter_data_s %>% 
                  mutate(education_level=case_when(education == "3rd Grade or less"~ "Low education level",
                                                   education == "Middle School - Grades 4 - 8"~ "Low education level",
                                                   education == "Completed some high school" ~ "Middle education level",
                                                   education == "High school graduate" ~ "Middle education level",
                                                   education == "Other post high school vocational training" ~ "Middle education level",
                                                   education == "Associate Degree" ~ "High education level",
                                                   education == "College Degree (such as B.A., B.S.)" ~  "High education level",
                                                   education == "Completed some college, but no degree" ~ "High education level",
                                                   education == "Completed some graduate, but no degree" ~ "High education level",
                                                   education == "Masters degree" ~ "Very high education level",
                                                   education == "Doctorate degree" ~ "Very high education level"))

## Mapping and creating a new state full name column:
filter_data_s <- filter_data_s %>% mutate(state_full_name =case_when(state == "AL"~"alabama",
                                                                     state == "AK"~"alaska",
                                                                     state == "AZ"~"arizona",
                                                                     state == "AR"~"arkansas",
                                                                     state == "CA"~"california",
                                                                     state == "CO"~"colorado",
                                                                     state == "CT"~"connecticut",
                                                                     state == "DE"~"delaware",
                                                                     state == "FL"~"florida",
                                                                     state == "GA"~"georgia",
                                                                     state == "HI"~"hawaii",
                                                                     state == "ID"~"idaho",
                                                                     state == "IL"~"illinois",
                                                                     state == "IN"~"indiana",
                                                                     state == "IA"~"iowa",
                                                                     state == "KS"~"kansas",
                                                                     state == "KY"~"kentucky",
                                                                     state == "LA"~"louisiana",
                                                                     state == "ME"~"maine",
                                                                     state == "MD"~"maryland",
                                                                     state == "MA"~"massachusetts",
                                                                     state == "MI"~"michigan",
                                                                     state == "MN"~"minnesota",
                                                                     state == "MS"~"mississippi",
                                                                     state == "MO"~"missouri",
                                                                     state == "MT"~"montana",
                                                                     state == "NE"~"nebraska",
                                                                     state == "NV"~"nevada",
                                                                     state == "NH"~"new hampshire",
                                                                     state == "NJ"~"new jersey",
                                                                     state == "NM"~"new mexico",
                                                                     state == "NY"~"new york",
                                                                     state == "NC"~"north carolina",
                                                                     state == "ND"~"north dakota",
                                                                     state == "OH"~"ohio",
                                                                     state == "OK"~"oklahoma",
                                                                     state == "OR"~"oregon",
                                                                     state == "PA"~"pennsylvania",
                                                                     state == "RI"~"rhode island",
                                                                     state == "SC"~"south carolina",
                                                                     state == "SD"~"south dakota",
                                                                     state == "TN"~"tennessee",
                                                                     state == "TX"~"texas",
                                                                     state == "UT"~"utah",
                                                                     state == "VT"~"vermont",
                                                                     state == "VA"~"virginia",
                                                                     state == "WA"~"washington",
                                                                     state == "WV"~"west virginia",
                                                                     state == "WI"~"wisconsin",
                                                                     state == "WY"~"wyoming",
                                                                     state == "DC"~"district of columbia"))



## Mapping and creating a new race group column:
filter_data_s <- filter_data_s %>%  
                    mutate(race_group=case_when(race_ethnicity == "White" ~ "White",
                                                race_ethnicity == "Black, or African American"~"Black",
                                                race_ethnicity == "Asian (Chinese)" ~ "Chinese",
                                                race_ethnicity == "Asian (Japanese)" ~ "Japanese",
                                                race_ethnicity == "Asian (Asian Indian)" ~ "Other Asian or Pacific Islander",
                                                race_ethnicity == "Asian (Filipino)" ~ "Other Asian or Pacific Islander",
                                                race_ethnicity == "Asian (Korean)" ~ "Other Asian or Pacific Islander",
                                                race_ethnicity == "Asian (Vietnamese)" ~ "Other Asian or Pacific Islander",
                                                race_ethnicity == "Asian (Other)" ~ "Other Asian or Pacific Islander",
                                                race_ethnicity == "Pacific Islander (Native Hawaiian)" ~ "Other Asian or Pacific Islander",
                                                race_ethnicity == "Pacific Islander (Guamanian)" ~ "Other Asian or Pacific Islander",
                                                race_ethnicity == "Pacific Islander (Samoan)" ~ "Other Asian or Pacific Islander",
                                                race_ethnicity == "Pacific Islander (Other)" ~  "Other Asian or Pacific Islander",
                                                race_ethnicity == "Some other race" ~ "Other race",
                                                race_ethnicity == "American Indian or Alaska Native" ~ "American Indian or Alaska Native"))
                                                                  



## Mapping and creating a new employment status:
filter_data_s <- filter_data_s %>%  
                    mutate(employment_status=case_when(employment == "Full-time employed"~ "Employed",
                                                       employment == "Part-time employed" ~ "Employed",
                                                       employment == "Self-employed" ~ "Employed",
                                                       employment == "Unemployed or temporarily on layoff" ~ "Unemployed",
                                                       employment == "Student" ~ "Students",
                                                       employment == "Homemaker" ~ "Homemaker",
                                                       employment == "Retired" ~"Others",
                                                       employment == "Permanently disabled" ~ "Others",
                                                       employment == "Other" ~"Others"))

# Transforming the cleaned dataset to a csv file:
write_csv(filter_data_s, "filter_data_s_1.csv")
