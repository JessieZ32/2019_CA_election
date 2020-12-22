#### Preamble #################################################################
# Purpose: Prepare and clean the 2019 CES data to match cleaned census data
# Data source: Canadian Election Study 
# Obtained from cesR package installed from github hodgettsp/cesR
# Author: Yijie Zhao
# Date: 1 December 2020
# Contact: yijie.zhao@mail.utoronto.ca
# License: MIT
# To save this script in folder Scripts
# To save the half-cleaned data in folder Inputs/data_in_process, in case of 
# unexpected crashes of Rstudio
# To save the 2019 CES cleaned data in folder Inputs/data_for_analysis



#### Workspace setup ##########################################################

# Install R packages - tidyverse and labelled
install.packages("tidyverse")
install.packages("labelled")

# Install package development tools - devtools
install.packages("devtools")

# Load R packages and devtools
library(tidyverse)
library(labelled)
library(devtools)

# Install cesR package
devtools::install_github("hodgettsp/cesR")

# Load cesR package
library(cesR)



#### Load data and choose variables ###########################################

# As online survey contains a much larger sample size than 2019 CES phone survey
# Call 2019 CES online survey
get_ces("ces2019_web")

# Convert variables into factors
ces2019_web <- to_factor(ces2019_web)

# There are 37822 observations of 620 variables in the original survey dataset.

# Select some variables of interest
reduced_ces2019 <- ces2019_web %>% 
  select(cps19_votechoice,
         cps19_citizenship, 
         cps19_age, 
         cps19_gender, 
         cps19_education, 
         cps19_employment, 
         cps19_income_number, 
         cps19_marital, 
         cps19_province)

# There are 37822 observations of 9 variables in the reduced survey dataset.

# Save data subset reduced_ces2019 in folder Inputs/data_in_process
write.csv(reduced_ces2019, 
          file="Inputs/data_in_process/reduced_ces2019.csv", row.names=F)

# Delete dataset ces2019_web and reduced_ces2019
rm(ces2019_web, reduced_ces2019)
# Use data file, reduced_ces2019.csv in the following section



#### Clean data ###############################################################

# Load data of reduced_ces2019.csv
ces2019_reduced <- read_csv("Inputs/data_in_process/reduced_ces2019.csv")


# I. Remove - "cannot vote"
# Remove data of observations who weren't eligible to vote in 2019 FED election
# - age under 18
# - without Canadian citizenship

# Check the summary table of variable cps19_age
summary(ces2019_reduced$cps19_age)
# Min value of age is 18.
# Nothing need to be removed with respect to age

# Check available categories of variable cps19_citizenship
ces2019_reduced %>% group_by(cps19_citizenship) %>% summarise(count=n())

# Remove observations with status of permanent resident
ces2019_reduced <- ces2019_reduced[!(
  ces2019_reduced$cps19_citizenship=="Permanent resident"),]

# There are 36480 observations of 9 variables in the reduced dataset.


# II. Remove - "made no response"
# Remove rows with content of "Don't know/ Prefer not to answer", which can be 
# treated as non-response.
ces2019_reduced <- ces2019_reduced[!(
  ces2019_reduced$cps19_votechoice=="Don't know/ Prefer not to answer"),]
ces2019_reduced <- ces2019_reduced[!(
  ces2019_reduced$cps19_education=="Don't know/ Prefer not to answer"),]
ces2019_reduced <- ces2019_reduced[!(
  ces2019_reduced$cps19_employment=="Don't know/ Prefer not to answer"),]
ces2019_reduced <- ces2019_reduced[!(
  ces2019_reduced$cps19_marital=="Don't know/ Prefer not to answer"),]

# There are 31136 observations of 9 variables in the reduced dataset.


# III. Remove - "missing values"
# Drop rows with NAs
ces2019_reduced <- na.omit(ces2019_reduced)

# There are 18953 observations of 9 variables in the reduced dataset.

# Save dataset ces2019_reduced in folder Inputs/data_in_process
write.csv(ces2019_reduced, 
          file="Inputs/data_in_process/ces2019_reduced.csv", row.names=F)

# Delete dataset ces2019_reduced
rm(ces2019_reduced)
# Use data file, ces2019_reduced.csv in the following section



#### Adjust variables #########################################################

# In order to use MRP approach, I need to match the homogeneous variables of 
# 2019 CES data and 2016 Canadian census data first. I will adjust these 
# variables, to make each pair of homogeneous variables exactly the same.

# Load data of ces2019_reduced.csv
cleaned_ces2019 <- read_csv("Inputs/data_in_process/ces2019_reduced.csv")


# I. cps19_votechoice
# Use cps19_votechoice to create a binary variable - vote_Liberal
cleaned_ces2019 <- cleaned_ces2019 %>% 
  mutate(vote_Liberal=ifelse(cps19_votechoice=="Liberal Party",1,0))
# Use vote_Liberal as response variable


# II. cps19_age
# Use cps19_age to create a new categorical variable - age_group
cleaned_ces2019 <- cleaned_ces2019 %>%
  mutate(age_group=case_when(cps19_age>=18 & cps19_age<=29 ~ "18~29 years old",
                             cps19_age>=30 & cps19_age<=39 ~ "30~39 years old",
                             cps19_age>=40 & cps19_age<=49 ~ "40~49 years old",
                             cps19_age>=50 & cps19_age<=59 ~ "50~59 years old",
                             cps19_age>=60 & cps19_age<=69 ~ "60~69 years old",
                             cps19_age>=70 ~ "70 years old and over"))


# III. cps19_gender
# As the variable, sex, of 2016 census data only has two categories, female and
# male, I need to delete the observations with other genders.
cleaned_ces2019 <- cleaned_ces2019[!(
  cleaned_ces2019$cps19_gender==
    "Other (e.g. Trans, non-binary, two-spirit, gender-queer)"),]

# Create new variable, gender, with two categories "Female" and "Male"
# instead of variable cps19_gender with two categories "A woman" and "A man"
cleaned_ces2019 <- cleaned_ces2019 %>%
  mutate(gender=case_when(cps19_gender=="A woman" ~ "Female",
                          cps19_gender=="A man" ~ "Male"))


# IV. cps19_education
# Create new variable named education, to represent the highest certificate,
# diploma or degree that respondents have completed
# Combine some categories of cps19_education to match 2016 census data
cleaned_ces2019 <- cleaned_ces2019 %>%
  mutate(education=case_when(
    cps19_education=="No schooling" | 
      cps19_education=="Some elementary school" |
      cps19_education=="Completed elementary school" | 
      cps19_education=="Some secondary/ high school" ~ "No high school diploma",
    cps19_education=="Completed secondary/ high school" ~ "High school diploma",
    cps19_education==
      "Some technical, community college, CEGEP, College Classique" |
      cps19_education=="Some university" ~ "Some university or college",
    cps19_education==
      "Completed technical, community college, CEGEP, College Classique" ~ 
      "College diploma or equivalency certificate",
    cps19_education=="Bachelor's degree" ~ "Bachelor's degree",
    cps19_education=="Master's degree" | 
      cps19_education=="Professional degree or doctorate" ~ "Graduate degree"))


# V. cps19_employment
# Remove observations with other employment status, which contains so many 
# kinds of responses and each kind has a few observations.
cleaned_ces2019 <- cleaned_ces2019[!(
  cleaned_ces2019$cps19_employment=="Other (please specify)"),]

# Create new variable named working_status, to represent status of working
# i.e., whether the respondents are working or not
# Combine some categories of cps19_employment to match 2016 census data
cleaned_ces2019 <- cleaned_ces2019 %>%
  mutate(working_status=case_when(
    cps19_employment=="Caring for a family" | 
      cps19_employment=="Disabled" |
      cps19_employment=="Retired" |
      cps19_employment=="Student" |
      cps19_employment=="Unemployed/ looking for work" ~ "Not working",
    cps19_employment=="Caring for family and working for pay" |
      cps19_employment=="Retired and working for pay" |
      cps19_employment=="Self employed (with or without employees)" |
      cps19_employment=="Student and working for pay" |
      cps19_employment=="Working for pay full-time" |
      cps19_employment=="Working for pay part-time" ~ "Working"))


# VI. cps19_income_number
# Create new variable named income, to represent the households' annual income
cleaned_ces2019 <- cleaned_ces2019 %>%
  mutate(income=case_when(
    cps19_income_number >= 0 & cps19_income_number < 45000 ~ 
      "less than $45,000",
    cps19_income_number >= 45000 & cps19_income_number < 90000 ~ 
      "$45,000 to $89,999",
    cps19_income_number >= 90000 & cps19_income_number < 140000 ~ 
      "$90,000 to $139,999",
    cps19_income_number >= 140000 & cps19_income_number < 200000 ~ 
      "$140,000 to $199,999",
    cps19_income_number >= 200000 ~ "$200,000 and more"))


# VII. cps19_marital
# Create new variable named marital to represent the respondents' marital status
cleaned_ces2019 <- cleaned_ces2019 %>%
  mutate(marital=case_when(cps19_marital=="Divorced" |
                             cps19_marital=="Never Married" |
                             cps19_marital=="Separated" |
                             cps19_marital=="Widowed" ~ "Single",
                           cps19_marital=="Living with a partner" |
                             cps19_marital=="Married" ~ "Married"))


# VIII. cps19_province
# Create new variable named province 
# Combine some territory
cleaned_ces2019 <- cleaned_ces2019 %>%
  mutate(province=case_when(
    cps19_province=="Newfoundland and Labrador" ~ "Newfoundland and Labrador",
    cps19_province=="Prince Edward Island" ~ "Prince Edward Island",
    cps19_province=="Nova Scotia" ~ "Nova Scotia",
    cps19_province=="New Brunswick" ~ "New Brunswick",
    cps19_province=="Quebec" ~ "Quebec",
    cps19_province=="Ontario" ~ "Ontario",
    cps19_province=="Manitoba" ~ "Manitoba",
    cps19_province=="Saskatchewan" ~ "Saskatchewan",
    cps19_province=="Alberta" ~ "Alberta",
    cps19_province=="British Columbia" ~ "British Columbia",
    cps19_province=="Northwest Territories" |
      cps19_province=="Nunavut" |
      cps19_province=="Yukon" ~ "Northern Canada"))

# There are 18645 observations of 17 variables in the cleaned dataset.

# Save dataset cleaned_ces2019 in folder Inputs/data_in_process
write.csv(cleaned_ces2019, 
          file="Inputs/data_in_process/cleaned_ces2019.csv", row.names=F)

# Remove dataset cleaned_ces2019
rm(cleaned_ces2019)



#### variable deletion ########################################################

# Load data of cleaned_ces2019.csv
ces2019_clean <- read_csv("Inputs/data_in_process/cleaned_ces2019.csv")

# Select variables for analysis
ces2019_clean <- ces2019_clean %>% 
  select(vote_Liberal,
         age_group, 
         gender, 
         education, 
         working_status, 
         income, 
         marital, 
         province)

# There are 18645 observations of 8 variables in the cleaned dataset.

# Save dataset ces2019_clean in folder Inputs/data_for_analysis
write.csv(ces2019_clean, 
          file="Inputs/data_for_analysis/ces2019_clean.csv", row.names=F)

# Remove dataset ces2019_clean
rm(ces2019_clean)




