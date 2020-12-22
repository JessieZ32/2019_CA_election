#### Preamble #################################################################
# Purpose: Prepare and clean the 2016 Canadian census data 
# Data source: Statistics Canada 2016 census program 
# Obtained from U of T library (cited in report)
# Author: Yijie Zhao
# Date: 1 December 2020
# Contact: yijie.zhao@mail.utoronto.ca
# License: MIT
# Prerequisites: downloaded the data from U of T library. To do that:
## 1. Go to: http://www.chass.utoronto.ca/
## 2. Data centre --> UofT users or http://dc.chass.utoronto.ca/myaccess.html
## 3. Click Canadian Census, should redirect to sign in. Sign in.
## 4. Click Public use microdata files (PUMF) - Census PUMF files.
## 5. Choose Census of Canada, 2016 - individuals and click "Data". 
## 6. Click download --> Customized Subset.
## 7. Select CSV data file, data definitions for STATA.
## 8. Select variables by clicking button. Then continue.
## 9. Create the files, download and save.
# To save the raw data in folder Inputs/data_raw 
# To save this script in folder Scripts
# To save the half-cleaned data in folder Inputs/data_in_process, in case of 
# unexpected crashes of Rstudio
# To save the cleaned census data in folder Inputs/data_for_analysis



#### Workspace setup ##########################################################

# Install R packages - tidyverse and labelled
install.packages("tidyverse")
install.packages("labelled")

# Load R packages - tidyverse and labelled
library(tidyverse)
library(labelled)



#### Load data and choose variables ###########################################

# Since 2011, Statistics Canada started to use only one mandatory short form of
# questionnaire and move most of questions of long form to a voluntary survey 
# called National Household Survey. I choose the data of "Census of Canada, 
# 2016: individual public use microdata file", the most recent census data.

# Read the raw data of 2016 Canadian census
census2016_raw <- read_csv("Inputs/data_raw/AAqA7Hod.csv")

# Convert variables into factors
census2016_raw <- to_factor(census2016_raw)

# There are 930421 observations of 10 variables in the original census dataset.

# Select some variables of interest
reduced_census2016 <- census2016_raw %>% 
  select(citizen, 
         agegrp, 
         sex, 
         hdgree, 
         lfact, 
         hhinc, 
         marsth, 
         pr)

# There are 930421 observations of 8 variables in the reduced census dataset.



#### Clean data ###############################################################

# Refer to the codebook of 2016 Canadian census data: PUMF

# I. Remove - "cannot vote"
# Remove data of observations who would be not eligible to vote in FED election
# - age under 18
# - without Canadian citizenship

# Remove observations under 18 
reduced_census2016 <- reduced_census2016[!(reduced_census2016$agegrp==1 |
                                             reduced_census2016$agegrp==2 |
                                             reduced_census2016$agegrp==3 |
                                             reduced_census2016$agegrp==4 |
                                             reduced_census2016$agegrp==5 |
                                             reduced_census2016$agegrp==6),]

# Remove observations without Canadian citizenship
reduced_census2016 <- reduced_census2016[!(reduced_census2016$citizen==3),]

# There are 685699 observations of 8 variables in the reduced dataset.


# II. Remove - "made no response"
# Remove rows with values representing something like "Not available", which 
# can be treated as non-response.
reduced_census2016 <- reduced_census2016[!(reduced_census2016$agegrp==88),]
reduced_census2016 <- reduced_census2016[!(reduced_census2016$hdgree==88 |
                                             reduced_census2016$hdgree==99),]
reduced_census2016 <- reduced_census2016[!(reduced_census2016$lfact==99),]
reduced_census2016 <- reduced_census2016[!(reduced_census2016$hhinc==88),]

# There are 668655 observations of 8 variables in the reduced dataset.

# Save dataset reduced_census2016 in folder Inputs/data_in_process
write.csv(reduced_census2016, 
          file="Inputs/data_in_process/reduced_census2016.csv", row.names=F)

# Delete dataset census2016_raw and reduced_census2016
rm(census2016_raw, reduced_census2016)
# Use data file, reduced_census2016.csv in the following section



#### Adjust variables #########################################################

# In order to use MRP approach, I need to match the homogeneous variables of 
# 2019 CES data and 2016 Canadian census data first. I will adjust these 
# variables, to make each pair of homogeneous variables exactly the same.

# Load data of reduced_census2016.csv
census2016_clean <- read_csv("Inputs/data_in_process/reduced_census2016.csv")


# I. agegrp
# Use agegrp to create a new categorical variable - age_group
census2016_clean <- census2016_clean %>%
  mutate(age_group=case_when(
    agegrp==7 | agegrp==8 | agegrp==9 ~ "18~29 years old",
    agegrp==10 | agegrp==11 ~ "30~39 years old",
    agegrp==12 | agegrp==13 ~ "40~49 years old",
    agegrp==14 | agegrp==15 ~ "50~59 years old",
    agegrp==16 | agegrp==17 ~ "60~69 years old",
    agegrp==18 | agegrp==19 | agegrp==20 | 
      agegrp==21 ~ "70 years old and over"))


# II. sex
# Use sex to create a new categorical variable - gender
# with two categories "Female" and "Male"
census2016_clean <- census2016_clean %>%
  mutate(gender=case_when(sex==1 ~ "Female",
                          sex==2 ~ "Male"))


# III. hdgree
# Create new variable named education, to represent the highest certificate,
# diploma or degree that respondents have completed
# Combine some categories of hdgree
census2016_clean <- census2016_clean %>%
  mutate(education=case_when(
    hdgree==1 ~ "No high school diploma",
    hdgree==2 ~ "High school diploma",
    hdgree==3 | hdgree==4 ~ "Some university or college",
    hdgree==5 | hdgree==6 | hdgree==7 | hdgree==8 ~ 
      "College diploma or equivalency certificate",
    hdgree==9 ~ "Bachelor's degree",
    hdgree==10 | hdgree==11 | hdgree==12 | hdgree==13 ~ "Graduate degree"))


# IV. lfact
# Create new variable named working_status, to represent status of working
# i.e., whether the respondents are working or not
# Combine some categories of lfact 
census2016_clean <- census2016_clean %>%
  mutate(working_status=case_when(
    lfact==3 | lfact==4 | lfact==5 | lfact==6 | lfact==7 | lfact==8 |
      lfact==9 | lfact==10 | lfact==11 | lfact==12 | lfact==13 | 
      lfact==14 ~ "Not working",
    lfact==1 | lfact==2 ~ "Working"))


# V. hhinc
# Create new variable named income, to represent the households' annual income
census2016_clean <- census2016_clean %>%
  mutate(income=case_when(
    hhinc==1 | hhinc==2 | hhinc==3 | hhinc==4 | hhinc==5 | hhinc==6 | 
      hhinc==7 | hhinc==8 | hhinc==9 | hhinc==10 | hhinc==11 | hhinc==12 | 
      hhinc==13 ~ "less than $45,000",
    hhinc==14 | hhinc==15 | hhinc==16 | hhinc==17 | hhinc==18 | hhinc==19 | 
      hhinc==20 | hhinc==21 | hhinc==22 ~ "$45,000 to $89,999",
    hhinc==23  | hhinc==24 | hhinc==25 | hhinc==26 | hhinc==27 | 
      hhinc==28 ~ "$90,000 to $139,999",
    hhinc==29 | hhinc==30 | hhinc==31 ~ "$140,000 to $199,999",
    hhinc==32 | hhinc==33 ~ "$200,000 and more"))


# VI. marsth
# Create new variable named marital to represent the respondents' marital status
census2016_clean <- census2016_clean %>%
  mutate(marital=case_when(marsth==1 | marsth==4 | marsth==5 | 
                             marsth==6 ~ "Single",
                           marsth==2 | marsth==3~ "Married"))


# VII. pr
# Create new variable named province instead of pr
census2016_clean <- census2016_clean %>%
  mutate(province=case_when(pr==10 ~ "Newfoundland and Labrador",
                            pr==11 ~ "Prince Edward Island",
                            pr==12 ~ "Nova Scotia",
                            pr==13 ~ "New Brunswick",
                            pr==24 ~ "Quebec",
                            pr==35 ~ "Ontario",
                            pr==46 ~ "Manitoba",
                            pr==47 ~ "Saskatchewan",
                            pr==48 ~ "Alberta",
                            pr==59 ~ "British Columbia",
                            pr==70 ~ "Northern Canada"))

# There are 668655 observations of 15 variables in the cleaned dataset.



#### variable deletion ########################################################

# Select variables for analysis
census2016_clean <- census2016_clean %>% 
  select(age_group, 
         gender, 
         education, 
         working_status, 
         income, 
         marital, 
         province)

# There are 668655 observations of 7 variables in the cleaned dataset.

# Save dataset census2016_clean in folder Inputs/data_for_analysis
write.csv(census2016_clean, 
          file="Inputs/data_for_analysis/census2016_clean.csv", row.names=F)

# Remove dataset census2016_clean
rm(census2016_clean)



