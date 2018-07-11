#################################
#Name: Michelle R. Jackson
#Original Date: July 11, 2018
#R version: 3.5.0 "Joy in Playing"
#Purpose: Code explains how to clean and organize data with the 'tidyr'/'tivdyverse' packages.
#Note: According to Hadley Wickham (author of 'tidyr' and 'tidyverse' packages) datasets are outlined as such:
# 1) Each variable must have its own column.
# 2) Each observation must have its own row.
# 3) Each value must have its own cell.
#Source:http://garrettgman.github.io/tidying/
#Source:http://r4ds.had.co.nz/tidy-data.html
#################################
#Download required packages
if (!require("tidyr")) install.packages("tidyr")
if (!require("devtools")) install.packages("devtools")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
#################################
#Analyses
library(tidyr) #load 'tidyr'package to access functions to help tidy data
library(devtools) #load 'devtools' package to download data and packages from online developers
devtools::install_github("garrettgman/DSR") #download DSR dataset from Garrett Grolemund's Github
library(DSR) #load DSR dataset that you just downloaded
library(tidyverse) #load 'tidyverse' package for access to additional packages to help tidy data
library(dplyr)
table1 #show table 1
table2 #show table 2
table3 #show table 3
table4 #show table 4
table4a #show table 4a
table4b #show table 4b
table5 #show table 5
# Only table 1 is currently tidy!
# Example of working with tidy data 
#Compute rate per 10,000
table1 %>% 
  mutate(rate = cases / population * 10000)
# Compute cases per year
table1 %>% 
  count(year, wt = cases)
# Visualise changes over time
ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))
#Issues one might encounter with untidy data include: one variable might be spread across multiple columns and one observation might be scattered across multiple rows 
#Let's fix the column names in table 4a (e.g., 1999 & 2000) using the 'gather' function
table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")
# key = the name of the variable whose values for the column names
# value = the name of the variable whose values spread over the cells 
table4b %>% 
  gather(`1999`, `2000`, key = "year", value = "population") #code to tidy table 4b
left_join(tidy4a, tidy4b) #combines table 4a & 4b into 1 tibble
#spread - a function used when an observation is scattered across multiple rows (e.g., table 2)
table2 %>%
  spread(key = type, value = count)
#Other issues include one column containing two varables (e.g., table 3) - this can be fixed with the 'seperate' function
table3 %>% 
  separate(rate, into = c("cases", "population"))
#you can use a specific character to seperate your column (e.g., "/" below )
table3 %>% 
  separate(rate, into = c("cases", "population"), sep = "/")
#you can improve the default behavior of 'seperate" using "convert=TRUE" 
table3 %>% 
  separate(rate, into = c("cases", "population"), convert = TRUE)
#you can pass the vector of integers to "sep" 
table3 %>% 
  separate(year, into = c("century", "year"), sep = 2)
#how to use the 'unite' function - it combines multiple columns into a single column - let's use it on table 5
table5
table5 %>% 
  unite(new, century, year)
table5 %>% 
  unite(new, century, year, sep = "")
#how to fix missing data
stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)
stocks %>% 
  spread(year, return) %>% 
  gather(year, return, `2015`:`2016`, na.rm = TRUE)
stocks %>% 
  complete(year, qtr)
treatment <- tribble(
  ~ person,           ~ treatment, ~response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
)
treatment %>% 
  fill(person)
#case study with all tidyr examples
tidyr::who
who1 <- who %>% 
  gather(new_sp_m014:newrel_f65, key = "key", value = "cases", na.rm = TRUE)
who1
