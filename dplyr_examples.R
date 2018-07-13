#################################
#Name: Michelle R. Jackson
#Original Date: July 11, 2018
#R version: 3.5.0 "Joy in Playing"
#Purpose: Code explains how to transform data with the 'dplyr' function
#Source:http://r4ds.had.co.nz/transform.html#
#################################
#Download required packages
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("nycflights13")) install.packages("nycflights13")
#Analyses
library(nycflights13) #load nycflights13 data set
library(tidyverse) #load tidyverse dataset
nycflights13::flights #see flights tibble
View(flights) #see flights overall
#dplyr & tibble components 
#int stands for integers
#dbl stands for doubles, or real numbers
#chr stands for character vectors, or strings
#dttm stands for date-times (a date + a time)
#lgl stands for logical, vectors that contain only TRUE or FALSE
#fctr stands for factors, which R uses to represent categorical variables with fixed possible values
#date stands for dates
#Pick observations by their values (filter())
#Reorder the rows (arrange())
#Pick variables by their names (select())
#Create new variables with functions of existing variables (mutate())
#Collapse many values down to a single summary (summarise())

filter(flights, month == 1, day == 1)
jan1 <- filter(flights, month == 1, day == 1)
(dec25 <- filter(flights, month == 12, day == 25))
filter(flights, month ==1)
sqrt(2) ^ 2 == 2
#> [1] FALSE
1 / 49 * 49 == 1
#> [1] FALSE
near(sqrt(2) ^ 2,  2)
#> [1] TRUE
near(1 / 49 * 49, 1)
#> [1] TRUE









