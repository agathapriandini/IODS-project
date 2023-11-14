###Gatri Agatha Priandini
###Assignment 2:Regression and validation

date()

###########################


#Data wrangling


###########################

###Load libraries

{library(tidyverse)
  library(dplyr)
  library(readr)
  library(GGally)
  library(ggplot2)}

###1. Read in data

learn14 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)

str(learn14)
dim(learn14) #The output shows there are 60 columns and 156 rows containing numerical values between 1-5

###2. Combine columns and get the mean for deep, stra and surf. Scale all combination variables to original scales. Exclude observations where exam points variable is zero.

#Create an attitude column

learn14$attitude <- learn14$Attitude / 10

#deep
{deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
  deep_columns <- select(learn14, one_of(deep_questions))
  learn14$deep <- rowMeans(deep_columns)}

#strategic
{strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")
  strategic_columns <- select(learn14, one_of(strategic_questions))
  learn14$stra <- rowMeans(strategic_columns)}

#surface
{surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
  surface_columns <- select(learn14, one_of(surface_questions))
  learn14$surf <- rowMeans(surface_columns)}

#Filter variables

learning2014 <- learn14 %>%
  select(gender, Age, attitude, deep, stra, surf, Points)

learn14 <- learn14 %>% mutate(points = Points, age = Age)
learn14 <- learn14 %>% filter(points>0)
dim(learn14)

###3. Save the analysis dataset to the 'data' folder

write_csv(learning2014, file = "data/learning2014.csv")

#Demonstrate that you can also read the data again by using read_csv()

demo <- read_csv("data/learning2014.csv")

#Use `str()` and `head()` to make sure that the structure of the data is correct

str(demo)
head(demo)

###########################


#Data analysis


###########################

###Read in data

learning2014 <- read_csv("data/learning2014.csv")

dim(learning2014)
str(learning2014)

###Data summarization

p <- ggpairs(learning2014, mapping = aes(col = gender, alpha = 0.3), lower = list(combo = wrap("facethist", bins = 20)))

print(p, progress = F)
             