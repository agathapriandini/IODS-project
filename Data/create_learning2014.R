###Gatri Agatha Priandini
###Assignment 2:Regression and validation
###14 November 2023

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

#The dataset has 183 rows and 7 columns. This data contains results from a survey conducted among students, which studies their attitudes towards statistics and includes their exam points. The data contains the variables 'gender', 'Age' and 'attitude'. The questions and answers conducted in this survey have also been categorized into three groups to the corresponding dimensions being deep ('deep'), strategic ('stra'), and surf ('surf'), which were calculated as the averages of the answers given to the questions following the three categories.  

###Data summarization

##show summary

summary(learning2014)

##plot data using ggpairs

p <- ggpairs(learning2014, mapping = aes(col = gender), lower = list(combo = wrap("facethist", bins = 20)))

print(p)

##interpretation of the output

#The output as a whole shows that there is generally a positive correlation between the participants' attitudes and the exam points. Looking more closely at the variables, there seems to be very low correlation between the variables themselves. Participant data also shows that there are significantly more females, and furthermore that the age is skewed towards the younger age range with most participants being younger than 30.

###Choose three variables as explanatory variables and fit a regression model where exam points is the target (dependent, outcome) variable. Show a summary of the fitted model and comment and interpret the results. Explain and interpret the statistical test related to the model parameters

##Fit a linear regression model using the explanatory variables chosen are attitude, strategic learning and surface learning due to their highest correlation to exam points. 

learninglm <- lm(Points ~ attitude + stra + surf, data = learning2014)

##Summary

summary(learninglm)

##Interpretation

#The output of the linear regression model shows that the most statistically significant variable is attitude, meaning students with more positive attitudes statistically prove to have better exam points. On the other hand, strategic and surface learning did not show any significant results in this model.

##Refit model only with the statistically significant variable

learninglm_refit <- lm(Points ~ attitude, data = learning2014)

summary(learninglm_refit)

##Interpret the refitted model

#The refitted model shows very similar results to the previous model, which shows that attitude as an explanatory variable has the highest statistical significance to exam points as indicated by the multiple R-squared value 0.1151 which indicates that 

##Residual vs Fitted values, Normal QQ-plot, Residuals vs Leverage

par(mfrow=c(2,2))
plot(learninglm_refit, which=c(1,2,5))

##The assumptions of the model and interpretation of validity based on diagnostic plots

#The linear regression assumes that there is a linear relationship between the explanatory and response variables. The model shows that the residual variables are normally distributed and have equal variance.

#The output of the Residuals vs Fitted and Residual vs Leverage plots do not show any specific pattern nor curve, but the observations are rather scattered evenly close to the zero line. On the other hand, the output of the QQ-plot supports the assumption that there is a linear regression among the variables and that there is normality as the observations fit along the line quite closely. Therefore, the prior assumption seems to be correct as we see in the plots that the explanatory variables and residuals are normally distributed and have an equal variance.
             