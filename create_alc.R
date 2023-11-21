#Agatha Priandini
#21 Nov 2023

##This script is used for the data wrangling section for week 3's assignment, the data is taken from http://www.archive.ics.uci.edu/dataset/320/student+performance

###load in libraries
library(tidyverse)
library(dplyr)

###Read in student-mat.csv and student-por.csv into the script and explore the dimensions of the data

mat <- read.table("data/student-mat.csv", header = T, sep = ";")
dim(mat)
str(mat)

por <- read.table("data/student-por.csv", header = T, sep = ";")
dim(por)
str(por)


###Join the mat and por data

free_cols <-c("failures", "paid","absences","G1", "G2", "G3")
join_cols <- setdiff(colnames(por), free_cols)
mat_por <- inner_join(mat, por, by = join_cols, suffix = c(".math", ".por"))
dim(mat_por)
str(mat_por)

###Get rid of duplicate data

alc <- select(mat_por, all_of(join_cols))

for(col_name in free_cols) {
  two_cols <- select(mat_por, starts_with(col_name))
  first_col <- select(two_cols, 1)[[1]]
  #if teh virst column vector is numeric
  if(is.numeric(first_col)) {
    alc[col_name] <- round(rowMeans(two_cols))
  } else {
    alc[col_name] <- first_col
  }
}

dim(alc)
str(alc)

###Take the average of the answers related to weekday and weekend alcohol consumption to create a new column 'alc_use' to the joined data. Then use 'alc_use' to create a new logical column 'high_use' which is TRUE for students for which 'alc_use' is greater than 2 (and FALSE otherwise)

alc <- alc %>%
  mutate(alc_use = (Dalc + Walc) / 2,
         high_use = alc_use > 2)

###Glimpse at the joined and modified data to make sure everything is in order and save the joined data

glimpse(alc)
write.csv(alc, "./data/alc.csv")

###File test
read_csv("./data/alc.csv")

