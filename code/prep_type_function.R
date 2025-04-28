#########
# Title: Preparation Type Mapping Function
# Author: Joanna Corimanya, Nikki Lemus, and A. Townsend Peterson
# Date: 28-Apr-2025
#########

#set working directory
setwd("~/Desktop/KU/Projects/BirdData")

#read in data
prep<- read.csv("data/NAOC_preparations.csv", sep = ",")

#load packages
library(dplyr)
library(stringr) #for splitting terms contained in single column

# examine data
head(prep)

# remove unnecessary columns
prep<-prep[,-c(7:21)]

# Split values using comma, pipe, or semicolon
split_terms <- str_split(prep$uppercase.value, pattern = "\\s*[,|;]\\s*")

# Flatten and remove any leading/trailing whitespace
all_terms <- unique(unlist(split_terms))
all_terms <- trimws(all_terms)
all_terms <- sort(unique(all_terms))

#function to map constant preservation type terms in new column

#function to map constant body parts terms in new column