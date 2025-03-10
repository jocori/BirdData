# Load necessary library
library(dplyr)
setwd("~/Desktop/KU/Projects/BirdData")

# Define the classification function
classify_sex <- function(value) {
  # Save the original value for count extraction
  original_value <- value
  
  # Standardize text: uppercase and trim whitespace
  value <- toupper(trimws(value))
  
  # Return "blank" if the value is empty
  if (value == "") {
    return(c("blank", NA))
  }
  # Define translation table vectors for male and female across languages REVISE JO
  male_words <- c("MALE", "MACHO", "MASCLE", "HOMME", "HOMBRE", 
                  "MASCULINO", "MASCULIN", "MASCULÍ", "MASCULÍN", "MASCULÍNE", "MASCHIO","HANE","MÂLE","TESTES","M")
  female_words <- c("FEMALE", "FEMELLE", "HEMBRA", "FEMME", 
                    "MUJER", "FÉMININE", "FEMENINO", "FEMININ", "FEMELLA", "DONNA","FÊEMA",
                    "FÊMA","FEMEA","FÊMEA","FÍMEA","F")
  unknown_words<-c("INDETERMINADOS","INDETERMINATE","O? Filhote","NON DÉT","SEXE INDÉTERMINÉ","INDEFINIDO",
                   "DESCONEGUT","UNKNOWN","UNDETERMINED","UNSEXED","UNKNOW","SEX?")
  atypical_words<-c("INTERSEX","GYNANDROMORPH","HERMAPHRODITE","M=F","MACHO Y HEMBRA","MIXED")
  indeterminate_words<-c("UNCLEAR", "UNSURE", "POSSIBLE", "QUESTIONABLE", "AMBIGUOUS")
  combined_words<-c(male_words, female_words)
  
  #create regex patterns for male, female, unknown, atypical, indeterminate
  male_pattern <- paste0("\\b(", paste(male_words, collapse="|"), ")\\b(?!\\?)|\\bM\\b")
  female_pattern <- paste0("\\b(", paste(female_words, collapse="|"), ")\\b(?!\\?)|\\bF\\b")
  unknown_pattern <- paste0("\\b(", paste(unknown_words, collapse="|"), ")\\b(?!\\?)|\\bU\\b")
  atypical_pattern <- paste0("\\b(", paste(atypical_words, collapse="|"), ")\\b(?!\\?)|\\bA\\b")
  indeterminate_pattern <- paste0("\\b(", paste(indeterminate_words, collapse="|"), ")\\b|\\?")
  mixed_pattern <- paste0("\\b\\d+\\s*(", paste(combined_words, collapse="|"), ")\\b")
  
  # Look for matches of male and female patterns in the value
  male_match <- gregexpr(male_pattern, value, perl=TRUE)
  female_match <- gregexpr(female_pattern, value, perl=TRUE)
  unknown_match <-gregexpr(unknown_pattern,value,perl=TRUE)
  atypical_match <-gregexpr(atypical_pattern,value,perl=TRUE)
  indeterminate_match <-gregexpr(indeterminate_pattern,value,perl=TRUE)
  mixed_match <- gregexpr(mixed_pattern,value,perl=TRUE)
  
  male_found <- regmatches(value, male_match)[[1]]
  female_found <- regmatches(value, female_match)[[1]]
  unknown_found<-regmatches(value,unknown_match)[[1]]
  atypical_found<-regmatches(value,atypical_match)[[1]]
  indeterminate_found<-regmatches(value,indeterminate_match)[[1]]
  mixed_found <-regmatches(value,mixed_match)[[1]]
  
  #if then statements for mixed,male,female,atypical,unknown, indeterminate; unspecified for everything not captured by all other patterns
  if (length(male_found) > 0) & (length(female_found) > 0) | 
      (length(male_found) >1) | (length(female_found) >1)) & (length(mixed_found)>0){
    sex_category <- "MIXED"
  } else if 
    (length(male_found) == 1){
    sex_category <= "MALE"
    } else if (length(female_found) == 1){
    sex_category == "FEMALE"
    } else if (length(atypical_found) == 1){
    sex_category <- "ATYPICAL"
    } else if (length(unknown_found) == 1){
    sex_catergory <- "UNKNOWN"
  } else 
  
  
#MIXED WILL BE RECORDS THAT HAVE MULTIPLE TERMS IN SEX COLUMN
  
  #INDETERMINATE RECORDS CONTAIN A QUESTION MARK
  #UNSPECIFIED WILL CAPTURE WORDS THAT ARE NOT INCLUDED IN DEFINED TERMS (SO EVERYTHING ELSE)
  
  