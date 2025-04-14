# Load necessary library
library(dplyr)
setwd("~/Desktop/KU/Projects/BirdData")
sex <- read.csv("data/NAOC_sex_mappings_JLC_NL25.csv")
sex <- sex[,-c(12:28)]

# Revised classify_sex function
classify_sex <- function(value) {
  original_value <- value
  value <- toupper(trimws(value))
  
  if (value == "") return(c("blank", NA))
  
  male_words <- c("MALE", "MACHO", "MASCLE", "HOMME", "HOMBRE", 
                  "MASCULINO", "MASCULIN", "MASCULÍ", "MASCULÍN", "MASCULÍNE",
                  "MASCHIO","HANE","MÂLE","TESTES","M")
  female_words <- c("FEMALE", "FEMELLE", "HEMBRA", "FEMME", 
                    "MUJER", "FÉMININE", "FEMENINO", "FEMININ", "FEMELLA",
                    "DONNA","FÊEMA","FÊMA","FEMEA","FÊMEA","FÍMEA","F")
  unknown_words <- c("INDETERMINADOS","INDETERMINATE","O? FILHOTE","NON DÉT",
                     "SEXE INDÉTERMINÉ","INDEFINIDO","DESCONEGUT","UNKNOWN",
                     "UNDETERMINED","UNSEXED","UNKNOW","SEX?")
  atypical_words <- c("INTERSEX","GYNANDROMORPH","HERMAPHRODITE","M=F","MACHO Y HEMBRA","MIXED")
  indeterminate_words <- c("UNCLEAR", "UNSURE", "POSSIBLE", "QUESTIONABLE", "AMBIGUOUS")
  
  combined_words <- c(male_words, female_words)
  
  male_pattern <- paste0("\\b(", paste(male_words, collapse="|"), ")\\b(?!\\?)|\\bM\\b")
  female_pattern <- paste0("\\b(", paste(female_words, collapse="|"), ")\\b(?!\\?)|\\bF\\b")
  attached_count_pattern <- "(?i)^(\\d+)(MALE|FEMALE|MACHO|MASCLE|FEMELLE|HEMBRA)S?$"
  unknown_pattern <- paste0("\\b(", paste(unknown_words, collapse="|"), ")\\b(?!\\?)|\\bU\\b")
  atypical_pattern <- paste0("\\b(", paste(atypical_words, collapse="|"), ")\\b(?!\\?)|\\bA\\b")
  indeterminate_pattern <- paste0("\\b(", paste(indeterminate_words, collapse = "|"), ")\\b|\\?|:\\s*\\?")
  mixed_pattern <- paste0("\\b\\d+\\s*(", paste(combined_words, collapse="|"), ")\\b")
  single_letter_sex_pattern <- "^\\s*([MF])(?:\\s+\\1)+\\s*$"
  
  attached_found <- regmatches(value, regexec(attached_count_pattern, value, perl=TRUE))[[1]]
  single_letter_found <- regmatches(value, regexec(single_letter_sex_pattern, value, perl=TRUE))[[1]]
  
  male_found <- length(regmatches(value, gregexpr(male_pattern, value, perl=TRUE))[[1]])
  female_found <- length(regmatches(value, gregexpr(female_pattern, value, perl=TRUE))[[1]])
  unknown_found <- length(regmatches(value, gregexpr(unknown_pattern, value, perl=TRUE))[[1]])
  atypical_found <- length(regmatches(value, gregexpr(atypical_pattern, value, perl=TRUE))[[1]])
  indeterminate_found <- length(regmatches(value, gregexpr(indeterminate_pattern, value, perl=TRUE))[[1]])
  mixed_found <- length(regmatches(value, gregexpr(mixed_pattern, value, perl=TRUE))[[1]])
  
  if (length(attached_found) > 0) {
    term_found <- toupper(attached_found[3])
    sex_category <- ifelse(term_found %in% male_words, "MALE",
                           ifelse(term_found %in% female_words, "FEMALE", "UNSPECIFIED"))
  } else if (length(single_letter_found) > 0) {
    sex_letter <- single_letter_found[2]
    sex_category <- ifelse(sex_letter == "F", "FEMALE", "MALE")
  } else if (indeterminate_found > 0) {
    sex_category <- "INDETERMINATE"
  } else if ((male_found > 0 & female_found > 0) | mixed_found > 0) {
    sex_category <- "MIXED"
  } else if (male_found >= 1 & female_found == 0) {
    sex_category <- "MALE"
  } else if (female_found >= 1 & male_found == 0) {
    sex_category <- "FEMALE"
  } else if (atypical_found >= 1) {
    sex_category <- "ATYPICAL"
  } else if (unknown_found >= 1) {
    sex_category <- "UNKNOWN"
  } else {
    sex_category <- "UNSPECIFIED"
  }
  
  final_notes <- original_value  # Always store original text
  
  return(c(sex_category, final_notes))
}

# Apply function to data
data <- sex %>% 
  rowwise() %>% 
  mutate(
    result = list(classify_sex(uppercase.value)),
    JLC.working = result[[1]],
    notes = result[[2]]
  ) %>% 
  ungroup() %>% 
  select(-result)



#MIXED WILL BE RECORDS THAT HAVE MULTIPLE TERMS IN SEX COLUMN
  
  #INDETERMINATE RECORDS CONTAIN A QUESTION MARK
  #UNSPECIFIED WILL CAPTURE WORDS THAT ARE NOT INCLUDED IN DEFINED TERMS (SO EVERYTHING ELSE)
#remove numbers at end for notes for "mixed", check terms, make sure jlc.working matches updated.mapping and maps.to.sex.concept
  ##make it female if any word starts with "F" UNLESS it is "mixed". same with male and "M" this will include languages we missed
  