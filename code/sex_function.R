# Load necessary library
library(dplyr)
setwd("~/Desktop/KU/Projects/BirdData")
sex<- read.csv("data/NAOC_sex_mappings_JLC_NL25.csv")
sex<-sex[,-c(12:28)]
head(sex)
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
  if ((length(male_found) > 0 & length(female_found) > 0) | 
      (length(male_found) > 1) | 
      (length(female_found) > 1) |
      (length(mixed_found) > 0)) {
    sex_category <- "MIXED"
  } else if (length(male_found) == 1) {
    sex_category <- "MALE"
  } else if (length(female_found) == 1) {
    sex_category <- "FEMALE"
  } else if (length(atypical_found) == 1) {
    sex_category <- "ATYPICAL"
  } else if (length(unknown_found) == 1) {
    sex_category <- "UNKNOWN"
  } else if (length(indeterminate_found) == 1) {
    sex_category <- "INDETERMINATE"
  } else {
    sex_category <- "UNSPECIFIED"
  }
  
  #Add notes with counts and extra information

  notes_parts <- c()
  # 1. Extract numeric counts with associated sex terms from the original value.
  count_pattern <- "(?i)\\b\\d+\\s*(?:MALE|FEMALE|MACHO|MASCLE|FEMELLE|HEMBRA)S?\\b"
  count_matches <- unlist(regmatches(original_value, gregexpr(count_pattern, original_value, perl=TRUE)))
  
  # Add counts to notes explicitly for MIXED cases
  if (sex_category == "MIXED") {
    if (length(count_matches) > 0) {
      notes_parts <- c(notes_parts, paste(count_matches, collapse="; "))
    }
    
  } else {
    if (length(count_matches) > 0) {
      notes_parts <- c(notes_parts, paste(count_matches, collapse="; "))
    }
  }
  
  # 3. Capture extra text after removing recognized tokens
  extra_text <- value
  extra_text <- gsub(male_pattern, "", extra_text, perl=TRUE)
  extra_text <- gsub(female_pattern, "", extra_text, perl=TRUE)
  extra_text <- gsub(count_pattern, "", extra_text, ignore.case=TRUE, perl=TRUE)
  extra_text <- gsub("[[:punct:]]", " ", extra_text)  # remove punctuation
  extra_text <- trimws(extra_text)
  
  # Remove any trailing numbers in mixed cases
  if (sex_category == "MIXED") {
    extra_text <- sub("(\\s*\\d+)+\\s*$", "", extra_text, perl=TRUE)
  }
  
  if (nchar(extra_text) > 0) {
    notes_parts <- c(notes_parts, extra_text)
  }
  
  final_notes <- if (length(notes_parts) > 0) paste(notes_parts, collapse="; ") else NA
  
  
  
  return(c(sex_category, final_notes))
}
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
  