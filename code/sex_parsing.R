setwd("~/Desktop/KU/Projects/BirdData")
#load packages
library(dplyr)
#read in data
sex<- read.csv("data/NAOC_sex_mappings_JLC_NL25.csv")
sex<-sex[,-c(12:28)]
head(sex)

unique(sex$uppercase.value)


# Define function to classify sex values
classify_sex <- function(value) {
  # Standardize text: uppercase and trim whitespace
  value <- toupper(trimws(value))
  
  # If the value is empty, return "blank" and no notes
  if (value == "") {
    return(c("blank", NA))
  }
  
  # Define regex patterns for classification
  male_pattern <- "\\bMALE\\b|\\bMACHO\\b|\\bMASCLE\\b|\\bM\\b"
  female_pattern <- "\\bFEMALE\\b|\\bFEMELLE\\b|\\bHEMBRA\\b|\\bF\\b"
  unknown_pattern <- "\\bUNKNOWN\\b|\\bUNDETERMINED\\b"
  atypical_pattern <- "\\bHERMAPHRODITE\\b|\\bGYNANDROMORPH\\b"
  not_specified_pattern <- "\\bNO DATA\\b|\\bNOT RECORDED\\b|\\bUNSEXED\\b"
  
  # If there is any question mark in the value, mark as INDETERMINATE
  if (grepl("\\?", value)) {
    count_pattern <- "(?i)\\b\\d+\\s*(?:MALE|FEMALE|MACHO|MASCLE|FEMELLE|HEMBRA)S?\\b"
    count_matches <- unlist(regmatches(value, gregexpr(count_pattern, value, perl=TRUE)))
    notes <- if (length(count_matches) > 0) paste(count_matches, collapse="; ") else NA
    return(c("INDETERMINATE", notes))
  }
  
  # Check for not specified keywords
  if (grepl(not_specified_pattern, value)) {
    return(c("NOT SPECIFIED", NA))
  }
  
  # Check for atypical (e.g., hermaphrodites)
  if (grepl(atypical_pattern, value)) {
    return(c("ATYPICAL", NA))
  }
  
  # Check for unknown terms
  if (grepl(unknown_pattern, value)) {
    return(c("UNKNOWN", NA))
  }
  
  # Extract any numeric count information for notes
  count_pattern <- "(?i)\\b\\d+\\s*(?:MALE|FEMALE|MACHO|MASCLE|FEMELLE|HEMBRA)S?\\b"
  count_matches <- unlist(regmatches(value, gregexpr(count_pattern, value, perl=TRUE)))
  notes <- if (length(count_matches) > 0) paste(count_matches, collapse="; ") else NA
  
  # Identify presence of male and/or female terms
  has_male <- grepl(male_pattern, value)
  has_female <- grepl(female_pattern, value)
  
  if (has_male & has_female) {
    return(c("MALE | FEMALE", notes))
  } else if (has_male) {
    return(c("MALE", notes))
  } else if (has_female) {
    return(c("FEMALE", notes))
  } else {
    return(c("blank", NA))
  }
}


##add to notes anything that is not male or female but is extra information
#add cases of indeterminate that are not denoted by ?
#? cases (if accompanied by m or f), map to sex but add that it was in question in notes
#figure out how to handle cases where the language used has a completely different word for a sex, like catalon.
#create translation table, need male and female words for all languages
unique(sex$in.language)
# Apply the classification function rowwise
data <- sex %>% 
  rowwise() %>% 
  mutate(
    result = list(classify_sex(uppercase.value)),
    JLC.working = result[[1]],
    notes = result[[2]]
  ) %>% 
  ungroup() %>% 
  select(-result)  # Remove the temporary list column

# Save the processed data
write.csv(data, "processed_sex_data.csv", row.names = FALSE)


############################################################## START HERE

#### need to make sure mixed cases are included, as they currently are not. 
#Make sure female and male in all languages is represented.
#Make sure atypical is represented; double check concepts and ensure all are represented

### see translations Nikki DM'ed me on discord


######## chat gpt's new attempt
# Load necessary library
library(dplyr)

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
  male_words <- c("MALE", "MACHO", "MASCLE", "HOMME", "HOMBRE", "MASCULINO", "MASCULIN", "MASCULÍ", "MASCULÍN", "MASCULÍNE", "MASCHIO")
  female_words <- c("FEMALE", "FEMELLE", "HEMBRA", "FEMME", "MUJER", "FÉMININE", "FEMENINO", "FEMININ", "FEMELLA", "DONNA", "FEMINA")
  
  # Define additional keywords for indeterminate cases (besides a trailing ?)
  indeterminate_words <- c("UNCLEAR", "UNSURE", "POSSIBLE", "QUESTIONABLE", "INDETERMINATE", "AMBIGUOUS")
  
  # Create regex patterns for male and female words.
  # The pattern captures an optional trailing "?" to flag uncertainty.
  male_pattern <- paste0("\\b(", paste(male_words, collapse="|"), ")(\\?)?\\b")
  female_pattern <- paste0("\\b(", paste(female_words, collapse="|"), ")(\\?)?\\b")
  
  # Look for matches of male and female patterns in the value
  male_match <- gregexpr(male_pattern, value, perl=TRUE)
  female_match <- gregexpr(female_pattern, value, perl=TRUE)
  male_found <- regmatches(value, male_match)[[1]]
  female_found <- regmatches(value, female_match)[[1]]
  
  # Check if any male or female token was flagged with a "?"
  male_in_question <- any(grepl("\\?$", male_found))
  female_in_question <- any(grepl("\\?$", female_found))
  
  # Remove the trailing "?" for classification purposes
  male_found_clean <- gsub("\\?", "", male_found)
  female_found_clean <- gsub("\\?", "", female_found)
  
  # Check for indeterminate keywords anywhere in the string (ignoring recognized tokens)
  is_indeterminate_word <- any(sapply(indeterminate_words, function(w) grepl(paste0("\\b", w, "\\b"), value)))
  
  # Determine the sex classification based on the presence of male and/or female words
  if (length(male_found_clean) > 0 & length(female_found_clean) > 0) {
    sex_category <- "MALE | FEMALE"
  } else if (length(male_found_clean) > 0) {
    sex_category <- "MALE"
  } else if (length(female_found_clean) > 0) {
    sex_category <- "FEMALE"
  } else if (is_indeterminate_word) {
    sex_category <- "INDETERMINATE"
  } else {
    sex_category <- "blank"
  }
  
  # --- Build the notes column ---
  notes_parts <- c()
  
  # 1. Extract numeric counts with associated sex terms from the original value.
  count_pattern <- "(?i)\\b\\d+\\s*(?:MALE|FEMALE|MACHO|MASCLE|FEMELLE|HEMBRA)S?\\b"
  count_matches <- unlist(regmatches(original_value, gregexpr(count_pattern, original_value, perl=TRUE)))
  if (length(count_matches) > 0) {
    notes_parts <- c(notes_parts, paste(count_matches, collapse="; "))
  }
  
  # 2. Add a note if any recognized male/female term had a trailing "?"
  if (male_in_question || female_in_question) {
    notes_parts <- c(notes_parts, "Sex designation was in question")
  }
  
  # 3. Remove recognized tokens (male/female words and counts) from the standardized value to capture extra info.
  extra_text <- value
  extra_text <- gsub(male_pattern, "", extra_text, perl=TRUE)
  extra_text <- gsub(female_pattern, "", extra_text, perl=TRUE)
  extra_text <- gsub(count_pattern, "", extra_text, ignore.case=TRUE, perl=TRUE)
  extra_text <- gsub("[[:punct:]]", " ", extra_text)  # remove punctuation
  extra_text <- trimws(extra_text)
  if (nchar(extra_text) > 0) {
    notes_parts <- c(notes_parts, extra_text)
  }
  
  final_notes <- if (length(notes_parts) > 0) paste(notes_parts, collapse="; ") else NA
  
  return(c(sex_category, final_notes))
}

# Load your dataset (adjust the file path as needed)
data <- read.csv("NAOC_sex_mappings_JLC_NL25.csv", stringsAsFactors = FALSE)

# Apply the classification function rowwise and create the new columns:
data <- data %>% 
  rowwise() %>% 
  mutate(
    result = list(classify_sex(uppercase.value)),
    JLC.working = result[[1]],
    notes = result[[2]]
  ) %>% 
  ungroup() %>% 
  select(-result)  # Remove the temporary list column

# Save the processed data to a CSV file
write.csv(data, "processed_sex_data.csv", row.names = FALSE)
