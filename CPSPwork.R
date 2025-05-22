# Load necessary packages
install.packages(c("dplyr", "stringr", "readxl"))
library(dplyr)
library(stringr)
library(readxl)

WholeHistoryDailyAdherence<-read_excel("H:/APE/20240528T194758_11097_Whole_History_Dailyadherence_by205958.xlsx")

# Create a function to standardize the ID formats
standardize_id <- function(id) {
# Remove any non-alphanumeric characters except underscores
  id <- str_replace_all(id, "[^A-Za-z0-9_]", "")
  
# Replace multiple underscores with a single underscore
  id <- str_replace_all(id, "_+", "_")
  
# Remove unwanted segments (_E_, _E-, _V1, etc.)
  id <- str_replace(id, "_E_|_E-|_V[0-9]*$", "")
  
# Ensure the prefix is CPSP or CPSP2 and extract the numeric part
  id <- str_replace(id, "^(CPSP2?)(_?)(\\d+)", "\\1_\\3")
  
# Extract the prefix and the numeric part separately
  parts <- str_match(id, "^(CPSP2?)(_?)(\\d+)$")
  if (!is.na(parts[3])) {
    prefix <- parts[2]
    numeric_part <- as.numeric(parts[4])
    if (!is.na(numeric_part)) {
      formatted_id <- sprintf("%s_%03d", prefix, numeric_part)
    } else {
      formatted_id <- id
    }
  } else {
    formatted_id <- id
  }
  
  return(formatted_id)
}

# Apply the function to your dataset
library(dplyr)
library(stringr)
WholeHistoryDailyAdherence <- WholeHistoryDailyAdherence %>%
  mutate(StandardID = sapply(PatientCode, standardize_id))



# Load necessary packages
install.packages(c("dplyr", "stringr", "readxl"))
library(dplyr)
library(stringr)
library(readxl)

#V1 still there, make sure CPSP in all caps in StandardID

# Create a function to standardize the ID formats
standardize_id <- function(id) {
id <- str_replace_all(id, "[^A-Za-z0-9_]", "") # Remove any non-alphanumeric characters except underscores
  
# Replace multiple underscores with a single underscore
  id <- str_replace_all(id, "_+", "_")
  
# Remove unwanted segments (_E_, _E-, _V[0-9]*, etc.)
  id <- str_replace(id, "_E_|_E-|_V[0-9]*$", "")
  
# Extract the prefix (CPSP or CPSP2) and the numeric part
 # parts <- str_match(id, "^(CPSP2?|CPSPE?)(\\d+)")
  
  if (!is.na(parts[3])) {
    prefix <- parts[2]
    numeric_part <- as.numeric(parts[3])
    if (!is.na(numeric_part)) {
      formatted_id <- sprintf("%s_%03d", prefix, numeric_part)
    } else {
      formatted_id <- id
    }
  } else {
    formatted_id <- id
  }
  
  return(formatted_id)
}

# Apply the function to your dataset
library(dplyr)
library(stringr)
WholeHistoryDailyAdherence <- WholeHistoryDailyAdherence %>%
  mutate(StandardID = sapply(PatientCode, standardize_id))
###
#CPSP_E011V1 still not right format, CPSP2 has deleted the 2 in CPSP2 which makes the count start over





# Load necessary packages
install.packages(c("dplyr", "stringr"))
library(dplyr)
library(stringr)

# Create a function to standardize the ID formats
standardize_id <- function(id) {
  # Remove any non-alphanumeric characters except underscores
  id <- str_replace_all(id, "[^A-Za-z0-9_]", "")
  
  # Replace multiple underscores with a single underscore
  id <- str_replace_all(id, "_+", "_")
  
  # Remove unwanted segments (_E_, _E-, _V[0-9]*, etc.)
  id <- str_replace(id, "_E_|_E-|_V[0-9]*$", "")
  
  #determine prefix (CSPS or CSPS2)
  prefix <- ifelse(str_detect(id, "CPSP2"), "CPSP2", "CPSP")
  
  # Extract the numeric part
  numeric_part <- str_extract(id, "\\d+")
  if (!is.na(numeric_part)) {
    formatted_id <- sprintf("%s_%03d", prefix, as.numeric(numeric_part))
  } else {
    formatted_id <- id
  }
  
  return(formatted_id)
}
  



# Sample data frame (replace this with your actual data loading code)
#df <- data.frame(PatientCode = c("CPSP_E011V1", "CPSP2_018", "CPSPE_003", "CPSP2_E045V1", "CPSP2_063_V1"))

# Apply the function to your dataset
library(dplyr)
library(stringr)
WholeHistoryDailyAdherence <- WholeHistoryDailyAdherence %>%
  mutate(StandardID = sapply(PatientCode, standardize_id))














# Install necessary packages (if not already installed)
install.packages(c("dplyr", "stringr"))

# Load necessary libraries
library(dplyr)
library(stringr)

# Create a function to standardize the ID formats
standardize_id <- function(id) {
  # Remove unwanted segments (_E_, _E-, _V[0-9]*, etc.)
  id <- str_replace_all(id, "_E_|_E-|_V[0-9]*$", "")
  
  # Extract the prefix (CPSP or CPSP2)
  prefix <- ifelse(str_detect(id, "CPSP2"), "CPSP2", "CPSP")
  
  # Extract the numeric part
  numeric_part <- str_extract(id, "\\d+")
  
  # Check if numeric part is available and format the ID
  if (!is.na(numeric_part)) {
    formatted_id <- sprintf("%s_%03d", prefix, as.numeric(numeric_part))
  } else {
    formatted_id <- id
  }
  
  return(formatted_id)
}



# Apply the function to your dataset
WholeHistoryDailyAdherence <- WholeHistoryDailyAdherence %>%
  mutate(StandardID = sapply(PatientCode, standardize_id))

install.packages("openxlsx")
library(openxlsx)

#output_file_path <- "H:/APE/CPSPwork2.xlsx"

write.xlsx(WholeHistoryDailyAdherence, "WholeHistory.xlsx")






