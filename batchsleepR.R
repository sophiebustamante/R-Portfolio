install.packages(c("dplyr", "stringr", "readxl"))
library(dplyr)
library(stringr)
library(readxl)

BatchSleep<-read_excel("H:/APE/BatchSleepExportDetails(2024-05-24_12-05-51).xlsx")

library(dplyr)
library(stringr)

# Create a function to extract and format the numeric part
extract_standard_id <- function(filename) {
  # Extract numeric part from FileName using regular expression
  numeric_part <- str_extract(filename, "\\d+")
  
  # Format the numeric part into CPSP_### format
  if (!is.na(numeric_part)) {
    formatted_id <- sprintf("CPSP_%03d", as.numeric(numeric_part))
  } else {
    formatted_id <- NA  # If no numeric part found, return NA
  }
  
  return(formatted_id)
}

# Apply the function to create the StandardID variable in your dataset
library(dplyr)
library(stringr)
BatchSleep <- BatchSleep %>%
  mutate(StandardID = sapply(FileName, extract_standard_id))

install.packages("openxlsx")
library(openxlsx)
write.xlsx(BatchSleep, "BatchSleep.xlsx")

library(readxl)
WholeHistory <- read_excel("20240528T194758_11097_Whole_History_Dailyadherence_by205958.xlsx")
BatchSleep <- read_excel("BatchSleepExportDetails(2024-05-24_12-05-51).xlsx")
# Merge datasets based on StandardID
merged_CPSP <- merge(WholeHistory, BatchSleep, by = "StandardID", all = TRUE)

install.packages("openxlsx")
library(openxlsx)
write.xlsx(merged_CPSP, file ="H:/APE/merged_CPSP.xlsx")


