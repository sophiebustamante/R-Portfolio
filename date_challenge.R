install.packages("readxl")
library(readxl)
# Read the XLSX file
date <- read_excel("H:/APE/date_challenge.xlsx", sheet = 1)
print(date)
summary(date)

# Extract year and month using substr
date$YEAR <- as.integer(substr(date$DC_YYYYMM, 1, 4))
date$MONTH <- as.integer(substr(date$DC_YYYYMM, 5, 6))

# Print the first and last few rows to verify
head(date)
tail(date)

install.packages("openxlsx")
library(openxlsx)
# Save the manipulated dataset to an Excel file
write.xlsx(date, "date.xlsx")
