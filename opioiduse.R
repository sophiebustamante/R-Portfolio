install.packages(c("dplyr", "ggplot2", "readxl", "GGally"))
library(dplyr)
library(readxl)
library(ggplot2) 
library(GGally)

#See if POST OP daily total sleep time and efficiency is correlated with opioid use based on MEMS
complete_data<-read_excel("H:/APE/complete_data.xlsx")

#check missing values
sum(is.na(complete_data$PostSurgical_TotalSleepTime))
sum(is.na(complete_data$NbOpenings))
sum(is.na(complete_data$PostSurgical_Efficiency))
sum(is.na(complete_data$TotalMEMS))
#remove rows with missing values in these columns
complete_data <- complete_data %>%
  filter(!is.na(PostSurgical_TotalSleepTime) & !is.na(NbOpenings) & !is.na(PostSurgical_Efficiency))

#subset of just post op (TimePeriod of 1), 0 openings included
library(dplyr)
post_op<-complete_data %>%
  filter (TimePeriod==1) 
#subset of post op with NbOpenings greater than 0 (ppl who opened)
post_op_opening<-post_op %>%
  filter (NbOpenings >= 1)
#use dplyr to see how many observations per person-->that as width for rolling corr
library(dplyr)
observation_counts <- complete_data %>%
  group_by(StandardID) %>%
  summarise(observation_count = n())
print(observation_counts)
summary(observation_counts)

#max number of observations for 1 participant is 21,632-->width for rolling corr
install.packages("zoo")
library(zoo)



#post-op totalsleeptime and any Nbopenings
#define the safe_cor function
safe_cor <- function(x, y) {
  if (sd(x) == 0 || sd(y) == 0) {
    return(NA)
  } else {
    return(cor(x, y, use = "complete.obs"))
  }
}

# Define a function to calculate rolling correlation for a single person: post-op sleep time and any Nbopenings (post_op)
calculate_rolling_corr <- function(data) {
  data <- post_op %>% arrange(Date)  # Ensure data is sorted by time or relevant order
  data$RollingCorrelation <- rollapply(data[, c("PostSurgical_TotalSleepTime", "NbOpenings")], width = 1000, 
                                       FUN = function(window) safe_cor(window[, "PostSurgical_TotalSleepTime"], window[, "NbOpenings"]),
                                       by.column = FALSE, fill = NA)
  return(data)
}

# Split post-op data by StandardID
grouped_post_op_data <- post_op %>% group_by(StandardID) %>% group_split()

# Apply rolling correlation calculation to each group
grouped_post_op_data <- lapply(grouped_post_op_data, calculate_rolling_corr)

# Combine the grouped data back into a single data frame
post_op_with_corr <- bind_rows(grouped_post_op_data)

# Extract correlation coefficients
correlation_coefficients <- post_op_with_corr %>%
  select(StandardID, Date, RollingCorrelation) %>%
  filter(!is.na(RollingCorrelation))

# Print the correlation coefficients
print(correlation_coefficients)

# Plot the rolling correlation for each person in the post-surgery period with NbOpenings >= 1
ggplot(post_op_with_corr, aes(x = Date, y = RollingCorrelation, color = factor(StandardID))) +
  geom_line() +
  labs(title = "Rolling Correlation between TotalSleepTime and NbOpenings (Post-Surgery, NbOpenings >= 0) by Person",
       x = "Date",
       y = "Rolling Correlation") +
  theme_minimal() +
  theme(plot.title = element_text(size = 9))  # Adjust the font size of the title








#post-op sleep efficiency and any NbOpenings
safe_cor <- function(x, y) {
  if (sd(x) == 0 || sd(y) == 0) {
    return(NA)
  } else {
    return(cor(x, y, use = "complete.obs"))
  }
}

# Define a function to calculate rolling correlation for a single person: post-op sleep efficiency and any Nbopenings (post_op)
calculate_rolling_corr2 <- function(data) {
  data <- post_op %>% arrange(Date)  # Ensure data is sorted by time or relevant order
  data$RollingCorrelation <- rollapply(data[, c("PostSurgical_Efficiency", "NbOpenings")], width = 21632, 
                                       FUN = function(window) safe_cor(window[, "PostSurgical_Efficiency"], window[, "NbOpenings"]),
                                       by.column = FALSE, fill = NA)
  return(data)
}

# Split post-op data by StandardID
grouped_post_op_data2 <- post_op %>% group_by(StandardID) %>% group_split()

# Apply rolling correlation calculation to each group
grouped_post_op_data2 <- lapply(grouped_post_op_data2, calculate_rolling_corr2)

# Combine the grouped data back into a single data frame
post_op_with_corr2 <- bind_rows(grouped_post_op_data2)

# Extract correlation coefficients
correlation_coefficients2 <- post_op_with_corr2 %>%
  select(StandardID, Date, RollingCorrelation) %>%
  filter(!is.na(RollingCorrelation))

# Print the correlation coefficients
print(correlation_coefficients2)

# Plot the rolling correlation for each person in the post-surgery period with NbOpenings >= 1
ggplot(post_op_with_corr2, aes(x = Date, y = RollingCorrelation, color = factor(StandardID))) +
  geom_line() +
  labs(title = "Rolling Correlation between Sleep Efficiency and any NbOpenings (Post-Surgery, NbOpenings >= 0) by Person",
       x = "Date",
       y = "Rolling Correlation") +
  theme_minimal() +
  theme(plot.title = element_text(size = 9)) 











#post-op totalsleeptime and Nbopenings >=1
#define the safe_cor function
safe_cor <- function(x, y) {
  if (sd(x) == 0 || sd(y) == 0) {
    return(NA)
  } else {
    return(cor(x, y, use = "complete.obs"))
  }
}

# Define a function to calculate rolling correlation for a single person: post-op sleep time and any Nbopenings (post_op)
calculate_rolling_corr3 <- function(data) {
  data <- post_op_opening %>% arrange(Date)  # Ensure data is sorted by time or relevant order
  data$RollingCorrelation <- rollapply(data[, c("PostSurgical_TotalSleepTime", "NbOpenings")], width = 21632, 
                                       FUN = function(window) safe_cor(window[, "PostSurgical_TotalSleepTime"], window[, "NbOpenings"]),
                                       by.column = FALSE, fill = NA)
  return(data)
}

# Split post-op data by StandardID
grouped_post_op_opening_data <- post_op_opening %>% group_by(StandardID) %>% group_split()

# Apply rolling correlation calculation to each group
grouped_post_op_opening_data <- lapply(grouped_post_op_opening_data, calculate_rolling_corr3)

# Combine the grouped data back into a single data frame
post_op_opening_with_corr <- bind_rows(grouped_post_op_opening_data)

# Extract correlation coefficients
correlation_coefficients3 <- post_op_opening_with_corr %>%
  select(StandardID, Date, RollingCorrelation) %>%
  filter(!is.na(RollingCorrelation))

# Print the correlation coefficients
print(correlation_coefficients3)

# Plot the rolling correlation for each person in the post-surgery period with NbOpenings >= 1
ggplot(post_op_opening_with_corr, aes(x = Date, y = RollingCorrelation, color = factor(StandardID))) +
  geom_line() +
  labs(title = "Rolling Correlation between TotalSleepTime and NbOpenings (Post-Surgery, NbOpenings >= 1) by Person",
       x = "Date",
       y = "Rolling Correlation") +
  theme_minimal() +
  theme(plot.title = element_text(size = 9))













#post-op sleep efficiency and Nbopenings >=1
#define the safe_cor function
safe_cor <- function(x, y) {
  if (sd(x) == 0 || sd(y) == 0) {
    return(NA)
  } else {
    return(cor(x, y, use = "complete.obs"))
  }
}

# Define a function to calculate rolling correlation for a single person: post-op sleep time and any Nbopenings (post_op)
calculate_rolling_corr4 <- function(data) {
  data <- post_op_opening %>% arrange(Date)  # Ensure data is sorted by time or relevant order
  data$RollingCorrelation <- rollapply(data[, c("PostSurgical_Efficiency", "NbOpenings")], width = 21632, 
                                       FUN = function(window) safe_cor(window[, "PostSurgical_Efficiency"], window[, "NbOpenings"]),
                                       by.column = FALSE, fill = NA)
  return(data)
}

# Split post-op data by StandardID
grouped_post_op_opening_data2 <- post_op_opening %>% group_by(StandardID) %>% group_split()

# Apply rolling correlation calculation to each group
grouped_post_op_opening_data2 <- lapply(grouped_post_op_opening_data2, calculate_rolling_corr4)

# Combine the grouped data back into a single data frame
post_op_opening_with_corr2 <- bind_rows(grouped_post_op_opening_data2)

# Extract correlation coefficients
correlation_coefficients4 <- post_op_opening_with_corr2 %>%
  select(StandardID, Date, RollingCorrelation) %>%
  filter(!is.na(RollingCorrelation))

# Print the correlation coefficients
print(correlation_coefficients4)

# Plot the rolling correlation for each person in the post-surgery period with NbOpenings >= 1
ggplot(post_op_opening_with_corr2, aes(x = Date, y = RollingCorrelation, color = factor(StandardID))) +
  geom_line() +
  labs(title = "Rolling Correlation between Sleep Efficiency and NbOpenings (Post-Surgery, NbOpenings >= 1) by Person",
       x = "Date",
       y = "Rolling Correlation") +
  theme_minimal() +
  theme(plot.title = element_text(size = 9))


