install.packages(c("dplyr", "ggplot2", "readxl", "GGally"))
library(dplyr)
library(readxl)
library(ggplot2) 
library(GGally)

#See if daily total sleep time is correlated with opioid use based on MEMS
complete_data<-read_excel("H:/APE/complete_data.xlsx")

#check missing values
sum(is.na(complete_data$TotalSleepTime))
sum(is.na(complete_data$NbOpenings))
#remove rows with missing values in these columns
complete_data <- complete_data %>%
filter(!is.na(TotalSleepTime) & !is.na(NbOpenings))

#correlation coefficient
correlation <- cor(complete_data$TotalSleepTime, complete_data$NbOpenings)
print(paste("Correlation between TotalSleepTime and NbOpenings:", correlation))
#visualization
ggplot(complete_data, aes(x = TotalSleepTime, y = NbOpenings)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Correlation between TotalSleepTime and NbOpenings",
       x = "Total Sleep Time",
       y = "Number of Openings") +
  theme_minimal()
#statistical test
cor_test <- cor.test(complete_data$TotalSleepTime, complete_data$NbOpenings)
print(cor_test)

#see if daily sleep efficiency scores is correlated with opioid use based on MEMS
#check missing values
sum(is.na(complete_data$Efficiency))
sum(is.na(complete_data$NbOpenings))
#remove observations with missing Efficiency/NbOpenings
complete_data<-complete_data %>%
  filter(!is.na(Efficiency) & !is.na(NbOpenings))
#correlation coefficient
correlation2<-cor(complete_data$Efficiency, complete_data$NbOpenings)
print(paste("Correlation between Sleep Efficiency and NbOpenings:", correlation2))
#statistical test of correlation coefficient
cor_test2<-cor.test(complete_data$Efficiency, complete_data$NbOpenings)
print(cor_test2)

#see if daily pre or post surgery sleep efficiency scores is correlated with opioid use based on MEMS
#missing values
sum(is.na(complete_data$PreSurgical_Efficiency))
sum(is.na(complete_data$PostSurgical_Efficiency))
#remove obs with missing values
complete_data<-complete_data %>%
  filter(!is.na(PreSurgical_Efficiency) & !is.na(PostSurgical_Efficiency))
#correlation coeff of pre surgical
correlation3<-cor(complete_data$PreSurgical_Efficiency, complete_data$NbOpenings)
print(paste("Correlation between Pre Surgical Sleep Efficiency and NbOpenings:", correlation3))
#statistical test of corr coeff of pre surgical
cor_test3<-cor.test(complete_data$PreSurgical_Efficiency, complete_data$NbOpenings)
print(cor_test3)
#correlation coeff of post surgical
correlation4<-cor(complete_data$PostSurgical_Efficiency, complete_data$NbOpenings)
print(paste("Correlation between Post Surgical Sleep Efficiency and NbOpenings:", correlation4))
#statistical test of corr coeff of post surgical
cor_test4<-cor.test(complete_data$PostSurgical_Efficiency, complete_data$NbOpenings)
print(cor_test4)
  
#partial correlation? what variables to control for?

#rolling correlation of daily total sleep time and opioid use based on MEMS
install.packages("zoo")
library(zoo)
#remove NA
sum(is.na(complete_data$TotalSleepTime))
sum(is.na(complete_data$NbOpenings))

complete_data<-complete_data %>%
filter(!is.na(TotalSleepTime) & !is.na(NbOpenings))

# Inspect the structure of the data frame
str(complete_data)
rollingcorr<- na.omit(complete_data[, c(25, 9)])

#rollapply(rollingcorr, width=30, function(x) cor(x[,1],x[,2]), by.column=FALSE)
#warnings()


#post op days, post op sleep + any opening, opened or not (post_op) look at sleep quality(post op efficiency) with NbOpenings, total MEMS with post op sleep efficiency
#ppl with opening data and watch data

#subset of just post op (TimePeriod of 1)
library(dplyr)
post_op<-complete_data %>%
  filter (TimePeriod==1) 
#subset of post op with NbOpenings greater than 0
post_op_opening<-post_op %>%
  filter (NbOpenings >= 1)

#rolling correlation of NbOpenings and sleep score for post op
library(dplyr)
library(zoo)
library(ggplot2)

# Define the safe_cor function
safe_cor <- function(x, y) {
  if (sd(x) == 0 || sd(y) == 0) {
    return(NA)
  } else {
    return(cor(x, y, use = "complete.obs"))
  }
}

# Define a function to calculate rolling correlation for a single person
calculate_rolling_corr <- function(data) {
  data <- data %>% arrange(Date)  # Ensure data is sorted by time or relevant order
  data$RollingCorrelation <- rollapply(data[, c("TotalSleepTime", "NbOpenings")], width = 30, 
                                       FUN = function(window) safe_cor(window[, "TotalSleepTime"], window[, "NbOpenings"]),
                                       by.column = FALSE, fill = NA)
  return(data)
}

# Split post-op-opening data by StandardID
grouped_post_op_opening_data <- post_op_opening %>% group_by(StandardID) %>% group_split()

# Apply rolling correlation calculation to each group
grouped_post_op_opening_data <- lapply(grouped_post_op_opening_data, calculate_rolling_corr)

# Combine the grouped data back into a single data frame
post_op_opening_with_corr <- bind_rows(grouped_post_op_opening_data)

# Extract correlation coefficients
correlation_coefficients <- post_op_opening_with_corr %>%
  select(StandardID, Date, RollingCorrelation) %>%
  filter(!is.na(RollingCorrelation))

# Print the correlation coefficients
print(correlation_coefficients)

# Plot the rolling correlation for each person in the post-surgery period with NbOpenings >= 1
ggplot(post_op_opening_with_corr, aes(x = Date, y = RollingCorrelation, color = factor(StandardID))) +
  geom_line() +
  labs(title = "Rolling Correlation between TotalSleepTime and NbOpenings (Post-Surgery, NbOpenings >= 1) by Person",
       x = "Date",
       y = "Rolling Correlation") +
  theme_minimal() +
  theme(plot.title = element_text(size = 9))  # Adjust the font size of the title




#look at MEMS and sleep efficiency/time
library(readxl)
average_scores<-read_excel("H:/APE/average_scores.xlsx")
MEMS_data<-read_excel("H://APE/MEMS_data.xlsx")
#merge MEMS totoal with average scores
correlation_data<-average_scores %>% 
  left_join(MEMS_data, by="StandardID")

#post surgery total sleep time and total MEMS
correlation_data <- correlation_data %>%
  filter(!is.na(PostSurgical_TotalSleepTime) & !is.na(TotalMEMS))

#statistical test
cor_test5 <- cor.test(correlation_data$PostSurgical_TotalSleepTime, correlation_data$TotalMEMS)
print(cor_test)

#post surgery sleep efficiency and total MEMS
correlation_data <- correlation_data %>%
  filter(!is.na(PostSurgical_Efficiency))
#statistical test
cor_test6 <- cor.test(correlation_data$PostSurgical_Efficiency, correlation_data$TotalMEMS)
print(cor_test)


#rolling correlation for post-op NbOpenings of any value (0 included) and post-op sleep time
library(dplyr)
library(zoo)
library(ggplot2)

# Define the safe_cor function
safe_cor <- function(x, y) {
  if (sd(x) == 0 || sd(y) == 0) {
    return(NA)
  } else {
    return(cor(x, y, use = "complete.obs"))
  }
}

# Define a function to calculate rolling correlation for a single person
calculate_rolling_corr2 <- function(data) {
  data <- data %>% arrange(Date)  # Ensure data is sorted by time or relevant order
  data$RollingCorrelation <- rollapply(data[, c("TotalSleepTime", "NbOpenings")], width = 30, 
                                       FUN = function(window) safe_cor(window[, "TotalSleepTime"], window[, "NbOpenings"]),
                                       by.column = FALSE, fill = NA)
  return(data)
}

# Split post-op-opening data by StandardID
grouped_post_op_data <- post_op_opening %>% group_by(StandardID) %>% group_split()

# Apply rolling correlation calculation to each group
grouped_post_op_data <- lapply(grouped_post_op_data, calculate_rolling_corr)

# Combine the grouped data back into a single data frame
post_op_with_corr <- bind_rows(grouped_post_op_data)

# Extract correlation coefficients
correlation_coefficients2 <- post_op_with_corr %>%
  select(StandardID, Date, RollingCorrelation) %>%
  filter(!is.na(RollingCorrelation))

# Print the correlation coefficients
print(correlation_coefficients2)

# Plot the rolling correlation for each person in the post-surgery period with NbOpenings >= 1
ggplot(post_op_with_corr, aes(x = Date, y = RollingCorrelation, color = factor(StandardID))) +
  geom_line() +
  labs(title = "Rolling Correlation between TotalSleepTime and NbOpenings (Post-Surgery, NbOpenings include 0) by Person",
       x = "Date",
       y = "Rolling Correlation") +
  theme_minimal() +
  theme(plot.title = element_text(size = 9))  # Adjust the font size of the title


#use dplyr to see how many observations per person-->that as width for rolling corr
