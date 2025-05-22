#Post-op opioid use and sleep scores (totalsleeptime, efficiency)
install.packages(c("dplyr", "readxl"))
library(dplyr)
library(readxl)
#library(GGally)
complete_data<-read_excel("H:/APE/complete_data.xlsx")
#NAs and make post-op subset
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
#remove observations with PatientCode = 001x (wrong entry of CPSP#001?...have very diff dates than 001 normal)
post_op <- post_op %>%
  filter(PatientCode != "001x")
# Ensure Date is in the proper DateTime format
install.packages("lubridate")
library(lubridate)
post_op$Date <- lubridate::ymd_hms(post_op$Date)
# Order by Date within each StandardID
post_op <- post_op %>%
  arrange(StandardID, Date)
library(dplyr)
library(lubridate)


# Ensure Date is in the proper DateTime format
post_op$Date <- ymd_hms(post_op$Date)

# Create a new variable for days since surgery
library(dplyr)
post_op <- post_op %>%
  group_by(StandardID) %>%
  mutate(FirstDayPostSurgery = min(Date),
         DaysSinceSurgery = as.numeric(difftime(Date, FirstDayPostSurgery, units = "days"))) %>%
  ungroup()

# Plot Lowess plot with days since surgery as x and sleep score as y: total sleep time
library(ggplot2)
ggplot(post_op, aes(x = DaysSinceSurgery, y = PostSurgical_TotalSleepTime)) +  # replace SleepScore with the actual column name for sleep score
  geom_smooth(method = "loess", span = 0.3, color = "blue", se = FALSE) +
  labs(title = "Lowess Plot of Total Sleep Time over Days Since Surgery",
       x = "Days Since Surgery",
       y = "Post Surgical Total Sleep Time") +
  theme_minimal()

#lowess plot of post-op sleep efficiency and days since surgery
library(ggplot2)
ggplot(post_op, aes(x = DaysSinceSurgery, y = PostSurgical_Efficiency)) +  
  geom_smooth(method = "loess", span = 0.3, color = "blue", se = FALSE) +
  labs(title = "Lowess Plot of Sleep Efficiency over Days Since Surgery",
       x = "Days Since Surgery",
       y = "Post Surgical Sleep Efficiency") +
  theme_minimal()

#TotalMEMS and days since surgery
library(ggplot2)
ggplot(post_op, aes(x = DaysSinceSurgery, y = TotalMEMS)) +  
  geom_smooth(method = "loess", span = 0.3, color = "blue", se = FALSE) +
  labs(title = "Lowess Plot of Total MEMS over Days Since Surgery",
       x = "Days Since Surgery",
       y = "Post Surgical Total MEMS") +
  theme_minimal()

summary(post_op$TotalMEMS)



#bar graph of TotalMEMS pre and post surgery (count)
library(dplyr)
library(ggplot2)

#Summarize TotalMEMS for each participant and TimePeriod
summary_data <- complete_data %>%
  group_by(StandardID, TimePeriod) %>%
  summarise(MEMS= sum(NbOpenings)) %>%
  ungroup()
# Check the structure of summary_data to ensure it's what you expect
str(summary_data)

# Plotting the bar graph
ggplot(summary_data, aes(x = TimePeriod, y = MEMS, fill = TimePeriod)) +
  geom_bar(stat = "identity") +
  labs(title = "TotalMEMS Counts Pre- and Post-Surgery", 
       x = "Time Period",
       y = "TotalMEMS Count") 
      fill = "Surgery Period" +
  scale_fill_manual(values = c("Pre-Surgery" = "red", "Post-Surgery" = "blue")) +
  theme_minimal()
      

      
#count of those who ever opened MEMS bottle (MEMS>=1) bargraph
#Create  new variable indicating "Never opened" or "Opened"
      library(dplyr)
      post_op <- post_op %>%
        mutate(OpenStatus = ifelse(TotalMEMS >= 1, "Opened", "Never opened"))
      
#Summarize the data to get the counts for each category
      open_status_counts <- post_op %>%
        group_by(OpenStatus) %>%
        summarise(Count = n_distinct(StandardID))  # Count unique participants
      
#Plot the bar graph
      ggplot(open_status_counts, aes(x = OpenStatus, y = Count, fill = OpenStatus)) +
        geom_bar(stat = "identity") +
        labs(title = "Counts of Participants Opening MEMS Bottle Post-Surgery",
             x = "Open Status",
             y = "Count of Participants",
             fill = "Open Status") +
        scale_fill_manual(values = c("Never opened" = "blue", "Opened" = "green")) +
        theme_minimal()
  
      
      
          
#proportion of those who ever opened MEMS bottle (MEMS>=1) bargraph
post_op <- post_op %>%
        mutate(OpenStatus = ifelse(TotalMEMS >= 1, "Opened", "Never opened"))
      
#Calculate the total number of unique participants
      total_participants <- post_op %>%
        summarise(Total = n_distinct(StandardID)) %>%
        pull(Total)
      
#Summarize the data to get the counts and proportions for each category
      open_status_counts <- post_op %>%
        group_by(OpenStatus) %>%
        summarise(Count = n_distinct(StandardID)) %>%
        mutate(Proportion = Count / total_participants)
      
#Plot the bar graph using proportions
      ggplot(open_status_counts, aes(x = OpenStatus, y = Proportion, fill = OpenStatus)) +
        geom_bar(stat = "identity") +
        labs(title = "Proportion of Participants Opening MEMS Bottle Post-Surgery",
             x = "Open Status",
             y = "Proportion of Participants",
             fill = "Open Status") +
        scale_fill_manual(values = c("Never opened" = "blue", "Opened" = "green")) +
        theme_minimal() +
        scale_y_continuous(labels = scales::percent)

      
install.packages("readxl")
library(readxl)
average_scores<-read_excel("H:/APE/average_scores.xlsx")
summary(average_scores$PostSurgical_Efficiency) 
summary(average_scores$PostSurgical_TotalSleepTime)




#14 days post_op subset
library(dplyr)
post_op_14<-post_op %>%
  filter(DaysSinceSurgery <= 14)
# Create a binary indicator for whether the bottle was opened on a given day
#make new variable of #openings to be 0 if not opened 1 if opened PER DAY
post_op_14 <- post_op_14 %>%
  mutate(OpenedBinary = ifelse(NbOpenings > 0, 1, 0))

# Summarise to get the percentage of days the bottle was opened
open_percentage <- post_op_14 %>%
  group_by(StandardID, DaysSinceSurgery) %>%
  summarise(OpenedBinary = max(OpenedBinary)) %>%  # Max to ensure 1 if opened at least once
  group_by(StandardID) %>%
  summarise(DaysOpened = sum(OpenedBinary),
            TotalDays = n_distinct(DaysSinceSurgery),
            OpenPercentage = (DaysOpened / TotalDays) * 100) %>%
  ungroup()

# Classify participants into two groups based on the 50% threshold
open_percentage <- open_percentage %>%
  mutate(OpenGroup = ifelse(OpenPercentage > 50, ">50%", "<=50%"))

# Merge the open_percentage data with the original post_op data to get sleep time and quality
merged_data <- post_op %>%
  inner_join(open_percentage, by = "StandardID")

# Summarise sleep time and quality for the two groups
sleep_comparison <- merged_data %>%
  group_by(OpenGroup) %>%
  summarise(AvgSleepTime = mean(PostSurgical_TotalSleepTime, na.rm = TRUE),
            AvgSleepEfficiency = mean(PostSurgical_Efficiency, na.rm = TRUE)) 

install.packages("ggplot2")
library(ggplot2)

# Plot the distribution of open percentages
ggplot(open_percentage, aes(x = OpenPercentage)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of % Days Opened in First Two Weeks Post-Surgery",
       x = "% Days Opened",
       y = "Count") +
  theme_minimal()

# Plot the comparison of average sleep time and quality between the two groups
ggplot(sleep_comparison, aes(x = OpenGroup, y = AvgSleepTime, fill = OpenGroup)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Post-op Sleep Time by Open Group",
       x = "Open Group",
       y = "Average Sleep Time") +
  theme_minimal()

ggplot(sleep_comparison, aes(x = OpenGroup, y = AvgSleepEfficiency, fill = OpenGroup)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Sleep Efficiency by Open Group",
       x = "Open Group",
       y = "Average Sleep Efficiency") +
  theme_minimal()



#explore difference in the slope of sleep time or efficiency between the two groups if you do a model like:
#AvgSleepTime ~ OpenGroup + DaysSince Surgery
install.packages("lme4")
library(dplyr)
library(lme4)

merged_data <- merged_data %>%
  mutate(DaysSinceSurgery = scale(DaysSinceSurgery))
#simplify model
sleep_time_model_simple <- lmer(PostSurgical_TotalSleepTime ~ OpenGroup + DaysSinceSurgery + (1 | StandardID), data = merged_data)
summary(sleep_time_model_simple)


# Fit the linear mixed-effects model for AvgSleepTime
sleep_time_model <- lmer(PostSurgical_TotalSleepTime ~ OpenGroup * DaysSinceSurgery + (1 | StandardID), data = merged_data)

# Fit the linear mixed-effects model for AvgSleepEfficiency
sleep_efficiency_model <- lmer(PostSurgical_Efficiency ~ OpenGroup * DaysSinceSurgery + (1 | StandardID), data = merged_data)

# Summarize the models
summary(sleep_time_model)
summary(sleep_efficiency_model)

# Install and load the writexl package
install.packages("writexl")
library(writexl)

# Save the dataset as an Excel file
install.packages("writexl")
library(writexl)
write_xlsx(merged_data, "H:/APE/merged_data.xlsx")
write_xlsx(open_percentage, "H:/APE/open_percentage.xlsx")
write_xlsx(open_status_counts, "H:/APE/open_status_counts.xlsx")
write_xlsx(post_op, "H:/APE/post_op.xlsx")
write_xlsx(post_op_14, "H:/APE/post_op_14.xlsx")
write_xlsx(sleep_comparison, "H:/APE/sleep_comparison.xlsx")

#calculate avg days since surgery? and use that variable in model? no

install.packages("openxlsx")
library(openxlsx)

merged_data<-read_excel("H:/APE/merged_data.xlsx")
open_percentage<-read_excel("H:/APE/open_percentage.xlsx")
open_status_counts<-read_excel("H:/APE/open_status_counts.xlsx")
post_op<-read_excel("H:/APE/post_op.xlsx")
psot_op_14<-read_excel("H:/APE/post_op_14.xlsx")
sleep_comparison<-read_excel("H:/APE/sleep_comparison.xlsx")

#on monday:make sure merged_data variable DaysSinceSurgery isnt negative

