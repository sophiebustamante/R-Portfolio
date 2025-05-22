install.packages(c("dplyr", "stringr", "readxl"))
library(dplyr)
library(readxl)
library(stringr) 

merged_data<-read_excel("H:/APE/merged_CPSP.xlsx")

#new variable: total number of MEMS doses per participant
MEMS_data<-merged_data %>%
  group_by(StandardID) %>%
  summarise(TotalMEMS=sum(NbOpenings))
new_data<-merged_data %>% 
  left_join(MEMS_data, by="StandardID")

install.packages("openxlsx")
library(openxlsx)
write.xlsx(MEMS_data, file ="H:/APE/MEMS_data.xlsx")
write.xlsx(new_data, file ="H:/APE/new_data.xlsx")

#subset of all post-op efficiency
new_data<-read_excel("H:/APE/new_data.xlsx")
efficiency_data<-subset(new_data, TimePeriod == 1)

#order by Onset date/time
library(dplyr)
  #convert Onset to POSIXct format
efficiency_data$Onset <- as.POSIXct(efficiency_data$Onset, format = "%Y-%m-%d %H:%M:%S")
  #sort data by Onset time/date
sorted_data <- efficiency_data %>%
  arrange(Onset)

install.packages(c("purrr","tidyr"))
library(purrr)
library(tidyr)
library(dplyr)
#slope change in Efficiency
slope_data <- sorted_data %>%
  group_by(StandardID) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(Efficiency ~ Onset, data = .)),
         EfficiencySlope = map_dbl(model, ~ coef(.x)[["Onset"]])) %>%
  select(StandardID, EfficiencySlope)
#merge slope data with whole dataset
whole_data<-new_data %>% 
  left_join(slope_data, by="StandardID")

install.packages("openxlsx")
library(openxlsx)
write.xlsx(whole_data, file ="H:/APE/whole_data.xlsx")



install.packages(c("dplyr", "stringr", "readxl", "tidyr"))
library(dplyr)
library(readxl)
library(stringr) 
library(tidyr)

whole_data<-read_excel("H:/APE/whole_data.xlsx")

library(dplyr)
library(tidyr)

# Separate datasets for pre-surgical and post-surgical periods
pre_surgical <- whole_data %>%
  filter(TimePeriod == 0) %>%
  group_by(StandardID) %>%
  summarize(
    PreSurgical_Efficiency = mean(Efficiency, na.rm = TRUE),
    PreSurgical_Latency = mean(Latency, na.rm = TRUE),
    PreSurgical_TotalSleepTime = mean(TotalSleepTime, na.rm = TRUE),
    PreSurgical_WASO = mean(WASO, na.rm = TRUE),
    PreSurgical_NumberOfAwakenings = mean(NumberofAwakenings, na.rm = TRUE),
    PreSurgical_LengthOfAwakenings = mean(LengthofAwakeningsinMinutes, na.rm = TRUE),
    PreSurgical_ActivityCounts = mean(ActivityCounts, na.rm = TRUE),
    PreSurgical_MovementIndex = mean(MovementIndex, na.rm = TRUE),
    PreSurgical_FragmentationIndex = mean(FragmentationIndex, na.rm = TRUE),
    PreSurgical_SleepFragmentationIndex = mean(SleepFragmentationIndex, na.rm = TRUE),
    .groups = 'drop'
  )

post_surgical <- whole_data %>%
  filter(TimePeriod == 1) %>%
  group_by(StandardID) %>%
  summarize(
    PostSurgical_Efficiency = mean(Efficiency, na.rm = TRUE),
    PostSurgical_Latency = mean(Latency, na.rm = TRUE),
    PostSurgical_TotalSleepTime = mean(TotalSleepTime, na.rm = TRUE),
    PostSurgical_WASO = mean(WASO, na.rm = TRUE),
    PostSurgical_NumberOfAwakenings = mean(NumberofAwakenings, na.rm = TRUE),
    PostSurgical_LengthOfAwakenings = mean(LengthofAwakeningsinMinutes, na.rm = TRUE),
    PostSurgical_ActivityCounts = mean(ActivityCounts, na.rm = TRUE),
    PostSurgical_MovementIndex = mean(MovementIndex, na.rm = TRUE),
    PostSurgical_FragmentationIndex = mean(FragmentationIndex, na.rm = TRUE),
    PostSurgical_SleepFragmentationIndex = mean(SleepFragmentationIndex, na.rm = TRUE),
    .groups = 'drop'
  )

#join the two datasets on StandardID
average_scores <- pre_surgical %>%
  full_join(post_surgical, by = "StandardID")
#join average to whole dataset
complete_data <- whole_data %>%
  left_join(average_scores, by = "StandardID")

install.packages("openxlsx")
library(openxlsx)
write.xlsx(complete_data, file ="H:/APE/complete_data.xlsx")
write.xlsx(average_scores, file ="H:/APE/average_scores.xlsx")








