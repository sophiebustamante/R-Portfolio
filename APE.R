# Install and load packages
install.packages(c("readxl", "dplyr"))
library(readxl)
library(dplyr)
# Read the XLSX file
data <- read_excel("/Users/sophiabustamante/Downloads/merged_sudors.xlsx")

#remove people who are younger than 18: 8907 --> 8903 observations
data_clean <- data %>% filter(Age >= 18)
#remove people whose injurylocation is blank/NA: 8903--> 8896
data_clean<-data_clean %>% filter(!is.na(InjuryLocationLabel))

#changing education years of 99 to NA
value_to_change<-99
data_clean<-data_clean %>% mutate(EducationYears=ifelse(EducationYears 
                                                          == value_to_change, NA, 
                                                          EducationYears))
summary(data_clean$EducationYears)

#convert opioid_r_cod to factor
summary(data_clean$opioid_r_cod)
data_clean <- data_clean %>%
  mutate(opioid_r_cod = factor(opioid_r_cod, levels = c(0, 1), labels = c("No", "Yes")))
str(data_clean$opioid_r_cod)
# Plot opioid COD frequencies using ggplot2
install.packages("ggplot2")
library(ggplot2)
ggplot(data_clean, aes(x = opioid_r_cod)) +
  geom_bar() +
  labs(title = "Opioid as Cause of Death", x = "Opioid Related", y = "Count")
summary(data_clean$opioid_r_cod)
#opioid cod bar graph with proportions
library(ggplot2)
ggplot(data_clean, aes(x = opioid_r_cod, y = (..count..)/sum(..count..))) +
  geom_bar() +
  labs(title = "Opioid as Cause of Death", x = "Opioid Related", y = "Proportion") +
  scale_y_continuous(labels = scales::percent_format()) # This converts proportions to percentage labels


#create education categories
data_clean$EducationCategory <- with(data_clean, 
                                  ifelse(is.na(EducationYears), NA,
                                  ifelse(EducationYears <= 12, "Less Than High School",
                                  ifelse(EducationYears == 13, "Finished High School",
                                  ifelse(EducationYears <= 16, "Some College",
                                  ifelse(EducationYears == 17, "Finished College", NA))))))

str(data_clean$InjuryLocationLabel)
summary(data_clean$InjuryLocationLabel)
#remove InjuryLocationLabel NA's
data_clean<-data_clean %>% filter(!is.na(InjuryLocationLabel))
freq_table<-table(data_clean$InjuryLocationLabel, useNA="always")
print(freq_table)
percentages<-prop.table(freq_table)*100
print(percentages)

#make new variable: TimeFrame, with 2 levels pre(2018,2019) and during(2020,2021,2022)
library(dplyr)
data_clean <- data_clean %>%
  mutate(TimeFrame = case_when(
    Incident_Year %in% c(2018, 2019) ~ "Pre-pandemic",
    Incident_Year %in% c(2020, 2021, 2022) ~ "During Pandemic"))
#convert TimeFrame to a factor 0=pre, 1=during
data_clean <- data_clean %>%
  mutate(TimeFrame = factor(TimeFrame, levels = c("Pre-pandemic", "During Pandemic"), 
                            labels = c(0, 1)))

table(data_clean$TimeFrame)
str(data_clean$TimeFrame)

#make Race a mutually exclusive variable
library(dplyr)
#count number of TRUE values per row
data_clean <- data_clean %>%
  rowwise() %>%
  mutate(TrueCount = sum(c_across(starts_with("Race")) == "TRUE", na.rm = TRUE)) %>%
  ungroup()
#create new Race variable
data_clean <- data_clean %>%
  mutate(Race = case_when(
    TrueCount > 1 ~ "Multiracial",
    RaceAmericanIndian == "TRUE" ~ "American Indian",
    RaceAsian == "TRUE" ~ "Asian",
    RaceBlack == "TRUE" ~ "Black",
    RaceOther == "TRUE" ~ "Other",
    RacePacificIslander == "TRUE" ~ "Pacific Islander",
    RaceUnspecified == "TRUE" ~ "Unspecified",
    RaceWhite == "TRUE" ~ "White",
    TRUE ~ NA_character_ # This handles cases where none of the above are TRUE or all are NA
  ))
# Drop the TrueCount column bc it's no longer needed
data_clean <- data_clean %>% select(-TrueCount)
#make categories: white, black , multiple/other, NA
data_clean$RaceCategory<-with(data_clean,
                              ifelse (Race %in% c("Multiracial", "American Indian", "Asian",
                                                  "Other", "Pacific Islander"),
                                      "Multiple/Other",
                                      ifelse (Race == "White",
                                              "White",
                                      ifelse (Race== "Black",
                                              "Black",
                                             NA))))
                              

#create location category
data_clean$LocationCategory <- with(data_clean, 
                              ifelse(InjuryLocationLabel %in% c("ABANDONED HOUSE, BUILDING, OR WAREHOUSE", "BRIDGE", "CEMETERY, GRAVEYARD, OR OTHER BURIAL GROUND", 
                                                                "HIGHWAY, FREEWAY", "NATURAL AREA (E.G., FIELD, RIVER, BEACHES, WOODS)", "PARK, PLAYGROUND, PUBLIC USE AREA", 
                                                                "PARKING LOT/PUBLIC PARKING GARAGE", "PUBLIC TRANSPORTATION OR STATION (E.G., BUS, TRAIN, PLANE, AIRPORT, DEPOT, TAXI)", 
                                                                 "RAILROAD TRACKS", "SERVICE STATION", "STREET/ROAD, SIDEWALK, ALLEY"), 
                                           "Public Space", 
                              ifelse(InjuryLocationLabel %in% c("BANK, CREDIT UNION, ATM LOCATION", "BAR, NIGHTCLUB", "HOTEL/MOTEL", "INDUSTRIAL OR CONSTRUCTION AREAS (E.G., FACTORY, WAREHOUSE)", 
                                                                "LIQUOR STORE", "MOTOR VEHICLE (EXCLUDING SCHOOL BUS, 15, AND PUBLIC TRANSPORTATION, 21)", "OFFICE BUILDING", 
                                                                "OTHER COMMERCIAL ESTABLISHMENT (E.G., GROCERY STORE, RETAIL OUTLET, LAUNDROMAT)", "SPORTS OR ATHLETIC AREA",
                                                                "SYNAGOGUE, CHURCH, TEMPLE, MOSQUE, SHRINE, TABERNACLE, CATHEDRAL"),
                                           "Private Space", 
                              ifelse(InjuryLocationLabel %in% c("COLLEGE/UNIVERSITY, INCLUDING DORMITORY, FRATERNITY/SORORITY", "HOSPITAL OR MEDICAL FACILITY", "JAIL, PRISON, DETENTION FACILITY",
                                                                "SUPERVISED RESIDENTIAL FACILITY (E.G., SHELTER, HALFWAY HOUSE, GROUP HOME)"),
                                          "Residencies",
                              ifelse(InjuryLocationLabel %in% c("OTHER", "UNKNOWN"),
                                          "Other/Unknown",
                              ifelse(InjuryLocationLabel == "HOUSE, APARTMENT",
                                          "House, Apartment",
                                                  NA))))))  # Replace NA with another category if needed
#convert new locationcategory to factor
data_clean$LocationCategory <- as.factor(data_clean$LocationCategory)
summary(data_clean$LocationCategory)
#manually set reference level to Reference Group
data_clean$LocationCategory <- relevel(data_clean$LocationCategory, ref = "House, Apartment")
print(table(data_clean$LocationCategory, useNA = "always"))

#make category for states that aren't GA (GA, Other, Unknown)
data_clean <- data_clean %>%
  mutate(StateCategory = case_when(
    ResidenceStateLabel == "GEORGIA" ~ "Georgia",
    ResidenceStateLabel %in% c("UNKNOWN", "NOT APPLICABLE") | is.na(ResidenceStateLabel) ~ "Unknown",
    TRUE ~ "Other"
  ))

#create categories for BystandersPresent
table(data_clean$BystanderCategory)
data_clean<-data_clean %>%
mutate(BystanderCategory = case_when(
BystandersPresentDescription== "NO BYSTANDERS PRESENT" ~ "No",
BystandersPresentDescription %in% c("1 BYSTANDER PRESENT", "BYSTANDERS PRESENT, UNKNOWN NUMBER",
                                        "MULTIPLE BYSTANDERS PRESENT") ~ "Yes",
BystandersPresentDescription == "UNKNOWN IF BYSTANDER PRESENT" | is.na(BystandersPresentDescription) ~ "Unknown"))




#time-defined subsets for basic stats/info (will use TimeFrame variable for analysis)
data_pre <- subset(data_clean, Incident_Year %in% c(2018, 2019))
data_during <- subset(data_clean, Incident_Year %in% c(2020, 2021, 2022))




#basic statistics of numeric variables: **********pre-pandemic (2018, 2019)
mean(data_pre$Age, na.rm=TRUE)  
median(data_pre$Age, na.rm=TRUE)  
sd(data_pre$Age, na.rm=TRUE)   

mean(data_pre$EducationYears, na.rm=TRUE)  
median(data_pre$EducationYears, na.rm=TRUE) 
sd(data_pre$EducationYears, na.rm=TRUE)
freq_table <- table(data_pre$EducationCategory, useNA="always")
print(freq_table)    
percentages <- prop.table(freq_table) * 100
print(percentages)

#basic statistics of categorical variables: pre
freq_table <- table(data_pre$opioid_r_cod)
print(freq_table)    
percentages <- prop.table(freq_table) * 100
print(percentages)

freq_table <- table(data_pre$SexLabel)
print(freq_table)  
percentages <- prop.table(freq_table) * 100
print(percentages)

freq_table <- table(data_pre$StateCategory)
print(freq_table)  
percentages <- prop.table(freq_table) * 100
print(percentages) 
table(data_pre$ResidenceStateLabel, useNA = "ifany")

freq_table <- table(data_pre$MaritalStatusLabel)
print(freq_table)  
percentages <- prop.table(freq_table) * 100
print(percentages)

freq_table <- table(data_pre$NaloxoneAdministered, useNA = "always")
print(freq_table)
percentages <- prop.table(freq_table) * 100
print(percentages) 

#mutually exclusive race variable
freq_table<-table(data_pre$RaceCategory, useNA="always")
print(freq_table)
percentages<-prop.table(freq_table)*100
print (percentages)

freq_table <- table(data_pre$HomelessLabel, useNA = "ifany")
print(freq_table)
percentages <- prop.table(freq_table) * 100
print(percentages)

freq_table <- table(data_pre$HxOpioidUse, useNA = "ifany")
print(freq_table)
percentages <- prop.table(freq_table) * 100
print(percentages)

freq_table <- table(data_pre$BystanderCategory, useNA = "ifany")
print(freq_table)
percentages <- prop.table(freq_table) * 100
print(percentages)

freq_table <- table(data_pre$LocationCategory, useNA = "always")
print(freq_table)
percentages <- prop.table(freq_table) * 100
print(percentages)


#basic statistics of numeric variables: ********during-pandemic (2020, 2021, 2022)
mean(data_during$Age, na.rm=TRUE)  
median(data_during$Age, na.rm=TRUE)  
sd(data_during$Age, na.rm=TRUE)   

mean(data_during$EducationYears, na.rm=TRUE)  
median(data_during$EducationYears, na.rm=TRUE) 
sd(data_during$EducationYears, na.rm=TRUE)
freq_table <- table(data_during$EducationCategory, useNA="always")
print(freq_table)    
percentages <- prop.table(freq_table) * 100
print(percentages)

#basic statistics of categorical variables: during
freq_table <- table(data_during$opioid_r_cod)
print(freq_table)    
percentages <- prop.table(freq_table) * 100
print(percentages)

freq_table <- table(data_during$SexLabel)
print(freq_table)  
percentages <- prop.table(freq_table) * 100
print(percentages)

freq_table <- table(data_during$StateCategory)
print(freq_table)  
percentages <- prop.table(freq_table) * 100
print(percentages) 

freq_table <- table(data_during$MaritalStatusLabel)
print(freq_table)  
percentages <- prop.table(freq_table) * 100
print(percentages)

freq_table <- table(data_during$NaloxoneAdministered, useNA = "always")
print(freq_table)
percentages <- prop.table(freq_table) * 100
print(percentages) 

#mutually exclusive race variable
freq_table<-table(data_during$RaceCategory, useNA="always")
print(freq_table)
percentages<-prop.table(freq_table)*100
print (percentages)

freq_table <- table(data_during$HomelessLabel, useNA = "ifany")
print(freq_table)
percentages <- prop.table(freq_table) * 100
print(percentages)

freq_table <- table(data_during$HxOpioidUse, useNA = "ifany")
print(freq_table)
percentages <- prop.table(freq_table) * 100
print(percentages)

freq_table <- table(data_during$BystanderCategory, useNA = "ifany")
print(freq_table)
percentages <- prop.table(freq_table) * 100
print(percentages)

freq_table <- table(data_during$LocationCategory, useNA = "always")
print(freq_table)
percentages <- prop.table(freq_table) * 100
print(percentages)




#descriptive stats of ENTIRE******* dataset
mean(data_clean$Age, na.rm=TRUE)  
median(data_clean$Age, na.rm=TRUE)  
sd(data_clean$Age, na.rm=TRUE)   

mean(data_clean$EducationYears, na.rm=TRUE)  
median(data_clean$EducationYears, na.rm=TRUE) 
sd(data_clean$EducationYears, na.rm=TRUE)
freq_table <- table(data_clean$EducationCategory, useNA="always")
print(freq_table)    
percentages <- prop.table(freq_table) * 100
print(percentages)

#basic statistics of categorical variables:
freq_table <- table(data_clean$opioid_r_cod)
print(freq_table)    
percentages <- prop.table(freq_table) * 100
print(percentages)

freq_table <- table(data_clean$SexLabel)
print(freq_table)  
percentages <- prop.table(freq_table) * 100
print(percentages)

freq_table <- table(data_clean$StateCategory)
print(freq_table)  
percentages <- prop.table(freq_table) * 100
print(percentages) 

freq_table <- table(data_clean$MaritalStatusLabel)
print(freq_table)  
percentages <- prop.table(freq_table) * 100
print(percentages)

freq_table <- table(data_clean$NaloxoneAdministered, useNA = "always")
print(freq_table)
percentages <- prop.table(freq_table) * 100
print(percentages) 

#mutually exclusive race variable
freq_table<-table(data_clean$RaceCategory, useNA="always")
print(freq_table)
percentages<-prop.table(freq_table)*100
print (percentages)

freq_table <- table(data_clean$HomelessLabel, useNA = "ifany")
print(freq_table)
percentages <- prop.table(freq_table) * 100
print(percentages)

freq_table <- table(data_clean$HxOpioidUse, useNA = "ifany")
print(freq_table)
percentages <- prop.table(freq_table) * 100
print(percentages)

freq_table <- table(data_clean$BystanderCategory, useNA = "ifany")
print(freq_table)
percentages <- prop.table(freq_table) * 100
print(percentages)

freq_table <- table(data_clean$LocationCategory, useNA = "always")
print(freq_table)
percentages <- prop.table(freq_table) * 100
print(percentages)


str(data_clean$TimeFrame)
data_clean$TimeFrame <- factor(data_clean$TimeFrame, levels = c(0, 1), labels = c("Pre-pandemic", "During Pandemic"))
summary(data_clean$TimeFrame)

#bar graph of locationcategories in pre and during
library(dplyr)
library(ggplot2)
data_counts <- data_clean %>%
  group_by(TimeFrame) %>%
  mutate(Total = n()) %>%
  ungroup() %>%
  group_by(LocationCategory, TimeFrame, Total) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Proportion = Count / Total)

# Create the bar graph with proportions
ggplot(data_counts, aes(x = LocationCategory, y = Proportion, fill = TimeFrame)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::percent(Proportion, accuracy=0.01)), vjust = -0.5, position = position_dodge(0.9), size = 2) +
  labs(title = "Proportion of Fatal Overdoses at Each Location Category",
       x = "Location Category",
       y = "Proportion of Fatal Overdoses",
       fill = "TimeFrame") +
  scale_fill_manual(values = c("Pre-pandemic" = "blue", "During Pandemic" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size=6, angle=30, hjust=1))


# P-VALUES**********

#continuous variables

#age
#check normality
shapiro_pre_age <- shapiro.test(data_pre$Age)$p.value
shapiro_during_age <- shapiro.test(data_during$Age)$p.value

hist(data_pre$Age, main = "Pre-pandemic Age Distribution", xlab = "Age")
qqnorm(data_pre$Age)
qqline(data_pre$Age)

hist(data_during$Age, main = "Pre-pandemic Age Distribution", xlab = "Age")
qqnorm(data_during$Age)
qqline(data_during$Age)

#NOT normal distribution--> use Wilcoxon rank-sum test:
wilcox.test(data_pre$Age, data_during$Age)

#years of education
#check normality
shapiro_pre_educationyears <- shapiro.test(data_pre$EducationYears)$p.value
shapiro_during_educationyears <- shapiro.test(data_during$EducationYears)$p.value

hist(data_pre$EducationYears, main = "Pre-pandemic Education Years Distribution", xlab = "EducationYears")
qqnorm(data_pre$EducationYears)
qqline(data_pre$EducationYears)

hist(data_during$EducationYears, main = "Pre-pandemic Education Years Distribution", xlab = "EducationYears")
qqnorm(data_during$EducationYears)
qqline(data_during$EducationYears)

#NOT normal distribution-->use Wilcoxon rank-sum test:
wilcox.test(data_pre$EducationYears, data_during$EducationYears)



#categorical variables: chi-square (or fisher's exact test)

#SexLabel
pvalue <- chisq.test(table(data_clean$SexLabel, data_clean$TimeFrame))$p.value
print(pvalue)

#Race
pvalue <- chisq.test(table(data_clean$RaceCategory, data_clean$TimeFrame))$p.value
print(pvalue)

#EducationCategory
pvalue <- chisq.test(table(data_clean$EducationCategory, data_clean$TimeFrame))$p.value
print(pvalue)

#state of residence 
pvalue <- chisq.test(table(data_clean$StateCategory, data_clean$TimeFrame))$p.value
print(pvalue)

#opiod_r_cod
pvalue <- chisq.test(table(data_clean$opioid_r_cod, data_clean$TimeFrame))$p.value
print(pvalue)

#homeless status
pvalue <- chisq.test(table(data_clean$HomelessLabel, data_clean$TimeFrame))$p.value
print(pvalue)

#Hx of opioid use
pvalue <- chisq.test(table(data_clean$HxOpioidUse, data_clean$TimeFrame))$p.value
print(pvalue)

#bystanders present
pvalue <- chisq.test(table(data_clean$BystanderCategory, data_clean$TimeFrame))$p.value
print(pvalue)

#marital status
pvalue <- chisq.test(table(data_clean$MaritalStatusLabel, data_clean$TimeFrame))$p.value
print(pvalue)

#naloxone administered (fisher's)
#simulate bc calculation too intensive, cant run
fisher.test(table(data_clean$NaloxoneAdministered, data_clean$TimeFrame), simulate.p.value = TRUE)

#location category
pvalue <- chisq.test(table(data_clean$LocationCategory, data_clean$TimeFrame))$p.value
print(pvalue)




#multinomial regression****
str(data_clean$LocationCategory)  
str(data_clean$TimeFrame)

#ensure reference group as reference
data_clean$LocationCategory <- relevel(factor(data_clean$LocationCategory), ref = "Reference Group")

#check for collinearity
install.packages("car")
library(car)
linear_model <- lm(LocationCategory ~ TimeFrame + Age + SexLabel + EducationYears + MaritalStatusLabel
                   + InjuryCountyLabel + opioid_r_cod + HomelessLabel + HxOpioidUse
                   + BystandersPresentDescription + NaloxoneAdministered + Polysubstance
                   + EducationCategory + Race, data = data_clean)
vif(linear_model)
table(data_clean$LocationCategory)

# Ensure that your response variable is treated correctly
data_clean$LocationCategory_numeric <- as.numeric(data_clean$LocationCategory)
table(data_clean$LocationCategory_numeric)

str(data_clean$MaritalStatusLabel)
data_clean$TimeFrame <- as.factor(data_clean$TimeFrame)
data_clean$SexLabel <- as.factor(data_clean$SexLabel)
data_clean$MaritalStatusLabel <- as.factor(data_clean$MaritalStatusLabel)
data_clean$opioid_r_cod <- as.factor(data_clean$opioid_r_cod)
data_clean$HomelessLabel <- as.factor(data_clean$HomelessLabel)
data_clean$HxOpioidUse <- as.factor(data_clean$HxOpioidUse)
data_clean$BystandersPresentDescription <- as.factor(data_clean$BystandersPresentDescription)
data_clean$NaloxoneAdministered <- as.factor(data_clean$NaloxoneAdministered)
data_clean$Polysubstance <- as.factor(data_clean$Polysubstance)
data_clean$EducationCategory <- as.factor(data_clean$EducationCategory)
data_clean$Race <- as.factor(data_clean$Race)

#check levels of each factor variable
lapply(data_clean[c("TimeFrame", "SexLabel", "MaritalStatusLabel", "opioid_r_cod",
                    "HomelessLabel", "HxOpioidUse", "BystandersPresentDescription",
                    "NaloxoneAdministered", "Polysubstance", "EducationCategory", "Race")], levels)
freq_table <- table(data_clean$Polysubstance, useNA = "always")
print(freq_table) 
#not including polysubstance

# Fit the linear model for collinearity
linear_model <- lm(LocationCategory_numeric ~ TimeFrame + Age + SexLabel + MaritalStatusLabel
                   + opioid_r_cod + HomelessLabel + HxOpioidUse + BystandersPresentDescription
                   + NaloxoneAdministered + EducationCategory + Race, 
                   data=data_clean)
library(car)
vif(linear_model)



install.packages("nnet") #CRUDE*****
library(nnet)
#fit multinomial logistic regression model
crude_model <- multinom(LocationCategory ~ TimeFrame, data = data_clean)
#summary of the model
summary(crude_model)
#odds ratios and 95% confidence intervals
exp(coef(crude_model))
confint(crude_model)


library(nnet)
adjusted_model <- multinom(LocationCategory ~ TimeFrame + Age + SexLabel + MaritalStatusLabel +
                    opioid_r_cod + HomelessLabel + HxOpioidUse + 
                    BystandersPresentDescription + NaloxoneAdministered +
                    EducationCategory + Race, data = data_clean)
#summary of the model
summary(adjusted_model)
#odds ratios and 95% confidence intervals
exp(coef(adjusted_model))
confint(adjusted_model)







