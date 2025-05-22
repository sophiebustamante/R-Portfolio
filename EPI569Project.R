# Install and load packages
roster <- readRDS("/Users/sophiabustamante/Downloads/roster_2024.rds")
rff<- readRDS("/Users/sophiabustamante/Downloads/rff_2024.rds")
transmissionpairs<- readRDS ("/Users/sophiabustamante/Downloads/transmission_pairs_2024.rds")

write.csv(readRDS("/Users/sophiabustamante/Downloads/rff_2024.rds"), "rff_2024.csv")

library(dplyr)
rff_clean <- rff %>% 
  rename(c(ta569 = "If you are in EPI 569, which TA group are you in?", 
           Date_of_Exposure = "Date of Exposure",  
           Time_of_Exposure = "Time of Exposure", 
           symptoms = "Did you have symptoms?", 
           symptom_onset_hours = "How many hours after exposure did you develop symptoms?", 
           onset_recovery_hours = "How many hours after your symptom onset did you feel better?", 
           exposure_count = "How many people did you expose?",
           infect_count = "How many people did you infect?",  
           first_exposure_date = "Date of First Exposure", 
           first_exposure_time = "Time of First Exposure", 
           second_exposure_date = "Date of Second Exposure", 
           second_exposure_time = "Time of Second Exposure", 
           third_exposure_date = "Date of Third Exposure", 
           third_exposure_time = "Time of Third Exposure",  
           fourth_exposure_date = "Date of Fourth Exposure", 
           fourth_exposure_time = "Time of Fourth Exposure",  
           fifth_exposure_date = "Date of Fifth Exposure",  
           fifth_exposure_time = "Time of Fifth Exposure")) %>% 
  mutate(symptoms = if_else(caseID == 41, "Yes", symptoms),  
         onset_recovery_hours = if_else(symptoms=="No", NA, onset_recovery_hours) 
         )

rff2 <- rff_clean %>% 
  group_by(Date_of_Onset) %>% 
  summarise(cases=n()) %>% 
  arrange(Date_of_Onset)  %>% 
  mutate(date = Date_of_Onset) 

rff3 <- rff %>% 
  group_by(Date_of_Recovery) %>% 
  summarise(recovered=n()) %>% 
  arrange(Date_of_Recovery) %>% 
  mutate(date = Date_of_Recovery) 

rff_4 <- merge(rff2, rff3, by="date") %>% 
  select(-c(Date_of_Recovery, Date_of_Onset)) %>% 
  mutate(cumulative_cases = cumsum(cases)) 

rff_curve <- rff_4 %>% 
  mutate(susceptible = 152-cumulative_cases) 


#Epi curve (draft, incomplete) 
library(ggplot2)
case_histogram <- ggplot(rff_curve, aes(x=date)) +  
  geom_bar(aes(y=cases), stat="identity", fill="steelblue", color="black") + 
  labs(title = "Daily Cases of Rollins Fall Fever", 
       x = "Date of Onset", 
       y = "Number of Cases") + 
  theme(plot.title = element_text(hjust = 0.5)) 
print(case_histogram)

#SIR model
#rate of contact (alpha)=2.12 
#transmission probability (beta)=0.59
#recovery rate (sigma)=0.47 but calibrate to 0.82
#loss of immunity (w)=0
#birth and death rate (mu)=0
#vaccination rate of general student body (p2)=.10
#vaccination rate of new students=0 bc birth/death rate=0
#Proportion successfully immunized (Vaccine efficacy for an all-or-nothing vaccine)=90%

#calculate d (duration of infection/infectiousness)
#onset recovery hours, remove NA's 


#Calculate alpha (daily contacts per individual) as the mean of exposure_count  
alpha <- mean(rff_clean$exposure_count, na.rm = TRUE) #2.12 use this one

#other alpha approach: divide by days sick
#duration_days:
#rff_clean <- rff_clean %>%
  #mutate(duration_days = as.numeric(difftime(Date_Time_Recovery, Date_Time_Exposure, units = "days")))

#rff_clean <- rff_clean %>%
 #mutate(dailycontact=exposure_count/duration_days)
#alpha2<-mean(rff_clean$dailycontact, na.rm=TRUE) #1.88

#Calculate beta (probability of transmission per contact) based on the SAR for each primary case
library(dplyr)
rff_clean <- rff_clean %>%
  mutate(SAR = infect_count / exposure_count)
#Average SAR across all
SARavg <- mean(rff_clean$SAR, na.rm = TRUE) #0.59

#Calculate d (duration of infection/infectiousness): turn onset_recovery_hours into days and avg out
#avg_recovery_hours<-mean(rff_clean$onset_recovery_hours, na.rm=TRUE) #26.14
#avg_duration<-avg_recovery_hours/24 #1.09 (a little over a day)
#sigma
#sigma1<-1/avg_duration #0.92

# Calculate the duration of infectiousness using Date_of_Exposure and Date_of_Recovery
#rff_clean <- rff_clean %>%
  #mutate(duration_days = as.numeric(difftime(Date_of_Recovery, Date_of_Exposure, units = "days"))) #.45

rff_clean <- rff_clean %>%
  mutate(duration_hours = rff_clean$symptom_onset_hours + rff_clean$onset_recovery_hours) #dont need to use hours

Avghrsofinfection <- mean(rff_clean$duration_hours, na.rm = TRUE)

Avgdaysofinfection <- Avghrsofinfection/24 #2.12=d
sigma<-1/Avgdaysofinfection #.47 

#d=1.44 days
sigma<-1/1.44 #0.69 use this one

#rff_clean <- rff_clean %>%
 # mutate(duration_days2 = as.numeric(difftime(Date_of_Recovery, Date_of_Onset, units = "days"))) 
#avgduration_days2<-mean(rff_clean$duration_days2, na.rm=TRUE)

# Calculate sigma as 1 / average duration (excluding NAs)
#average_duration_days <- mean(rff_clean$duration_days, na.rm = TRUE)
#sigma <- 1 / average_duration_days #.52


#critical vaccination threshold for 70% VE
#pc=(1-(1/R0))/VE=HIT/VE
#R0=(alpha*beta)/(sigma+mu)
alpha<-2.12
beta<-0.59
sigma<-0.69 #calculated .69 but calibrated to .82 to get 91 total cases
X<-.70
mu<-0
p1<-0
p2<-0
w<-0
R0<-(alpha*beta)/(sigma+mu)  #R0=1.81
(1-(1/R0))/.70 #64.05% of population needs to be vaccinated in order to stop transmission with 70% VE


#Model with NO vaccine:
library(deSolve)
library(reshape2)
library(dplyr)
#Define parameters
parms <- c(alpha = 2.12,       # alpha = daily contacts rate
           beta = 0.59,      # beta = probability of infection on contact
           sigma = 0.82,     # sigma = rate of recovery per day 
           omega = 0.0,     # omega = rate of immune loss per day
           mu = 0,       # mu =  per capita birth and death rate
           p_2 = 0,       # p_2 = Vaccination rate of general student body (rate per day)
           p_1 = 0,       # p_1 = Vaccination rate of new student admissions (proportion of births vaccinated)
           chi  = 0.70)     # chi = proportion who respond to vaccine among vaccinated

#Initial conditions
init <- c(S = 151,           # number initially susceptible
          I = 1,            # number initially infectious
          R = 0,            # number initially immune or "recovered"
          V = 0)            # number initially vaccinated            

#Define model equations -- do not change -- or change with care!
sir_ode <- function(times,init,parms){
  with(as.list(c(parms,init)), {
    # ODEs
    N <- S+I+R+V
    dS <- omega*R - alpha*beta*(I/N)*S - mu*S - p_2*chi*S + (1-p_1*chi)*mu*N
    dI <- alpha*beta*(I/N)*S - sigma*I - mu*I
    dR <- sigma*I  - omega*R - mu*R
    dV <- - mu*V + p_1*chi*mu*N + p_2*chi*S
    list(c(dS,dI,dR,dV)) 
  })
}

# This creates the output from model equations.  
#If you want to run the model for longer, change the second term eg: seq(0,200,...)
times <- seq(0,50,length.out=50)
sir_out <- lsoda(init,times,sir_ode,parms)
sir_out_long <- melt(as.data.frame(sir_out),"time")

sir_out_long %>% 
  filter(variable == "I") %>% 
  summarise(total_cases = (parms['sigma'])*(sum(value))) -> final_size

#Plot model
ggplot(data = sir_out_long, aes(x = time, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  labs(title = "No Vaccination",
       x = "Time (days)", 
       y = "Population",
       color = "Compartment") +
  theme_minimal()

#final size: 90.5~~91 cases
#peaks at: day 9.09 with 10.91 cases
#stabilizes and ends at:around day 20.20 with I falling below 1 (.77 Infected)




#Model when vaccinate 10% of the general student body on any given day:p2=.10 & 90% VE
library(deSolve)
library(reshape2)
library(dplyr)
#Define parameters
parms <- c(alpha = 2.12,       # alpha = daily contacts rate
           beta = 0.59,      # beta = probability of infection on contact
           sigma = 0.82,     # sigma = rate of recovery per day
           omega = 0.0,     # omega = rate of immune loss per day
           mu = 0,       # mu =  per capita birth and death rate
           p_2 = 0.10,       # p_2 = Vaccination rate of general student body (rate per day)
           p_1 = 0,       # p_1 = Vaccination rate of new student admissions (proportion of births vaccinated)
           chi  = 0.90)     # chi = proportion who respond to vaccine among vaccinated

#Initial conditions
init <- c(S = 151,           # number initially susceptible
          I = 1,            # number initially infectious
          R = 0,            # number initially immune or "recovered"
          V = 0)            # number initially vaccinated            

#Define model equations -- do not change -- or change with care!
sir_ode <- function(times,init,parms){
  with(as.list(c(parms,init)), {
    # ODEs
    N <- S+I+R+V
    dS <- omega*R - alpha*beta*(I/N)*S - mu*S - p_2*chi*S + (1-p_1*chi)*mu*N
    dI <- alpha*beta*(I/N)*S - sigma*I - mu*I
    dR <- sigma*I  - omega*R - mu*R
    dV <- - mu*V + p_1*chi*mu*N + p_2*chi*S
    list(c(dS,dI,dR,dV)) 
  })
}

# This creates the output from model equations.  
#If you want to run the model for longer, change the second term eg: seq(0,200,...)
times <- seq(0,100,length.out=100)
sir_out <- lsoda(init,times,sir_ode,parms)
sir_out_long <- melt(as.data.frame(sir_out),"time")

sir_out_long %>% 
  filter(variable == "I") %>% 
  summarise(total_cases = (parms['sigma'])*(sum(value))) -> final_size

#Plot model
ggplot(data = sir_out_long, aes(x = time, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  labs(title = "90% VE: 10% of Population Vaccinated per Day",
       x = "Time (days)", 
       y = "Population",
       color = "Compartment") +
  theme_minimal()
#final size: 14.6-->15 cases
#peaks at: day 4.04 with 2.22 cases
#stabilizes and ends at:around day 9.09 with I falling below 1 (.85 Infected)




#Model when vaccinate 5% of the general student body on any given day:p2=.05 & 90% VE
library(deSolve)
library(reshape2)
library(dplyr)
#Define parameters
parms <- c(alpha = 2.12,       # alpha = daily contacts rate
           beta = 0.59,      # beta = probability of infection on contact
           sigma = 0.82,     # sigma = rate of recovery per day
           omega = 0.0,     # omega = rate of immune loss per day
           mu = 0,       # mu =  per capita birth and death rate
           p_2 = 0.05,       # p_2 = Vaccination rate of general student body (rate per day)
           p_1 = 0,       # p_1 = Vaccination rate of new student admissions (proportion of births vaccinated)
           chi  = 0.90)     # chi = proportion who respond to vaccine among vaccinated

#Initial conditions
init <- c(S = 151,           # number initially susceptible
          I = 1,            # number initially infectious
          R = 0,            # number initially immune or "recovered"
          V = 0)            # number initially vaccinated            

#Define model equations -- do not change -- or change with care!
sir_ode <- function(times,init,parms){
  with(as.list(c(parms,init)), {
    # ODEs
    N <- S+I+R+V
    dS <- omega*R - alpha*beta*(I/N)*S - mu*S - p_2*chi*S + (1-p_1*chi)*mu*N
    dI <- alpha*beta*(I/N)*S - sigma*I - mu*I
    dR <- sigma*I  - omega*R - mu*R
    dV <- - mu*V + p_1*chi*mu*N + p_2*chi*S
    list(c(dS,dI,dR,dV)) 
  })
}

# This creates the output from model equations.  
#If you want to run the model for longer, change the second term eg: seq(0,200,...)
times <- seq(0,100,length.out=100)
sir_out <- lsoda(init,times,sir_ode,parms)
sir_out_long <- melt(as.data.frame(sir_out),"time")

sir_out_long %>% 
  filter(variable == "I") %>% 
  summarise(total_cases = (parms['sigma'])*(sum(value))) -> final_size

#Plot model
ggplot(data = sir_out_long, aes(x = time, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  labs(title = "90% VE: 5% of Population Vaccinated per Day",
       x = "Time (days)", 
       y = "Population",
       color = "Compartment") +
  theme_minimal()
#final size: 30.75-->31 cases
#peaks at: day 6.06 with 3.73 cases
#stabilizes and ends at:around day 14.14 with I falling below 1 (.84 Infected)






#What if the vaccine is less effective? 80% VE:


#Model when vaccinate 10% of the general student body on any given day:p2=.10 & 80% VE
library(deSolve)
library(reshape2)
library(dplyr)
#Define parameters
parms <- c(alpha = 2.12,       # alpha = daily contacts rate
           beta = 0.59,      # beta = probability of infection on contact
           sigma = 0.82,     # sigma = rate of recovery per day
           omega = 0.0,     # omega = rate of immune loss per day
           mu = 0,       # mu =  per capita birth and death rate
           p_2 = 0.10,       # p_2 = Vaccination rate of general student body (rate per day)
           p_1 = 0,       # p_1 = Vaccination rate of new student admissions (proportion of births vaccinated)
           chi  = 0.80)     # chi = proportion who respond to vaccine among vaccinated

#Initial conditions
init <- c(S = 151,           # number initially susceptible
          I = 1,            # number initially infectious
          R = 0,            # number initially immune or "recovered"
          V = 0)            # number initially vaccinated            

#Define model equations -- do not change -- or change with care!
sir_ode <- function(times,init,parms){
  with(as.list(c(parms,init)), {
    # ODEs
    N <- S+I+R+V
    dS <- omega*R - alpha*beta*(I/N)*S - mu*S - p_2*chi*S + (1-p_1*chi)*mu*N
    dI <- alpha*beta*(I/N)*S - sigma*I - mu*I
    dR <- sigma*I  - omega*R - mu*R
    dV <- - mu*V + p_1*chi*mu*N + p_2*chi*S
    list(c(dS,dI,dR,dV)) 
  })
}

# This creates the output from model equations.  
#If you want to run the model for longer, change the second term eg: seq(0,200,...)
times <- seq(0,100,length.out=100)
sir_out <- lsoda(init,times,sir_ode,parms)
sir_out_long <- melt(as.data.frame(sir_out),"time")

sir_out_long %>% 
  filter(variable == "I") %>% 
  summarise(total_cases = (parms['sigma'])*(sum(value))) -> final_size

#Plot model
ggplot(data = sir_out_long, aes(x = time, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  labs(title = "80% VE: 10% of Population Vaccinated per Day",
       x = "Time (days)", 
       y = "Population",
       color = "Compartment") +
  theme_minimal()
#final size: 16.58-->17 cases
#peaks at: day 4.04 with 2.39 cases
#stabilizes and ends at:around day 10.10 with I falling below 1 (.80 Infected)

#Model when vaccinate 5% of the general student body on any given day:p2=.05 & 80% VE
library(deSolve)
library(reshape2)
library(dplyr)
#Define parameters
parms <- c(alpha = 2.12,       # alpha = daily contacts rate
           beta = 0.59,      # beta = probability of infection on contact
           sigma = 0.82,     # sigma = rate of recovery per day
           omega = 0.0,     # omega = rate of immune loss per day
           mu = 0,       # mu =  per capita birth and death rate
           p_2 = 0.05,       # p_2 = Vaccination rate of general student body (rate per day)
           p_1 = 0,       # p_1 = Vaccination rate of new student admissions (proportion of births vaccinated)
           chi  = 0.80)     # chi = proportion who respond to vaccine among vaccinated

#Initial conditions
init <- c(S = 151,           # number initially susceptible
          I = 1,            # number initially infectious
          R = 0,            # number initially immune or "recovered"
          V = 0)            # number initially vaccinated            

#Define model equations -- do not change -- or change with care!
sir_ode <- function(times,init,parms){
  with(as.list(c(parms,init)), {
    # ODEs
    N <- S+I+R+V
    dS <- omega*R - alpha*beta*(I/N)*S - mu*S - p_2*chi*S + (1-p_1*chi)*mu*N
    dI <- alpha*beta*(I/N)*S - sigma*I - mu*I
    dR <- sigma*I  - omega*R - mu*R
    dV <- - mu*V + p_1*chi*mu*N + p_2*chi*S
    list(c(dS,dI,dR,dV)) 
  })
}

# This creates the output from model equations.  
#If you want to run the model for longer, change the second term eg: seq(0,200,...)
times <- seq(0,100,length.out=100)
sir_out <- lsoda(init,times,sir_ode,parms)
sir_out_long <- melt(as.data.frame(sir_out),"time")

sir_out_long %>% 
  filter(variable == "I") %>% 
  summarise(total_cases = (parms['sigma'])*(sum(value))) -> final_size

#Plot model
ggplot(data = sir_out_long, aes(x = time, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  labs(title = "80% VE: 5% of Population Vaccinated per Day",
       x = "Time (days)", 
       y = "Population",
       color = "Compartment") +
  theme_minimal()
#final size: 34.44-->35 cases
#peaks at: day 7.07 with 4.06 cases
#stabilizes and ends at:around day 15.15 with I falling below 1 (.78 Infected)


#70% effective all-or-nothing vaccine, what happens if CVT is met?
#critical vaccination threshold
#pc=(1-(1/R0))/VE=HIT/VE
#R0=(alpha*beta)/(sigma+mu)
alpha<-2.12
beta<-0.59
sigma<-0.69 #calculated .69 but calibrated to .82 to get 91 total cases
X<-.70
mu<-0
p1<-0
p2<-0
w<-0
R0<-(alpha*beta)/(sigma+mu)  #R0=1.53 if sigma=.82; 2.66 if sigma=.47 which one???
(1-(1/R0))/.70 #38.27% of population needs to be vaccinated in order to stop transmission with 90% VE
                #if r0=1.53; 69.36 if r0=2.66

parms <- c(alpha = 2.12,       # alpha = daily contacts rate
           beta = 0.59,      # beta = probability of infection on contact
           sigma = 0.82,     # sigma = rate of recovery per day 
           omega = 0.0,     # omega = rate of immune loss per day
           mu = 0,       # mu =  per capita birth and death rate
           p_2 = 0.05,       # p_2 = Vaccination rate of general student body (rate per day)
           p_1 = 0,       # p_1 = Vaccination rate of new student admissions (proportion of births vaccinated)
           chi  = 0.70)     # chi = proportion who respond to vaccine among vaccinated

#Initial conditions
init <- c(S = 151,           # number initially susceptible
          I = 1,            # number initially infectious
          R = 0,            # number initially immune or "recovered"
          V = 0)            # number initially vaccinated            

#Define model equations -- do not change -- or change with care!
sir_ode <- function(times,init,parms){
  with(as.list(c(parms,init)), {
    # ODEs
    N <- S+I+R+V
    dS <- omega*R - alpha*beta*(I/N)*S - mu*S - p_2*chi*S + (1-p_1*chi)*mu*N
    dI <- alpha*beta*(I/N)*S - sigma*I - mu*I
    dR <- sigma*I  - omega*R - mu*R
    dV <- - mu*V + p_1*chi*mu*N + p_2*chi*S
    list(c(dS,dI,dR,dV)) 
  })
}

# This creates the output from model equations.  
#If you want to run the model for longer, change the second term eg: seq(0,200,...)
times <- seq(0,50,length.out=50)
sir_out <- lsoda(init,times,sir_ode,parms)
sir_out_long <- melt(as.data.frame(sir_out),"time")

sir_out_long %>% 
  filter(variable == "I") %>% 
  summarise(total_cases = (parms['sigma'])*(sum(value))) -> final_size

#Plot model
ggplot(data = sir_out_long, aes(x = time, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  labs(title = "70% VE: 65% of Population Vaccinated (Critical Vaccination Threshold)",
       x = "Time (days)", 
       y = "Population",
       color = "Compartment") +
  theme_minimal()

#final size: 39 cases
#peaks at: day 7 with 5 cases
#stabilizes and ends at: day 15 with I falling below 1 (0.96 Infected)



#CVT not met ()
parms <- c(alpha = 2.12,       # alpha = daily contacts rate
           beta = 0.59,      # beta = probability of infection on contact
           sigma = 0.82,     # sigma = rate of recovery per day 
           omega = 0.0,     # omega = rate of immune loss per day
           mu = 0,       # mu =  per capita birth and death rate
           p_2 = 0.02,       # p_2 = Vaccination rate of general student body (rate per day)
           p_1 = 0,       # p_1 = Vaccination rate of new student admissions (proportion of births vaccinated)
           chi  = 0.70)     # chi = proportion who respond to vaccine among vaccinated

#Initial conditions
init <- c(S = 151,           # number initially susceptible
          I = 1,            # number initially infectious
          R = 0,            # number initially immune or "recovered"
          V = 0)            # number initially vaccinated            

#Define model equations -- do not change -- or change with care!
sir_ode <- function(times,init,parms){
  with(as.list(c(parms,init)), {
    # ODEs
    N <- S+I+R+V
    dS <- omega*R - alpha*beta*(I/N)*S - mu*S - p_2*chi*S + (1-p_1*chi)*mu*N
    dI <- alpha*beta*(I/N)*S - sigma*I - mu*I
    dR <- sigma*I  - omega*R - mu*R
    dV <- - mu*V + p_1*chi*mu*N + p_2*chi*S
    list(c(dS,dI,dR,dV)) 
  })
}

# This creates the output from model equations.  
#If you want to run the model for longer, change the second term eg: seq(0,200,...)
times <- seq(0,50,length.out=50)
sir_out <- lsoda(init,times,sir_ode,parms)
sir_out_long <- melt(as.data.frame(sir_out),"time")

sir_out_long %>% 
  filter(variable == "I") %>% 
  summarise(total_cases = (parms['sigma'])*(sum(value))) -> final_size

#Plot model
ggplot(data = sir_out_long, aes(x = time, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  labs(title = "70% VE: 2% of Population Vaccinated at Random Per Day",
       x = "Time (days)", 
       y = "Population",
       color = "Compartment") +
  theme_minimal()

#final size: 66 cases
#peaks at: day 8 with 8 cases
#stabilizes/ends at: day 18 with I falling below 1 (0.99 Infected)




#if very low VE: 10% vax per day
parms <- c(alpha = 2.12,       # alpha = daily contacts rate
           beta = 0.59,      # beta = probability of infection on contact
           sigma = 0.82,     # sigma = rate of recovery per day 
           omega = 0.0,     # omega = rate of immune loss per day
           mu = 0,       # mu =  per capita birth and death rate
           p_2 = 0.10,       # p_2 = Vaccination rate of general student body (rate per day)
           p_1 = 0,       # p_1 = Vaccination rate of new student admissions (proportion of births vaccinated)
           chi  = 0.40)     # chi = proportion who respond to vaccine among vaccinated

#Initial conditions
init <- c(S = 151,           # number initially susceptible
          I = 1,            # number initially infectious
          R = 0,            # number initially immune or "recovered"
          V = 0)            # number initially vaccinated            

#Define model equations -- do not change -- or change with care!
sir_ode <- function(times,init,parms){
  with(as.list(c(parms,init)), {
    # ODEs
    N <- S+I+R+V
    dS <- omega*R - alpha*beta*(I/N)*S - mu*S - p_2*chi*S + (1-p_1*chi)*mu*N
    dI <- alpha*beta*(I/N)*S - sigma*I - mu*I
    dR <- sigma*I  - omega*R - mu*R
    dV <- - mu*V + p_1*chi*mu*N + p_2*chi*S
    list(c(dS,dI,dR,dV)) 
  })
}

# This creates the output from model equations.  
#If you want to run the model for longer, change the second term eg: seq(0,200,...)
times <- seq(0,100,length.out=100)
sir_out <- lsoda(init,times,sir_ode,parms)
sir_out_long <- melt(as.data.frame(sir_out),"time")

sir_out_long %>% 
  filter(variable == "I") %>% 
  summarise(total_cases = (parms['sigma'])*(sum(value))) -> final_size

#Plot model
ggplot(data = sir_out_long, aes(x = time, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  labs(title = "40% VE: 10% of Population Vaccinated per Day",
       x = "Time (days)", 
       y = "Population",
       color = "Compartment") +
  theme_minimal()

#final size: 34.44--> 35 cases
#peaks at: day 7 with 4.06 case
#stabilizes/ends at: day 15 with I falling below 1 (0.78 Infected)



#if very low VE: 5% vax per day
parms <- c(alpha = 2.12,       # alpha = daily contacts rate
           beta = 0.59,      # beta = probability of infection on contact
           sigma = 0.82,     # sigma = rate of recovery per day 
           omega = 0.0,     # omega = rate of immune loss per day
           mu = 0,       # mu =  per capita birth and death rate
           p_2 = 0.05,       # p_2 = Vaccination rate of general student body (rate per day)
           p_1 = 0,       # p_1 = Vaccination rate of new student admissions (proportion of births vaccinated)
           chi  = 0.40)     # chi = proportion who respond to vaccine among vaccinated

#Initial conditions
init <- c(S = 151,           # number initially susceptible
          I = 1,            # number initially infectious
          R = 0,            # number initially immune or "recovered"
          V = 0)            # number initially vaccinated            

#Define model equations -- do not change -- or change with care!
sir_ode <- function(times,init,parms){
  with(as.list(c(parms,init)), {
    # ODEs
    N <- S+I+R+V
    dS <- omega*R - alpha*beta*(I/N)*S - mu*S - p_2*chi*S + (1-p_1*chi)*mu*N
    dI <- alpha*beta*(I/N)*S - sigma*I - mu*I
    dR <- sigma*I  - omega*R - mu*R
    dV <- - mu*V + p_1*chi*mu*N + p_2*chi*S
    list(c(dS,dI,dR,dV)) 
  })
}

# This creates the output from model equations.  
#If you want to run the model for longer, change the second term eg: seq(0,200,...)
times <- seq(0,100,length.out=100)
sir_out <- lsoda(init,times,sir_ode,parms)
sir_out_long <- melt(as.data.frame(sir_out),"time")

sir_out_long %>% 
  filter(variable == "I") %>% 
  summarise(total_cases = (parms['sigma'])*(sum(value))) -> final_size

#Plot model
ggplot(data = sir_out_long, aes(x = time, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  labs(title = "40% VE: 5% of Population Vaccinated per Day",
       x = "Time (days)", 
       y = "Population",
       color = "Compartment") +
  theme_minimal()

#final size: 56.74--> 57 cases
#peaks at: day 8 with 6.42 cases
#stabilizes/ends at: day 18 with I falling below 1 (0.81 Infected)




#Model when vaccinate 5% of the general student body on any given day:p2=.05 & 70% VE
library(deSolve)
library(reshape2)
library(dplyr)
#Define parameters
parms <- c(alpha = 2.12,       # alpha = daily contacts rate
           beta = 0.59,      # beta = probability of infection on contact
           sigma = 0.82,     # sigma = rate of recovery per day
           omega = 0.0,     # omega = rate of immune loss per day
           mu = 0,       # mu =  per capita birth and death rate
           p_2 = 0.05,       # p_2 = Vaccination rate of general student body (rate per day)
           p_1 = 0,       # p_1 = Vaccination rate of new student admissions (proportion of births vaccinated)
           chi  = 0.70)     # chi = proportion who respond to vaccine among vaccinated

#Initial conditions
init <- c(S = 151,           # number initially susceptible
          I = 1,            # number initially infectious
          R = 0,            # number initially immune or "recovered"
          V = 0)            # number initially vaccinated            

#Define model equations -- do not change -- or change with care!
sir_ode <- function(times,init,parms){
  with(as.list(c(parms,init)), {
    # ODEs
    N <- S+I+R+V
    dS <- omega*R - alpha*beta*(I/N)*S - mu*S - p_2*chi*S + (1-p_1*chi)*mu*N
    dI <- alpha*beta*(I/N)*S - sigma*I - mu*I
    dR <- sigma*I  - omega*R - mu*R
    dV <- - mu*V + p_1*chi*mu*N + p_2*chi*S
    list(c(dS,dI,dR,dV)) 
  })
}

# This creates the output from model equations.  
#If you want to run the model for longer, change the second term eg: seq(0,200,...)
times <- seq(0,50,length.out=50)
sir_out <- lsoda(init,times,sir_ode,parms)
sir_out_long <- melt(as.data.frame(sir_out),"time")

sir_out_long %>% 
  filter(variable == "I") %>% 
  summarise(total_cases = (parms['sigma'])*(sum(value))) -> final_size


#Plot model
ggplot(data = sir_out_long, aes(x = time, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  labs(title = "70% VE: 5% of Population Vaccinated per Day",
       x = "Time (days)", 
       y = "Population",
       color = "Compartment") +
  theme_minimal()
#final size: 38.8-->39 cases
#peaks at: day 7 with 4.5-->5 cases
#stabilizes and ends at:around day 16 with I falling below 1 (.73 Infected)
