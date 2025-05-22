## ----setup, include=FALSE---------------------------------------------------------------------------------------------------
library(EpiModel)


## ---------------------------------------------------------------------------------------------------------------------------
AoN <- function(t, t0, parms) {
with(as.list(c(t0, parms)), {
# 1. Track the total population size
  N <- s.num + i.num + r.num + v.num
# 2. Define lambda
  lambda<-tau * c * i.num/N
# 3. Write four differential equations
  dS<- -lambda*s.num + (1 - omega * chi)*mu*N - mu*s.num
  dI <- lambda*s.num - gamma*i.num - mu*i.num
  dR <- gamma*i.num - mu*r.num
  dV <- omega*chi*mu*N - mu*v.num
# 4. Outputs
list(c(dS, dI, dR, dV,
       si.flow=lambda*s.num, #incidence
       v.flow=omega*chi*mu*N ))    #number entering vax state
})
}
param<-param.dcm(tau=0.4, c=3, mu=1/(365*50), gamma=1/14, omega=0.95, chi=c(0, 0.50, 1))
init<-init.dcm(s.num=100000, i.num=1, r.num=0, v.num=0, si.flow=0, v.flow=0)
control<-control.dcm(nsteps=5475, new.mod=AoN) #15 years=15*365=5475 days

sim <- dcm(param, init, control)
#add new proportional disease prevalence variable
sim<-mutate_epi(sim, prop.prev=i.num/(s.num+i.num+r.num+v.num))
sim

df_full<-as.data.frame(sim) #full dataframe with all chi values
print(head(df_full,10))

df_chi0   <- subset(df_full, run == 1) 
print(head(df_chi0,10))

df_chi0.5 <- subset(df_full, run == 2)
print(head(df_chi0.5,10))

df_chi1   <- subset(df_full, run == 3)
print(head(df_chi1,10))

#peak prevalence and time of peak prev for each model runs
#run 1(chi=0)
peak_prev_0<-max(df_chi0$i.num)
print(peak_prev_0) #77100.0022
peak_prev_0_time<-which.max(df_chi0$i.num)
print(peak_prev_0_time) #day 14

#run 2(chi=0.5)
peak_prev_0.5<-max(df_chi0.5$i.num)
print(peak_prev_0.5) #77071.99
peak_prev_0.5_time<-which.max(df_chi0.5$i.num)
print(peak_prev_0.5_time) #day 14

#run 3(chi=1)
peak_prev_1<-max(df_chi1$i.num)
print(peak_prev_1) #77043.96
peak_prev_1_time<-which.max(df_chi1$i.num)
print(peak_prev_1_time) #day 14

#plots of disease prevalence
#first 100 days:
par(mfrow = c(1,2), mar = c(3,3,1,1), mgp = c(2,1,0))
#plot(sim, y ="i.num", xlab="Days", ylab="Number", main = "Disease Prevalence, First 100 Days", lwd = 1, xlim = c(0, 100), cex.main = 0.5)
#plot(df_chi0$prop.prev, type = "l", xlab="Days", ylab="Proportional Prevalence", main = "Proportional Disease Prevalence, First 100 Days", lwd = 1, xlim = c(0, 100),col="green", cex.main = 0.7)
# Add the second line (chi = 0.5)
#lines(df_chi0.5$prop.prev, col = "red", lwd = 1, xlim = c(0, 100))
# Add the third line (chi = 1)
#lines(df_chi1$prop.prev, col = "blue", lwd = 1, xlim = c(0, 100))
# Add a legend to differentiate the lines
#legend("topright", legend = c("Chi = 0", "Chi = 0.5", "Chi = 1"),
      # col = c("green", "red", "blue"), lwd = 2, cex = 0.7)
plot(sim, y="prop.prev", xlab="Days", ylab="Proportional Prevalence", main = "Proportional Disease Prevalence, First 100 Days", lwd = 1, xlim = c(0, 100), cex.main = 0.7)

#lower 5% of prevalence
plot(sim, y ="prop.prev", xlab="Days", ylab="Proportional Prevalence", main = "Proportional Disease Prevalence, Lower 5%", lwd = 1, ylim = c(0, 0.05), cex.main = 0.7)
legend("topright", legend = c("Chi=0, run 1", "Chi=0.5, run 2", "Chi=1, run 3"),
       col = c("red", "blue", "green"), lwd = 2, cex = 0.7)


## ---------------------------------------------------------------------------------------------------------------------------
AoN <- function(t, t0, parms) {
with(as.list(c(t0, parms)), {
# 1. Track the total population size
  N <- s.num + i.num + r.num + v.num
# 2. Define lambda
  lambda<-tau * c * i.num/N
# 3. Write four differential equations
  dS<- -lambda*s.num + (1 - omega * chi)*mu*N - mu*s.num
  dI <- lambda*s.num - gamma*i.num - mu*i.num
  dR <- gamma*i.num - mu*r.num
  dV <- omega*chi*mu*N - mu*v.num
# 4. Outputs
list(c(dS, dI, dR, dV,
       si.flow=lambda*s.num, #incidence
       v.flow=omega*chi*mu*N ))    #number entering vax state
})
}
param<-param.dcm(tau=0.4, c=3, mu=1/(365*50), gamma=1/14, omega=c(0, 0.5, 0.9), chi=0.9)
init<-init.dcm(s.num=100000, i.num=1, r.num=0, v.num=0, si.flow=0, v.flow=0)
control<-control.dcm(nsteps=5475, new.mod=AoN) #15 years=15*365=5475 days

sim2 <- dcm(param, init, control)

df_full_omega<-as.data.frame(sim2) #full dataframe with all omega values
print(head(df_full_omega,10))
library("dplyr")
df_full_omega <- df_full_omega %>% mutate(cum_incidence = cumsum(si.flow))

df_omega0   <- subset(df_full_omega, run == 1) 
print(head(df_omega0,10))

df_omega0.5 <- subset(df_full_omega, run == 2)
print(head(df_omega0.5,10))

df_omega0.9 <- subset(df_full_omega, run == 3)
print(head(df_omega0.9,10))

#cumulative incidence
#omega=0
cum_incidence_omega0 <- sum(df_omega0$si.flow)
print(cum_incidence_omega0) #121,270.3 

#omega=0.5
cum_incidence_omega0.5 <- sum(df_omega0.5$si.flow)
print(cum_incidence_omega0.5) #109,159.7

#omega=0.9
cum_incidence_omega0.9 <- sum(df_omega0.9$si.flow)
print(cum_incidence_omega0.9) #100,004.2

#NUMBER infections averted
#comparing the lower vaccination scenario to the reference (no-vaccine) scenario aka omega=0.5 vs 0
infections_averted_low <- cum_incidence_omega0 - cum_incidence_omega0.5
print(infections_averted_low) #12,110.6
#comparing the higher vaccination scenario to the reference (no-vaccine) scenario aka omega=0.9 vs 0
infections_averted_high <- cum_incidence_omega0 - cum_incidence_omega0.9
print(infections_averted_high) #21,266.17

#PERCENT infections averted
#low to no
percented_averted_low<-(infections_averted_low / cum_incidence_omega0)*100
print(percented_averted_low) #9.99%
#high to no
percented_averted_high<-(infections_averted_high / cum_incidence_omega0)*100
print(percented_averted_high) #17.54%


## ---------------------------------------------------------------------------------------------------------------------------
Wane <- function(t, t0, parms) {
with(as.list(c(t0, parms)), {
# 1. Track the total population size
  N <- s.num + i.num + r.num + v.num
# 2. Define lambda
  lambda<-tau * c * i.num/N
# 3. Write out the four differential equations
  dS<- -lambda*s.num + (1 - omega * chi)*mu*N - mu*s.num + sigma*v.num
  dI <- lambda*s.num - gamma*i.num - mu*i.num
  dR <- gamma*i.num - mu*r.num
  dV <- omega*chi*mu*N - mu*v.num - sigma*v.num
# 4. Outputs
list(c(dS, dI, dR, dV,
       si.flow=lambda*s.num, #incidence
       v.flow=omega*chi*mu*N  #number entering vax state 
))
})
}

param<-param.dcm(tau=0.4, c=0.5, mu=1/(365*10), gamma=1/15, omega=0.95, chi=1, 
                sigma=c(1/365, 1/730, 1/1095, 1/1460, 1/1825)) #1-5 YEARS of protection-->converted to days (increasing length of vaccine induced immunity)
init<-init.dcm(s.num=1000, i.num=1, r.num=0, v.num=0, si.flow=0, v.flow=0)
control<-control.dcm(nsteps=18250, new.mod=Wane) #50 years=50*365=18250 days

sim3 <- dcm(param, init, control)

df_full_sigma<-as.data.frame(sim3) #full dataframe with all sigma values
print(head(df_full_sigma,10))

#plots
par(mfrow = c(2,2), mar = c(3,3,1,1), mgp = c(2,1,0))
#susceptible
plot(sim3, y ="s.num", xlab="Days", ylab="Number", main = "Susceptible", lwd = 1, cex.main = 0.7)
legend("topright", legend = c("run 1", "run 2", "run 3", "run 4", "run 5"),
       col = c("red", "blue", "green", "purple", "orange"), lwd = 2, cex = 0.7)
#infected
plot(sim3, y ="i.num", xlab="Days", ylab="Number", main = "Infected", lwd = 1, cex.main = 0.7)
legend("topright", legend = c("run 1", "run 2", "run 3", "run 4", "run 5"),
       col = c("red", "blue", "green", "purple", "orange"), lwd = 2, cex = 0.7)
#recovered
plot(sim3, y ="r.num", xlab="Days", ylab="Number", main = "Recovered", lwd = 1, cex.main = 0.7)
legend("topright", legend = c("run 1", "run 2", "run 3", "run 4", "run 5"),
       col = c("red", "blue", "green", "purple", "orange"), lwd = 2, cex = 0.7)
#vaccinated 
plot(sim3, y ="v.num", xlab="Days", ylab="Number", main = "Vaccinated", lwd = 1, cex.main = 0.7)
legend("topright", legend = c("run 1", "run 2", "run 3", "run 4", "run 5"),
       col = c("red", "blue", "green", "purple", "orange"), lwd = 2, cex = 0.7)


## ---------------------------------------------------------------------------------------------------------------------------
library("dplyr")
df_full_sigma <- df_full_sigma %>% mutate(cum_incidence = cumsum(si.flow))

df_sigma1   <- subset(df_full_sigma, run == 1) 
print(head(df_sigma1,10))
print(tail(df_sigma1))

df_sigma3   <- subset(df_full_sigma, run == 3) 
print(head(df_sigma3,10))
print(tail(df_sigma3))

df_sigma5   <- subset(df_full_sigma, run == 5) 
print(head(df_sigma5,10))
print(tail(df_sigma5))

#cumulative incidence
#sigma=1 REFERENCE
cum_incidence_sigma1 <- sum(df_sigma1$si.flow)
print(cum_incidence_sigma1) #3481.822

#sigma=3 
cum_incidence_sigma3 <- sum(df_sigma3$si.flow)
print(cum_incidence_sigma3) #2730.61

#sigma=5 
cum_incidence_sigma5 <- sum(df_sigma5$si.flow)
print(cum_incidence_sigma5) #2186.757

#NUMBER infections averted NIA
#comparing the 3 year scenario to the reference (1 year) scenario aka sigma=3 vs 1
infections_averted_3 <- cum_incidence_sigma1 - cum_incidence_sigma3
print(infections_averted_3) #751.2119
#comparing the 5 year scenario to the reference (1 year) scenario sigma=5 vs 1
infections_averted_5 <- cum_incidence_sigma1 - cum_incidence_sigma5
print(infections_averted_5) #1295.065

#PERCENT infections averted PIA
#3 vs 1
percented_averted_3<-(infections_averted_3 / cum_incidence_sigma1)*100
print(percented_averted_3) #21.57526
#5 vs 1
percented_averted_5<-(infections_averted_5 / cum_incidence_sigma1)*100
print(percented_averted_5) #37.19504

#total number vaccinated 
#sigma=1 REFERENCE
vax_sigma1 <-sum(df_sigma1$v.flow)
print(vax_sigma1)  #4754.75

#sigma=3 
vax_sigma3 <-sum(df_sigma3$v.flow)
print(vax_sigma3) #4754.75


#sigma=5 
vax_sigma5 <- sum(df_sigma5$v.flow)
print(vax_sigma5) #4754.75


#number needed to treat (NNT) to prevent one new infection (# needed to vaccinate to prevent one infection)
# number treatments / number infections averted       number "treated"=number vaccinated

#sigma=3 vs 1
NNT3<- 4754.75/751.2119
print(NNT3) #6.33

#sigma=5 vs 1
NNT5<-4754.75/1295.065
print(NNT5) #3.67


## ---------------------------------------------------------------------------------------------------------------------------
#Leaky <- function(t, t0, parms) {
#with(as.list(c(t0, parms)), {
# 1. Track the total population size
# 2. Define lambdas
# 3. Write out the four differential equations
# 4. Outputs (include both si.flow + v.flow)
#list(c(
#))
#})
##}
#param <- param.dcm(tau = 0.4,
#c = 0.5,
#mu = 1/(365*50),
#gamma = 1/14,
#psi = 0.6,
#omega = 0)
#init <- init.dcm(s.uv.num = 1e5*(1-param$omega),
#s.v.num = 1e5*param$omega,
#i.num = 1,
#r.num = 0,
#v.flow = 0,
#si.flow = 0)
#control <- control.dcm(nsteps = 365, new.mod = Leaky)

#sim <- dcm(param, init, control)
#df <- as.data.frame(sim)

#param2 <- param.dcm(tau = 0.4,
#c = 0.5,
#mu = 1/(365*50),
#gamma = 1/14,
#psi = 0.6,
#omega = 0.7)
#init2 <- init.dcm(s.uv.num = 1e5*(1-param2$omega),
#s.v.num = 1e5*param2$omega,
#i.num = 1,
#r.num = 0,
#v.flow = 0,
#si.flow = 0)

#sim2 <- dcm(param2, init2, control)
#df2 <- as.data.frame(sim2)

