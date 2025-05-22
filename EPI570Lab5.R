## ----setup, include=FALSE--------------------------------------------------------------------------------------------------
library(EpiModel)


## --------------------------------------------------------------------------------------------------------------------------
SImod <- function(t, t0, parms) {
  with(as.list(c(t0, parms)), {
    
    # Derived parameters/states for men and women
    num_m <- s.num_m + i.num_m
    num_f <- s.num_f + i.num_f
    c_f <- (c_m*num_m)/num_f
    lambda_m <- (tau_m * c_m) * (i.num_f/num_f) #(tau*c)*(I/N) .num is a state, num is total pop
    lambda_f <- (tau_f * c_f) * (i.num_m/num_m) #(tau*c)*(I/N) .num is a state, num is total pop
    
    # Differential equations
    dS_m <- (-lambda_m*s.num_m) + (nu*(num_f/2)) - (mu_s_m*s.num_m)  #infection=lambda*s.num
    dI_m <- (lambda_m*s.num_m) - (mu_i_m*i.num_m) 
    
    dS_f <- (-lambda_f*s.num_f) + (nu*(num_f/2)) - (mu_s_f*s.num_f)
    dI_f <- (lambda_f*s.num_f) - (mu_i_f*i.num_f)
    # 1. track the total size of men and total size of women
    # 2. define contact rate for women based on the rate for men: cf=cmNm/Nf
    # 3. define the sex-specific force of infections
    # 4. Write the differential equations
    # 5. Output for the four differential equations and two incidences
    #list(c(
   # ))
  #})
#}
  # Output, write out in list format, internal of list is what changes
    list(c(dS_m,
           dI_m,
           dS_f,
           dI_f,
           si.flow_m = lambda_m * s.num_m,
           si.flow_f = lambda_f * s.num_f))
  })
}

param <- param.dcm(tau_m=0.02,tau_f=0.02, c_m=3,  
                   nu=0.02,mu_s_m=0.01,mu_i_m=0.01,mu_s_f=0.01,mu_i_f=0.01)
init <- init.dcm(s.num_m=999, i.num_m=1, s.num_f=999, i.num_f=1, si.flow_m=0, si.flow_f=0)
control <- control.dcm(nsteps=500, new.mod = SImod)

sim <- dcm(param, init, control)
sim


## --------------------------------------------------------------------------------------------------------------------------
SImod <- function(t, t0, parms) {
  with(as.list(c(t0, parms)), {
    
    # Derived parameters/states for men and women
    num_m <- s.num_m + i.num_m
    num_f <- s.num_f + i.num_f
    c_f <- (c_m*num_m)/num_f #changes, is a fxn of the contact rate of men and other variables
    lambda_m <- (tau_m * c_m) * (i.num_f/num_f) #(tau*c)*(I/N) .num is a state, num is total pop
    lambda_f <- (tau_f * c_f) * (i.num_m/num_m) #(tau*c)*(I/N) .num is a state, num is total pop
    
    # Differential equations
    dS_m <- (-lambda_m*s.num_m) + (nu*(num_f/2)) - (mu_s_m*s.num_m)  #infection=lambda*s.num
    dI_m <- (lambda_m*s.num_m) - (mu_i_m*i.num_m) 
    
    dS_f <- (-lambda_f*s.num_f) + (nu*(num_f/2)) - (mu_s_f*s.num_f)
    dI_f <- (lambda_f*s.num_f) - (mu_i_f*i.num_f)
    # 1. track the total size of men and total size of women
    # 2. define contact rate for women based on the rate for men: cf=cmNm/Nf
    # 3. define the sex-specific force of infections
    # 4. Write the differential equations
    # 5. Output for the four differential equations and two incidences
    #list(c(
   # ))
  #})
#}
  # Output, write out in list format, internal of list is what changes
    list(c(dS_m,
           dI_m,
           dS_f,
           dI_f,
           si.flow_m = lambda_m * s.num_m,
           si.flow_f = lambda_f * s.num_f))
  })
}

param <- param.dcm(tau_m=0.02,tau_f=0.02, c_m=3,  
                   nu=0.02,mu_s_m=0.01,mu_i_m=0.01,mu_s_f=0.01,mu_i_f=0.01)
init <- init.dcm(s.num_m=999, i.num_m=1, s.num_f=999, i.num_f=1, si.flow_m=0, si.flow_f=0)
control <- control.dcm(nsteps=500, new.mod = SImod)

sim <- dcm(param, init, control)
sim

#add 4 new variables:
sim <- dcm(param, init, control)
sim <- mutate_epi(sim, num_m = s.num_m + i.num_m) #mutate_epi to add new variables to dataset
sim <- mutate_epi(sim, num_f = s.num_f + i.num_f)
sim <- mutate_epi(sim, prev_m = i.num_m / num_m)
sim <- mutate_epi(sim, prev_f = i.num_f / num_f)
sim

df <- as.data.frame(sim)
round(head(df, 10), 3)
#plots: (PROPORTIONS for prevalence plots)
par(mfrow = c(2,2), mar = c(3,3,1,1), mgp = c(2,1,0))
plot(sim, y ="prev_m", main = "Disease Prevalence: Men", lwd = 1)
plot(sim, y = "prev_f", main = "Disease Prevalence: Women", lwd = 1)
plot(sim, y = c("si.flow_m"), main="Disease Incidence: Men", lwd=1)
plot(sim, y = c("si.flow_f"), main="Disease Incidence: Women", lwd=1)

#numerical summaries of results: final/end point prevalence, peak prevalence (create prevalence variable and print tail of dataset for women and men) 
head(df)
tail(df)

#men:
peak_prev_m <- max(df$i.num_m)
print(peak_prev_m) #833.33 at day 458
peak_prev_time_m<-458
print(peak_prev_time_m)

#women:
peak_prev_f <- max(df$i.num_f)
print(peak_prev_f) #833.33
peak_prev_time_f<-458
print(peak_prev_time_f) 

tail(df$i.num_m)
tail(df$i.num_f)
#both stay at this peak prevalence from day 458 to day 500 (final day)


## --------------------------------------------------------------------------------------------------------------------------
SImod <- function(t, t0, parms) {
  with(as.list(c(t0, parms)), {
    
    # Derived parameters/states for men and women
    num_m <- s.num_m + i.num_m
    num_f <- s.num_f + i.num_f
    c_f <- (c_m*num_m)/num_f #changes, is a fxn of the contact rate of men and other variables
    lambda_m <- (tau_m * c_m) * (i.num_f/num_f) #(tau*c)*(I/N) .num is a state, num is total pop
    lambda_f <- (tau_f * c_f) * (i.num_m/num_m) #(tau*c)*(I/N) .num is a state, num is total pop
    
    # Differential equations
    dS_m <- (-lambda_m*s.num_m) + (nu*(num_f/2)) - (mu_s_m*s.num_m)  #infection=lambda*s.num
    dI_m <- (lambda_m*s.num_m) - (mu_i_m*i.num_m) 
    
    dS_f <- (-lambda_f*s.num_f) + (nu*(num_f/2)) - (mu_s_f*s.num_f)
    dI_f <- (lambda_f*s.num_f) - (mu_i_f*i.num_f)
  # Output, write out in list format, internal of list is what changes
    list(c(dS_m,
           dI_m,
           dS_f,
           dI_f,
           si.flow_m = lambda_m * s.num_m,
           si.flow_f = lambda_f * s.num_f))
  })
}

param <- param.dcm(tau_m=0.02,tau_f=0.04, c_m=3,  
                   nu=0.02,mu_s_m=0.01,mu_i_m=0.01,mu_s_f=0.01,mu_i_f=0.01)
init <- init.dcm(s.num_m=999, i.num_m=1, s.num_f=999, i.num_f=1, si.flow_m=0, si.flow_f=0)
control <- control.dcm(nsteps=500, new.mod = SImod)

sim <- dcm(param, init, control)
sim

#add 4 new variables:
sim <- dcm(param, init, control)
sim <- mutate_epi(sim, num_m = s.num_m + i.num_m) #mutate_epi to add new variables to dataset
sim <- mutate_epi(sim, num_f = s.num_f + i.num_f)
sim <- mutate_epi(sim, prev_m = i.num_m / num_m)
sim <- mutate_epi(sim, prev_f = i.num_f / num_f)
sim

df <- as.data.frame(sim)
round(head(df, 10), 3)

#plots: (PROPORTIONS for prevalence curves)
par(mfrow = c(1,2), mar = c(3,3,1,1), mgp = c(2,1,0))
plot(sim, y ="prev_m", main = "Disease Prevalence: Men", lwd = 1)
plot(sim, y = "prev_f", main = "Disease Prevalence: Women", lwd = 1)

#calculate the values of female-male prevalence ratio at the final time step
sim<-mutate_epi(sim, prev_ratio=prev_f / prev_m) 
sim<-mutate_epi(sim, trans_ratio= 0.04*((3*num_m)/num_f) / (0.02*3)) 
sim  
df<-as.data.frame(sim)
round(head(df, 10), 3)
prev_ratio_final<-df$prev_ratio[df$time==500]
print(prev_ratio_final) #1.076923


## --------------------------------------------------------------------------------------------------------------------------
SImod <- function(t, t0, parms) {
  with(as.list(c(t0, parms)), {
    
    # Derived parameters/states for men and women
    num_m <- s.num_m + i.num_m
    num_f <- s.num_f + i.num_f
    c_f <- (c_m*num_m)/num_f #changes, is a fxn of the contact rate of men and other variables
    lambda_m <- (tau_m * c_m) * (i.num_f/num_f) #(tau*c)*(I/N) .num is a state, num is total pop
    lambda_f <- (tau_f * c_f) * (i.num_m/num_m) #(tau*c)*(I/N) .num is a state, num is total pop
    
    # Differential equations
    dS_m <- (-lambda_m*s.num_m) + (nu*(num_f/2)) - (mu_s_m*s.num_m)  #infection=lambda*s.num
    dI_m <- (lambda_m*s.num_m) - (mu_i_m*i.num_m) 
    
    dS_f <- (-lambda_f*s.num_f) + (nu*(num_f/2)) - (mu_s_f*s.num_f)
    dI_f <- (lambda_f*s.num_f) - (mu_i_f*i.num_f)
  # Output, write out in list format, internal of list is what changes
    list(c(dS_m,
           dI_m,
           dS_f,
           dI_f,
           si.flow_m = lambda_m * s.num_m,
           si.flow_f = lambda_f * s.num_f))
  })
}

param <- param.dcm(tau_m=0.02,tau_f=0.04, c_m=3,  
                   nu=0.02,mu_s_m=0.01,mu_i_m=0.015,mu_s_f=0.01,mu_i_f=0.015)
init <- init.dcm(s.num_m=999, i.num_m=1, s.num_f=999, i.num_f=1, si.flow_m=0, si.flow_f=0)
control <- control.dcm(nsteps=500, new.mod = SImod)

sim <- dcm(param, init, control)
sim
df<-as.data.frame(sim)
round(head(df, 10), 3)

#calculate dynamic contact rate for women from model and add as a variable to the sim object
sim<-mutate_epi(sim, c_m=3)
sim<-mutate_epi(sim, num_m=s.num_m + i.num_m)
sim<-mutate_epi(sim, num_f=s.num_f + i.num_f)
sim<-mutate_epi(sim, c_f=(c_m*num_m) / (num_f))
sim
df<-as.data.frame(sim)
print(head(df,10))

#plot contact rates and add horizontal line with abline that plots fixed value for men
plot(df$time, df$c_f, type="l", col="blue", lwd=1,
     xlab="Time", 
     ylab="Contact Rate",
     main="Contact Rate Over Time")
abline(h=3, col="red", lwd=1, lty=3)
legend("topright", legend=c("Female Contact Rate", "Male Contact Rate"),
       col=c("blue", "red"), lwd=1, lty=c(1,3))

#c_f at start and end
c_f_start <- df$c_f[df$time == 1]
print(c_f_start)
c_f_end <- df$c_f[df$time == 500]
print(c_f_end)

#plot of men vs women population size
#plot(df$time, df$num_m)
par(mfrow = c(1,2), mar = c(3,3,1,1), mgp = c(2,1,0))
plot(sim, y ="num_m", main = "Population Size: Men", lwd = 1)
plot(sim, y = "num_f", main = "Population Size: Women", lwd = 1)


## --------------------------------------------------------------------------------------------------------------------------
SEIR <- function(t, t0, parms) {
  with(as.list(c(t0, parms)), {

    ## Derived values
    y.num <- s.y.num + e.y.num + i.y.num + r.y.num
    o.num <- s.o.num + e.o.num + i.o.num + r.o.num
    num <- y.num + o.num

    beta1 <- ce1 / y.num
    beta2 <- ce2 / num

    lambda.yy <- beta1 * i.y.num
    lambda.yo <- 0.5*beta2 * i.o.num
    lambda.y <- lambda.yy + lambda.yo

    lambda.oo <- beta2 * i.o.num
    lambda.oy <- 0.5*beta2 * i.y.num
    lambda.o <- lambda.oo + lambda.oy

    gamma <- 1/e.dur
    rho <- 1/i.dur
    alpha <- 1/(10*365)

    ## Differential equations
    dSy <- -lambda.y*s.y.num - alpha*s.y.num
    dEy <- lambda.y*s.y.num - gamma*e.y.num - alpha*e.y.num
    dIy <- gamma*e.y.num - rho*i.y.num - alpha*i.y.num
    dRy <- rho*i.y.num - alpha*r.y.num

    dSo <- -lambda.o*s.o.num + alpha*s.y.num
    dEo <- lambda.o*s.o.num - gamma*e.o.num + alpha*e.y.num
    dIo <- gamma*e.o.num - rho*i.o.num + alpha*i.y.num
    dRo <- rho*i.o.num + alpha*r.y.num

    ## Output
    list(c(dSy, dEy, dIy, dRy,
           dSo, dEo, dIo, dRo))
  })
}

param <- param.dcm(ce1 = 0.34, ce2 = 0.07, e.dur = 10, i.dur = 14)
init <- init.dcm(s.y.num = 1000, e.y.num = 10, i.y.num = 0, r.y.num = 0,
                 s.o.num = 1000, e.o.num = 0, i.o.num = 0, r.o.num = 0)
control <- control.dcm(nsteps = 300, new.mod = SEIR)


## --------------------------------------------------------------------------------------------------------------------------
#ce=effective contact rate
SEIR <- function(t, t0, parms) {
  with(as.list(c(t0, parms)), {

    ## Derived values
    y.num <- s.y.num + e.y.num + i.y.num + r.y.num
    o.num <- s.o.num + e.o.num + i.o.num + r.o.num
    num <- y.num + o.num

    beta1 <- ce1 / y.num
    beta2 <- ce2 / num

    lambda.yy <- beta1 * i.y.num
    lambda.yo <- 0.5*beta2 * i.o.num
    lambda.y <- lambda.yy + lambda.yo

    lambda.oo <- beta2 * i.o.num
    lambda.oy <- 0.5*beta2 * i.y.num
    lambda.o <- lambda.oo + lambda.oy

    gamma <- 1/e.dur
    rho <- 1/i.dur
    alpha <- 1/(10*365)

    ## Differential equations
    dSy <- -lambda.y*s.y.num - alpha*s.y.num
    dEy <- lambda.y*s.y.num - gamma*e.y.num - alpha*e.y.num
    dIy <- gamma*e.y.num - rho*i.y.num - alpha*i.y.num
    dRy <- rho*i.y.num - alpha*r.y.num

    dSo <- -lambda.o*s.o.num + alpha*s.y.num
    dEo <- lambda.o*s.o.num - gamma*e.o.num + alpha*e.y.num
    dIo <- gamma*e.o.num - rho*i.o.num + alpha*i.y.num
    dRo <- rho*i.o.num + alpha*r.y.num

    ## Output
    list(c(dSy, dEy, dIy, dRy,
           dSo, dEo, dIo, dRo))
  })
}

param <- param.dcm(ce1=seq(0.2, 2, 0.1), ce2 = 0.07, e.dur = 10, i.dur = 14)
init <- init.dcm(s.y.num = 1000, e.y.num = 10, i.y.num = 0, r.y.num = 0,
                 s.o.num = 1000, e.o.num = 0, i.o.num = 0, r.o.num = 0)
control <- control.dcm(nsteps = 300, new.mod = SEIR)

sim <- dcm(param, init, control)
sim
d4<-as.data.frame(sim)
#add new prevalence variables
sim<-mutate_epi(sim, y.num=s.y.num + e.y.num + i.y.num + r.y.num)
sim<-mutate_epi(sim, prev.y=i.y.num/y.num)

sim<-mutate_epi(sim, o.num=s.o.num + e.o.num + i.o.num + r.o.num)
sim<-mutate_epi(sim, prev.o=i.o.num/o.num)
df4<-as.data.frame(sim)
round(head(df4, 10), 3)
#4 plots
par(mfrow = c(1,2), mar = c(3,3,1,1), mgp = c(2,1,0))
plot(sim, y ="prev.y", main = "Proportional Prevalence by Varing Effective Contact Rate in Young: Young", lwd = 1, cex.main=0.5)
plot(sim, y = "prev.o", main = "Proportional Prevalence by Varying Effective Contact Rate in Young: Old", lwd = 1, cex.main=0.5)

par(mfrow = c(1,2), mar = c(3,3,1,1), mgp = c(2,1,0))
plot(sim, y ="i.y.num", main = "Prevalence by Varying Effective Contact Rate in Young: Young", lwd = 1, cex.main=0.5)
plot(sim, y = "i.o.num", main = "Prevalence by Varying Effective Contact Rate in Young: Old", lwd = 1, cex.main=0.5)


#numerical summaries/analysis
#prevalence YOUNG:
peak.prev.y <- max(df4$i.y.num)
print(peak.prev.y) #410.4662 
peak.prev.time.y<-df4$time[which.max(df4$i.y.num)]
print(peak.prev.time.y) #at time=27 (run 19)
peak.prev.run.y<-df4$run[which.max(df4$i.y.num)]
print(peak.prev.run.y) #run 19

#proportional prevalence YOUNG:
peak.prop.prev.y <- max(df4$prev.y)
print(peak.prop.prev.y) #0.4093074
peak.prop.prev.time.y<-df4$time[which.max(df4$prev.y)]
print(peak.prop.prev.time.y) #at time=27
peak.prop.prev.run.y<-df4$run[which.max(df4$prev.y)]
print(peak.prop.prev.run.y) #run 19

#prevalence OLD:
peak.prev.o <- max(df4$i.o.num)
print(peak.prev.o) #71.44523 
peak.prev.time.o<-df4$time[which.max(df4$i.o.num)]
print(peak.prev.time.o) #at time=53 
peak.prev.run.o<-df4$run[which.max(df4$i.o.num)]
print(peak.prev.run.o) #run 19

#proportional prevalence OLD:
peak.prop.prev.o <- max(df4$prev.o)
print(peak.prop.prev.o) #0.07043886
peak.prop.prev.time.o<-df4$time[which.max(df4$prev.o)]
print(peak.prop.prev.time.o) #at time=53
peak.prop.prev.run.o<-df4$run[which.max(df4$prev.o)]
print(peak.prop.prev.run.o) #run 19


tail(df4$i.y.num)
tail(df4$i.o.num)
tail(df4$prev.y)
tail(df4$prev.o)


#peak (proportional) prevalence for YOUNG people when the young-young effective contact rate is lowest (run 1) vs highest (run 19)
df_lowest<-as.data.frame(sim, run=1)
#peak proportional prev when ce is lowest:
peak.prop.prev.lowest<-max(df_lowest$prev.y)
print(peak.prop.prev.lowest) #0.1618481

df_highest<-as.data.frame(sim, run=19)
#peak proportional prev when ce is highest:
peak.prop.prev.highest<-max(df_highest$prev.y)
print(peak.prop.prev.highest) #0.4093074


#peak (proportional) prevalence for OLD people when the young-young effective contact rate is lowest (run 1) vs highest (run 19) (already made dataframes)

#peak proportional prev when ce is lowest:
peak.prop.prev.lowest.old<-max(df_lowest$prev.o)
print(peak.prop.prev.lowest.old) #0.04397967

#peak proportional prev when ce is highest:
peak.prop.prev.highest.old<-max(df_highest$prev.o)
print(peak.prop.prev.highest.old) #0.07043886

