## ------------------------------------------------------------------------------------------------------------------
tau <- 0.25
c <- 2
D <- 7

S.init <- 999
I.init <- 1

nsteps <- 100

S <- S.init
I <- I.init
N <- S + I

#for loop
for (t in 1:(nsteps - 1)) {   #inside {} are calculations

# derived variables (might be changing as a fxn of other things that change over time)
  lambda_t <- (tau * c) * (I[t]/N) #I[t]/N might be changing  []=subscript
  gamma <- 1/D  #D=duration of infection, turning into a rate by putting it under 1

# difference equations
  S[t + 1] <- S[t] - (lambda_t * S[t]) + (gamma * I[t]) #lambdat*St=incidence
  I[t + 1] <- I[t] + (lambda_t * S[t]) - (gamma * I[t]) #gamma*It=recovery
  }  #end of for loop

df <- data.frame(S, I)
head(df, 20)

par(mar = c(3,3,1,1), mgp = c(2,1,0), mfrow = c(1,1))
plot(df$S, type = "l", col = "blue", lwd = 2, ylim = c(0, N[1]),
     ylab = "Prevalence", xlab = "Day")
lines(df$I, col = "red", lwd = 2)


## ------------------------------------------------------------------------------------------------------------------
#3
df$lambda_t <- (tau * c) * (df$I/N) #I[t] needs to be changed to df$I

par(mar = c(3,3,1,1), mgp = c(2,1,0), mfrow = c(1,1))
plot(df$lambda_t, type = "l", col = "blue", lwd = 2, ylab = "Force of Infection", xlab = "Day")


## ------------------------------------------------------------------------------------------------------------------
#4
tau <- 0.25
c <- 2
D <- 7

S.init <- 999
I.init <- 1

S <- S.init
I <- I.init
N <- S + I

nsteps<-100

incid <- numeric(nsteps)
recov <- numeric(nsteps)  


#for loop
for (t in 1:(nsteps - 1)) {   #inside {} are calculations

# derived variables (might be changing as a fxn of other things that change over time)
  lambda_t <- (tau * c) * (I[t]/N) #I[t]/N might be changing  []=subscript
  gamma <- 1/D  #D=duration of infection, turning into a rate by putting it under 1
  incid[t+1] <- lambda_t * S[t]
  recov[t+1]<- gamma * I[t]

# difference equations
  S[t + 1] <- S[t] - incid[t+1] + recov[t+1] #lambdat*St=incidence
  I[t + 1] <- I[t] + incid[t+1] - recov[t+1] #gamma*It=recovery
} 

df2 <- data.frame(S, I, incid, recov)

par(mfrow=c(1,2))

plot(df2$S, type = "l", col = "blue", lwd = 2, ylim = c(0, N[1]),
     ylab = "Compartment Size", xlab = "Day") #blue=#susceptible
lines(df2$I, col = "red", lwd = 2) #red=#infected

plot(df2$incid, type = "l", col = "blue", lwd = 2,ylim = c(0, 150),
     ylab = "Flow Size", xlab = "Day") #blue=indcidence
lines(df2$recov, col = "red", lwd = 2) #red=recoveries



## ------------------------------------------------------------------------------------------------------------------
#5
tau <- 0.25
c <- 2
D <- 7

S.init <- 500
I.init <- 500

S <- S.init
I <- I.init
N <- S + I

nsteps<-100

incid <- numeric(nsteps)
recov <- numeric(nsteps)  


#for loop
for (t in 1:(nsteps - 1)) {   #inside {} are calculations

# derived variables (might be changing as a fxn of other things that change over time)
  lambda_t <- (tau * c) * (I[t]/N) #I[t]/N might be changing  []=subscript
  gamma <- 1/D  #D=duration of infection, turning into a rate by putting it under 1
  incid[t+1] <- lambda_t * S[t]
  recov[t+1]<- gamma * I[t]

# difference equations
  S[t + 1] <- S[t] - incid[t+1] + recov[t+1] #lambdat*St=incidence
  I[t + 1] <- I[t] + incid[t+1] - recov[t+1] #gamma*It=recovery
} 

df3 <- data.frame(S, I, incid, recov)

par(mfrow=c(1,2))

plot(df3$S, type = "l", col = "blue", lwd = 2, ylim = c(0, N[1]),
     ylab = "Compartment Size", xlab = "Day") #blue=#susceptible
lines(df3$I, col = "red", lwd = 2) #red=#infected

plot(df3$incid, type = "l", col = "blue", lwd = 2, ylim = c(0, 150),
     ylab = "Flow Size", xlab = "Day") #blue=incidence
lines(df3$recov, col = "red", lwd = 2) #red=recoveries



## ------------------------------------------------------------------------------------------------------------------
#6
R0 <- 2
dur.preinf <- 3
dur.inf <- 2

S.init <- 999
E.init <- 1
I.init <- 0
R.init <- 0

S <- S.init
E <- E.init
I <- I.init
R <- R.init
N <- S + E + I + R

nsteps <- 150

for (t in 1:(nsteps - 1)) {

# derived variables
lambda_t <- R0/(N * dur.inf) * I[t]
gamma <- 1/dur.preinf
rho <- 1/dur.inf
# difference equations--have to write out ourselves
S[t + 1] <- S[t] - (lambda_t * S[t])
E[t + 1] <- E[t] + (lambda_t * S[t]) - (gamma*E[t])
I[t + 1] <- I[t] + (gamma*E[t]) - (rho*I[t])
R[t + 1] <- R[t] + (rho*I[t]) 

} 

df4 <- data.frame(S, E, I, R)

plot(df4$I, type = "l", col = "red", lwd = 2, 
     xlab = "Day", ylab = "Number of Infected", 
     main = "Infected Compartment Over Time")


## ------------------------------------------------------------------------------------------------------------------
#6 continued
peak_prev <- max(df4$I)
print(peak_prev) #64.25424
peak_prev_time<-which.max(df4$I)
print(peak_prev_time) #day 45 time step


## ------------------------------------------------------------------------------------------------------------------
#7
R0 <- 4
dur.preinf <- 3
dur.inf <- 2

S.init <- 999
E.init <- 1
I.init <- 0
R.init <- 0

S <- S.init
E <- E.init
I <- I.init
R <- R.init
N <- S + E + I + R

nsteps <- 150

for (t in 1:(nsteps - 1)) {

# derived variables
lambda_t <- R0/(N * dur.inf) * I[t]
gamma <- 1/dur.preinf
rho <- 1/dur.inf
# difference equations--have to write out ourselves
S[t + 1] <- S[t] - (lambda_t * S[t])
E[t + 1] <- E[t] + (lambda_t * S[t]) - (gamma*E[t])
I[t + 1] <- I[t] + (gamma*E[t]) - (rho*I[t])
R[t + 1] <- R[t] + (rho*I[t]) 

} 

df5 <- data.frame(S, E, I, R)

plot(df5$I, type = "l", col = "red", lwd = 2, 
     xlab = "Day", ylab = "Number of Infected", 
     main = "Infected Compartment Over Time")


## ------------------------------------------------------------------------------------------------------------------
#7 continued
peak_prev2 <- max(df5$I)
print(peak_prev2) #173.81
peak_prev_time2<-which.max(df5$I)
print(peak_prev_time2) #day 25 time step

