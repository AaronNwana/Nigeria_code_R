############ My MLE Code###############################
require(epimdr)
require(deSolve)
require(ggplot2)
require(zoo)
require(xts) ## for time series date
require(bbmle)
#Data ----
setwd("C:/Users/Brain Computers/Desktop/R-files")
act<-read.csv("Ekitistate.csv")
View(act)


dates_act<-seq(as.Date("2014/1/1"), as.Date("2017/12/1"), by="month")
case1<-act$Total.ACT.Distributed
mcases<-act$Total.Malaria.Cases


plot(case1~dates_act, data = act, col="blue", main="Malaria cases & ACT distributed",
     type="l", xlab="Time(months)", ylab = "ACT", lwd="2")
par (new=TRUE)
plot(mcases, col="red",
     type="l",lwd="2", xlab = "", ylab = "", xaxt="n", yaxt="n")


#Model ----
seir_model<-function(t, state, parameters) {
  with(as.list(c(state, parameters)),
       {
         
         # define variables
         N_h <- (S_h+E_h+I_h+A_h+T_h+R_h)
         N_v <- (S_v+E_v+I_v)
         lambda_1<-lambda_v *(1+amp*cos(2*pi*(t-phi)/12))
         
         # rate of change
         dS_h <- lambda_h + gamma*R_h -(beta_h*theta_v*I_v*S_h/N_h) - mu_h*S_h
         dE_h <- (beta_h*theta_v*I_v*S_h/N_h) - (eta + mu_h)*E_h
         dI_h <- eta*alpha*E_h + (rho_1*beta_h*theta_v*I_v*A_h/N_h) - (tau+delta_h+mu_h)*I_h
         dA_h <- eta*(1-alpha)*E_h + epsilon*(1-q)*T_h - (rho_1*beta_h*theta_v*I_v*A_h/N_h) - (omega+mu_h)*A_h
         dT_h<-  tau*I_h-(epsilon+mu_h)*T_h    
         dR_h <- epsilon*q*T_h + omega*A_h - (gamma+mu_h)*R_h
         dY_h <- eta*alpha*E_h + (rho_1*beta_h*theta_v*I_v*A_h/N_h)
         dX_h = tau*I_h
         dS_v <- lambda_1 - (beta_v*theta_v*S_v*(rho_2*I_h+A_h)/N_h) - mu_v*S_v
         dE_v <- (beta_v*theta_v*S_v*(rho_2*I_h+A_h)/N_h) - (eta_v + mu_v)*E_v
         dI_v <- eta_v*E_v- mu_v*I_v
         
         # return the rate of change
         list(c(dS_h, dE_h, dI_h, dA_h,dT_h, dR_h, dY_h, dX_h, dS_v, dE_v, dI_v))
       }
  )
  
}


# Likelihood function ----
llfn_malaria <- function(p,x){
  time <- seq(0,length(act$Month), 1)
  initial_state<- c(S_h = 2904307 , E_h = 0, I_h = 8120, A_h = 4*8120, T_h =3392,R_h = 3155,
                    Y_h =0, X_h=0 , S_v = 6037266, E_v=0, I_v=886845)
  
  
  parameters <- c(lambda_h = 4376, mu_h = 0.0015, delta_h = 0.000102, eta = 3,
                  alpha= p[1], omega = 1/9.5, tau = 0.54, epsilon = 11.397, q = p[2], gamma = p[3],
                  beta_h = p[4], lambda_v = 28926130, mu_v = 2, eta_v = 3.33,
                  theta_v = p[5], beta_v = p[6], rho_1 = p[7],rho_2 = p[8], amp= p[9], phi= p[10])
  
  outcome <- as.data.frame(ode(y = initial_state, times = time, func = seir_model,
                               parms = parameters))
  predinci= c(initial_state["I_h"], diff(outcome$Y_h))
  par(mfrow = c(1,1))
  # lss <- -sum(dpois(x, lambda  = predinci, log = TRUE))
  plot(outcome$time,predinci, type ="l", col = "purple", lwd = 2, ylab = "Incidence")
  lines(x, col = "red", lwd =2, type = "l")# xaxt = "n", yaxt = "n")
  legend("bottomright",col = c("purple", "red"),legend = c("Model",
                                                           "Data"),lty = c(1,1), lwd = c(2,2), cex = 0.6)
  lss <- -sum(dpois(x, lambda  = predinci, log = TRUE))
  #lss <- c((sum(predinci-x)^2)) #sum(predtreat-a)^2)
  
  
  return(lss)
}
param0 =  c(0.14,0.8, 0.02,  0.035, 12, 0.2, 0.21, 0.3, 0.17,3)
malaria_fit0=optim(param0,llfn_malaria, x = mcases, method = "L-BFGS-B",
                   lower = c(0.11, 0.01,0.0018, 0.03, 2.38, 0.016, 0.01, 0.10, 0.06,-10),
                   upper = c(1,1, 1, 0.333,15, 1, 1, 1, 0.27,10 ))
par(mfrow = c(1,1))
malaria_fit0$par
#########################MLE CODE#####################################
#MLE
llfn_malaria <- function(p,x){
  time <- seq(0,length(act$Month), 1)
  state <- c(S_h = 2904307 , E_h = 0, I_h = 8120, A_h = 4*8120, T_h =3392,R_h = 3155,
             Y_h =0, X_h=0 , S_v = 6037266, E_v=0, I_v=886845)
  
  
  parameters <- c(lambda_h = 4376, mu_h = 0.0015, delta_h = 0.000102, eta = 3,
                  alpha= p[1], omega = 1/9.5, tau = 0.54, epsilon = 11.397, q = p[2], gamma = p[3],
                  beta_h = p[4], lambda_v = 28926130, mu_v = 2, eta_v = 3.33,
                  theta_v = p[5], beta_v = p[6], rho_1 = p[7],rho_2 = p[8], amp= p[9], phi= p[10])
  
  outcome <- as.data.frame(ode(y = state, times = time, func = seir_model,
                               parms = parameters))
  predinci= c(state["I_h"], diff(outcome$Y_h))
  
  plot(outcome$time,predinci, type ="l", col = "purple", lwd = 2, ylab = "Incidence")
  lines(x, col = "red", lwd =2, type = "p")# xaxt = "n", yaxt = "n")
  legend("bottomright",col = c("purple", "red"),legend = c("Model",
                                                           "Data"),lty = c(1,1), lwd = c(2,2), cex = 0.6)
  
  
  
  lss <- -sum(dnbinom(x,size=10.94817, mu  = predinci, log = TRUE))
  
  
  return(lss)
}


#malaria_fit1$par
param1 <- malaria_fit0$par
malaria_fit1=optim(param1,llfn_malaria, x = mcases, method = "L-BFGS-B",
                   lower = c(0.11, 0.01,0.0018, 0.03, 2.38, 0.016, 0.01, 0.10, 0.06,-10),
                   upper = c(1,1, 1, 0.333,15, 1, 1, 1, 0.27,10 ))


malaria_fit1$par
malaria_fit0$par
par0 <- malaria_fit0$par

###############################################################
############ My MLE Code 2#####################################
require(epimdr)
require(deSolve)
require(ggplot2)
require(zoo)
require(xts) ## for time series date
require(bbmle)
#Data ----
setwd("C:/Users/Brain Computers/Desktop/R-files")
act<-read.csv("Ekitistate.csv")
View(act)


dates_act<-seq(as.Date("2014/1/1"), as.Date("2017/12/1"), by="month")
case1<-act$Total.ACT.Distributed
mcases<-act$Total.Malaria.Cases


plot(case1~dates_act, data = act, col="blue", main="Malaria cases & ACT distributed",
     type="l", xlab="Time(months)", ylab = "ACT", lwd="2")
par (new=TRUE)
plot(mcases, col="red",
     type="l",lwd="2", xlab = "", ylab = "", xaxt="n", yaxt="n")


#Model ----
seir_model<-function(t, state, parameters) {
  with(as.list(c(state, parameters)),
       {
         
         # define variables
         N_h <- (S_h+E_h+I_h+A_h+T_h+R_h)
         N_v <- (S_v+E_v+I_v)
         lambda_1<-lambda_v *(1+amp*cos(2*pi*(t-phi)/12))
         
         # rate of change
         dS_h <- lambda_h + gamma*R_h -(beta_h*theta_v*I_v*S_h/N_h) - mu_h*S_h
         dE_h <- (beta_h*theta_v*I_v*S_h/N_h) - (eta + mu_h)*E_h
         dI_h <- eta*alpha*E_h + (rho_1*beta_h*theta_v*I_v*A_h/N_h) - (tau+delta_h+mu_h)*I_h
         dA_h <- eta*(1-alpha)*E_h + epsilon*(1-q)*T_h - (rho_1*beta_h*theta_v*I_v*A_h/N_h) - (omega+mu_h)*A_h
         dT_h<-  tau*I_h-(epsilon+mu_h)*T_h    
         dR_h <- epsilon*q*T_h + omega*A_h - (gamma+mu_h)*R_h
         dY_h <- eta*alpha*E_h + (rho_1*beta_h*theta_v*I_v*A_h/N_h)
         dX_h = tau*I_h
         dS_v <- lambda_1 - (beta_v*theta_v*S_v*(rho_2*I_h+A_h)/N_h) - mu_v*S_v
         dE_v <- (beta_v*theta_v*S_v*(rho_2*I_h+A_h)/N_h) - (eta_v + mu_v)*E_v
         dI_v <- eta_v*E_v- mu_v*I_v
         
         # return the rate of change
         list(c(dS_h, dE_h, dI_h, dA_h,dT_h, dR_h, dY_h, dX_h, dS_v, dE_v, dI_v))
       }
  )
  
}


# Likelihood function ----
llfn_malaria <- function(p,x){
  time <- seq(0,length(act$Month), 1)
  initial_state<- c(S_h = 2904307 , E_h = 0, I_h = 8120, A_h = 4*8120, T_h =3392,R_h = 3155,
                    Y_h =0, X_h=0 , S_v = 6037266, E_v=0, I_v=886845)
  
  
  parameters <- c(lambda_h = 4376, mu_h = 0.0015, delta_h = 0.000102, eta = 3,
                  alpha= p[1], omega = 1/9.5, tau = 0.54, epsilon = 11.397, q = p[2], gamma = p[3],
                  beta_h = p[4], lambda_v = 28926130, mu_v = 2, eta_v = 3.33,
                  theta_v = p[5], beta_v = p[6], rho_1 = p[7],rho_2 = p[8], amp= p[9], phi= p[10])
  
  outcome <- as.data.frame(ode(y = initial_state, times = time, func = seir_model,
                               parms = parameters))
  predinci= c(initial_state["I_h"], diff(outcome$Y_h))
  
  #lss <- -sum(dpois(x, lambda  = predinci, log = TRUE))
  plot(outcome$time,predinci, type ="l", col = "purple", lwd = 2, ylab = "Incidence")
  lines(x, col = "red", lwd =2, type = "l")# xaxt = "n", yaxt = "n")
  legend("bottomright",col = c("purple", "red"),legend = c("Model",
                                                           "Data"),lty = c(1,1), lwd = c(2,2), cex = 0.6)
  lss <- -sum(dpois(x, lambda  = predinci, log = TRUE))
  #lss <- c((sum(predinci-x)^2), sum(predtreat-a)^2)
  
  
  return(lss)
}
param0 =  c(0.14,0.8, 0.02,  0.035, 12, 0.2, 0.21, 0.3, 0.17,3)
malaria_fit0=optim(param0,llfn_malaria, x = mcases, method = "L-BFGS-B",
                   lower = c(0.11, 0.01,0.0018, 0.03, 2.38, 0.016, 0.01, 0.10, 0.06,-10),
                   upper = c(1,1, 1, 0.333,15, 1, 1, 1, 0.27,10 ))


#########################MLE CODE#####################################
llfn_malaria <- function(p,x){
  time <- seq(0,length(act$Month), 1)
  state <- c(S_h = 2904307 , E_h = 0, I_h = 8120, A_h = 4*8120, T_h =3392,R_h = 3155,
             Y_h =0, X_h=0 , S_v = 6037266, E_v=0, I_v=886845)
  
  
  parameters <- c(lambda_h = 4376, mu_h = 0.0015, delta_h = 0.000102, eta = 3,
                  alpha= p[1], omega = 1/9.5, tau = 0.54, epsilon = 11.397, q = p[2], gamma = p[3],
                  beta_h = p[4], lambda_v = 28926130, mu_v = 2, eta_v = 3.33,
                  theta_v = p[5], beta_v = p[6], rho_1 = p[7],rho_2 = p[8], amp= p[9], phi= p[10])
  
  outcome <- as.data.frame(ode(y = state, times = time, func = seir_model,
                               parms = parameters))
  predinci= c(state["I_h"], diff(outcome$Y_h))
  
  plot(outcome$time,predinci, type ="l", col = "purple", lwd = 2, ylab = "Incidence")
  lines(x, col = "red", lwd =2, type = "p")# xaxt = "n", yaxt = "n")
  legend("bottomright",col = c("purple", "red"),legend = c("Model",
                                                           "Data"),lty = c(1,1), lwd = c(2,2), cex = 0.6)
  
  
  
  lss <- -sum(dnbinom(x,size=10.94817, mu  = predinci, log = TRUE))
  
  
  return(lss)
}


malaria_fit0$par
param1 <- malaria_fit0$par
malaria_fit1=optim(param1,llfn_malaria, x = mcases, method = "L-BFGS-B",
                   lower = c(0.11, 0.01,0.0018, 0.03, 2.38, 0.016, 0.01, 0.10, 0.06,-10),
                   upper = c(1,1, 1, 0.333,15, 1, 1, 1, 0.27,10 ))


malaria_fit0$par
malaria_fit1$par
par0 <- malaria_fit0$par

#########################TO include the 95% CI#########################
library(deSolve)

# Define your likelihood function
llfn_malaria <- function(p, x) {
  time <- seq(0, length(act$Month), 1)
  state <- c(S_h = 2904307 , E_h = 0, I_h = 8120, A_h = 4 * 8120, T_h = 3392, R_h = 3155,
             Y_h = 0, X_h = 0 , S_v = 6037266, E_v = 0, I_v = 886845)
  
  parameters <- c(lambda_h = 4376, mu_h = 0.0015, delta_h = 0.000102, eta = 3,
                  alpha = p[1], omega = 1/9.5, tau = 0.54, epsilon = 11.397, q = p[2], gamma = p[3],
                  beta_h = p[4], lambda_v = 28926130, mu_v = 2, eta_v = 3.33,
                  theta_v = p[5], beta_v = p[6], rho_1 = p[7], rho_2 = p[8], amp = p[9], phi = p[10])
  
  # Simulate model
  outcome <- as.data.frame(ode(y = state, times = time, func = seir_model, parms = parameters))
  predinci <- c(state["I_h"], diff(outcome$Y_h))
  
  # Calculate 95% confidence intervals
  # Assuming residuals are normally distributed; this could be adapted based on actual data
  se_pred <- sqrt(predinci) # Simplified standard error calculation; adjust as needed
  ci_upper <- predinci + 1.96 * se_pred
  ci_lower <- pmax(0, predinci - 1.96 * se_pred) # Ensure CI lower bound is not below 0
  
  # Plotting
  plot(outcome$time, predinci, type = "l", col = "purple", lwd = 2, ylab = "Incidence")
  lines(x, col = "red", lwd = 2, type = "p") # Observed data points
  
  # Add confidence interval shading
  polygon(c(outcome$time, rev(outcome$time)), c(ci_upper, rev(ci_lower)),
          col = rgb(0.5, 0.5, 0.5, 0.3), border = NA) # Grey shading for CI
  
  # Add legend
  legend("bottomright", col = c("purple", "red"), legend = c("Model", "Data"),
         lty = c(1, 1), lwd = c(2, 2), cex = 0.6)
  
  # Calculate negative log-likelihood
  lss <- -sum(dnbinom(x, size = 10.94817, mu = predinci, log = TRUE))
  
  return(lss)
}

# Optimization call
malaria_fit1 <- optim(malaria_fit0$par, llfn_malaria, x = mcases, method = "L-BFGS-B",
                      lower = c(0.11, 0.01, 0.0018, 0.03, 2.38, 0.016, 0.01, 0.10, 0.06, -10),
                      upper = c(1, 1, 1, 0.333, 15, 1, 1, 1, 0.27, 10))

result <- data.frame(
  Parameter = names,
  Estimate = malaria_fit1,
  SE = se_pred,
  CI_Lower = ci_lower,
  CI_Upper = ci_upper
)
print(result)



####################### New data################################
require(epimdr)
require(deSolve)
require(ggplot2)
require(zoo)
require(xts) ## for time series date
require(bbmle)
#Data ----
setwd("C:/Users/Brain Computers/Desktop/R-files")
act<-read.csv("ACT_Ekiti.csv")
View(act)


dates_act<-seq(as.Date("2015/1/15"), as.Date("2024/05/24"), by="month")
case1<-act$Persons.with.Confirmed.Uncomplicated.Malaria.treated.with.ACT
mcases<-act$Confirmed.uncomplicated.Malaria


plot(case1~dates_act, data = act, col="blue", main="Malaria cases & ACT distributed",
     type="l", xlab="Time(months)", ylab = "ACT", lwd="2")
par (new=TRUE)
plot(mcases, col="red",
     type="l",lwd="2", xlab = "", ylab = "", xaxt="n", yaxt="n")


#Model ----
seir_model<-function(t, state, parameters) {
  with(as.list(c(state, parameters)),
       {
         
         # define variables
         N_h <- (S_h+E_h+I_h+A_h+T_h+R_h)
         N_v <- (S_v+E_v+I_v)
         lambda_1<-lambda_v *(1+amp*cos(2*pi*(t-phi)/12))
         
         # rate of change
         dS_h <- lambda_h + gamma*R_h -(beta_h*theta_v*I_v*S_h/N_h) - mu_h*S_h
         dE_h <- (beta_h*theta_v*I_v*S_h/N_h) - (eta + mu_h)*E_h
         dI_h <- eta*alpha*E_h + (rho_1*beta_h*theta_v*I_v*A_h/N_h) - (tau+delta_h+mu_h)*I_h
         dA_h <- eta*(1-alpha)*E_h + epsilon*(1-q)*T_h - (rho_1*beta_h*theta_v*I_v*A_h/N_h) - (omega+mu_h)*A_h
         dT_h<-  tau*I_h-(epsilon+mu_h)*T_h    
         dR_h <- epsilon*q*T_h + omega*A_h - (gamma+mu_h)*R_h
         dY_h <- eta*alpha*E_h + (rho_1*beta_h*theta_v*I_v*A_h/N_h)
         dX_h = tau*I_h
         dS_v <- lambda_1 - (beta_v*theta_v*S_v*(rho_2*I_h+A_h)/N_h) - mu_v*S_v
         dE_v <- (beta_v*theta_v*S_v*(rho_2*I_h+A_h)/N_h) - (eta_v + mu_v)*E_v
         dI_v <- eta_v*E_v- mu_v*I_v
         
         # return the rate of change
         list(c(dS_h, dE_h, dI_h, dA_h,dT_h, dR_h, dY_h, dX_h, dS_v, dE_v, dI_v))
       }
  )
  
}


# Likelihood function ----
llfn_malaria <- function(p,x){
  time <- seq(0,length(act$periodname), 1)
  initial_state<- c(S_h = 2904307 , E_h = 0, I_h = 8120, A_h = 4*8120, T_h =3392,R_h = 3155,
                    Y_h =0, X_h=0 , S_v = 6037266, E_v=0, I_v=886845)
  
  
  parameters <- c(lambda_h = 4376, mu_h = 0.0015, delta_h = 0.000102, eta = 3,
                  alpha= p[1], omega = 1/9.5, tau = 0.54, epsilon = 11.397, q = p[2], gamma = p[3],
                  beta_h = p[4], lambda_v = 28926130, mu_v = 2, eta_v = 3.33,
                  theta_v = p[5], beta_v = p[6], rho_1 = p[7],rho_2 = p[8], amp= p[9], phi= p[10])
  
  outcome <- as.data.frame(ode(y = initial_state, times = time, func = seir_model,
                               parms = parameters))
  predinci= c(initial_state["I_h"], diff(outcome$Y_h))
  
  #lss <- -sum(dpois(x, lambda  = predinci, log = TRUE))
  plot(outcome$time,predinci, type ="l", col = "purple", lwd = 2, ylab = "Incidence")
  lines(x, col = "red", lwd =2, type = "l")# xaxt = "n", yaxt = "n")
  legend("bottomright",col = c("purple", "red"),legend = c("Model",
                                                           "Data"),lty = c(1,1), lwd = c(2,2), cex = 0.6)
  lss <- -sum(dpois(x, lambda  = predinci, log = TRUE))
  #lss <- c((sum(predinci-x)^2), sum(predtreat-a)^2)
  
  
  return(lss)
}
param0 =  c(0.14,0.8, 0.02,  0.035, 12, 0.2, 0.21, 0.3, 0.17,3)
malaria_fit0=optim(param0,llfn_malaria, x = mcases, method = "L-BFGS-B",
                   lower = c(0.11, 0.01,0.0018, 0.03, 2.38, 0.016, 0.01, 0.10, 0.06,-10),
                   upper = c(1,1, 1, 0.333,15, 1, 1, 1, 0.27,10 ))

##########################MLE#######################################
#Model ----
seir_model<-function(t, state, parameters) {
  with(as.list(c(state, parameters)),
       {
         
         # define variables
         N_h <- (S_h+E_h+I_h+A_h+T_h+R_h)
         N_v <- (S_v+E_v+I_v)
         lambda_1<-lambda_v *(1+amp*cos(2*pi*(t-phi)/12))
         
         # rate of change
         dS_h <- lambda_h + gamma*R_h -(beta_h*theta_v*I_v*S_h/N_h) - mu_h*S_h
         dE_h <- (beta_h*theta_v*I_v*S_h/N_h) - (eta + mu_h)*E_h
         dI_h <- eta*alpha*E_h + (rho_1*beta_h*theta_v*I_v*A_h/N_h) - (tau+delta_h+mu_h)*I_h
         dA_h <- eta*(1-alpha)*E_h + epsilon*(1-q)*T_h - (rho_1*beta_h*theta_v*I_v*A_h/N_h) - (omega+mu_h)*A_h
         dT_h<-  tau*I_h-(epsilon+mu_h)*T_h    
         dR_h <- epsilon*q*T_h + omega*A_h - (gamma+mu_h)*R_h
         dY_h <- eta*alpha*E_h + (rho_1*beta_h*theta_v*I_v*A_h/N_h)
         dX_h = tau*I_h
         dS_v <- lambda_1 - (beta_v*theta_v*S_v*(rho_2*I_h+A_h)/N_h) - mu_v*S_v
         dE_v <- (beta_v*theta_v*S_v*(rho_2*I_h+A_h)/N_h) - (eta_v + mu_v)*E_v
         dI_v <- eta_v*E_v- mu_v*I_v
         
         # return the rate of change
         list(c(dS_h, dE_h, dI_h, dA_h,dT_h, dR_h, dY_h, dX_h, dS_v, dE_v, dI_v))
       }
  )
  
}


llfn_malaria <- function(p, x) {
  # Define the time sequence correctly
  time <- seq(0, length(act$periodname) - 1, 1) # Ensure length is correct
  
  # Initial state
  state <- c(S_h = 2904307, E_h = 0, I_h = 8120, A_h = 4 * 8120, T_h = 3392, R_h = 3155,
             Y_h = 0, X_h = 0, S_v = 6037266, E_v = 0, I_v = 886845)
  
  # Define parameters
  parameters <- c(lambda_h = 4376, mu_h = 0.0015, delta_h = 0.000102, eta = 3,
                  alpha = p[1], omega = 1/9.5, tau = 0.54, epsilon = 11.397, q = p[2], gamma = p[3],
                  beta_h = p[4], lambda_v = 28926130, mu_v = 2, eta_v = 3.33,
                  theta_v = p[5], beta_v = p[6], rho_1 = p[7], rho_2 = p[8], amp = p[9], phi = p[10])
  
  # Ensure time sequence is valid
  if (any(is.na(time)) || length(time) <= 1 || any(diff(time) <= 0)) {
    stop("Time sequence is invalid: contains NA, non-increasing values, or is too short.")
  }
  
  # Simulate model using the ode solver
  outcome <- as.data.frame(ode(y = state, times = time, func = seir_model, parms = parameters, hmax = max(diff(time))))
  
  # Predicted incidence
  predinci <- c(state["I_h"], diff(outcome$Y_h))
  
  # Calculate 95% confidence intervals
  se_pred <- sqrt(predinci) # Simplified standard error calculation; adjust as needed
  ci_upper <- predinci + 1.96 * se_pred
  ci_lower <- pmax(0, predinci - 1.96 * se_pred) # Ensure CI lower bound is not below 0
  
  # Plotting
  plot(outcome$time, predinci, type = "l", col = "purple", lwd = 2, ylab = "Incidence")
  lines(x, col = "red", lwd = 2, type = "p") # Observed data points
  
  # Add confidence interval shading
  polygon(c(outcome$time, rev(outcome$time)), c(ci_upper, rev(ci_lower)),
          col = rgb(0.5, 0.5, 0.5, 0.3), border = NA) # Grey shading for CI
  
  # Add legend
  legend("bottomright", col = c("purple", "red"), legend = c("Model", "Data"),
         lty = c(1, 1), lwd = c(2, 2), cex = 0.6)
  
  # Calculate negative log-likelihood
  lss <- -sum(dnbinom(x, size = 1.844351, mu = predinci, log = TRUE))
  
  #penalty <- sum((p - prior_mean)^2 / prior_variance)  # Example regularization
  
  # Return the penalized likelihood
  #return(lss + penalty)
  
  return(lss)
}

# Optimization call
# malaria_fit1 <- optim(malaria_fit0$par, llfn_malaria, x = mcases, method = "L-BFGS-B",#"Nelder-Mead",
#                       lower = c(0.11, 0.01, 0.0018, 0.03, 2.38, 0.016, 0.01, 0.10, 0.06, -10),
#                       upper = c(1, 1, 1, 0.333, 15, 1, 1, 1, 0.27, 10), 
#                       hessian = TRUE)
param0 =  c(0.14,0.8, 0.02,  0.035, 12, 0.2, 0.21, 0.3, 0.17,5)
malaria_fit1 <- optim(param0, llfn_malaria, x = mcases, method = "L-BFGS-B",#"Nelder-Mead",
                      lower = c(0.11, 0.01, 0.0018, 0.03, 2.38, 0.016, 0.01, 0.10, 0.06, 0),
                      upper = c(1, 1, 1, 0.333, 15, 1, 1, 1, 0.27, 12), 
                      hessian = TRUE)
# Extract parameter estimates
param_estimates <- malaria_fit1$par

# Check if optimization converged
if (malaria_fit1$convergence == 0) {
  hessian <- malaria_fit1$hessian
  
  if (!is.null(hessian) && all(diag(hessian) > 0)) {
    # Regularize Hessian to prevent singularity
    hessian_regularized <- hessian + diag(1e-6, nrow(hessian))
    
    # Calculate the covariance matrix and standard errors
    cov_matrix <- solve(hessian_regularized)
    se <- sqrt(diag(cov_matrix))
    
    # Calculate 95% confidence intervals
    ci_lower <- param_estimates - 1.96 * se
    ci_upper <- param_estimates + 1.96 * se
  } else {
    warning("Hessian is singular or not positive definite, CIs cannot be reliably calculated.")
    se <- rep(NA, length(param_estimates)) # Assign NAs if Hessian is not usable
    ci_lower <- rep(NA, length(param_estimates))
    ci_upper <- rep(NA, length(param_estimates))
  }
} else {
  warning("Optimization did not converge, CIs cannot be reliably calculated.")
  se <- rep(NA, length(param_estimates))
  ci_lower <- rep(NA, length(param_estimates))
  ci_upper <- rep(NA, length(param_estimates))
}

# Output parameter estimates with confidence intervals
result <- data.frame(
  Parameter = names,
  Estimate = param_estimates,
  SE = se,
  CI_Lower = ci_lower,
  CI_Upper = ci_upper
)
print(result)

a<- mean(mcases)
b<- var(mcases)
size <- a^2/(b-a)
size
##################################################
# Load necessary library
library(ggplot2)

# Assuming act is the dataframe and the dates_act is a sequence of dates for plotting
act$dates_act <- seq(as.Date("2015/1/15"), as.Date("2024/05/24"), by = "month")

# Create the ggplot
ggplot(act, aes(x = dates_act)) +
  
  # First line for Persons with Confirmed Uncomplicated Malaria treated with ACT
  geom_line(aes(y = Persons.with.Confirmed.Uncomplicated.Malaria.treated.with.ACT, color = "ACT Cases"), linewidth = 1.2)+
  
  # Second line for Confirmed uncomplicated Malaria cases
  geom_line(aes(y = Confirmed.uncomplicated.Malaria, color = "Malaria Cases"), linewidth = 1.2) +
  
  # Labels and title
  labs(title = "Malaria Cases & ACT Distributed Over Time", 
       x = "Time (months)", 
       y = "Number of Cases") +
  
  # Customize colors
  scale_color_manual(values = c("ACT Cases" = "blue", "Malaria Cases" = "red")) +
  
  # Format the x-axis for dates
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  
  # Theme adjustments for better visualization
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
################################################################################
# Corrected ggplot code to show only ACT cases
ggplot(act, aes(x = dates_act)) +
  
  # Plotting ACT treatment line
  geom_line(aes(y = Persons.with.Confirmed.Uncomplicated.Malaria.treated.with.ACT, color = "ACT treatment"), size = 1.2) +  
  
  # Labels and title
  labs(title = "", 
       x = "Time (months)", 
       y = "Number of cases treated with ACT") +
  
  # Customize colors
  scale_color_manual(values = c("ACT treatment" = "blue")) +
  
  # Format the x-axis for dates
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  
  # Theme adjustments for better visualization
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
###############################################################
# Corrected ggplot code to show ACT cases as a bar plot
ggplot(act, aes(x = dates_act, y = Persons.with.Confirmed.Uncomplicated.Malaria.treated.with.ACT)) +
  
  # Plotting ACT treatment as bars
  geom_col(fill = "blue") +  
  
  # Labels and title
  labs(title = "ACT Treatment Over Time", 
       x = "Time (months)", 
       y = "Number of Cases") +
  
  # Format the x-axis for dates
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  
  # Theme adjustments for better visualization
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
