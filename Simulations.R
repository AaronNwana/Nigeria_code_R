getwd()
setwd("C:/Users/Brain Computers/Desktop/R-files")
require(deSolve)
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



times<-seq(0,48, by=1/30)
#date_data<-seq(as.Date("2014/1/1"), as.Date("2017/12/1"), by="month")
parameters <- c(lambda_h = 4376, mu_h = 0.0015, delta_h = 0.000102, eta = 3,
                alpha=0.2, omega = 1/9.5, tau = 0.54, epsilon = 11.397, q = 0.8, gamma = 0.28,
                beta_h = 0.035,rho_1 = 0.95, lambda_v = 28926130, mu_v = 2,
                eta_v = 2.73, theta_v = 13.2, beta_v = 0.48, rho_2 = 0.85, amp=1, phi= 7.7)
#theta_h = 190.5, omega_1 = 0.5,


initN_h<-2951454 #2892613
initN_v<-6924111 #14463065


initE_h<-0
initI_h<- 8120
initA_h<-4*8120
initT_h<- 3392
#initM_h<-0
initR_h<-3155
initY <- 0#8120
initX <- 0#6900
initE_v<-0
initI_v<-886845




initS_h<-initN_h-initE_h-initI_h-initA_h-initT_h-initR_h
initS_v<-initN_v-initE_v-initI_v
state <- c(S_h = initS_h, E_h=initE_h, I_h = initI_h,A_h = initA_h,T_h = initT_h,
           R_h = initR_h,Y_h = initY , X_h = initX, S_v = initS_v, E_v = initE_v, I_v = initI_v)


# set up a function to solve the model
seir_model<-function(t, state, parameters)
{
  with(as.list(c(state, parameters)),
       {
         
         # define variables
         N_h <- (S_h+E_h+I_h+A_h+T_h+R_h)
         N_v <- (S_v+E_v+I_v)
         lambda_1<-lambda_v *(1+amp*cos(2*pi*(t-phi)/12))
         #beta_1<- beta_h*(1+amp*cos(2*pi*(t-phi)/12))
         #beta<-(R0*(muo+nui)*(gamma+muo))/gamma
         #lam <- beta*seas*I/P
         
         # rate of change
         dS_h <- lambda_h + gamma*R_h -(beta_h*theta_v*I_v*S_h/N_h) - mu_h*S_h
         dE_h <- (beta_h*theta_v*I_v*S_h/N_h) - (eta + mu_h)*E_h
         dI_h <- eta*alpha*E_h + (rho_1*beta_h*theta_v*I_v*A_h/N_h) - (tau+delta_h+mu_h)*I_h
         dA_h <- eta*(1-alpha)*E_h + epsilon*(1-q)*T_h - (rho_1*beta_h*theta_v*I_v*A_h/N_h) - (omega+mu_h)*A_h
         dT_h<-  tau*I_h-(epsilon+mu_h)*T_h
         #dM_h<-  (beta_1*theta_h*I_v*S_h/N_h)    
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


out <- ode(y = state, times = times, func = seir_model, parms = parameters)
pop<-out[,"S_h"]+out[,"E_h"]+out[,"I_h"]+out[,"A_h"]+out[,"T_h"]+out[,"R_h"]+out[,"S_v"]+
  out[,"E_v"]+out[,"I_v"]
time<-out[,"time"]
View(out)


incidence <- c(initY, diff(out[,8]))
incidence
treatment <- c(initX, diff(out[,9]))
treatment
par(mfrow = c(3,3), mar = c(2,2,1,1))
plot(time, incidence,  xlab="Time", ylab = "malaria", type = "l", lwd = 3, col = "purple", main = "Incidence")


plot(time, out[,"S_h"],  xlab="Time", ylab = "malaria", type = "l", lwd = 3, col = "purple", main = "Susceptible population")
plot(time, out[,"E_h"], xlab="Time", ylab = "malaria", type = "l", lwd = 3, col = "blue", main = "Exposed population to malaria")
plot(time, out[,"I_h"], xlab="Time", ylab = "malaria", type = "l", lwd = 3, col = "red", main = "Infected population with malaria", xaxt="n", yaxt="n")
plot(time, out[,"A_h"], xlab="Time", ylab = "malaria", type = "l", lwd = 3, col = "orange", main = "Asymptomatic population with malaria")
plot(time, out[,"T_h"], xlab="Time", ylab = "malaria", type = "l", lwd = 3, col = "grey", main = "Treated population", xaxt="n", yaxt="n")
plot(time, out[,"R_h"], xlab="Time", ylab = "malaria", type = "l", lwd = 3, col = "green", main = "Recovered population from malaria")
plot(time, out[,"S_v"], xlab="Time", ylab = "malaria vector", type = "l", lwd = 3, col = "purple", main = "Susceptible vector")
plot(time, out[,"E_v"], xlab="Time", ylab = "malaria vector", type = "l", lwd = 3, col = "blue", main = "Exposed vector")
plot(time, out[,"I_v"], xlab="Time", ylab = "malaria vector", type = "l", lwd = 3, col = "red", main = "Infected vector")


##################################################
#par(new=TRUE)
lines (mcases, col="red",type="p",lwd="3", xlab = "", ylab = "")


plot(time, treatment,  xlab="Time", ylab = "malaria", type = "l", lwd = 3, col = "purple", main = "ACT treatment")
lines (case1, col="red",type="p",lwd="3", xlab = "", ylab = "")




######################################
plot (mcases, col="red",
      type="l",lwd="3", xlab = "", ylab = "")
lines(time,incidence, xlab="Time", ylab = "malaria", type = "l", lwd = 3, col = "purple", main = "Incidence")
#######################################         ##############################  
####################################### NEW DATA #############################
######################################          ##############################

getwd()
setwd("C:/Users/Brain Computers/Desktop/R-files")
require(deSolve)
act<-read.csv("ACT_Ekiti.csv")
View(act)


dates_act<-seq(as.Date("2015/1/1"), as.Date("2024/05/24"), by="month")
case1<-act$Persons.with.Confirmed.Uncomplicated.Malaria.treated.with.ACT
mcases<-act$Confirmed.uncomplicated.Malaria


plot(case1~dates_act, data = act, col="blue", main="Malaria cases & ACT distributed",
     type="p", xlab="Time(months)", ylab = "ACT", lwd="2")
par (new=TRUE)
plot(mcases, col="red",
     type="l",lwd="2", xlab = "", ylab = "", xaxt="n", yaxt="n")



times<-seq(0,48, by=1/30)
#date_data<-seq(as.Date("2014/1/1"), as.Date("2017/12/1"), by="month")
parameters <- c(lambda_h = 4376, mu_h = 0.0015, delta_h = 0.000102, eta = 3,
                alpha=0.2, omega = 1/9.5, tau = 0.54, epsilon = 11.397, q = 0.8, gamma = 0.28,
                beta_h = 0.035,rho_1 = 0.95, lambda_v = 28926130, mu_v = 2,
                eta_v = 2.73, theta_v = 13.2, beta_v = 0.48, rho_2 = 0.85, amp=1, phi= 7.7)
#theta_h = 190.5, omega_1 = 0.5,


initN_h<-2951454 #2892613
initN_v<-6924111 #14463065


initE_h<-0
initI_h<- 8120
initA_h<-4*8120
initT_h<- 3392
#initM_h<-0
initR_h<-3155
initY <- 0#8120
initX <- 0#6900
initE_v<-0
initI_v<-886845




initS_h<-initN_h-initE_h-initI_h-initA_h-initT_h-initR_h
initS_v<-initN_v-initE_v-initI_v
state <- c(S_h = initS_h, E_h=initE_h, I_h = initI_h,A_h = initA_h,T_h = initT_h,
           R_h = initR_h,Y_h = initY , X_h = initX, S_v = initS_v, E_v = initE_v, I_v = initI_v)


# set up a function to solve the model
seir_model<-function(t, state, parameters)
{
  with(as.list(c(state, parameters)),
       {
         
         # define variables
         N_h <- (S_h+E_h+I_h+A_h+T_h+R_h)
         N_v <- (S_v+E_v+I_v)
         lambda_1<-lambda_v *(1+amp*cos(2*pi*(t-phi)/12))
         #beta_1<- beta_h*(1+amp*cos(2*pi*(t-phi)/12))
         #beta<-(R0*(muo+nui)*(gamma+muo))/gamma
         #lam <- beta*seas*I/P
         
         # rate of change
         dS_h <- lambda_h + gamma*R_h -(beta_h*theta_v*I_v*S_h/N_h) - mu_h*S_h
         dE_h <- (beta_h*theta_v*I_v*S_h/N_h) - (eta + mu_h)*E_h
         dI_h <- eta*alpha*E_h + (rho_1*beta_h*theta_v*I_v*A_h/N_h) - (tau+delta_h+mu_h)*I_h
         dA_h <- eta*(1-alpha)*E_h + epsilon*(1-q)*T_h - (rho_1*beta_h*theta_v*I_v*A_h/N_h) - (omega+mu_h)*A_h
         dT_h<-  tau*I_h-(epsilon+mu_h)*T_h
         #dM_h<-  (beta_1*theta_h*I_v*S_h/N_h)    
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


out <- ode(y = state, times = times, func = seir_model, parms = parameters)
pop<-out[,"S_h"]+out[,"E_h"]+out[,"I_h"]+out[,"A_h"]+out[,"T_h"]+out[,"R_h"]+out[,"S_v"]+
  out[,"E_v"]+out[,"I_v"]
time<-out[,"time"]
View(out)


incidence <- c(initY, diff(out[,8]))
incidence
treatment <- c(initX, diff(out[,9]))
treatment
par(mfrow = c(3,3), mar = c(2,2,1,1))
plot(time, incidence,  xlab="Time", ylab = "malaria", type = "l", lwd = 3, col = "purple", main = "Incidence")


plot(time, out[,"S_h"],  xlab="Time", ylab = "malaria", type = "l", lwd = 3, col = "purple", main = "Susceptible population")
plot(time, out[,"E_h"], xlab="Time", ylab = "malaria", type = "l", lwd = 3, col = "blue", main = "Exposed population to malaria")
plot(time, out[,"I_h"], xlab="Time", ylab = "malaria", type = "l", lwd = 3, col = "red", main = "Infected population with malaria", xaxt="n", yaxt="n")
plot(time, out[,"A_h"], xlab="Time", ylab = "malaria", type = "l", lwd = 3, col = "orange", main = "Asymptomatic population with malaria")
plot(time, out[,"T_h"], xlab="Time", ylab = "malaria", type = "l", lwd = 3, col = "grey", main = "Treated population", xaxt="n", yaxt="n")
plot(time, out[,"R_h"], xlab="Time", ylab = "malaria", type = "l", lwd = 3, col = "green", main = "Recovered population from malaria")
plot(time, out[,"S_v"], xlab="Time", ylab = "malaria vector", type = "l", lwd = 3, col = "purple", main = "Susceptible vector")
plot(time, out[,"E_v"], xlab="Time", ylab = "malaria vector", type = "l", lwd = 3, col = "blue", main = "Exposed vector")
plot(time, out[,"I_v"], xlab="Time", ylab = "malaria vector", type = "l", lwd = 3, col = "red", main = "Infected vector")


##################################################
#par(new=TRUE)
lines (mcases, col="red",type="p",lwd="3", xlab = "", ylab = "")


plot(time, treatment,  xlab="Time", ylab = "malaria", type = "l", lwd = 3, col = "purple", main = "ACT treatment")
lines (case1, col="red",type="p",lwd="3", xlab = "", ylab = "")




######################################
plot (mcases, col="red",
      type="l",lwd="3", xlab = "", ylab = "")
lines(time,incidence, xlab="Time", ylab = "malaria", type = "l", lwd = 3, col = "purple", main = "Incidence")
#######################################


