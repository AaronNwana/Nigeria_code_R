############################################################################
Simulation/projection
#Treatment Code
setwd("C:/Users/Brain Computers/Desktop/R-files")
act<-read.csv("Ekitistate.csv")
View(act)
mean(act$Total.Malaria.Cases)
var(act$Total.Malaria.Cases)
require(deSolve)
require(zoo)
require(xts)
dates_act<-seq(as.Date("2014/1/1"), as.Date("2017/12/1"), by="month")
case1<-act$Total.ACT.Distributed
mcases<-act$Total.Malaria.Cases


plot(case1~dates_act, data = act, col="blue", main="Malaria cases & ACT distributed",
     type="l", xlab="Time(months)", ylab = "ACT", lwd="2")
par (new=TRUE)
plot(mcases, col="red",
     type="l",lwd="2", xlab = "", ylab = "") #xaxt="n", yaxt="n")


require(deSolve)
times<-seq(0,132, by=1)
#date_data<-seq(as.Date("2014/1/1"), as.Date("2017/12/1"), by="month")


#####Cases parameters1
parameters <- c(lambda_h = 4376, mu_h = 0.0015, delta_h = 0.000102, eta = 3,
                alpha=0.11000000, omega = 1/9.5, tau = 0.54, epsilon = 11.397, q =0.90692681,
                gamma = 0.12634273, beta_h = 0.08166898, lambda_v = 28926130, mu_v = 2,
                eta_v = 3.33, theta_v = 5.16812785, beta_v = 0.14978312,
                rho_1 = 0.02530312, rho_2 = 0.28886204, amp=0.26119328, phi= -7.24340365)




#####Cases parameters2
#parameters <- c(lambda_h = 4376, mu_h = 0.0015, delta_h = 0.000102, eta = 3,
#               alpha=0.11000000, omega = 1/9.5, tau = 0.54, epsilon = 11.397, q =0.96374108,
#              gamma = 0.15827187, beta_h = 0.08968125, lambda_v = 28926130, mu_v = 2,
#             eta_v = 3.33, theta_v = 4.56795813, beta_v = 0.16726859,
#            rho_1 = 0.01863610, rho_2 = 0.19196965, amp=0.24021263, phi= -6.92120614)


####Treatment parameters
#parameters <- c(lambda_h = 4376, mu_h = 0.0015, delta_h = 0.000102, eta = 3,
#               alpha=0.11000000, omega = 1/9.5, tau = 0.54, epsilon = 11.397, q =1,
#              gamma = 0.05429083, beta_h = 0.16501601, lambda_v = 28926130, mu_v = 2,
#             eta_v = 3.33, theta_v = 3.27696529, beta_v = 0.12586995,
#            rho_1 = 0.01000000, rho_2 = 1, amp=0.27000000 , phi= -8.44644765)






initS_h<-2904307
initS_v<-6037266


initE_h<-0
initI_h<- 8120
initA_h<-4*8120
initT_h<- 3392
initR_h<-3155
initY <- 0
initX <- 0
initE_v<-0
initI_v<-886845




initN_h<-initS_h+initE_h+initI_h+initA_h+initT_h+initR_h
initN_v<-initS_v+initE_v+initI_v
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


out <- ode(y = state, times = times, func = seir_model, parms = parameters)
pop<-out[,"S_h"]+out[,"E_h"]+out[,"I_h"]+out[,"A_h"]+out[,"R_h"]+out[,"S_v"]+
  out[,"E_v"]+out[,"I_v"]
time<-out[,"time"]
View(out)


incidence <- c(initY, diff(out[,8]))


incidence
treatment <- c(initX, diff(out[,9]))
treatment
#par(mfrow = c(3,3), mar = c(2,2,1,1))




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
plot(time, incidence,  xlab="Time", ylab = "malaria", type = "l", lwd = 3, col = "purple", main = "Incidence")
lines (mcases, col="red",type="p",lwd="3", xlab = "", ylab = "")
legend("bottomright",col = c("purple", "red"),legend = c("Model",
                                                         "Data"),lty = c(1,1), lwd = c(3,3), cex = 0.5)
abline(v = 48, col = "black", lty = 2)


plot(time, treatment,  xlab="Time", ylab = "malaria", type = "l", lwd = 3, col = "purple", main = "ACT treatment")
lines (case1, col="red",type="p",lwd="3", xlab = "", ylab = "")
legend("bottomright",col = c("purple", "red"),legend = c("Model",
                                                         "Data"),lty = c(1,1), lwd = c(3,3), cex = 0.5)
abline(v = 48, col = "black", lty = 2)




######################################
plot (mcases, col="red",
      type="l",lwd="3", xlab = "", ylab = "")
lines(time,incidence, xlab="Time", ylab = "malaria", type = "l", lwd = 3, col = "purple", main = "Incidence")
##################################################################



# Scenario analysis -------------------------------------------------------


#ggplot1
times<-seq(0,144, by=1)
#date_data<-seq(as.Date("2014/1/1"), as.Date("2017/12/1"), by="month")
parameters <- c(lambda_h = 4376, mu_h = 0.0015, delta_h = 0.000102, eta = 3,
                alpha=0.11000000, omega = 1/9.5, tau = 0.54, epsilon = 26.97, q =0.96272505,
                gamma = 0.12392539, beta_h = 0.13530735, lambda_v = 28926130, mu_v = 2,
                eta_v = 3.33, theta_v = 3.14470254, beta_v = 0.24501916,
                rho_1 = 0.02647566, rho_2 = 0.22387166, amp=0.26619293, phi= -7.25267374)
#theta_h = 190.5, omega_1 = 0.5,


initN_h<-2951454
initN_v<-6924111


initE_h<-0
initI_h<- 8120
initA_h<-4*8120
initT_h<- 3392
initR_h<-3155
initY <- 0
initX <- 0
initE_v<-0
initI_v<-886845




initS_h<-initN_h-initE_h-initI_h-initA_h-initT_h-initR_h
initS_v<-initN_v-initE_v-initI_v
initial_state <- c(S_h = initS_h, E_h=initE_h, I_h = initI_h,A_h = initA_h,T_h = initT_h,
                   R_h = initR_h,Y_h = initY , X_h = initX, S_v = initS_v, E_v = initE_v, I_v = initI_v)


# set up a function to solve the model
seir_model<-function(t, state, parameters)
{
  with(as.list(c(state, parameters)),
       {
         S_h <- state["S_h"]
         E_h <- state["E_h"]
         I_h <- state["I_h"]
         A_h <- state["A_h"]
         T_h <- state["T_h"]
         X_h <- state["X_h"]
         Y_h <- state["Y_h"]
         R_h <- state["R_h"]
         S_v <- state["S_v"]
         E_v <- state["E_v"]
         I_v <- state["I_v"]
         
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


treatment_values <- seq(0.1, 1, by = 0.1)
#treatment_values <- c(1, 0.69, 0.57, 0.54, 0.45, 0.46, 0.34, 0.26)


# Simulate the model for each treatment value and store the results
results <- lapply(treatment_values, function(tau) {
  parameters["tau"] <-   tau
  
  out <- ode(y = initial_state, times = seq(0, 144, by = 1),
             func = seir_model, parms = parameters)
  return(data.frame(time = out[, "time"], infectious = out[, "I_h"], treatment = tau))
})

#results<- as.data.frame(results)
class(results)

#####################
# Combine results into a single data frame
results <- do.call(rbind, results)
results$time <- rep(seq(as.Date("2014-01-01"),by = "month", length.out=145 ),10)

# Plot the effect of treatment on the infectious class
plot1<- ggplot(results, aes(x = time, y = infectious, color = as.factor(treatment))) +
  geom_line() +
  #geom_point(mapping = aes(y = mcases)) +
  labs(x = "Time (month)", y = "Malaria incidence", color = "ACT treatment") +
  ggtitle("Effect of ACT treatment on malaria incidence in Ekiti state") +
  theme_minimal() +
  geom_vline(xintercept = as.Date("2017-12-01"), linetype = "dashed", color="black", linewidth=0.5)+
  scale_color_manual(values = rainbow(length(treatment_values)))
plot1
#####################
# Create the plot
scientific_plot <- ggplot(results, aes(x = time, y = infectious, color = as.factor(treatment))) +
  geom_line() +
  labs(x = "Time (month)", y = "Malaria incidence", color = "ACT treatment") +
  ggtitle("Effect of ACT Treatment on Malaria Incidence in Ekiti State") +
  theme_minimal() +
  geom_vline(xintercept = as.Date("2017-12-01"), linetype = "dashed", color = "black", size = 0.5) +
  scale_color_manual(values = rainbow(length(treatment_values)))

# Save the plot (optional)
ggsave("scientific_plot.png", scientific_plot, width = 6, height = 4, dpi = 300)

# Display the plot
scientific_plot

####################################
library(ggplot2)
#Ggplot2
outcome <- as.data.frame(ode(y = initial_state, times = time, func = seir_model,
                             parms = parameters))
predinci= c(initial_state["I_h"], diff(outcome$M_h))


dafr <- data.frame(
  time = seq(as.Date("2019/01/31"), by = "month", length.out=48),
  incidence = predinci,
  Data = x
)

# plot(outcome$time,predinci, type ="l", col = "purple", lwd = 2, ylab = "Incidence", xlab="Time(Months)")
# lines(x, col = "red", lwd =2, type = "p")# xaxt = "n", yaxt = "n")
# legend("topright",col = c("purple", "red"),legend = c("Model",
#                                                       "Data"),lty = c(1,1), lwd = c(2,2), cex = 0.6)
#
#

# Plot the predicted and observed incidence
plot<-  ggplot(dafr, aes(x = time)) +
  geom_line(aes(y = predinci, color = "Model"), size=2) +
  geom_point(aes(y = Data, color = "Data"),shape = "o", size=4) +
  labs(x = "Time (Months)", y = "Incidence", color = "Legend") +
  theme_minimal() +
  theme(legend.position = "bottom")+
  scale_color_manual(values = c("red", "purple"))+
  guides(col=guide_legend(title = NULL, override.aes = list(linetype=c("solid", "solid"),size=2)))
print(plot)

#lss <- -sum(dnbinom(x,size=28.35963, mu  = predinci, log = TRUE))

#return(lss)

