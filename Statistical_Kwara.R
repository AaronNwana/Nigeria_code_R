setwd('C:/Users/Brain Computers/Desktop/R-files')
getwd()
#malaria: global, Africa, Nigeria, state, locality
kwara_data<- read.csv("Kwara_State_Weather.csv")
kwara_data

# incidence<- (kwara_data$Confirmed.uncomplicated.Malaria/kwara_data$Population)*1000
# incidence
# 
# kwara_data$incidence<-round(incidence,3)
# kwara_data
# 
# par(mfrow=c(1,1), pty="s", cex.main=1.8,cex.axis=1.3,cex.lab=1.5)
# barplot(kwara_data$incidence,names.arg = expression(2015:2024), 
#          col=2, main="Malaria incidence Kwara", 
#          xlab = "Year", ylab = "Incidence(per 1000)")
# 
# #######################################################
# library(ggplot2)
# 
# # Assuming kwara_data already has Year and Month columns
# # Convert the Month column to month abbreviations (Jan, Feb, etc.)
# kwara_data$Months <- factor(month.abb[kwara_data$Months], levels = month.abb)
# 
# # Plot malaria incidence by months with faceted years
# ggplot(kwara_data, aes(x=Months, y=incidence)) + 
#   geom_bar(stat="identity", fill="red") + 
#   labs(title="Monthly Malaria Incidence in Kwara (2015-2024)", 
#        x="Months", 
#        y="Incidence (per 1000)") + 
#   theme_minimal(base_size = 15)
# 
# 
# kwara_temp<-kwara_data$Temp
# # par(mfrow=c(1,1), pty="s", cex.main=1.8,cex.axis=1.3,cex.lab=1.5)
# # barplot(kwara_temp,names.arg = expression(2015:2024), 
# #         col=2, main="Kwara temperature", 
# #         xlab = "Year", ylab = "Temperature (oC)")
# 
# kwara_rainfall<-kwara_data$Rainfall
# # par(mfrow=c(1,1), pty="s", cex.main=1.8,cex.axis=1.3,cex.lab=1.5)
# # barplot(kwara_rainfall,names.arg = expression(2015:2024), 
# #         col=2, main="Kwara state rainfall", 
# #         xlab = "Year", ylab = "Rainfall (mmHg)")
# 
# View(kwara_data)
# 
# #####################################################################################
# # Define the years from 2015 to 2024
# years <- c(rep(2015:2023, each = 12), rep(2024, 4))
# 
# # Add the 'Year' column to the dataset
# kwara_data$Year <- years
# 
# # Check the number of rows in kwara_data and years
# print(nrow(kwara_data))  # Ensure this matches the length of 'years'
# 
# # Now you can proceed with your ggplot code
# kwara_data$incidence <- round((kwara_data$Confirmed.uncomplicated.Malaria / kwara_data$Population) * 1000, 3)

# # Create a ggplot bar plot###############################################
# library(ggplot2)
# ggplot(kwara_data, aes(x=factor(Year), y=incidence)) + 
#   geom_bar(stat="identity", fill="red") + 
#   labs(title="Malaria Incidence in Kwara (2015-2024)", 
#        x="Year", 
#        y="Incidence (per 1000)") + 
#   theme_minimal(base_size = 15)
# ########################################################################
# ggplot(kwara_data, aes(x=factor(Year), y=kwara_temp)) + 
#   geom_bar(stat="identity", fill="blue") + 
#   labs(title="Kwara state temperature (2015-2024)", 
#        x="Year", 
#        y="Temperature (oC)") + 
#   theme_minimal(base_size = 15)
# #########################################################################
# ggplot(kwara_data, aes(x=factor(Year), y=kwara_rainfall)) + 
#   geom_bar(stat="identity", fill="purple") + 
#   labs(title="Kwara state rainfall (2015-2024)", 
#        x="Year", 
#        y="Rainfall (mmHg)") + 
#   theme_minimal(base_size = 15)
########################################################################
library(ggplot2)
library(readr)
setwd('C:/Users/Brain Computers/Desktop/R-files')
getwd()
kwara_data<- read.csv("Kwara_State_Weather.csv")
kwara_data

View(kwara_data)

incidence <- (kwara_data$Confirmed.uncomplicated.Malaria/kwara_data$Population)*1000
kwara_data$incidence <-round(incidence, digits=3)
par(mfrow=c(1,1))
time <- seq(as.Date("2015-01-01"), by = "month", length.out = 112)
kwara_data$time <- time

ggplot(kwara_data, aes(time, incidence)) +
  geom_col() +
  theme_bw()+
  geom_hline(yintercept = mean(incidence))+
  labs(title =  "Malaria incidence",x="Time (Years)",y = "Incidence")

ggplot(kwara_data, aes(time, incidence)) +
  geom_col() +
  theme_bw()+
  geom_hline(yintercept = quantile(kwara_data$incidence, p=0.75), lty=2)+
  labs(title =  "Malaria incidence",x="Time (Years)",y = "Incidence")


?geom_abline
####### Temperature ###############################
kwara_temp<-round(kwara_data$Temp, digits = 3)

ggplot(kwara_data, aes(time, kwara_temp)) +
  geom_col() +
  theme_bw()+
  geom_hline(yintercept = mean(kwara_temp))+
  labs(title =  "Average temperature",x="Time (Years)",y = "Average teperature (Celsius)")

?geom_abline
####### Temperature ###############################
kwara_rainfall <-round(kwara_data$Rainfall, digits = 3)

ggplot(kwara_data, aes(time, kwara_rainfall)) +
  geom_col() +
  theme_bw()+
  geom_hline(yintercept = mean(kwara_rainfall))+
  labs(title =  "Average rainfall",x="Time (Years)",y = "Average rainfall (mmHg)")

# ------------------ Question 2 ----------------------

# Was policy intervention in 2020 effective?

ggplot(kwara_data, aes(time, incidence)) +
  geom_col(fill = "blue") +
  theme_bw()+
  geom_vline(xintercept = as.Date("2021-01-01"), lty=2)+
  labs(title =  "Malaria incidence",x="Time (Years)",y = "Incidence")


