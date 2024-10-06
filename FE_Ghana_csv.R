# Set working directory
getwd()
setwd("C:/Users/Brain Computers/Desktop/R-files")

# Load packages
library(tidyr)
library(dplyr)
library(ggplot2)

# Load dataset from part one
data_file = "output_cleaned.csv"
if (file.exists(data_file)){
  output_cleaned = read.csv("output_cleaned.csv", header = TRUE,
                            stringsAsFactors = FALSE)
}else{
  # print(paste0("Resource not found: ", data_file))
  print(sprintf("The data file (%s) does not exist!", data_file))
}

View (output_cleaned)

head(output_cleaned)

aggregated_data = output_cleaned %>%
  group_by(Arm, Day) %>%
  summarise(
    count_Var1 = n(),
    mean_Var1 = mean(Var1, na.rm = TRUE),
    sd_Var1 = sd(Var1, na.rm = TRUE),
    
    count_Var3 = n(),
    mean_Var3 = mean(Var3, na.rm = TRUE),
    sd_Var3 = sd(Var3, na.rm = TRUE)
  )



aggregated_data <- aggregated_data %>%
  mutate(
    upper_Var1 = mean_Var1 + 1.96*sd_Var1 / sqrt(count_Var1),
    lower_Var1 = mean_Var1 - 1.96*sd_Var1 / sqrt(count_Var1),
    
    upper_Var3 = mean_Var3 + 1.96*sd_Var3 / sqrt(count_Var3),
    lower_Var3 = mean_Var3 - 1.96*sd_Var3 / sqrt(count_Var3)
  )

head(aggregated_data)


plots <- ggplot(aggregated_data, aes(x = Day)) +
  theme_minimal() +
  # variable 1 (Var 1)
  geom_line(aes(y = mean_Var1, color="Var1"),linewidth = 2) +
  geom_ribbon(aes(ymin = lower_Var1, ymax = upper_Var1, fill="Var1"), size = 1.5, alpha = 0.5) +
  
  # Variable 3 (Var 3)
  geom_line(aes(y = mean_Var3, color="Var3"), linewidth = 2) +
  geom_ribbon(aes(ymin = lower_Var3, ymax = upper_Var3, fill="Var3"), size = 1.5, alpha = 0.5) +
  facet_wrap(~ Arm) +
  labs(
    title = "Daily mean prediction intervals of Var1 and Var3 by Arm",
    x = "Day",
    y = "Prediction value",
    color = "Variable",
    fill = "Variable"
  )+
  scale_color_manual(values = c("Var1" = "purple", "Var3" = "red")) +
  scale_fill_manual(values = c("Var1" = "grey", "Var3" = "blue"))


print(plots)


# Save the plots in .png format
ggsave("daily_prediction_intervals_Var13_by_Arm.png", plot=plots, width = 12, height = 6)

########################2########################################
aggregated_data = output_cleaned %>%
  ungroup()%>%
  group_by(Site, Day) %>%
  summarise(
    count_Var1 = n(),
    mean_Var1 = mean(Var1, na.rm = TRUE),
    sd_Var1 = sd(Var1, na.rm = TRUE),
    
    count_Var3 = n(),
    mean_Var3 = mean(Var3, na.rm = TRUE),
    sd_Var3 = sd(Var3, na.rm = TRUE),
    .groups = "drop"
  )



aggregated_data <- aggregated_data %>%
  mutate(
    upper_Var1 = mean_Var1 + 1.96*sd_Var1 / sqrt(count_Var1),
    lower_Var1 = mean_Var1 - 1.96*sd_Var1 / sqrt(count_Var1),
    
    upper_Var3 = mean_Var3 + 1.96*sd_Var3 / sqrt(count_Var3),
    lower_Var3 = mean_Var3 - 1.96*sd_Var3 / sqrt(count_Var3)
  )

head(aggregated_data)


plots <- ggplot(aggregated_data, aes(x = Day)) +
  theme_minimal() +
  # variable 1 (Var 1)
  geom_line(aes(y = mean_Var1, color="Var1"),linewidth = 2) +
  geom_ribbon(aes(ymin = lower_Var1, ymax = upper_Var1, fill="Var1"), size = 1.5, alpha = 0.5) +
  
  # Variable 3 (Var 3)
  geom_line(aes(y = mean_Var3, color="Var3"), linewidth = 2) +
  geom_ribbon(aes(ymin = lower_Var3, ymax = upper_Var3, fill="Var3"), size = 1.5, alpha = 0.5) +
  facet_wrap(~ Site) +
  labs(
    title = "Daily mean prediction intervals of Var1 and Var3 by Arm",
    x = "Day",
    y = "Prediction value",
    color = "Variable",
    fill = "Variable"
  )+
  scale_color_manual(values = c("Var1" = "purple", "Var3" = "red")) +
  scale_fill_manual(values = c("Var1" = "grey", "Var3" = "blue"))


print(plots)


# Save the plots in .png format
ggsave("daily_prediction_intervals_Var13_by_Site.png", plot=plots, width = 12, height = 6)

########################3 Trial_Number##############################
aggregated_data = output_cleaned %>%
  ungroup()%>%
  group_by(Trial_Number, Day) %>%
  summarise(
    count_Var1 = n(),
    mean_Var1 = mean(Var1, na.rm = TRUE),
    sd_Var1 = sd(Var1, na.rm = TRUE),
    
    count_Var3 = n(),
    mean_Var3 = mean(Var3, na.rm = TRUE),
    sd_Var3 = sd(Var3, na.rm = TRUE),
    .groups = "drop"
  )



aggregated_data <- aggregated_data %>%
  mutate(
    upper_Var1 = mean_Var1 + 1.96*sd_Var1 / sqrt(count_Var1),
    lower_Var1 = mean_Var1 - 1.96*sd_Var1 / sqrt(count_Var1),
    
    upper_Var3 = mean_Var3 + 1.96*sd_Var3 / sqrt(count_Var3),
    lower_Var3 = mean_Var3 - 1.96*sd_Var3 / sqrt(count_Var3)
  )

head(aggregated_data)


plots <- ggplot(aggregated_data, aes(x = Day)) +
  theme_minimal() +
  # variable 1 (Var 1)
  geom_line(aes(y = mean_Var1, color="Var1"),linewidth = 2) +
  geom_ribbon(aes(ymin = lower_Var1, ymax = upper_Var1, fill="Var1"), size = 1.5, alpha = 0.5) +
  
  # Variable 3 (Var 3)
  geom_line(aes(y = mean_Var3, color="Var3"), linewidth = 2) +
  geom_ribbon(aes(ymin = lower_Var3, ymax = upper_Var3, fill="Var3"), size = 1.5, alpha = 0.5) +
  facet_wrap(~ Trial_Number) +
  labs(
    title = "Daily mean prediction intervals of Var1 and Var3 by Number of trials",
    x = "Day",
    y = "Prediction value",
    color = "Variable",
    fill = "Variable"
  )+
  scale_color_manual(values = c("Var1" = "purple", "Var3" = "red")) +
  scale_fill_manual(values = c("Var1" = "grey", "Var3" = "blue"))


print(plots)


# Save the plots in .png format
ggsave("daily_prediction_intervals_Var13_by_Site.png", plot=plots, width = 12, height = 6)


################################################################################
################################################################################
################################################################################
library(ggplot2)
setwd("C:/Users/Brain Computers/Desktop/R-files")
getwd()
malaria_data<- read.csv("U5_PfPR_ClinicalIncidence.csv")
malaria_data
View (malaria_data)

# Assuming your data frame is named df
# Scatter plot
ggplot(malaria_data, aes(x = year, y = annualeir)) +
  geom_point() +
  labs(title = "Annual EIR over Years",
       x = "Year",
       y = "Annual EIR") +
  theme_minimal()

# Line plot (useful if you want to connect the points to show trends)
ggplot(malaria_data, aes(x = year, y = annualeir)) +
  geom_line() +
  geom_point() + # Optional: adds points on the line
  labs(title = "Annual EIR over Years",
       x = "Year",
       y = "Annual EIR") +
  theme_minimal()
##########################################################################
library(dplyr)

# Summarize data by year, calculating mean annualeir
malaria_data_summary <- malaria_data %>%
  group_by(year) %>%
  summarize(mean_annualeir = mean(annualeir))

# Plot summarized data
ggplot(malaria_data_summary, aes(x = year, y = mean_annualeir)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Annual EIR over Years",
       x = "Year",
       y = "Mean Annual EIR") +
  theme_minimal()
################# U5 PFPR#########################
# Summarize data by year, calculating mean annualeir
malaria_data_summary <- malaria_data %>%
  group_by(month) %>%
  summarize(mean_Cases = mean(Cases.U5))

# Plot summarized data
ggplot(malaria_data_summary, aes(x = month, y = mean_Cases)) +
  geom_line() +
  geom_point() +
  geom_bar(stat = "identity", fill = "red") + 
  labs(title = "Mean Monthly U5 Prevalence over Years",
       x = "month",
       y = "Mean Annual U5 Prevalence") +
  theme_minimal()
#############Compare
# Summarize data by year, calculating mean annualeir
malaria_data_summary <- malaria_data %>%
  group_by(year) %>%
  summarize(mean_Cases = mean(Cases.U10))

# Plot summarized data
ggplot(malaria_data_summary, aes(x = year, y = mean_Cases)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Annual U10 Prevalence over Years",
       x = "Year",
       y = "Mean Annual U10 Prevalence") +
  theme_minimal()

#############CompareU15#######
# Summarize data by year, calculating mean annualeir
malaria_data_summary <- malaria_data %>%
  group_by(year) %>%
  summarize(mean_Cases = mean(Cases.U15))

# Plot summarized data
ggplot(malaria_data_summary, aes(x = year, y = mean_Cases)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Annual U15 Prevalence over Years",
       x = "Year",
       y = "Mean Annual U15 Prevalence") +
  theme_minimal()

############################U115 PFPR##########################
# Summarize data by year, calculating mean annualeir
malaria_data_summary <- malaria_data %>%
  group_by(year) %>%
  summarize(mean_Cases = mean(Cases.U115))

# Plot summarized data
ggplot(malaria_data_summary, aes(x = year, y = mean_Cases)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Annual U115 Prevalence over Years",
       x = "Year",
       y = "Mean Annual U115 Prevalence") +
  theme_minimal()

##################U5 prevalence3####################### 
malaria_data3<- read.csv("U5_PfPR_ClinicalIncidence3.csv")
malaria_data3

# Summarize data by year, calculating mean annualeir
malaria_data_summary <- malaria_data3 %>%
  group_by(year) %>%
  summarize(mean_PfPR = mean(PfPR.U5))

# Plot summarized data
ggplot(malaria_data_summary, aes(x = year, y = mean_PfPR)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Annual U5 Prevalence over Years",
       x = "Year",
       y = "Mean Annual U5 Prevalence") +
  theme_minimal()
#############################PfPR 15########################
# Summarize data by year, calculating mean annualeir
malaria_data_summary <- malaria_data3 %>%
  group_by(year) %>%
  summarize(mean_PfPR = mean(PfPR.U15))

# Plot summarized data
ggplot(malaria_data_summary, aes(x = year, y = mean_PfPR)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Annual U15 Prevalence over Years",
       x = "Year",
       y = "Mean Annual U15 Prevalence") +
  theme_minimal()
#######################PfPR 80#####################
# Summarize data by year, calculating mean annualeir
malaria_data_summary <- malaria_data3 %>%
  group_by(year) %>%
  summarize(mean_PfPR = mean(PfPR.U80))

# Plot summarized data
ggplot(malaria_data_summary, aes(x = year, y = mean_PfPR)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Annual U80 Prevalence over Years",
       x = "Year",
       y = "Mean Annual U80 Prevalence") +
  theme_minimal()
####################Cases#################
# Summarize data by year, calculating mean annualeir
malaria_data_summary <- malaria_data3 %>%
  group_by(year) %>%
  summarize(mean_Cases = mean(Cases.U5))

# Plot summarized data
ggplot(malaria_data_summary, aes(x = year, y = mean_Cases)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Annual U5 Cases over Years",
       x = "Year",
       y = "Mean Annual U5 Cases") +
  theme_minimal()
############################Cases U15####################
# Summarize data by year, calculating mean annualeir
malaria_data_summary <- malaria_data3 %>%
  group_by(year) %>%
  summarize(mean_Cases = mean(Cases.U15))

# Plot summarized data
ggplot(malaria_data_summary, aes(x = year, y = mean_Cases)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Annual U15 Cases over Years",
       x = "Year",
       y = "Mean Annual U15 Cases") +
  theme_minimal()
#########################Cases U80#############
# Summarize data by year, calculating mean annualeir
malaria_data_summary <- malaria_data3 %>%
  group_by(year) %>%
  summarize(mean_Cases = mean(Cases.U80))

# Plot summarized data
ggplot(malaria_data_summary, aes(x = year, y = mean_Cases)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Annual U80 Cases over Years",
       x = "Year",
       y = "Mean Annual U80 Cases") +
  theme_minimal()



###################################U10 PFPR########################
# Summarize data by year, calculating mean annualeir
malaria_data_summary <- malaria_data %>%
  group_by(year) %>%
  summarize(mean_Cases = mean(Cases.U10))

# Plot summarized data
ggplot(malaria_data_summary, aes(x = year, y = mean_Cases)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Annual U10 Prevalence over Years",
       x = "Year",
       y = "Mean Annual U10 Prevalence") +
  theme_minimal()
##########################U15 PFPR#############################
# Summarize data by year, calculating mean annualeir
malaria_data_summary <- malaria_data %>%
  group_by(year) %>%
  summarize(mean_Cases = mean(Cases.U15))

# Plot summarized data
ggplot(malaria_data_summary, aes(x = year, y = mean_Cases)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Annual U15 Prevalence over Years",
       x = "Year",
       y = "Mean Annual U15 Prevalence") +
  theme_minimal()
######################################################################
# Summarize data by year, calculating mean annualeir
malaria_data_summary <- malaria_data %>%
  group_by(year) %>%
  summarize(mean_PFPR = mean(PfPR.U5))

# Plot summarized data
ggplot(malaria_data_summary, aes(x = year, y = mean_PFPR)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Annual U5 Prevalence over Years",
       x = "Year",
       y = "Mean Annual U5 Prevalence") +
  theme_minimal()
#########################95% CI##############################
library(patchwork)
# Summarize data by year, calculating mean, standard error, and confidence intervals
malaria_data_summary <- malaria_data %>%
  group_by(year) %>%
  summarize(
    mean_PFPR = mean(PfPR.U5, na.rm = TRUE),
    se = sd(PfPR.U5, na.rm = TRUE) / sqrt(n()),  # Standard error
    lower_ci = mean_PFPR - 1.96 * se,           # Lower bound of the 95% CI
    upper_ci = mean_PFPR + 1.96 * se            # Upper bound of the 95% CI
  )

# Plot summarized data with confidence interval ribbon
plot1<-ggplot(malaria_data_summary, aes(x = year, y = mean_PFPR)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) + # Add confidence interval ribbon
  labs(
    title = "Mean Annual U5 Prevalence over Years",
    x = "Year",
    y = "Mean Annual U5 Prevalence"
  ) +
  theme_minimal()
##################################################################
# Summarize data by year, calculating mean annualeir
malaria_data_summary <- malaria_data %>%
  group_by(year) %>%
  summarize(mean_PFPR = mean(PfPR.U15))

# Plot summarized data
ggplot(malaria_data_summary, aes(x = year, y = mean_PFPR)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Annual U15 Prevalence over Years",
       x = "Year",
       y = "Mean Annual U15 Prevalence") +
  theme_minimal()
##################################################################################
#################################Without IPTP#####################################
##################################################################################
malaria_data2<- read.csv("U5_PfPR_ClinicalIncidence2.csv")
malaria_data2
View (malaria_data2)

# Summarize data by year, calculating mean, standard error, and confidence intervals
malaria_data_summary <- malaria_data2 %>%
  group_by(year) %>%
  summarize(
    mean_PFPR = mean(PfPR.U5, na.rm = TRUE),
    se = sd(PfPR.U5, na.rm = TRUE) / sqrt(n()),  # Standard error
    lower_ci = mean_PFPR - 1.96 * se,           # Lower bound of the 95% CI
    upper_ci = mean_PFPR + 1.96 * se            # Upper bound of the 95% CI
  )

# Plot summarized data with confidence interval ribbon
plot2<- ggplot(malaria_data_summary, aes(x = year, y = mean_PFPR)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) + # Add confidence interval ribbon
  labs(
    title = "Mean Annual U5 Prevalence over Years (NO IPTP)",
    x = "Year",
    y = "Mean Annual U5 Prevalence"
  ) +
  theme_minimal()

combined_plot <- plot1 + plot2
combined_plot


#################################################################################
#########################################################
# Load required libraries
library(dplyr)
library(ggplot2)
Dataset_1 <- malaria_data[, c("PfPR.U5", "year")]
Dataset_2 <- malaria_data2[, c("PfPR.U5", "year")]

# Add dataset identifier column to each data frame
malaria_data <- malaria_data %>%
  mutate(dataset = "Dataset 1")

malaria_data2 <- malaria_data2 %>%
  mutate(dataset = "Dataset 2")

# Combine the datasets into one data frame
combined_data <- bind_rows(malaria_data, malaria_data2)

# Summarize data by year and dataset, calculating mean, standard error, and confidence intervals
combined_data_summary <- combined_data %>%
  group_by(year, dataset) %>%
  summarize(
    mean_PFPR = mean(PfPR.U5, na.rm = TRUE),
    se = sd(PfPR.U5, na.rm = TRUE) / sqrt(n()),  # Standard error
    lower_ci = mean_PFPR - 1.96 * se,           # Lower bound of the 95% CI
    upper_ci = mean_PFPR + 1.96 * se            # Upper bound of the 95% CI
  )

# Plot both datasets on the same plot with specified colors
ggplot(combined_data_summary, aes(x = year, y = mean_PFPR, color = dataset, fill = dataset)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
  scale_color_manual(values = c("Dataset 1" = "red", "Dataset 2" = "blue")) + # Specify line colors
  scale_fill_manual(values = c("Dataset 1" = "lightblue", "Dataset 2" = "pink")) + # Specify ribbon colors
  labs(
    title = "Mean Annual U5 Prevalence over Years (Superimposed)",
    x = "Year",
    y = "Mean Annual U5 Prevalence"
  ) +
  theme_minimal()
################################################################################
library(dplyr)
library(ggplot2)
Dataset_1 <- malaria_data[, c("Cases.U115", "year")]
Dataset_2 <- malaria_data2[, c("Cases.U115", "year")]
View<- Dataset_1
View<- Dataset_2
# Add dataset identifier column to each data frame
malaria_data <- malaria_data %>%
  mutate(dataset = "Dataset 1")

malaria_data2 <- malaria_data2 %>%
  mutate(dataset = "Dataset 2")

# Combine the datasets into one data frame
combined_data <- bind_rows(malaria_data, malaria_data2)

# Summarize data by year and dataset, calculating mean, standard error, and confidence intervals
combined_data_summary <- combined_data %>%
  group_by(year, dataset) %>%
  summarize(
    mean_Cases = mean(Cases.U115, na.rm = TRUE),
    se = sd(Cases.U115, na.rm = TRUE) / sqrt(n()),  # Standard error
    lower_ci = mean_Cases - 1.96 * se,           # Lower bound of the 95% CI
    upper_ci = mean_Cases + 1.96 * se            # Upper bound of the 95% CI
  )

# Plot both datasets on the same plot with specified colors
ggplot(combined_data_summary, aes(x = year, y = mean_Cases, color = dataset, fill = dataset)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
  scale_color_manual(values = c("Dataset 1" = "red", "Dataset 2" = "blue")) + # Specify line colors
  scale_fill_manual(values = c("Dataset 1" = "lightblue", "Dataset 2" = "pink")) + # Specify ribbon colors
  labs(
    title = "Mean Annual U115 Cases over Years (Superimposed)",
    x = "Year",
    y = "Mean Annual U115 Cases"
  ) +
  theme_minimal()


