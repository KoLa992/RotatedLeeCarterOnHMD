
# Load Rda

load("HMD_RotatedLC_Full_LogMSE.Rda")

# Select Country

CurrentCountry <- "ESP"

CurrentCountryResults <- HMD_RotatedLC_Full[HMD_RotatedLC_Full$Country == CurrentCountry,]

# Heatmaps

library(ggplot2)

ggplot(CurrentCountryResults, aes(e0l, p, fill= mse_female_validation)) + 
  geom_tile() +
  scale_fill_gradient(low="green", high="red") +
  labs(title = "Log(MSE) as a function of p and e0l on Validation Set - Females")

ggplot(CurrentCountryResults, aes(e0l, p, fill= mse_male_validation)) + 
  geom_tile() +
  scale_fill_gradient(low="green", high="red") +
  labs(title = "Log(MSE) as a function of p and e0l on Validation Set - Males")


ggplot(CurrentCountryResults, aes(e0l, p, fill= mse_female_test)) + 
  geom_tile() +
  scale_fill_gradient(low="green", high="red") +
  labs(title = "Log(MSE) as a function of p and e0l on Test Set - Females")

ggplot(CurrentCountryResults, aes(e0l, p, fill= mse_male_test)) + 
  geom_tile() +
  scale_fill_gradient(low="green", high="red") +
  labs(title = "Log(MSE) as a function of p and e0l on Test Set - Males")
