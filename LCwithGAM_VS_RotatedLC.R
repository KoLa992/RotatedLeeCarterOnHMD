load("HMD_BestResultsByCountry_Rotated+GAM.Rda")

library(ggplot2)
library(reshape2)

HMD_BestResultsByCountry_ToPlotMale <- HMD_BestResultsByCountry[,c("Country", "mse_male_test_RotatedLC", "mse_male_test_LCwithGAM")]
HMD_BestResultsByCountry_ToPlotMale$mse_male_test_Difference <- HMD_BestResultsByCountry_ToPlotMale$mse_male_test_LCwithGAM -
  HMD_BestResultsByCountry_ToPlotMale$mse_male_test_RotatedLC
HMD_BestResultsByCountry_ToPlotMale$WhichIsBetter <- ifelse(HMD_BestResultsByCountry_ToPlotMale$mse_male_test_Difference > 0,
                                                            "RotatedLC", "LCwithGAM")

ggplot(HMD_BestResultsByCountry_ToPlotMale, aes(x=reorder(Country,mse_male_test_Difference), y=mse_male_test_Difference, fill=WhichIsBetter)) +
  geom_bar(stat='identity', position='dodge') +
  labs(title = "Difference in MSE of RotatedLC and LC with GAM by Countries on Test Set - Males",
       x=NULL) + coord_flip()

HMD_BestResultsByCountry_ToPlotFemale <- HMD_BestResultsByCountry[,c("Country", "mse_female_test_RotatedLC", "mse_female_test_LCwithGAM")]
HMD_BestResultsByCountry_ToPlotFemale$mse_female_test_Difference <- HMD_BestResultsByCountry_ToPlotFemale$mse_female_test_LCwithGAM -
  HMD_BestResultsByCountry_ToPlotFemale$mse_female_test_RotatedLC
HMD_BestResultsByCountry_ToPlotFemale$WhichIsBetter <- ifelse(HMD_BestResultsByCountry_ToPlotFemale$mse_female_test_Difference > 0,
                                                              "RotatedLC", "LCwithGAM")

ggplot(HMD_BestResultsByCountry_ToPlotFemale, aes(x=reorder(Country,mse_female_test_Difference), y=mse_female_test_Difference, fill=WhichIsBetter)) +
  geom_bar(stat='identity', position='dodge') +
  labs(title = "Difference in MSE of RotatedLC and LC with GAM by Countries on Test Set - Females",
       x=NULL) + coord_flip()