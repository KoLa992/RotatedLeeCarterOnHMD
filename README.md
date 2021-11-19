# Rotated Lee-Carter and Lee-Carter with GAM On HMD

Results for optimizing hyperparameters of the Rotated LC and the LC with GAM on HMD data.

Train Set: 1950-1990 <br>
Validation Set: 1991-1999 <br>
Test Set: 2000-

We have 8 files for now:

- **HMD_RotatedLC_Full_LogMSE.Rda**: Contains a data frame that stores all the *log(mse)* measures of the Rotated LC model for both sexes on the validation and test sets along all the *p* and *e_0* hyperparameter values examined. Results are shown for each country in the HMD.
- **HMD_RotatedLC_Full_LogMSE.csv**: The same data frame as in **HMD_RotatedLC_Full_LogMSE.Rda**, but in *csv* format if for some reason the *Rda* becomes incompatible with latest R versions.
- **HMD_LCwithGAM_Full_LogMSE.Rda**: Contains a data frame that stores all the *log(mse)* measures of the LC with GAM model for both sexes on the validation and test sets along all the *basis_form* and *k_base* hyperparameter values examined. Results are shown for each country in the HMD.
- **HMD_LCwithGAM_Full_LogMSE.csv**: The same data frame as in **HMD_LCwithGAM_Full_LogMSE.Rda**, but in *csv* format if for some reason the *Rda* becomes incompatible with latest R versions.
- **HMD_BestResultsByCountry_Rotated+GAM.Rda**: A pivot table from the joined **HMD_RotatedLC_Full_LogMSE.Rda** and **HMD_LCwithGAM_Full_LogMSE.Rda** tables. Contains the best *log(mse)* results per country of both models for both sexes on the test and validation sets.
- **HMD_BestResultsByCountry_Rotated+GAM.csv**: The same table as in **HMD_BestResultsByCountry_Rotated+GAM.Rda**, but in *csv* format if for some reason the *Rda* becomes incompatible with latest R versions.
- **HeatMapVisualizationCodes.R**: R script file for heatmap visualizations of the *log(mse)* surfaces of the Rotated LC as a function of *p* and *e_0* for each country.
- **LCwithGAM_VS_RotatedLC.R**: R script file for visualizing the *log(mse)* differences of the LC with GAM and Rotated LC models for each country.
