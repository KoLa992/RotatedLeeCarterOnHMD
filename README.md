# Rotated Lee-Carter On HMD

Results for optimizing hyperparameters of the Rotated LC on HMD data.

Train Set: 1950-1990 <br>
Validation Set: 1991-1999 <br>
Test Set: 2000-

We have 5 files for now:

- **HMD_RotatedLC_Full_LogMSE.Rda**: Contains a data frame that stores all the *log(mse)* measures of the Rotated LC model for both sexes on the validation and test sets along all the *p* and *e_0* hyperparameter values examined. Results are shown for each country in the HMD.
- **HMD_RotatedLC_Full_LogMSE.csv**: The same data frame as in **HMD_RotatedLC_Full_LogMSE.Rda**, but in *csv* format if for some reason the *Rda* becomes incompatible with latest R versions.
- **HMD_BestResultsOnTestByCountry_LogMSE.Rda**: A pivot table from **HMD_RotatedLC_Full_LogMSE.Rda**. Contains the best *log(mse)* results per country for both sexes on the test set.
- **HMD_BestResultsOnTestByCountry_LogMSE.csv**: The same table as in **HMD_BestResultsOnTestByCountry_LogMSE.Rda**, but in *csv* format if for some reason the *Rda* becomes incompatible with latest R versions.
- **HeatMapVisualizationCodes.R**: R script file for heatmap visualizations of the *log(mse)* surfaces as a function of *p* and *e_0* for each country.
