
# Function for loading packages
installOrLoadPack <- function(pack) {
  
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  
  if (length(create.pkg))
    
    install.packages(create.pkg, dependencies = TRUE)
  
  sapply(pack, require, character.only = TRUE)
}

# Function to download and clean up WPP2017 Data
CleanUp_WPP <- function(country) {
  
  installOrLoadPack(c("wpp2017"))
  
  data(mxF, mxM, e0Fproj, e0Mproj, e0F, e0M, package = "wpp2017")
  
  mxm_train <- subset(mxM, name == country)[,4:14]
  mxf_train <- subset(mxF, name == country)[,4:14]
  
  mxm_test <- subset(mxM, name == country)[,15:16]
  mxf_test <- subset(mxF, name == country)[,15:16]
  
  e0fproj <- c(as.numeric(subset(e0F, name == country)[colnames(mxf_test)[colnames(mxf_test) %in% colnames(e0F)]]),
               as.numeric(subset(e0Fproj, name == country)[colnames(mxf_test)[colnames(mxf_test) %in% colnames(e0Fproj)]]))
  
  e0mproj <- c(as.numeric(subset(e0M, name == country)[colnames(mxm_test)[colnames(mxm_test) %in% colnames(e0M)]]),
               as.numeric(subset(e0Mproj, name == country)[colnames(mxm_test)[colnames(mxm_test) %in% colnames(e0Mproj)]]))
  
  rownames(mxm_train) <- c(0,1, seq(5, 100, by=5))
  rownames(mxf_train) <- c(0,1, seq(5, 100, by=5))
  
  rownames(mxm_test) <- c(0,1, seq(5, 100, by=5))
  rownames(mxf_test) <- c(0,1, seq(5, 100, by=5))
  
  return(list(
    "mx_male_train"=mxm_train,
    "mx_female_train"=mxf_train,
    "mx_male_test" = mxm_test,
    "mx_female_test" = mxf_test,
    "e0_male_proj" = e0mproj,
    "e0_female_proj" = e0fproj
  ))
}

# Function to download and clean up HMD Data
CleanUp_HMD <- function(country, MinimalYear, EndOfTrainingYear, EndOfValidationYear) {
  
  installOrLoadPack(c("MortalityLaws", "tidyr"))
  
  #Mortality rates
  mxt <- ReadHMD("mx"
                 , countries = country
                 , interval = "1x1"
                 , "peter.vekas@uni-corvinus.hu"
                 , "aktuarius")
  mxt <- mxt$data
  
  mxt <- mxt[mxt$Year>=MinimalYear,]
  
  
  mx_male <- mxt[,c("Age", "Year", "Male")] %>% spread(Year, Male)
  row.names(mx_male) <- mx_male$Age
  mx_male$Age <- NULL
  mx_male <- as.matrix(mx_male[1:101,])
  mx_male[mx_male == 0] <- NA
  mx_male <- zoo::na.approx(mx_male)
  mx_male[is.na(mx_male)] <- 0
  
  mx_female <- mxt[,c("Age", "Year", "Female")] %>% spread(Year, Female)
  row.names(mx_female) <- mx_female$Age
  mx_female$Age <- NULL
  mx_female <- as.matrix(mx_female[1:101,])
  mx_female[mx_female == 0] <- NA
  mx_female <- zoo::na.approx(mx_female)
  mx_female[is.na(mx_female)] <- 0
  
  # Go through each row and determine if a value is zero
  row_sub_male = apply(mx_male, 1, function(row) all(row !=0 ))
  row_sub_female = apply(mx_female, 1, function(row) all(row !=0 ))
  
  row_sub <- row_sub_male & row_sub_female
  
  # Subset as usual
  mx_male <- mx_male[row_sub,]
  mx_female <- mx_female[row_sub,]
  
  # Select Train, Validation and Test Sets
  
  mxm_train <- mx_male[,1:which(colnames(mx_male)==EndOfTrainingYear)]
  mxf_train <- mx_female[,1:which(colnames(mx_female)==EndOfTrainingYear)]
  
  mxm_validation <- mx_male[,(which(colnames(mx_male)==EndOfTrainingYear)+1):which(colnames(mx_male)==EndOfValidationYear)]
  mxf_validation <- mx_female[,(which(colnames(mx_female)==EndOfTrainingYear)+1):which(colnames(mx_female)==EndOfValidationYear)]
  
  mxm_test <- mx_male[,(which(colnames(mx_male)==EndOfValidationYear)+1):ncol(mx_male)]
  mxf_test <- mx_female[,(which(colnames(mx_female)==EndOfValidationYear)+1):ncol(mx_female)]
  
  return(list(
    "mx_male_train"=mxm_train,
    "mx_female_train"=mxf_train,
    "mx_male_validation" = mxm_validation,
    "mx_female_validation" = mxf_validation,
    "mx_male_test" = mxm_test,
    "mx_female_test" = mxf_test
  ))
}

# Function for for calculationg life expectancy at birth fot a given year
ForecastLifeExpectancy <- function(qx_matrix, Year) {
  est_e0 <- 0
  
  for (i in 1:nrow(qx_matrix)) {
    temp_result <- 1
    for (j in 0:(i-1)) {
      temp_result <- temp_result * (1-qx_matrix[(1+j),Year+j])
    }
    est_e0 <- est_e0 + temp_result
  }
  
  est_e0 <- est_e0 + 1/2
  
  return(est_e0)
}

# Function for extracting m_x forecasts from Rotated LC for both sexes
ForecastRotatedLC <- function(mx_male, mx_female, nx,
                              e0_female_proj, e0_male_proj, p, rotation_start_e0,
                              ForecastPeriods,
                              IsLog) {
  
  installOrLoadPack(c("MortCast", "forecast"))
  
  li_lc <- lileecarter.estimate(mx_male, mx_female, nx = nx)
  
  ARIMA_male <- auto.arima(li_lc$male$kt)
  #ARIMA_male <- arima(li_lc$male$kt, order=as.vector(arimaorder(ARIMA_male)), include.mean=TRUE)
  ktproj_male <- forecast(ARIMA_male, h=ForecastPeriods+nrow(mx_male))
  ktproj_male <- ktproj_male$mean
  #ktproj_male <- (ktproj_male$lower[,2]+ktproj_male$upper[,2])/2
  
  
  
  ARIMA_female <- auto.arima(li_lc$female$kt)
  #ARIMA_female <- arima(li_lc$female$kt, order=as.vector(arimaorder(ARIMA_female)), include.mean=TRUE)
  ktproj_female <- forecast(ARIMA_female, h=ForecastPeriods+nrow(mx_female))
  ktproj_female <- ktproj_female$mean
  #ktproj_female <- (ktproj_female$lower[,2]+ktproj_female$upper[,2])/2
  
  
  if (is.null(e0_female_proj)|is.null(e0_male_proj)) {
    plainlc_est_mx_male <- exp(li_lc$male$ax + optimbase::transpose(li_lc$male$bx) %*% as.vector(ktproj_male))
    est_qx_male <- plainlc_est_mx_male/(1+0.5*plainlc_est_mx_male)
    
    e0_male_proj <- rep(NA, ForecastPeriods)
    
    for (YearIndex in 1:ForecastPeriods) {
      e0_male_proj[YearIndex] <- ForecastLifeExpectancy(est_qx_male, YearIndex)
    }
    # for testing only
    #print(e0_male_proj)
    
    plainlc_est_mx_female <- exp(li_lc$female$ax + optimbase::transpose(li_lc$female$bx) %*% as.vector(ktproj_female))
    est_qx_female <- plainlc_est_mx_female/(1+0.5*plainlc_est_mx_female)
    
    e0_female_proj <- rep(NA, ForecastPeriods)
    
    for (YearIndex in 1:ForecastPeriods) {
      e0_female_proj[YearIndex] <- ForecastLifeExpectancy(est_qx_female, YearIndex)
    }
    # for testing only
    #print(e0_female_proj)
  }
  
  rotlc <- rotate.leecarter(li_lc$bx, li_lc$ultimate.bx, (e0_female_proj + e0_male_proj)/2, p=p, e0l = rotation_start_e0)
  
  if (IsLog ==TRUE) {
    est_mx_male <- li_lc$male$ax + rotlc[,1:ForecastPeriods] %*% diag(ktproj_male[1:ForecastPeriods])
    
    est_mx_female <- li_lc$female$ax + rotlc[,1:ForecastPeriods] %*% diag(ktproj_female[1:ForecastPeriods])
  } else {
    est_mx_male <- exp(li_lc$male$ax + rotlc[,1:ForecastPeriods] %*% diag(ktproj_male[1:ForecastPeriods]))
    
    est_mx_female <- exp(li_lc$female$ax + rotlc[,1:ForecastPeriods] %*% diag(ktproj_female[1:ForecastPeriods]))
  }
  
  return(list(
    "est_mx_male"=est_mx_male,
    "est_mx_female"=est_mx_female,
    "est_e0_female" = e0_female_proj,
    "est_e0_male" = e0_male_proj,
    "arima_male_coef" = ARIMA_male$coef,
    "arima_female_coef" = ARIMA_female$coef
  ))
}

# Function for optimising Rotated LC along parameters 'p' and 'e0l' on a separate validation set according to RMSE
OptimizeRotatedLC <- function(mx_male_train, mx_female_train,
                              mx_male_validation, mx_female_validation,
                              mx_male_test, mx_female_test,
                              nx,
                              e0_female_proj, e0_male_proj,
                              is_log) {
  
  installOrLoadPack(c("utils"))
  
  pb = txtProgressBar(min = 0, max = 99, initial = 0, style = 3)
  
  results <- NULL
  
  for (i in 1:100) {
    
    p <- (i-1)/100
    
    for (j in 60:90) {
      
      e0_start <- j
      
      RotatedLC_Object <- ForecastRotatedLC(mx_male_train, mx_female_train, nx, e0_female_proj, e0_male_proj, p,
                                            rotation_start_e0 = e0_start,
                                            ncol(mx_male_validation)+ncol(mx_male_test),
                                            is_log)
      
      estim_mx_male <- RotatedLC_Object$est_mx_male
      estim_mx_female <- RotatedLC_Object$est_mx_female
      
      if (is_log==TRUE) {
        mse_male_validation <- mean((log(as.matrix(mx_male_validation)) - estim_mx_male[,1:ncol(mx_male_validation)])^2)
        mse_female_validation <- mean((log(as.matrix(mx_female_validation)) - estim_mx_female[,1:ncol(mx_female_validation)])^2)
        
        mse_male_test <- mean((log(as.matrix(mx_male_test)) - estim_mx_male[,(ncol(mx_male_validation)+1):ncol(estim_mx_male)])^2)
        mse_female_test <- mean((log(as.matrix(mx_female_test)) - estim_mx_female[,(ncol(mx_female_validation)+1):ncol(estim_mx_female)])^2)
      } else {
        mse_male_validation <- mean((as.matrix(mx_male_validation) - estim_mx_male[,1:ncol(mx_male_validation)])^2)
        mse_female_validation <- mean((as.matrix(mx_female_validation) - estim_mx_female[,1:ncol(mx_female_validation)])^2)
        
        mse_male_test <- mean((as.matrix(mx_male_test) - estim_mx_male[,(ncol(mx_male_validation)+1):ncol(estim_mx_male)])^2)
        mse_female_test <- mean((as.matrix(mx_female_test) - estim_mx_female[,(ncol(mx_female_validation)+1):ncol(estim_mx_female)])^2)
      }
      
      results <- rbind(results, c(p, e0_start, mse_male_validation, mse_female_validation, mse_male_test, mse_female_test))
      
    }
    
    setTxtProgressBar(pb,i)
    
  }
  
  estim_e0_start <- min((RotatedLC_Object$est_e0_male+RotatedLC_Object$est_e0_female)/2)
  estim_e0_end <- max((RotatedLC_Object$est_e0_male+RotatedLC_Object$est_e0_female)/2)
  
  colnames(results) <- c("p", "e0l", "mse_male_validation", "mse_female_validation", "mse_male_test", "mse_female_test")
  
  results <- as.data.frame(results)
  
  optimal_p_male <- results$p[results$mse_male_validation==min(results$mse_male_validation)]
  optimal_p_female <- results$p[results$mse_female_validation==min(results$mse_female_validation)]
  
  optimal_e0l_male <- results$e0l[results$mse_male_validation==min(results$mse_male_validation)]
  optimal_e0l_female <- results$e0l[results$mse_female_validation==min(results$mse_female_validation)]
  
  if (length(optimal_p_male)>1 | length(optimal_e0l_male)>1) {
    
    optimum_set <- results[results$mse_male_validation==min(results$mse_male_validation),]
    
    if (estim_e0_end < min(optimum_set$e0l)) {
      
      optimal_p_male <- "no rotation"
      optimal_e0l_male <- "no rotation"
      
    } else {
      
      optimum_set <- optimum_set[optimum_set$e0l <= estim_e0_end,]
      
      optimal_e0l_male <- c(min(optimum_set$e0l), max(optimum_set$e0l))
      
      optimal_p_male <- c(min(optimum_set$p[optimum_set$e0l==min(optimum_set$e0l)]),
                          max(optimum_set$p[optimum_set$e0l==min(optimum_set$e0l)]),
                          min(optimum_set$p[optimum_set$e0l==max(optimum_set$e0l)]),
                          max(optimum_set$p[optimum_set$e0l==max(optimum_set$e0l)]))
      
    }
  }
  
  if (length(optimal_p_female)>1 | length(optimal_e0l_female)>1) {
    
    optimum_set <- results[results$mse_female_validation==min(results$mse_female_validation),]
    
    if (estim_e0_end < min(optimum_set$e0l)) {
      
      optimal_p_female <- "no rotation"
      optimal_e0l_female <- "no rotation"
      
    } else {
      
      optimum_set <- optimum_set[optimum_set$e0l <= estim_e0_end,]
      
      optimal_e0l_female <- c(min(optimum_set$e0l), max(optimum_set$e0l))
      
      optimal_p_female <-  c(min(optimum_set$p[optimum_set$e0l==min(optimum_set$e0l)]),
                             max(optimum_set$p[optimum_set$e0l==min(optimum_set$e0l)]),
                             min(optimum_set$p[optimum_set$e0l==max(optimum_set$e0l)]),
                             max(optimum_set$p[optimum_set$e0l==max(optimum_set$e0l)]))
      
    }
  }
  
  return(list(
    "mse_table"=results,
    "optimal_p_male"=optimal_p_male,
    "optimal_p_female"=optimal_p_female,
    "optimal_e0l_male"=optimal_e0l_male,
    "optimal_e0l_female"=optimal_e0l_female
  ))
}

# Function for extracting m_x forecasts from Plain LC + GAM model for both sexes
ForecastLCwithGAM <- function(mx_male, mx_female, nx,
                              k_base, basis_form,
                              ForecastPeriods,
                              IsLog) {
  installOrLoadPack(c("MortCast", "forecast", "mgcv"))
  
  li_lc <- lileecarter.estimate(mx_male, mx_female, nx = nx)
  
  ARIMA_male <- auto.arima(li_lc$male$kt)
  #ARIMA_male <- arima(li_lc$male$kt, order=as.vector(arimaorder(ARIMA_male)), include.mean=TRUE)
  ktproj_male <- forecast(ARIMA_male, h=ForecastPeriods+nrow(mx_male))
  ktproj_male <- ktproj_male$mean
  #ktproj_male <- (ktproj_male$lower[,2]+ktproj_male$upper[,2])/2
  
  
  
  ARIMA_female <- auto.arima(li_lc$female$kt)
  #ARIMA_female <- arima(li_lc$female$kt, order=as.vector(arimaorder(ARIMA_female)), include.mean=TRUE)
  ktproj_female <- forecast(ARIMA_female, h=ForecastPeriods+nrow(mx_female))
  ktproj_female <- ktproj_female$mean
  
  if (IsLog ==TRUE) {
    residual_male <- (li_lc$male$ax + optimbase::transpose(li_lc$male$bx) %*% as.vector(li_lc$male$kt)) - log(mx_male)
    
    residual_female <- (li_lc$female$ax + optimbase::transpose(li_lc$female$bx) %*% as.vector(li_lc$female$kt)) - log(mx_female)
  } else {
    residual_male <- exp(li_lc$male$ax + optimbase::transpose(li_lc$male$bx) %*% as.vector(li_lc$male$kt)) - mx_male
    
    residual_female <- exp(li_lc$female$ax + optimbase::transpose(li_lc$female$bx) %*% as.vector(li_lc$female$kt)) - mx_female
  }
  
  # GAM modelling for male m_x residuals
  
  x <- rep(0:(nrow(mx_male)-1), ncol(mx_male))
  t <- rep(0, nrow(mx_male) * ncol(mx_male))
  residual_vector <- rep(0, nrow(mx_male) * ncol(mx_male))
  
  actual_row <- 1
  
  for (year in 1:ncol(mx_male)){
    for (age in 0:(nrow(mx_male)-1)){
      residual_vector[actual_row] <- residual_male[age+1, year]
      actual_row <- actual_row+1
    }}
  
  for (row in 1:(nrow(mx_male) * ncol(mx_male))){
    t[row] <- floor((row - 1) / nrow(mx_male)) + 1}
  
  residtable <- data.frame(residual_vector, x, t)
  
  if (length(unique(t)) < (k_base^2)) {
    
    k_base_restricted <- length(unique(t))
    
    smoothing_male <- bam(residual_vector ~ ti(x, k = (k_base^1), bs = basis_form) +
                            ti(t, k = (k_base^1), bs = basis_form) +
                            ti(x, t, k = (k_base_restricted), bs = basis_form),
                          data=residtable, family=gaussian, method="fREML")
  } else{
    smoothing_male <- bam(residual_vector ~ ti(x, k = (k_base^1), bs = basis_form) +
                            ti(t, k = (k_base^1), bs = basis_form) +
                            ti(x, t, k = (k_base^2), bs = basis_form),
                          data=residtable, family=gaussian, method="fREML")
  }
  
  gc()
  
  #plotmo::plotmo(smoothing)
  
  # GAM forecast for male residuals
  
  x_forecast <- rep(0:(nrow(mx_male)-1), ForecastPeriods)
  t_forecast <- rep(0, nrow(mx_male) * ForecastPeriods)
  
  for (row in 1:(nrow(mx_male) * ForecastPeriods)){
    t_forecast[row] <- floor((row - 1) / nrow(mx_male)) + (ncol(mx_male) + 1)}
  
  residtable_forecast <- data.frame(x_forecast, t_forecast)
  colnames(residtable_forecast) <- c("x", "t")
  
  predicted_residual_vector <- predict(smoothing_male, newdata = residtable_forecast)
  
  predicted_residual_matrix <- matrix(predicted_residual_vector, nrow = nrow(mx_male), ncol = ForecastPeriods)
  
  # Forecast of male m_x
  
  if (IsLog ==TRUE) {
    est_mx_male <- (li_lc$male$ax + optimbase::transpose(li_lc$male$bx) %*% as.vector(ktproj_male[1:ForecastPeriods])) -
      predicted_residual_matrix
  } else {
    est_mx_male <- exp(li_lc$male$ax + optimbase::transpose(li_lc$male$bx) %*% as.vector(ktproj_male[1:ForecastPeriods])) -
      predicted_residual_matrix
  }
  
  # GAM modelling for female m_x residuals
  
  x <- rep(0:(nrow(mx_female)-1), ncol(mx_female))
  t <- rep(0, nrow(mx_female) * ncol(mx_female))
  residual_vector <- rep(0, nrow(mx_female) * ncol(mx_female))
  
  actual_row <- 1
  
  for (year in 1:ncol(mx_female)){
    for (age in 0:(nrow(mx_female)-1)){
      residual_vector[actual_row] <- residual_female[age+1, year]
      actual_row <- actual_row+1
    }}
  
  for (row in 1:(nrow(mx_female) * ncol(mx_female))){
    t[row] <- floor((row - 1) / nrow(mx_female)) + 1}
  
  residtable <- data.frame(residual_vector, x, t)
  
  if (length(unique(t)) < (k_base^2)) {
    k_base_restricted <- length(unique(t))
    
    smoothing_female <- bam(residual_vector ~ ti(x, k = (k_base^1), bs = basis_form) +
                            ti(t, k = (k_base^1), bs = basis_form) +
                            ti(x, t, k = (k_base_restricted), bs = basis_form),
                          data=residtable, family=gaussian, method="fREML")
  } else{
    smoothing_female <- bam(residual_vector ~ ti(x, k = (k_base^1), bs = basis_form) +
                            ti(t, k = (k_base^1), bs = basis_form) +
                            ti(x, t, k = (k_base^2), bs = basis_form),
                          data=residtable, family=gaussian, method="fREML")
  }
  
  gc()
  
  #plotmo::plotmo(smoothing)
  
  # GAM forecast for female residuals
  
  x_forecast <- rep(0:(nrow(mx_female)-1), ForecastPeriods)
  t_forecast <- rep(0, nrow(mx_female) * ForecastPeriods)
  
  for (row in 1:(nrow(mx_female) * ForecastPeriods)){
    t_forecast[row] <- floor((row - 1) / nrow(mx_female)) + (ncol(mx_female) + 1)}
  
  residtable_forecast <- data.frame(x_forecast, t_forecast)
  colnames(residtable_forecast) <- c("x", "t")
  
  predicted_residual_vector <- predict(smoothing_female, newdata = residtable_forecast)
  
  predicted_residual_matrix <- matrix(predicted_residual_vector, nrow = nrow(mx_female), ncol = ForecastPeriods)
  
  # Forecast of female m_x
  
  if (IsLog ==TRUE) {
    est_mx_female <- (li_lc$female$ax + optimbase::transpose(li_lc$female$bx) %*% as.vector(ktproj_female[1:ForecastPeriods])) -
      predicted_residual_matrix
  } else {
    est_mx_female <- exp(li_lc$female$ax + optimbase::transpose(li_lc$female$bx) %*% as.vector(ktproj_female[1:ForecastPeriods])) -
      predicted_residual_matrix
  }
  
  return(list(
    "est_mx_male"=est_mx_male,
    "est_mx_female"=est_mx_female,
    "GAM_male"=smoothing_male,
    "GAM_female"=smoothing_female,
    "arima_male_coef" = ARIMA_male$coef,
    "arima_female_coef" = ARIMA_female$coef
  ))
}

# Function for optimising LC+GAM along parameters 'k' and 'bs' on a separate validation set according to RMSE
OptimizeLCwithGAM <- function(mx_male_train, mx_female_train,
                              mx_male_validation, mx_female_validation,
                              mx_male_test, mx_female_test,
                              nx,
                              is_log) {
  
  installOrLoadPack(c("utils"))
  
  basis_form_vector <- c("cs", "tp", "ts")
  k_base_vector <- c(2, 3, 4, 5, 6, 7)
  
  pb = txtProgressBar(min = 0, max = (length(basis_form_vector)*length(k_base_vector)-1), initial = 0, style = 3)
  
  results <- NULL
  counter <- 1
  
  for (i in 1:length(basis_form_vector)) {
    
    for (j in 1:length(k_base_vector)) {
      
      tryCatch(
        expr = {
          LCwithGAM_Object <- ForecastLCwithGAM(mx_male = mx_male_train, mx_female = mx_female_train, nx=1,
                                                k_base = k_base_vector[j], basis_form = basis_form_vector[i],
                                                ForecastPeriods = ncol(mx_male_validation)+ncol(mx_male_test),
                                                IsLog = is_log)
          
          estim_mx_male <- LCwithGAM_Object$est_mx_male
          estim_mx_female <- LCwithGAM_Object$est_mx_female
          
          if (is_log==TRUE) {
            mse_male_validation <- mean((log(as.matrix(mx_male_validation)) - estim_mx_male[,1:ncol(mx_male_validation)])^2)
            mse_female_validation <- mean((log(as.matrix(mx_female_validation)) - estim_mx_female[,1:ncol(mx_female_validation)])^2)
            
            mse_male_test <- mean((log(as.matrix(mx_male_test)) - estim_mx_male[,(ncol(mx_male_validation)+1):ncol(estim_mx_male)])^2)
            mse_female_test <- mean((log(as.matrix(mx_female_test)) - estim_mx_female[,(ncol(mx_female_validation)+1):ncol(estim_mx_female)])^2)
          } else {
            mse_male_validation <- mean((as.matrix(mx_male_validation) - estim_mx_male[,1:ncol(mx_male_validation)])^2)
            mse_female_validation <- mean((as.matrix(mx_female_validation) - estim_mx_female[,1:ncol(mx_female_validation)])^2)
            
            mse_male_test <- mean((as.matrix(mx_male_test) - estim_mx_male[,(ncol(mx_male_validation)+1):ncol(estim_mx_male)])^2)
            mse_female_test <- mean((as.matrix(mx_female_test) - estim_mx_female[,(ncol(mx_female_validation)+1):ncol(estim_mx_female)])^2)
          }
        },
        error = function(error_condition){ 
          mse_male_validation <- error_condition
          mse_female_validation <- error_condition
          mse_male_test <- error_condition
          mse_female_test <- error_condition
        }
      )
      
      results <- rbind(results, c(basis_form_vector[i], k_base_vector[j], mse_male_validation, mse_female_validation, mse_male_test, mse_female_test))
      
      setTxtProgressBar(pb,counter)
      counter <- counter + 1
    }
    
  }
  
  colnames(results) <- c("basis_form", "k_base", "mse_male_validation", "mse_female_validation", "mse_male_test", "mse_female_test")
  
  results <- as.data.frame(results)
  
  results[,2:ncol(results)] <- lapply(results[,2:ncol(results)], as.numeric)
  
  optimal_parameters_male <- results[results$mse_male_validation == min(results$mse_male_validation),]
  optimal_parameters_female <- results[results$mse_female_validation == min(results$mse_female_validation),]
  
  return(list(
    "mse_table"=results,
    "optimal_parameters_male"=optimal_parameters_male,
    "optimal_parameters_female"=optimal_parameters_female
  ))
}
