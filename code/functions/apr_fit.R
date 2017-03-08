# *------------------------------------------------------------------
# | FUNCTION NAME: apr_fit
# | FILE NAME: apr_fit.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        annual_rec - a dataframe with columns "water_year" and "annual_flow"
# |                monthly_prop - a datamframe with columns "month" and "prop"
# |                first_month_wy - a numeric variable with the month that signifies the start of the water year, usually 10
# |     Out:       monthly_ts - a dataframe with results of the null model reconstruction
# | 
# |     Desc:      This function applies the APR model using elastic nets and a
# | 				parallel analysis to find best predictors, following Stagge et al.
# |
# *------------------------------------------------------------------

apr_fit <- function(data, monthly_param, annual_param, data_name) {
	require(assertthat)
	require(glmnet)
	require(caret)
	
	################################################
	### Preparing Data
	#################################################
	### Extract monthly and annual distributions
	month_distr <- monthly_param$distr
	annual_distr <- annual_param$distr


	################################################
	### Create dataframe of annual flows
	#################################################
	annual_ts <- data.frame(water_year=data$water_year, annual_flow=data$annual_mean)
	annual_ts <- unique(annual_ts)

	################################################
	### Expand to create a full dataframe of monthly data
	#################################################
	### Create a monthly time series of all combinations of months and year
	monthly_ts <- expand.grid(month = seq(1,12), year = seq(min(data$year, na.rm=TRUE), max(data$year, na.rm=TRUE)))
	
	### Set an order column to allow resorting at the end
	monthly_ts$t <- seq(1,dim(monthly_ts)[1])
	
	### Merge observed flows and month
	monthly_ts <- merge(monthly_ts, data, by=c("year", "month"), all.x=TRUE)
	
	### Merge annual flows and month
	monthly_ts <- merge(monthly_ts, annual_ts, by=c("water_year"), all.x=TRUE)
	
	### Re-sort to obtain time series
	monthly_ts <- monthly_ts[with(monthly_ts, order(t)), ]
	

	################################################
	### Calculate lags
	#################################################	
	monthly_ts$pos_1year <- shift(monthly_ts$annual_flow, shift_by=12)
	monthly_ts$neg_1year <- shift(monthly_ts$annual_flow, shift_by=-12)
	
	
	################################################
	### Convert monthly mean into standard normal
	#################################################  
	### Create a p function for the given monthly distribution 
	q_monthly_distr <- match.fun(paste("q",monthly_distr,sep=""))
	p_monthly_distr <- match.fun(paste("p",monthly_distr,sep=""))
	
	### Create a column to hold results
	monthly_ts$monthly_norm <- NA
	for (j in seq(1,12)) {
		### Extract only those flows for a given month
		monthly_test <- monthly_ts$month == j
		monthly_perc <- monthly_ts$monthly_mean[monthly_test]
		
		### Convert monthly distribution parameters to a list
		monthly_param_list <- list(q = monthly_perc)
		monthly_param_j <- monthly_param$param[j, ]
		
		param_name <- names(monthly_param_j)
		monthly_param_list[param_name] <- monthly_param_j

		### Calculate percentile and then convert to standard normal
		monthly_perc <- do.call(p_monthly_distr, monthly_param_list)
		monthly_norm <- qnorm(monthly_perc)
		### Save back to monthly data
		monthly_ts$monthly_norm[monthly_test] <- monthly_norm
	}
	
	
	################################################
	### Convert annual into standard normal
	#################################################  
	### Create a p function for annual flows
	q_annual_distr <- match.fun(paste("q",annual_distr,sep=""))
	p_annual_distr <- match.fun(paste("p",annual_distr,sep=""))
	
	### Extract fit parameters
	annual_fit_param <- annual_param$param[annual_param$param$data == data_name,seq(2,dim(annual_param$param)[2])]

	### Convert distribution parameters to a list
	annual_param_list <- list(q = monthly_ts$annual_flow)
	### Save to parameter list
	param_name <- names(annual_fit_param)
	annual_param_list[param_name] <- annual_fit_param
	
	### Calculate annual percentile and then conver to std norm
	monthly_ts$annual_norm <- do.call(p_annual_distr, annual_param_list)
	monthly_ts$annual_norm <- qnorm(monthly_ts$annual_norm)
	
	### Calculate lags
	annual_param_list$q <- monthly_ts$neg_1year
	monthly_ts$annual_norm_neg_1year <- qnorm(do.call(p_annual_distr, annual_param_list))
	annual_param_list$q <- monthly_ts$pos_1year
	monthly_ts$annual_norm_pos_1year <- qnorm(do.call(p_annual_distr, annual_param_list))


	################################################
	### Fit monthly model
	#################################################  
	### Set elastic grid search parameters
	lambda_grid <- 10^seq(2,-6,length=100)
	#lambda_grid <- seq(0,1,length=10)
	alpha_grid <- seq(0,1,length=10)
	eGrid <- expand.grid(.alpha = alpha_grid, .lambda = lambda_grid)

	### Set control for runs, 10-fold repeated measures, 4 iterations
	Control <- trainControl(method = "repeatedcv",repeats = 8, number=10, verboseIter =FALSE)
	
	### Create parallel clusters
	require(doParallel)
	cores <- detectCores()
	cl <- makePSOCKcluster(cores)
	registerDoParallel(cl)
	
	for (j in seq(1,12)) {
		### Extract only those flows for a given month
		monthly_test <- monthly_ts$month == j
		monthly_data <- monthly_ts[monthly_test, ]
		
		### Subset to only non NA values
		monthly_complete <- monthly_data[,c("annual_norm", "annual_norm_neg_1year", "annual_norm_pos_1year", "monthly_norm")]
		monthly_complete <- monthly_complete[complete.cases(monthly_complete),]
		
		### Extract the values
		x <- as.matrix(monthly_complete[,c("annual_norm", "annual_norm_neg_1year", "annual_norm_pos_1year")])
		y <- monthly_complete$monthly_norm
					
		### Fit the model
		netFit <- train(x =x, y = y,
          method = "glmnet",
          tuneGrid = eGrid,
          trControl = Control)
		
		### Extract the best tuning parameters
		my_glmnet_model <- netFit$finalModel
		
		### Extract coefficients
		model_coef <- coef(my_glmnet_model, s = netFit$bestTune$lambda)		
		model_results <- matrix(model_coef)
		rownames(model_results) <- dimnames(model_coef)[[1]]
	
		### Combine coefficients to a single matrix
		if (j == 1) {
			final_coef <- model_results
		} else {
			final_coef <- cbind(final_coef, model_results)
		}
	}
	
	### Stop clusters
	stopCluster(cl)
	
	### return results
	colnames(final_coef) <- seq(1,12)
	return(final_coef)
}
	