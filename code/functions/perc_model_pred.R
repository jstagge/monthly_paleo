# *------------------------------------------------------------------
# | FUNCTION NAME: null_model
# | FILE NAME: null_model.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        annual_rec - a dataframe with columns "water_year" and "annual_flow"
# |                monthly_prop - a datamframe with columns "month" and "prop"
# |                first_month_wy - a numeric variable with the month that signifies the start of the water year, usually 10
# |     Out:       monthly_ts - a dataframe with results of the null model reconstruction
# | 
# |     Desc:      This function applies the "null model", creating a monthly
# |                flow reconstruction for an initial annual flow time series.
# |                The Null Model works by applying the same seasonal proportion to all
# |                years and scaling flows to match the annual reconstruction. 
# *------------------------------------------------------------------

perc_model_pred <- function(annual_rec, predictors, monthly_param, annual_param, norm_model, first_month_wy, data_name) {
	require(assertthat)
	
	################################################
	### Preparing Data
	#################################################
	### Extract monthly and annual distributions
	month_distr <- monthly_param$distr
	annual_distr <- annual_param$distr


	################################################
	### Create dataframe of annual flows
	#################################################
	annual_ts <- data.frame(water_year=annual_rec$water_year, annual_flow=annual_rec$annual_flow)
	annual_ts <- unique(annual_ts)

	################################################
	### Expand to create a full dataframe of monthly data
	#################################################
	### Create a monthly time series of all combinations of months and year
	monthly_ts <- expand.grid(month = seq(1,12), water_year = seq(min(annual_rec$water_year, na.rm=TRUE), max(annual_rec$water_year, na.rm=TRUE)))
	
	### Calculate Year
	monthly_ts$year <- usgs_wateryear_inverse(water_year=monthly_ts$water_year, month=monthly_ts$month, first_month=first_month_wy)

	### Merge annual flows and month
	monthly_ts <- merge(monthly_ts, annual_ts, by=c("water_year"), all.x=TRUE)
	
	### Re-sort to obtain time series
	monthly_ts <- monthly_ts[with(monthly_ts, order(year, month)), ]
	

	################################################
	### Calculate lags
	#################################################	
	monthly_ts$pos_1year <- shift(monthly_ts$annual_flow, shift_by=12)
	monthly_ts$neg_1year <- shift(monthly_ts$annual_flow, shift_by=-12)
	
	
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


	### Merge time series with predictors
	monthly_ts <- merge(monthly_ts, predictors, by.x="year", by.y="Year", all.x=TRUE)

	################################################
	### Calculate monthly standard normal
	#################################################  
	### Create a holder for eventual results
	monthly_ts$monthly_norm <- NA
	
	for (j in seq(1,12)) {
		### Extract only those flows for a given month
		monthly_test <- monthly_ts$month == j
		monthly_subset <- monthly_ts[monthly_test,]
		
		### Extract the standard normal model for this month
		norm_model_subset <- norm_model[,j]
		names(norm_model_subset) <- rownames(norm_model)
		
		### Apply the model
		norm_model_subset_name <- names(norm_model_subset)
		norm_model_subset_name <- norm_model_subset_name[seq(2,length(norm_model_subset_name))]
		
		### Calculate monthly standard normal
		monthly_norm <- norm_model_subset["(Intercept)"]
		for (k in seq(1, length(norm_model_subset_name))) {
			var_name <- norm_model_subset_name[[k]]
			monthly_norm <- monthly_norm + monthly_subset[,var_name] * norm_model_subset[var_name]
		}
		
		### Save back to data frame
		monthly_ts$monthly_norm[monthly_test] <- monthly_norm
	}
	
	

	################################################
	### Prepare to calculate monthly percentiles
	#################################################  
	### Create a p function for the given monthly distribution 
	q_monthly_distr <- match.fun(paste("q",monthly_distr,sep=""))

	### Convert monthly normalized values to percentiles
	monthly_ts$monthly_perc <- pnorm(monthly_ts$monthly_norm)
	### Create a holder for results
	monthly_ts$flow_rec_m3s <- NA
	
	################################################
	### Calculate monthly percentiles
	#################################################  
	for (j in seq(1,12)) {
		### Extract only those flows for a given month
		monthly_test <- monthly_ts$month == j
		monthly_perc <- monthly_ts$monthly_perc[monthly_test]
		
		### Convert monthly distribution parameters to a list
		monthly_param_list <- list(p = monthly_perc)
		monthly_param_j <- monthly_param$param[j, ]
		
		param_name <- names(monthly_param_j)
		monthly_param_list[param_name] <- monthly_param_j

		### Save results back to monthly time series
		monthly_ts$flow_rec_m3s[monthly_test] <- do.call(q_monthly_distr, monthly_param_list)
	}
	      
	  
	################################################
	### Reformat results to return
	#################################################
	monthly_ts <- monthly_ts[with(monthly_ts, order(year, month)), ]
	monthly_ts <- monthly_ts[c("month", "water_year", "annual_flow", "monthly_norm", "flow_rec_m3s")]
	
  return(monthly_ts)
}
