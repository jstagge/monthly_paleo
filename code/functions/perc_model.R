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

perc_model <- function(annual_rec, monthly_param, annual_param, first_month_wy, data_name) {
	require(assertthat)
	
	################################################
	### Preparing Data
	#################################################
	### Extract monthly and annual distributions
	month_distr <- monthly_param$distr
	annual_distr <- annual_param$distr
	
	### Create a list of months starting with first_month_wy
	if (first_month_wy == 1) {
		month_list <- seq(1,12)
	} else {
		month_list <- c(seq(first_month_wy, 12), seq(1,first_month_wy-1))
	}
	
	### Create a monthly time series of all combinations of months and year
	monthly_ts <- expand.grid(month = month_list, water_year = annual_rec$water_year)
	
	### Set an order column to allow resorting at the end
	monthly_ts$t <- seq(1,dim(monthly_ts)[1])
	
	### Merge annual flows and month
	monthly_ts <- merge(monthly_ts, annual_rec, by="water_year")
	
	### Re-sort to obtain time series
	monthly_ts <- monthly_ts[with(monthly_ts, order(t)), ]
	
	
	
	################################################
	### Calculate annual percentiles
	#################################################  
	### Convert distribution parameters to a list
	annual_param_list <- list(q = monthly_ts$annual_flow)
	
	### Extract fit parameters
	annual_fit_param <- annual_param$param[annual_param$param$data == data_name,seq(2,dim(annual_param$param)[2])]
	
	### Save to parameter list
	param_name <- names(annual_fit_param)
	annual_param_list[param_name] <- annual_fit_param
	
	### Create a p function for the given annual distribution and run to obtain
	### Annual time series of percentiles
	p_annual_distr <- match.fun(paste("p",annual_distr,sep=""))
	monthly_ts$annual_perc <- do.call(p_annual_distr, annual_param_list)
	
	################################################
	### Prepare to calculate monthly percentiles
	#################################################  
	### Create a p function for the given monthly distribution 
	q_monthly_distr <- match.fun(paste("q",monthly_distr,sep=""))
	
	################################################
	### Calculate monthly percentiles
	#################################################  
	for (j in seq(1,12)) {
		### Extract only those flows for a given month
		monthly_test <- monthly_ts$month == j
		monthly_perc <- monthly_ts$annual_perc[monthly_test]
		
		### Convert monthly distribution parameters to a list
		monthly_param_list <- list(p = monthly_perc)
		monthly_param_j <- monthly_param$param[j, ]
		
		param_name <- names(monthly_param_j)
		monthly_param_list[param_name] <- monthly_param_j

		### Add a blank column to save results if first month
		if (j == 1) {monthly_ts$flow_rec_m3s <- NA}
		
		### Save results back to monthly time series
		monthly_ts$flow_rec_m3s[monthly_test] <- do.call(q_monthly_distr, monthly_param_list)
	}
	      
	  
	################################################
	### Reformat results to return
	#################################################
	rownames(monthly_ts) <- monthly_ts$t
	monthly_ts <- monthly_ts[c("t", "month", "water_year", "annual_flow", "annual_perc", "flow_rec_m3s")]
	
	
  return(monthly_ts)
}
