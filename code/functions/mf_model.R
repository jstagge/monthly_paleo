# *------------------------------------------------------------------
# | FUNCTION NAME: mf_model
# | FILE NAME: mf_model.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        annual_rec - a dataframe with columns "water_year" and "annual_flow"
# |                monthly_prop - a datamframe with columns "month" and "prop"
# |                first_month_wy - a numeric variable with the month that signifies the start of the water year, usually 10
# |     Out:       monthly_ts - a dataframe with results of the mf model reconstruction
# | 
# |     Desc:      This function applies the "mf model", creating a monthly
# |                flow reconstruction for an initial annual flow time series.
# |                The mf Model works by applying the same seasonal proportion to all
# |                years and scaling flows to match the annual reconstruction. 
# *------------------------------------------------------------------

mf_model <- function(annual_rec, monthly_prop, first_month_wy) {
	require(data.table)
	require(assertthat)
	
	################################################
	### Preparing Data
	#################################################
	### Change the column names of monthly_prop to produce clearer results
	colnames(monthly_prop) <- c("month", "monthly_prop")
	
	### Create a monthly time series of all combinations of months and year
	monthly_ts <- expand.grid(month = monthly_prop$month, water_year = annual_rec$water_year)
	
	### Set an order column to allow resorting at the end
	monthly_ts$t <- seq(1,dim(monthly_ts)[1])
	
	### Merge annual flows and monthly proportions
	monthly_ts <- merge(monthly_ts, annual_rec, by="water_year")
	monthly_ts <- merge(monthly_ts, monthly_prop, by="month")

	### Re-sort to obtain time series
	monthly_ts <- monthly_ts[with(monthly_ts, order(t)), ]
	
	################################################
	### Check monthly proportions sum to 1
	#################################################
	### Convert to a data.table and sum proportions by year
	monthly_ts_datatable <- data.table(monthly_ts)
	monthly_ts_datatable <- monthly_ts_datatable[,list(annual_prop_sum=sum(monthly_prop)), by='water_year']
	### And finally assert that the absolute value of proportion is not more thatn 0.000001 different from 1
    assert_that(sum((abs(as.numeric(monthly_ts_datatable$annual_prop_sum)-1) > 0.000001)) == 0)                                       
                 
	################################################
	### Calculate monthly flows
	#################################################                                       
    ### Multiply proportion by reconstructed annual
    ### Reconstructed annual is the mean annual flow, essentially monthly mean / 12 = mean annual
    ### So, we multiply by 12 to get mean monthly.
    monthly_ts$flow_rec_m3s <- (monthly_ts$annual_flow*12) * monthly_ts$monthly_prop

	################################################
	### Reformat results to return
	#################################################
	rownames(monthly_ts) <- monthly_ts$t
	monthly_ts <- monthly_ts[c("t", "month", "water_year", "annual_flow", "monthly_prop", "flow_rec_m3s")]
	
	
  return(monthly_ts)
}
