# *------------------------------------------------------------------
# | FUNCTION NAME: save_reconstruction
# | FILE NAME: save_reconstruction.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        month_ts - result of a reconstruction (null, percentile) run
# |                site_id - site id for gauge
# |                site_name - site name for gauge
# |                output_name - name for output, corresponding to whether it is null or percentile model (used in file naming)
# |                data_name - name for type if input data (used in file naming)
# |					method - a detailed description of model and data used in method column
# |     Out:       month_ts_results - the dataframe that is saved to a directory
# | 
# |     Desc:      This function saves time series reconstruction in the same format for null,
# |					percentile, or any other model. 
# *------------------------------------------------------------------


save_reconstruction <- function(month_ts, site_id, site_name, output_name, data_name, method, write_folder) {

	### Calculate dates	
	year_list <- usgs_wateryear_inverse(water_year=month_ts$water_year, month=month_ts$month)
	date_list = mid_month(year_list, month_ts$month)
	
	### Determine second to last column name (will differ using null or perc models)
	penult_col <- tail(names(month_ts), 2)[[1]]
	penult_vals <-  matrix(month_ts[,penult_col])
	colnames(penult_vals) <- penult_col
	
	### Reorganize data before saving
	month_ts_results <- data.frame(site_id = site_id, site_name=site_name, water_year = month_ts$water_year, year=year_list,  month = month_ts$month, date = date_list, penult_vals, flow_rec_m3s=month_ts$flow_rec_m3s, method=method)
	
	
	### Write result to CSV file
	write_location <- file.path(write_folder, paste0(site_id,"_",output_name, "_reconst_ts_",data_name,".csv"))
	write.csv(month_ts_results, file = write_location,row.names=FALSE)

}

