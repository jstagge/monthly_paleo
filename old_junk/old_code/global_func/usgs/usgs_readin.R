# *------------------------------------------------------------------
# | FUNCTION NAME: usgs_readin
# | FILE NAME: usgs_readin.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        site_id - USGS site id
# |                param_cv - USGS parameter id
# |                destination_folder - location of USGS file
# |                time_param - time accumulation
# |                
# |     Out:       usgs_data - a dataframe from USGS data
# | 
# |     Desc:      Reads in local USGS data file using dataRetreival
# |                package.
# |                
# *------------------------------------------------------------------


usgs_readin <- function(site_id, param_cd="00060", time_param="daily", destination_folder = getwd()) {
	require(dataRetrieval)

	### Read in file
	dl_location <- file.path(destination_folder, paste0(site_id,"_",param_cd,"_",time_param,".txt"))
	usgs_data <- importRDB1(dl_location)
	
	return(usgs_data)
	}
	
	
	
	