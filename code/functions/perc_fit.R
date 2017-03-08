# *------------------------------------------------------------------
# | FUNCTION NAME: perc_fit
# | FILE NAME: perc_fit.R
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

perc_fit <- function(flows, distr, plot_name, write_folder) {
	require(fitdistrplus)
	
	### extract the non NA values
	flows <- flows[!is.na(flows)]
	
	####################################################
	### Plot and save a Cullen-Fry diagram for flows
	####################################################
	descdist(flows, boot=500)
	
	#### Save to both pdf and png
	pdf_location <- file.path(write_folder, paste0("pdf/",site_id,"_cullenfrey_",plot_name,".pdf"))
	d_pdf = dev.copy(pdf,pdf_location,width=6, height=6)
    dev.off(d_pdf)
	
	png_location <- file.path(write_folder, paste0("png/",site_id,"_cullenfrey_",plot_name,".png"))
	d_png = dev.copy(png,png_location,width=6, height=6,units="in",res=600)
    dev.off(d_png)
	
	####################################################
	### Fit the distribution to flows
	####################################################
	### This function fits a distribution to flows
	### Could come back and do a bootstrap to estimate the uncertainty and carry
	### through the calculations.
	flow_fit <- try(fitdist(flows, distr))

	### Can play with this if the future
	#a_kernel_fit <- spdfit(month_flows, type="pwm")
	#pspd(c(1,2,3,4,5), a_kernel_fit)

	####################################################
	### Calculate Goodness of fit and return fit results
	####################################################
	### If the fit runs, collect results and goodness of fit statistics
	if (class(flow_fit)!= "try-error") {
		
		plot(flow_fit)
				
		### Plot the fit diagnostics to both pdf and png
		pdf_location <- file.path(write_folder, paste0("pdf/",site_id,"_fitdiagnost_", distr,"_",plot_name,".pdf"))
		d_pdf = dev.copy(pdf,pdf_location,width=6, height=6)
    	dev.off(d_pdf)
	
		png_location <- file.path(write_folder, paste0("png/",site_id,"_fitdiagnost_", distr,"_",plot_name,".png"))
		d_png = dev.copy(png,png_location,width=6, height=6,units="in",res=600)
    	dev.off(d_png)
	
	
		### Create a dataframe with results
		fit_param <- data.frame(data=plot_name,t(as.matrix(flow_fit$estimate)))

		### Add goodness of fit (AIC) and bootstrapped p-values from KS, AD, and CVM tests
		fit_param$aic <- gofstat(flow_fit)$aic
		p_tests <- gof_bootstrap(flow_fit, n_sims=5e3, parallel=TRUE)
		fit_param$ks <- p_tests$ks_p
		fit_param$ad <- p_tests$ad_p
		fit_param$cvm <- p_tests$cvm_p

	}

	return(fit_param)	
}
