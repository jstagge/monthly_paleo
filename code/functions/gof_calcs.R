
# *------------------------------------------------------------------
# | FUNCTION NAME: gof_calcs
# | FILE NAME: gof_calcs.R
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

gof_ts.frame <- function(dataframe, obs.col=1, pred.col=2) {
	obs <- dataframe[,obs.col]
	pred <- dataframe[,pred.col]
	gof.result <- gof_ts(pred,obs)
	return(gof.result)
}


# *------------------------------------------------------------------
# | FUNCTION NAME: gof_calcs
# | FILE NAME: gof_calcs.R
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

coord.equal.gen <- function(dataframe, value.columns=c(1,2), factor.column=3) {
	factor.unique <- unique(dataframe[,factor.column])
	
	for (j in seq(1,length(factor.unique))) {
		j.factor <- factor.unique[j]
		subset.df <- dataframe[dataframe[,factor.column]==j.factor,]
		min.val <- min(subset.df[,value.columns], na.rm=TRUE)
		max.val <- max(subset.df[,value.columns], na.rm=TRUE)
	
		result.temp <- dataframe[NULL, ]
		result.temp[1,value.columns] <- min.val
		result.temp[1,factor.column] <- j.factor
		result.temp[2,value.columns] <- max.val
		result.temp[2,factor.column] <- j.factor

	if(j == 1){
		result.df <- result.temp
	} else {
		result.df <- rbind(result.df, result.temp)
	}
	}
return(result.df)
}



# *------------------------------------------------------------------
# | FUNCTION NAME: gof_calcs
# | FILE NAME: gof_calcs.R
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

gof.plots <- function(x.obs, x.pred, x.factor) {
## Create dataframe
Plot.df <- data.frame(Obs=x.obs, Pred=x.pred, Factor=x.factor)
Plot.df$Resid <- Plot.df$Pred - Plot.df$Obs

limit.df <- coord.equal.gen(Plot.df, value.columns=c(1,2), factor.column=3)
limit.df$Resid <- rep(c(-1,1),length=dim(limit.df)[1])*0.5*rep(limit.df$Obs[seq(2, dim(limit.df)[1], 2)],each=2)

gof.df <- by(Plot.df, Plot.df$Factor, gof_ts.frame, obs.col=1, pred.col=2)
gof.df <- sapply(gof.df, function(x){unlist(x)})

p1 <- ggplot(Plot.df, aes(x=Obs, y=Resid))
p1 <- p1 + geom_abline(intercept = 0, slope = 0, colour="red")
p1 <- p1 + geom_point(colour="grey20")
p1 <- p1 + theme_classic_correct()
p1 <- p1 + scale_y_continuous(name=expression(bold(paste("Residuals (Pred - Obs) ( ",m^3,"/s )"))))
p1 <- p1 + scale_x_continuous(name=expression(bold(paste("Observed ( ",m^3,"/s )"))))

p2 <- ggplot(Plot.df, aes(x=Obs, y=Pred))
p2 <- p2 + geom_abline(intercept = 0, slope = 1, colour="red")
p2 <- p2 + geom_point(colour="grey20")
p2 <- p2 + theme_classic_correct()
p2 <- p2 + scale_y_continuous(name=expression(bold(paste("Predicted ( ",m^3,"/s )"))))
p2 <- p2 + scale_x_continuous(name=expression(bold(paste("Observed ( ",m^3,"/s )"))))

return(list(fitplot=p2, residplot=p1, limits=limit.df, gof=gof.df))
}







# *------------------------------------------------------------------
# | FUNCTION NAME: gof_plot_wrapper
# | FILE NAME: gof_calcs.R
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

gof_plot_wrapper <- function(x.obs, x.pred, x.factor, write_folder, write_file) {
require(ggplot2)
require(svglite)

### Calculate residuals 
resid_plot <- gof.plots(x.obs=x.obs, x.pred=x.pred, x.factor=x.factor)

### Extract plot of residuals
p1 <- resid_plot$residplot + geom_blank(data=resid_plot$limits)

### Save plot of residuals
output_folder <- file.path(write_folder,"resid_full/png")
suppressWarnings(dir.create(output_folder, recursive=TRUE))
ggsave(file.path(output_folder, paste0(write_file,"_resid_full.png")), p1, width=5, height=4, dpi=600)
output_folder <- file.path(write_folder,"resid_full/pdf")
suppressWarnings(dir.create(output_folder, recursive=TRUE))
ggsave(file.path(output_folder, paste0(write_file,"_resid_full.pdf")), p1, width=5, height=4)
output_folder <- file.path(write_folder,"resid_full/svg")
suppressWarnings(dir.create(output_folder, recursive=TRUE))
ggsave(file.path(output_folder, paste0(write_file,"_resid_full.svg")), p1, width=5, height=4)

### Convert to monthly facets
p1 <- p1 + facet_wrap(~ Factor, ncol=4, scales="free")

### Save plot of monthly residuals
output_folder <- file.path(write_folder,"resid_monthly/png")
suppressWarnings(dir.create(output_folder, recursive=TRUE))
ggsave(file.path(output_folder, paste0(write_file,"_resid_monthly.png")), p1, width=8, height=6, dpi=600)
output_folder <- file.path(write_folder,"resid_monthly/pdf")
suppressWarnings(dir.create(output_folder, recursive=TRUE))
ggsave(file.path(output_folder, paste0(write_file,"_resid_monthly.pdf")), p1, width=8, height=6)
output_folder <- file.path(write_folder,"resid_monthly/svg")
suppressWarnings(dir.create(output_folder, recursive=TRUE))
ggsave(file.path(output_folder, paste0(write_file,"_resid_monthly.svg")), p1, width=8, height=6)

### Extract plot of residuals
p2 <- resid_plot$fitplot + geom_blank(data=resid_plot$limits)

### Save plot of residuals
output_folder <- file.path(write_folder,"resid_full/png")
ggsave(file.path(output_folder, paste0(write_file,"_fit_full.png")), p2, width=5, height=4, dpi=600)
output_folder <- file.path(write_folder,"resid_full/pdf")
ggsave(file.path(output_folder, paste0(write_file,"_fit_full.pdf")), p2, width=5, height=4)
output_folder <- file.path(write_folder,"resid_full/svg")
ggsave(file.path(output_folder, paste0(write_file,"_fit_full.svg")), p2, width=5, height=4)

### Convert to monthly facets
p2 <- p2 + facet_wrap(~ Factor, ncol=4, scales="free")

### Save plot of monthly residuals
output_folder <- file.path(write_folder,"resid_monthly/png")
ggsave(file.path(output_folder, paste0(write_file,"_fit_monthly.png")), p2, width=8, height=6, dpi=600)
output_folder <- file.path(write_folder,"resid_monthly/pdf")
ggsave(file.path(output_folder, paste0(write_file,"_fit_monthly.pdf")), p2, width=8, height=6)
output_folder <- file.path(write_folder,"resid_monthly/svg")
ggsave(file.path(output_folder, paste0(write_file,"_fit_monthly.svg")), p2, width=8, height=6)

return(resid_plot$gof)
}








