# *------------------------------------------------------------------
# | FUNCTION NAME: ts_plot
# | FILE NAME: plots.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        data - a dataframe
# |                x_col - name of the column in data with values for x
# |     Out:       p - the resulting plot
# | 
# |     Desc:      This function provides a standard time series plot
# |					using ggplot's geom_line. 
# *------------------------------------------------------------------

ts_plot <- function(data, write_folder, write_file) {
require(ggplot2)
require(svglite)
require(reshape2)

### Extract the method name
#method_name <- as.character(data$method[[1]])

### Melt to get into proper format
plot_data <- melt(data,  id.vars="date", measure.vars=c("flow_est", "flow_obs", "flow_annual"))

### Fix column names and convert method to characters
colnames(plot_data) <- c("Date", "Measure", "Flow")
plot_data$Measure <- as.character(plot_data$Measure)

### Factor to make sure in correct order
#plot_data$Measure <- factor(plot_data$Measure, levels = c( recon_name, obs_name, annual_name))
plot_data$Measure <- factor(plot_data$Measure, levels = c("flow_obs", "flow_annual", "flow_est"), labels = c("Observed      ", "Annual Reconstruction", "Monthly Reconstruction"))

### Create plot with observed in black and predicted in red
p <- ggplot(plot_data, aes(x=Date, y=Flow, group=Measure, colour=Measure, linetype=Measure))
p <- p + geom_line(size=0.25)
p <- p + theme_classic_new()
p <- p + scale_x_date(name="Date", breaks=seq(as.Date("200-01-01"), as.Date("2030-01-01"), by="5 years"), date_labels = "%Y")
p <- p + scale_colour_manual(name= NULL, values = c("black", "blue", "red"))
p <- p + scale_linetype_manual(values=c("solid", "longdash", "solid"), guide=FALSE)
#p <- p + scale_y_continuous(name="Discharge (m3/s)")
p <- p + scale_y_continuous(name=expression(bold(paste("Monthly Mean Discharge  ( ",m^3,"/s )"))))
p <- p +  theme(legend.position="bottom")


### Save full plot
ggsave(paste0(file.path(write_folder,"png/"), write_file, "_full.png"), p, width=8, height=4, dpi=600)
ggsave(paste0(file.path(write_folder,"pdf/"), write_file, "_full.pdf"), p, width=8, height=4)
ggsave(paste0(file.path(write_folder,"svg/"), write_file, "_full.svg"), p, width=8, height=4)


### Loop through 15 year periods and save
plot_breaks <- seq(as.Date("1905-01-01"), as.Date("2030-01-01"), by="15 years")

for (k in seq(1,length(plot_breaks)-1)) {
### Identify the start of each break
start_break <- plot_breaks[k]
end_break <- plot_breaks[k+1]

### Cut to break point and reorganize x axis to 2 years
p_break <- p + coord_cartesian(xlim=c(as.Date(start_break), as.Date(end_break)))
suppressMessages(p_break <- p_break + scale_x_date(name="Date", breaks=seq(as.Date("1800-01-01"), as.Date("2030-01-01"), by="2 years"), date_labels = "%Y"))

### Save zoomed plots
write_file_subset <- paste0(write_file, "_",start_break,"_",end_break)
ggsave(paste0(file.path(write_folder,"png/"), write_file_subset, "_",start_break,"_",end_break,".png"), p_break, width=6, height=4, dpi=600)
ggsave(paste0(file.path(write_folder,"pdf/"), write_file_subset, "_",start_break,"_",end_break,".pdf"), p_break, width=6, height=4)
ggsave(paste0(file.path(write_folder,"svg/"), write_file_subset, "_",start_break,"_",end_break,".svg"), p_break, width=6, height=4)

}

return(p)
}





# *------------------------------------------------------------------
# | FUNCTION NAME: linear_fit_facet_plot
# | FILE NAME: plots.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        data - a dataframe
# |                x_col - name of the column in data with values for x
# |     Out:       p - the resulting plot
# | 
# |     Desc:      This function provides a standard time series plot
# |					using ggplot's geom_line. 
# *------------------------------------------------------------------

linear_fit_facet_plot <- function(data, x_col, x_name, y_col, y_name, x_pos, y_pos) {

require(ggplot2)

p <- ggplot(data, aes(x=get(x_col), y=get(y_col)))
p <- p + geom_point()
p <- p + geom_smooth(method=lm)
p <- p + theme_bw(9)
p <- p + scale_x_continuous(name=x_name) + scale_y_continuous(name=y_name)

### Create a plot with same scales
p_fixed <- p + facet_wrap(~ month, ncol = 4)
p_fixed <- p_fixed + stat_smooth_func(geom="text",method="lm",xpos = x_pos, ypos = y_pos, hjust=0,parse=TRUE, size=2.5)

### Create a plot with free scales
p_free <- p + facet_wrap(~ month, ncol = 4, scale="free")
p_free <- p_free + stat_smooth_func(geom="text",method="lm", hjust=0,parse=TRUE, size=2.5)

return(list(fixed_scale=p_fixed , free_scale=p_free))
}







# *------------------------------------------------------------------
# | FUNCTION NAME: gof_month_plot
# | FILE NAME: plots.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        data - a dataframe
# |                x_col - name of the column in data with values for x
# |     Out:       p - the resulting plot
# | 
# |     Desc:      This function provides a standard time series plot
# |					using ggplot's geom_line. 
# *------------------------------------------------------------------

gof_month_plot <- function(data, param_list, param_names, y_hor_line=NULL) {
require(ggplot2)
require(ggthemes)

p <- ggplot(data, aes(x=get(param_list$x), y=get(param_list$y), group=get(param_list$group), colour=get(param_list$color), linetype=get(param_list$linetype)))
### Add horizontal lines to plot
if (length(y_hor_line) > 0) {
for (k in seq(1,length(y_hor_line))) {
p <- p + geom_abline(intercept = y_hor_line[k], slope = 0, colour="grey60", size=0.25)
}}
p <- p + geom_line(size=0.4)
#p <- p + scale_colour_brewer(name=param_names$color, type="qual", palette = "Set1")
#p <- p + scale_colour_ptol(name=param_names$color)
p <- p + scale_colour_manual(name=param_names$color, values=cb_pal(pal="wong", n=3, sort=FALSE))
p <- p + scale_linetype_manual(name = param_names$linetype, values=c("longdash", "dashed", "twodash", "F1"))
p <- p + theme_classic_new()
p <- p + scale_x_discrete(name=param_names$x)
p <- p + scale_y_continuous(name=param_names$y)
p <- p + theme(legend.position="bottom", legend.box = "horizontal")

p <- p + guides(colour = guide_legend(label.position = "bottom", title.position="top", label.hjust=0, title.hjust=0.5, keywidth=4))
p <- p + guides(linetype = guide_legend(label.position = "bottom", title.position="top", label.hjust=0, title.hjust=0.5, keywidth=4))

return(p)
}





# *------------------------------------------------------------------
# | FUNCTION NAME: gof_month_plot
# | FILE NAME: plots.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        data - a dataframe
# |                x_col - name of the column in data with values for x
# |     Out:       p - the resulting plot
# | 
# |     Desc:      This function provides a standard time series plot
# |					using ggplot's geom_line. 
# *------------------------------------------------------------------

gof_month_plot2 <- function(data, param_list, param_names, y_hor_line=NULL, colors=NULL) {
require(ggplot2)
require(ggthemes)

p <- ggplot(data, aes(x=get(param_list$x), y=get(param_list$y), group=get(param_list$group), colour=get(param_list$color)))
### Add horizontal lines to plot
if (length(y_hor_line) > 0) {
for (k in seq(1,length(y_hor_line))) {
p <- p + geom_abline(intercept = y_hor_line[k], slope = 0, colour="grey60", size=0.25)
}}
p <- p + geom_line(size=0.4)
#p <- p + scale_colour_brewer(name=param_names$color, type="qual", palette = "Set1")
#p <- p + scale_colour_ptol(name=param_names$color)
#p <- p + scale_colour_manual(name=param_names$color, values=iwanthue_5)
p <- p + scale_colour_manual(name=param_names$color, values=colors)
p <- p + theme_classic_new()
p <- p + scale_x_discrete(name=param_names$x)
p <- p + scale_y_continuous(name=param_names$y)
p <- p + theme(legend.position="bottom", legend.box = "horizontal")

p <- p + guides(colour = guide_legend(label.position = "bottom", title.position="top", label.hjust=0.5, title.hjust=0.5, keywidth=4, byrow=TRUE))
p <- p + guides(linetype = guide_legend(label.position = "bottom", title.position="top", label.hjust=0.5, title.hjust=0.5, keywidth=4))

return(p)
}




# *------------------------------------------------------------------
# | FUNCTION NAME: norm_coef_plot
# | FILE NAME: plots.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        data - a dataframe
# |                x_col - name of the column in data with values for x
# |     Out:       p - the resulting plot
# | 
# |     Desc:      This function provides a standard time series plot
# |					using ggplot's geom_line. 
# *------------------------------------------------------------------

norm_coef_plot <- function(data, predictor_levels, predictor_labels) {
require(ggplot2)
require(reshape2)
require(ggthemes)

### Remove the intercept (first row)
#data <- data[2:dim(data)[1],]

### Force the column names to be months from 1 to 12
colnames(data) <- seq(1,12)

### Reorganize and subset to data
data$coef <- rownames(data)
predictor_test <- rownames(data) %in% predictor_levels
data <- data[predictor_test,]

### Melt to create a vertical dataframe
data <- melt(data)
colnames(data) <- c("predictor", "month", "beta")

### Force months to be a factor to allow for plotting as water year
data$month <- factor(data$month, levels=c(seq(10,12),seq(1,9)), label=c(seq(10,12),seq(1,9)))

### Factor the predictor
data$predictor <- factor(data$predictor, levels=predictor_levels, label=predictor_labels)


### Create plot
p <- ggplot(data, aes(x=month, y=beta, group=predictor, colour=predictor))
### Add horizontal lines to plot
p <- p + geom_abline(intercept = 0, slope = 0, colour="grey60", linetype="longdash", size=0.25)
p <- p + geom_line(size=0.5)
p <- p + scale_colour_few(name="Predictor")
p <- p + theme_classic_new()
p <- p + scale_x_discrete(name="Month")
p <- p + scale_y_continuous(name=expression(bold(paste("Model Coefficient  ( ",beta," )"))))
p <- p + theme(legend.position="bottom", legend.box = "horizontal")
#p <- p + guides(colour = guide_legend(title.position="top", title.hjust=0.5))
p

return(p)
}




# *------------------------------------------------------------------
# | FUNCTION NAME: loading_map
# | FILE NAME: plots.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        plot_data - a dataframe
# |                map_underlay - tiles of the map
# |					map_borders - dataframe with regional borders
# |					site_location - locations of sites
# |     Out:       p - the resulting plot
# | 
# |     Desc:      This function provides a spatial plot of loading. 
# *------------------------------------------------------------------

loading_map <- function(plot_data, map_underlay, map_borders, site_locations, lon.lim, lat.lim) {
legend_pt <- 10 

### Set the min and max of color scheme to be equal (positive/negative)
plot.max <- max(abs(plot_data$Loading))*1.1
plot.lim <- c(-plot.max, plot.max)

### Create map figure of loadings
p <- ggmap(map_underlay, base_layer=ggplot(aes(x=lon,y=lat), data=map_borders), extent = "normal", maprange=FALSE, darken = c(.5,"white"))
### add state borders
p <- p + geom_polygon(data=map_borders, aes(group=group),fill=NA,col='black')
### add points
p <- p + geom_point(data=plot_data, aes(x=Lon, y=Lat, fill=Loading, size=Contribution, shape=genus), alpha=0.9)
### add sites
p <- p + geom_point(data=site_locations, aes(x=dec_long_va, y=dec_lat_va), shape=23, fill="grey25", size=3.5)
p <- p + geom_text_repel(data=site_locations, aes(x=dec_long_va, y=dec_lat_va, label = site_no), size = 3, nudge_x=-0.36, nudge_y=0.1, box.padding = unit(0.5, 'lines'))

### Create theme
p <- p + theme_bw(legend_pt)
p <- p + theme(legend.title = element_text(color="black", face="bold", size=legend_pt*0.9))
p <- p + theme(axis.title = element_text(color="black", face="bold", size=legend_pt*0.9))

p <- p + scale_fill_distiller(name = "PC Loading", palette = "RdBu", limits=plot.lim, guide=guide_colourbar(order=1))
p <- p + scale_size_area(name="Contribution \nto PC (%)", labels = ggplot_percent_label, guide=guide_legend(order = 2))
p <- p + scale_shape_manual(name="Genus", values=c(21,22,24, 1, 2), guide=guide_legend(order = 3))
p <- p + coord_map("mercator", xlim=lon.lim, ylim=lat.lim)
p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p <- p + scale_x_continuous(name="Longitude")
p <- p + scale_y_continuous(name="Latitude")

return(p)

}




# *------------------------------------------------------------------
# | FUNCTION NAME: loading_genus
# | FILE NAME: plots.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        plot_data - a dataframe
# |                map_underlay - tiles of the map
# |					map_borders - dataframe with regional borders
# |					site_location - locations of sites
# |     Out:       p - the resulting plot
# | 
# |     Desc:      This function provides a spatial plot of loading. 
# *------------------------------------------------------------------

loading_genus <- function(plot_data, pc_number) {
legend_pt <- 10 

k <- pc_number

### Create columns for site id and PC number 
plot_data$ID <- factor(plot_data$ID, levels = plot_data$ID[order(-plot_data$Contribution)])
plot_data$variable <- paste("PC ", k,sep="")
### Create column for species names
plot_data$species_comb <- paste0(cap_first(as.character(plot_data$genus)), " ", plot_data$species)

p <- ggplot(plot_data, aes(x=ID, y=Loading, fill=common_name))
p <- p + geom_bar(stat="identity", colour="black")
p <- p + geom_hline(yintercept=0, color="black")
p <- p + facet_grid(variable~., scale="free", space="free_x")
p <- p + scale_fill_brewer(name="Species", palette="Set2", type="qual")
p <- p + theme_bw(legend_pt)
p <- p + theme(axis.title.x=element_blank())
p <- p + scale_y_continuous(name="")#, expand = c(0,0))
p <- p + theme(axis.text.x = element_text(angle = 25, size=8, hjust = 1))

### Extract legend
p_legend <- extract_legend(p) 
p <- p + theme(legend.position="none")

### Create object to return
plot_list <- list(plot=p, legend=p_legend)

return(plot_list)
}







# *------------------------------------------------------------------
# | FUNCTION NAME: gof_annual_plot
# | FILE NAME: plots.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        data - a dataframe
# |                x_col - name of the column in data with values for x
# |     Out:       p - the resulting plot
# | 
# |     Desc:      This function provides a standard time series plot
# |					using ggplot's geom_line. 
# *------------------------------------------------------------------

gof_annual_plot <- function(data, param_list, param_names, y_hor_line=NULL) {
require(ggplot2)
require(ggthemes)

p <- ggplot(data, aes(x=get(param_list$x), y=get(param_list$y), group=get(param_list$group), colour=get(param_list$color)))
### Add horizontal lines to plot
if (length(y_hor_line) > 0) {
for (k in seq(1,length(y_hor_line))) {
p <- p + geom_abline(intercept = y_hor_line[k], slope = 0, colour="grey60", size=0.25)
}}
p <- p + geom_line(size=0.4)
p <- p + scale_colour_brewer(name=param_names$color, type="qual", palette = "Set1")
#p <- p + scale_colour_ptol(name=param_names$color)
#p <- p + scale_colour_manual(name=param_names$color, values=iwanthue_7)
p <- p + theme_classic_new()
p <- p + scale_x_discrete(name=param_names$x)
p <- p + scale_y_continuous(name=param_names$y)
p <- p + theme(legend.position="bottom", legend.box = "horizontal")

p <- p + guides(colour = guide_legend(label.position = "bottom", title.position="top", label.hjust=0, title.hjust=0.5, keywidth=4, byrow=TRUE))
p <- p + guides(linetype = guide_legend(label.position = "bottom", title.position="top", label.hjust=0, title.hjust=0.5, keywidth=4))

return(p)
}













