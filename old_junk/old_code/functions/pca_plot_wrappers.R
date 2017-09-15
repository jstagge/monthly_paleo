

# *------------------------------------------------------------------
# | FUNCTION NAME: pca_plot_wrapper
# | FILE NAME: pca_plot.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:       data - a dataframe with PCA importance data
# |					write_folder  -  location to save plots
# | 				write_file  -  file name for plots
# |
# |     Out:      
# | 
# |     Desc:     Runs the following three PCA diagnostic plots
# |                
# *------------------------------------------------------------------


pca_plot_wrapper <- function(data, write_folder, write_file){
require(ggplot2)
require(svglite)

### Set up output folders
dir.create(file.path(file.path(write_folder,"png"), write_file), recursive=TRUE)
dir.create(file.path(file.path(write_folder,"pdf"), write_file), recursive=TRUE)
dir.create(file.path(file.path(write_folder,"svg"), write_file), recursive=TRUE)

### Run Eigen Plot
p <- pca_eigen_plot(importance=data)
	
### Save Eigen Plot
plot_name <- paste0(write_file, "_eigen")
ggsave(file.path(file.path(file.path(write_folder,"png"), write_file), paste0(plot_name, ".png")), p, width=4, height=3, dpi=600)
ggsave(file.path(file.path(file.path(write_folder,"svg"), write_file), paste0(plot_name, ".svg")), p, width=4, height=3)
ggsave(file.path(file.path(file.path(write_folder,"pdf"), write_file), paste0(plot_name, ".pdf")), p, width=4, height=3)

### Run Variance Explained Plot
p <- pca_var_plot(importance=data)
	
### Save Eigen Plot
plot_name <- paste0(write_file, "_var")
ggsave(file.path(file.path(file.path(write_folder,"png"), write_file), paste0(plot_name, ".png")), p, width=4, height=3, dpi=600)
ggsave(file.path(file.path(file.path(write_folder,"svg"), write_file), paste0(plot_name, ".svg")), p, width=4, height=3)
ggsave(file.path(file.path(file.path(write_folder,"pdf"), write_file), paste0(plot_name, ".pdf")), p, width=4, height=3)


### Run Eigen Plot
p <- pca_cum_var_plot(importance=data)
	
### Save Eigen Plot
plot_name <- paste0(write_file, "_cum_var")
ggsave(file.path(file.path(file.path(write_folder,"png"), write_file), paste0(plot_name, ".png")), p, width=4, height=3, dpi=600)
ggsave(file.path(file.path(file.path(write_folder,"svg"), write_file), paste0(plot_name, ".svg")), p, width=4, height=3)
ggsave(file.path(file.path(file.path(write_folder,"pdf"), write_file), paste0(plot_name, ".pdf")), p, width=4, height=3)


}



# *------------------------------------------------------------------
# | FUNCTION NAME: pca_loading_plot_wrapper
# | FILE NAME: pca_loading_plot_wrapper.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        pca_loading - a dataframe of loadings
# |                pc.n - number of pcs to plot
# |					write_folder  -  folder to write figures
# |					write_file  -  file name to write figures
# |     Out:       
# | 
# |     Desc:      This function runs through all PC loading figures.
# *------------------------------------------------------------------

pca_loading_plot_wrapper <- function(pca_loading, pc.n, write_folder, write_file){
require(ggplot2)
require(svglite)

### Calculate contribution of loading to total by first finding absolute value, summing
### and dividing each PC loading by sum
abs_load <- abs(pca_loading)
load_contrib <- sweep(abs_load, 2, colSums(abs_load), "/")	

### Create loop through all PCs
for (k in seq(1,pc.n)) {

### Create a data frame for loading map
plot_df <- data.frame(ID=rownames(pca_loading), Loading=pca_loading[,k], Contribution=load_contrib[,k])
plot_df <- merge(wadr_site, plot_df, by="ID")
### Re-sort data frame listing biggest contribution first
plot_df <- plot_df[order(-plot_df$Contribution),]

### Set the plotting limits based on max and min coordinates
lon.lim <- c(min(plot_df$Lon)-0.25,max(plot_df$Lon)+0.25)
lat.lim <- c(min(plot_df$Lat)-0.25,max(plot_df$Lat)+0.25)


### Make species uppercase
plot_df$genus <- cap_first(as.character(plot_df$genus))

### Create plot
p <- loading_map(plot_data=plot_df, map_underlay=map_big, map_borders=states, site_locations=site_info, lon.lim=lon.lim, lat.lim=lat.lim)

### Save Loading Plot
plot_name <- paste0(write_file, "_load_map_PC_",k,"_all")
ggsave(file.path(file.path(file.path(write_folder,"png"), write_file), paste0(plot_name, ".png")), p, width=6, height=7, dpi=600)
ggsave(file.path(file.path(file.path(write_folder,"svg"), write_file), paste0(plot_name, ".svg")), p, width=6, height=7)
ggsave(file.path(file.path(file.path(write_folder,"pdf"), write_file), paste0(plot_name, ".pdf")), p, width=6, height=7)

### Add labels
p <- p + geom_text_repel(data=plot_df, aes(x=Lon, y=Lat, label = ID), size = 3, nudge_x=0.2, nudge_y=0.05, box.padding = unit(0.5, 'lines'))

### Save Loading Plot with Labels
plot_name <- paste0(write_file, "_load_map_PC_",k,"_all_labels")
ggsave(file.path(file.path(file.path(write_folder,"png"), write_file), paste0(plot_name, ".png")), p, width=6, height=7, dpi=600)
ggsave(file.path(file.path(file.path(write_folder,"svg"), write_file), paste0(plot_name, ".svg")), p, width=6, height=7)
ggsave(file.path(file.path(file.path(write_folder,"pdf"), write_file), paste0(plot_name, ".pdf")), p, width=6, height=7)

### Extract a subset of sites
plot_subset <- subset(plot_df, Contribution > mean(plot_df$Contribution)*1.2)

### Create subset plot
p <- loading_map(plot_data=plot_subset, map_underlay=map_big, map_borders=states, site_locations=site_info, lon.lim=lon.lim, lat.lim=lat.lim)

### Save Loading Plot
plot_name <- paste0(write_file, "_load_map_PC_",k,"_subset")
ggsave(file.path(file.path(file.path(write_folder,"png"), write_file), paste0(plot_name, ".png")), p, width=6, height=7, dpi=600)
ggsave(file.path(file.path(file.path(write_folder,"svg"), write_file), paste0(plot_name, ".svg")), p, width=6, height=7)
ggsave(file.path(file.path(file.path(write_folder,"pdf"), write_file), paste0(plot_name, ".pdf")), p, width=6, height=7)

### Save to publication
if (k %in% seq(1,5) & write_file== "pca_impute"){

plot_name <- paste0("fig_4",toupper(letters)[k])
ggsave(file.path(file.path(pub_path,"png"), paste0(plot_name , "_map.png")), p, width=6, height=7, dpi=600)
ggsave(file.path(file.path(pub_path,"svg"), paste0(plot_name , "_map.svg")), p, width=6, height=7)
ggsave(file.path(file.path(pub_path,"pdf"), paste0(plot_name , "_map.pdf")), p, width=6, height=7)
}

### Add labels
p <- p + geom_text_repel(data=plot_subset, aes(x=Lon, y=Lat, label = ID), size = 3, nudge_x=0.2, nudge_y=0.05, box.padding = unit(0.5, 'lines'))

### Save Loading Plot with Labels
plot_name <- paste0(write_file, "_load_map_PC_",k,"_subset_labels")
ggsave(file.path(file.path(file.path(write_folder,"png"), write_file), paste0(plot_name, ".png")), p, width=6, height=7, dpi=600)
ggsave(file.path(file.path(file.path(write_folder,"svg"), write_file), paste0(plot_name, ".svg")), p, width=6, height=7)
ggsave(file.path(file.path(file.path(write_folder,"pdf"), write_file), paste0(plot_name, ".pdf")), p, width=6, height=7)


### Create loading plot by genus
p <- loading_genus(plot_data=plot_df, pc_number = k)

### Save Loading Plot by species
plot_name <- paste0(write_file, "_load_species_PC_",k)
ggsave(file.path(file.path(file.path(write_folder,"png"), write_file), paste0(plot_name, ".png")), p$plot, width=8, height=4, dpi=600)
ggsave(file.path(file.path(file.path(write_folder,"svg"), write_file), paste0(plot_name, ".svg")), p$plot, width=8, height=4)
ggsave(file.path(file.path(file.path(write_folder,"pdf"), write_file), paste0(plot_name, ".pdf")), p$plot, width=8, height=4)

### Save to publication
if (k %in% seq(1,5) & write_file== "pca_impute"){
plot_name <- paste0("fig_4",toupper(letters)[k])
ggsave(file.path(file.path(pub_path,"png"), paste0(plot_name , "_species.png")), p$plot, width=8, height=4, dpi=600)
ggsave(file.path(file.path(pub_path,"svg"), paste0(plot_name , "_species.svg")), p$plot, width=8, height=4)
ggsave(file.path(file.path(pub_path,"pdf"), paste0(plot_name , "_species.pdf")), p$plot, width=8, height=4)
}

### Save Loading Plot by species Legend
plot_name <- paste0(write_file, "_load_species_PC_",k,"_legend")
ggsave(file.path(file.path(file.path(write_folder,"png"), write_file), paste0(plot_name, ".png")), p$legend, width=3, height=4, dpi=600)
ggsave(file.path(file.path(file.path(write_folder,"svg"), write_file), paste0(plot_name, ".svg")), p$legend, width=3, height=4)
ggsave(file.path(file.path(file.path(write_folder,"pdf"), write_file), paste0(plot_name, ".pdf")), p$legend, width=3, height=4)

### Save to publication
if (k == 1 & write_file== "pca_impute"){
plot_name <- "fig_4"
ggsave(file.path(file.path(pub_path,"png"), paste0(plot_name , "_legend.png")), p$legend, width=3, height=4, dpi=600)
ggsave(file.path(file.path(pub_path,"svg"), paste0(plot_name , "_legend.svg")), p$legend, width=3, height=4)
ggsave(file.path(file.path(pub_path,"pdf"), paste0(plot_name , "_legend.pdf")), p$legend, width=3, height=4)
}


}

}






