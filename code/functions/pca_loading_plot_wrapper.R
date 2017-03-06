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
plot_df$genus <- capwords(as.character(plot_df$genus))

### Create plot
p <- loading_map(plot_data=plot_df, map_underlay=map_big, map_borders=states, site_locations=site_info, lon.lim=lon.lim, lat.lim=lat.lim)

### Save Loading Plot
ggsave(file.path(write_folder, paste0(write_file,"_load_map_PC_",k,"_all.png")), p, width=6, height=7, dpi=600)
ggsave(file.path(write_folder, paste0(write_file,"_load_map_PC_",k,"_all.svg")), p, width=6, height=7)
ggsave(file.path(write_folder, paste0(write_file,"_load_map_PC_",k,"_all.pdf")), p, width=6, height=7)

### Add labels
p <- p + geom_text_repel(data=plot_df, aes(x=Lon, y=Lat, label = ID), size = 3, nudge_x=0.2, nudge_y=0.05, box.padding = unit(0.5, 'lines'))

### Save Loading Plot with Labels
ggsave(file.path(write_folder, paste0(write_file,"_load_map_PC_",k,"_all_labels.png")), p, width=6, height=7, dpi=600)
ggsave(file.path(write_folder, paste0(write_file,"_load_map_PC_",k,"_all_labels.svg")), p, width=6, height=7)
ggsave(file.path(write_folder, paste0(write_file,"_load_map_PC_",k,"_all_labels.pdf")), p, width=6, height=7)

### Extract a subset of sites
plot_subset <- subset(plot_df, Contribution > mean(plot_df$Contribution)*1.2)

### Create subset plot
p <- loading_map(plot_data=plot_subset, map_underlay=map_big, map_borders=states, site_locations=site_info, lon.lim=lon.lim, lat.lim=lat.lim)

### Save Loading Plot
ggsave(file.path(write_folder, paste0(write_file,"_load_map_PC_",k,"_subset.png")), p, width=6, height=7, dpi=600)
ggsave(file.path(write_folder, paste0(write_file,"_load_map_PC_",k,"_subset.svg")), p, width=6, height=7)
ggsave(file.path(write_folder, paste0(write_file,"_load_map_PC_",k,"_subset.pdf")), p, width=6, height=7)

### Add labels
p <- p + geom_text_repel(data=plot_subset, aes(x=Lon, y=Lat, label = ID), size = 3, nudge_x=0.2, nudge_y=0.05, box.padding = unit(0.5, 'lines'))

### Save Loading Plot with Labels
ggsave(file.path(write_folder, paste0(write_file,"_load_map_PC_",k,"_subset_labels.png")), p, width=6, height=7, dpi=600)
ggsave(file.path(write_folder, paste0(write_file,"_load_map_PC_",k,"_subset_labels.svg")), p, width=6, height=7)
ggsave(file.path(write_folder, paste0(write_file,"_load_map_PC_",k,"_subset_labels.pdf")), p, width=6, height=7)


### Create loading plot by genus
p <- loading_genus(plot_data=plot_df, pc_number = k)

### Save Loading Plot by species
ggsave(file.path(write_folder, paste0(write_file,"_load_species_PC_",k,".png")), p$plot, width=8, height=4, dpi=600)
ggsave(file.path(write_folder, paste0(write_file,"_load_species_PC_",k,".svg")), p$plot, width=8, height=4)
ggsave(file.path(write_folder, paste0(write_file,"_load_species_PC_",k,".pdf")), p$plot, width=8, height=4)

### Save Loading Plot by species Legend
ggsave(file.path(write_folder, paste0(write_file,"_load_species_PC_",k,"_legend.png")), p$legend, width=3, height=4, dpi=600)
ggsave(file.path(write_folder, paste0(write_file,"_load_species_PC_",k,"_legend.svg")), p$legend, width=3, height=4)
ggsave(file.path(write_folder, paste0(write_file,"_load_species_PC_",k,"_legend.pdf")), p$legend, width=3, height=4)


}

}


