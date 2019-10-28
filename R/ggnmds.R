##----------------------------------------------------------------------
## Biplot for NMDS results using ggplot2
## Author:  German Gonzalez
##----------------------------------------------------------------------

ggnmds <- function(object, env, sites_lab=TRUE, species_lab=TRUE,
           hulls=FALSE, shape=shape, color=shape) 
{

  ## Site scores
  scores_sites <- as_tibble(scores(object, "sites"))
  if(!is.null(env)) {
    scores_sites <- bind_cols(scores_sites, env)
  }
  
  ## Species scores
  scores_species <- as.data.frame(scores(object, "species"))
  scores_species <- rownames_to_column(scores_species, var= "species")
  
  p <- ggplot() + 
        geom_point(data=scores_sites, aes_string(x="NMDS1", y="NMDS2", # add points
                   shape=shape, color=color), size=4) + 
        coord_equal() 
        
  if(sites_lab) {
   p <- p + geom_text_repel(data=scores_sites, # add the species labels
                            aes(x=NMDS1, y=NMDS2, 
                            label=site), alpha=0.5)  
  }
  
  if(species_lab) {
   p <- p + geom_text_repel(data=scores_species, # add the species labels
                            aes(x=NMDS1, y=NMDS2, 
                            label=species), alpha=0.5)  
  }

  if(hulls) {
  
    hull_data <- lapply(split(scores_sites, env$sitio), function(x){
        ch_grp <- chull(x[,c("NMDS1", "NMDS2")])
        x[ch_grp,] 
    })
    hull_data <- bind_rows(hull_data)
    
    p <- p + geom_polygon(data=hull_data, aes(x=NMDS1, y=NMDS2, fill=sitio, group=sitio), alpha=0.30)  # add the convex hulls
  }
  
  p
}

theme_pub <- function(base_size = 11, base_family = "", 
    base_line_size = base_size/22, 
    base_rect_size = base_size/22) 
{
  theme_bw(base_size = base_size, base_family = base_family, 
        base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
}
