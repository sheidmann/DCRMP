# density_multiplots.R
# Sarah Heidmann
# Created 6 June 2024 from old testing script models.R
# Last modified 6 June 2024

# This script looks at density of groups of interest by depth and relief.
# These groups are: total fish, simpson's diversity index, and the 4 trophic groups.
# It creates Figures 2 and S2 for the manuscript.

# Load libraries
library(tidyverse)
library(scatterplot3d)
library(FactoClass)
library(cowplot)

##### Import fish data #####
spcs_list <- read_csv("data/NCDC_fish_species_list_2023.csv") %>% 
   filter(keep=="y")

dens_all <- readRDS("data/NCDC_2017_2022_density.rds") %>% 
   left_join(spcs_list, by="SPECIES_CD")

sitemeta <- readRDS("data/NCDC_sitemeta.rds") 
dat <- sitemeta %>%
   # Convert to 100m2
   mutate(across(.cols=c(ends_with("dens"),herbivore:planktivore)) / 1.77)


dens_all <- dens_all %>% 
   left_join(select(sitemeta, PSU, DEPTH), 
             by="PSU")

##### Fig 2- 3d density by depth and relief #####
# The below function saves each plot into outputs/overall_3d unless modified
splot3d_overall <- function(dat, rvar, label){
   dat <- dat %>% 
      mutate(response = (!!as.name(rvar)))
   # Scatterplot3d
   ang <- 30
   myColorRamp <- function(colors, values) {
      v <- (values - min(values))/diff(range(values))
      x <- colorRamp(colors)(v)
      rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
   }
   
   colsAll <- myColorRamp(c("gray90","gray0"), dat$AVG_HARD_RELIEF)
   pdf(file=paste0("outputs/overall_3d/",label,"_3d.pdf"))
   with(dat,{
      rvar.p <- scatterplot3d(x=DEPTH, y=AVG_HARD_RELIEF,z=response,
                              box=FALSE,grid=FALSE, pch=1,
                              cex.lab = 1.5, cex.axis = 1.5,
                              #main = title,
                              xlab = "Depth (m)",
                              ylab = "Average Hard Relief (m)",
                              zlab = label,
                              type="h", angle = ang)
      addgrids3d(DEPTH, AVG_HARD_RELIEF, response,
                 grid=c("xy","xz","yz"), angle = ang)
      rvar.p$points3d(DEPTH, AVG_HARD_RELIEF,response, pch=16, type = "h", col=colsAll)
   })
   dev.off()
   
   return(print(paste0(label, " done")))
}
splot3d_overall(dat, "totdens", "Total Fish Density")
splot3d_overall(dat, "simpson", "Simpson's Diversity Index")
splot3d_overall(dat, "planktivore", "Planktivore Density")
splot3d_overall(dat, "herbivore", "Herbivore Density")
splot3d_overall(dat, "piscivore","Piscivore Density")
splot3d_overall(dat, "invertivore","Invertivore Density")

##### Fig S2- category means #####
plot_overall_ints <- function(dat, rvar, label){
   dat <- dat %>% 
      mutate(response = (!!as.name(rvar))) # Cool way to turn string to column name
   depthmed <- median(dat$DEPTH)
   reliefmed <- median(dat$AVG_HARD_RELIEF)
   catsum <- dat %>% 
      mutate(depth_half = ifelse(DEPTH<depthmed,"shallow","deep"),
             relief_half = ifelse(AVG_HARD_RELIEF<reliefmed, "low","high")) %>% 
      group_by(depth_half, relief_half) %>% 
      summarize(response = round(mean(response),2), .groups="drop") %>% 
      mutate(DR = paste(depth_half, relief_half, sep="_")) %>% 
      select(-depth_half, -relief_half) %>% 
      pivot_wider(names_from=DR, values_from = response)
   formatter <- function(){
      function(x) format(round(x, 2))
   }
   p <- catsum %>% 
      pivot_longer(cols=c(deep_high,deep_low,shallow_high,shallow_low), 
                   values_to = "response", names_to = "DR") %>% 
      separate(DR, c("depth_half","relief_half"), remove=F) %>% 
      ggplot(data=.,aes(x=depth_half, y=response, group=relief_half, color=relief_half)) +
      geom_point(size=4.5) +
      geom_line() +
      # black is high relief, light gray is low
      scale_color_discrete("Relief", type=c("#000000","#cccccc")) + 
      scale_x_discrete(limits=c("shallow","deep")) +
      scale_y_continuous(labels = formatter()) +
      xlab("") + ylab(label) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.key = element_blank(),
            strip.text = element_text(face = "italic"),
            strip.background = element_blank(),
            strip.placement = "inside",
            text = element_text(size = 15))
   return(p)
}
dens_int_p <- plot_overall_ints(dat, "totdens", "Total Fish Density")
div_int_p <- plot_overall_ints(dat, "simpson", "Simpson's Diversity Index")
plank_int_p <- plot_overall_ints(dat, "planktivore", "Planktivore Density")
herb_int_p <- plot_overall_ints(dat, "herbivore","Herbivore Density")
pisc_int_p <- plot_overall_ints(dat, "piscivore","Piscivore Density")
inv_int_p <- plot_overall_ints(dat, "invertivore","Invertivore Density")
# plot them together with cowplot
int_p_all_noleg <- plot_grid(dens_int_p + theme(legend.position="none"),
                             div_int_p + theme(legend.position="none"), 
                             plank_int_p + theme(legend.position="none"), 
                             herb_int_p + theme(legend.position="none"), 
                             pisc_int_p + theme(legend.position="none"), 
                             inv_int_p + theme(legend.position="none"), 
                             ncol = 2,align = "v")
legend <- get_legend(dens_int_p)
int_p_all <- plot_grid(int_p_all_noleg, legend, rel_widths = c(2,0.3))
int_p_all
# Save it
# tiff(filename="outputs/S2_overall_interactions.tiff", 
#      width = 4725, height = 4725, units="px", res=360)
# int_p_all
# dev.off()