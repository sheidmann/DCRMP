# trophic_trends.R
# Sarah Heidmann
# Created 25 Jan 2022
# Last modified 22 Jan 2024

# This script looks at trophic group trends in the data.
# It contains the code for Figure 5 of the manuscript.

# Load libraries
library(tidyverse)
library(vegan) # for relative abundance

##### Import fish data #####
#  Using TROPH_NOAA1
spcs_list <- read_csv("data/NCDC_fish_species_list_2023.csv") %>% 
   filter(keep=="y")

dat <- readRDS("data/NCDC_2017_2022.rds")

dens_all <- readRDS("data/NCDC_2017_2022_density.rds") %>% 
   left_join(spcs_list, by="SPECIES_CD")

sitemeta <- readRDS("data/NCDC_sitemeta.rds")

# colorblind-friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

##### Trophic dominance by depth #####
# Make it easy to call trophic columns (need to remove NAs)
trophlist <- paste0(c(na.omit(unique(spcs_list$TROPH_NOAA1))),".rel")
dens_rel_troph <- sitemeta %>%
   select(PSU,all_of(trophlist)) %>% 
   pivot_longer(cols=c(-PSU),names_to = "Troph", values_to = "RelDens") %>%
   # re-join
   left_join(sitemeta) %>%
   # Categorize depths
   mutate(DepthBin=factor(ifelse(DEPTH<=10, "1-10", 
                                 ifelse(DEPTH<=20,"11-20", 
                                        ifelse(DEPTH<=30, "21-30",
                                               ifelse(DEPTH<=40,"31-40",
                                                      "41-50")))), 
                          levels=c("1-10","11-20","21-30","31-40","41-50")))
# Relative density of trophs with continuous depth
# tiff(filename="outputs/Figure5_trophdepth.tiff",
#      width = 2000, height = 2000, units="px", res=360)
ggplot(data=dens_rel_troph, aes(x=DEPTH, y=RelDens, color = Troph,
                                shape = Troph,fill=Troph)) +
   geom_point() +
   geom_smooth(method="lm") +
   scale_color_manual(values=cbPalette, 
                      labels = c("Herbivore", "Invertivore",
                                 "Piscivore","Planktivore")) +
   scale_fill_manual(values=cbPalette, 
                      labels = c("Herbivore", "Invertivore",
                                 "Piscivore","Planktivore")) +
   scale_shape_manual(values=c(21,24,22,3),
                      labels = c("Herbivore", "Invertivore",
                                 "Piscivore","Planktivore")) +
   #guides(fill="none") +
   scale_x_continuous(expand = c(0, 0), name = "Depth (m)") +
   scale_y_continuous(expand = c(0, 0), name = "Relative Density") +
   theme(panel.background = element_blank(),
         axis.line=element_line(),
         legend.title = element_blank(),
         legend.position="bottom")
#dev.off()
# with increasing depth, more planktivores and fewer herbivores/invertivores
# just like Kaya found

# Relative density of trophs by categorical depth bin
ggplot(data=dens_rel_troph, aes(x=DepthBin, y=RelDens, 
                                fill = Troph, color=Troph)) +
   geom_bar(position="fill", stat="identity") +
   scale_fill_manual(values=cbPalette) +
   scale_color_manual(values=cbPalette) +
   xlab("Depth Category (m)") +
   scale_y_continuous(expand = c(0, 0), name = "Relative Density") +
   theme(panel.background = element_blank(),
         axis.line=element_line())

##### Habitat characteristics #####
# Relative density of trophs with hard relief
ggplot(data=dens_rel_troph, aes(x=AVG_HARD_RELIEF, y=RelDens, color = Troph)) +
   geom_point() +
   geom_smooth(method="lm") +
   scale_color_manual(values=cbPalette) +
   scale_x_continuous(expand = c(0, 0), name = "Average Hard Relief (m)") +
   scale_y_continuous(expand = c(0, 0), name = "Relative Density") +
   theme(panel.background = element_blank(),
         axis.line=element_line())
# nothing here really, each mostly flat

# Relative density of trophs with coral cover
ggplot(data=dens_rel_troph, aes(x=PCT_CORAL_REV, y=RelDens, color = Troph)) +
   geom_point() +
   geom_smooth(method="lm") +
   scale_color_manual(values=cbPalette) +
   scale_x_continuous(expand = c(0, 0), name = "Coral Cover") +
   scale_y_continuous(expand = c(0, 0), name = "Relative Density") +
   theme(panel.background = element_blank(),
         axis.line=element_line())
# More planktivores with coral cover. Is that related to depth? Also fewer invertivores

# Coral cover with depth
ggplot(data=sitemeta, aes(x=DEPTH, y=PCT_CORAL)) +
   geom_point() +
   geom_smooth(method="lm") +
   scale_x_continuous(expand = c(0, 0), name = "Depth (m)") +
   scale_y_continuous(expand = c(0, 0), name = "Coral Cover") +
   theme(panel.background = element_blank(),
         axis.line=element_line())
# seems like coral cover peaks ~25-40m. >40% only 15-40m
# At least the highs get higher

# Coral cover with relief
ggplot(data=sitemeta, aes(x=PCT_CORAL, y=AVG_HARD_RELIEF)) +
   geom_point() +
   geom_smooth(method="lm") +
   scale_x_continuous(expand = c(0, 0), name = "Coral Cover") +
   scale_y_continuous(expand = c(0, 0), name = "Avg Hard Relief (m)") +
   theme(panel.background = element_blank(),
         axis.line=element_line())
# lots of variability, but they seem to vaguely go up together

# Relative density of trophs with sand cover
ggplot(data=dens_rel_troph, aes(x=PCT_SAND, y=RelDens, color = Troph)) +
   geom_point() +
   geom_smooth(method="lm") +
   scale_color_manual(values=cbPalette) +
   scale_x_continuous(expand = c(0, 0), name = "Sand Cover") +
   scale_y_continuous(expand = c(0, 0), name = "Relative Density") +
   theme(panel.background = element_blank(),
         axis.line=element_line())
# More invertivores and fewer herbivores with more sand (slight slope)

