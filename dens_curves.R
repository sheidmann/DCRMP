# Density Accumulation Curves
# Sarah Heidmann
# Created 20 April 2022
# Last modified 6 June 2024

# Purpose: Which species contribute most to each trophic group?
# This script creates Figure 4 in the manuscript.

# Load libraries
library(tidyverse)

##### Import the data #####
spcs_list <- read_csv("data/NCDC_fish_species_list_2023.csv") %>% 
   filter(keep=="y")
dens_all <- readRDS("data/NCDC_2017_2022_density.rds")

sitemeta <- readRDS("data/NCDC_sitemeta.rds")

dens_all <- dens_all %>% 
   left_join(select(sitemeta, PSU, DEPTH), 
             by="PSU") %>% 
   mutate(DEPTH_CAT = ifelse(DEPTH<30,"shallow","deep"))


##### Fig 4- Species accumulation curves by trophic group #####
# Calculate total density by species
dens_all_sum <- dens_all %>%
   group_by(TROPH_NOAA1, SCIENTIFIC.NAME) %>% 
   summarize(totaldensity = sum(density), .groups="drop")

# Calculate total for each trophic group
dens_sum_troph <- dens_all_sum %>% 
   group_by(TROPH_NOAA1) %>% 
   summarize(trophdensity = sum(totaldensity))

# Calculate percentage contribution of each species to its trophic group
dens_percs <- dens_all_sum %>% 
   left_join(dens_sum_troph, by = "TROPH_NOAA1") %>% 
   mutate(percdens = totaldensity / trophdensity) %>% 
   arrange(TROPH_NOAA1, desc(totaldensity))
# Convert to a cumulative percentage
dens_percs_cum <- dens_percs %>% 
   group_by(TROPH_NOAA1) %>% 
   reframe(cum_perc = cumsum(percdens)) %>% # summarize for multi-row groups
   # Add calculation back to main tibble
   select(cum_perc) %>% 
   bind_cols(dens_percs)
# Clean up for figure
dens_percs_cum_sub <- dens_percs_cum %>% 
   group_by(TROPH_NOAA1) %>% 
   arrange(desc(percdens)) %>% 
   # Take only the top 10 for simplicity
   slice_head(n=10) %>% 
   # Take out NAs
   filter(TROPH_NOAA1 %in% c("herbivore","invertivore","piscivore","planktivore")) %>% 
   # Put in the right order
   mutate(TROPH_NOAA1 = factor(TROPH_NOAA1, levels = c("planktivore","herbivore","piscivore","invertivore")))
# Look at result
dens_percs_cum_sub

# tiff(filename="outputs/trophic_accumulation_curves.tiff", 
#      width = 2000, height = 2000, units="px", res=360)
# pdf(file="outputs/trophic_accumulation_curves.pdf", 
#     height = 14, width = 14)
ggplot(data=dens_percs_cum_sub, 
       aes(x=cum_perc, y=reorder(SCIENTIFIC.NAME, -cum_perc), 
           group=1)) +
   geom_point() +
   geom_line() +
   geom_vline(aes(xintercept=0.5)) +
   facet_wrap(~TROPH_NOAA1, scales="free_y") +
   scale_x_continuous(limits = c(0,1)) +
   xlab("Cumulative Percentage of Density") +
   ylab("") +
   theme(panel.background = element_blank(),
         axis.line = element_line(),
         axis.text.y = element_text(face="italic"),
         #text = element_text(size = 10), # for TIFF
         text = element_text(size = 25), # for pdf
         strip.background = element_blank())
# dev.off()

##### Plot by depth cat too #####
# Calculate total density by depth cat and trophic group
dens_depth_sum <- dens_all %>%
   group_by(DEPTH_CAT, SPECIES_CD, COMMON.NAME_UVI, TROPH_NOAA1) %>% 
   summarize(totaldensity = sum(density), .groups="drop")

dens_depth_sum_troph <- dens_depth_sum %>% 
   group_by(DEPTH_CAT, TROPH_NOAA1) %>% 
   summarize(trophdensity = sum(totaldensity), .groups="drop")

dens_depth_percs <- dens_depth_sum %>% 
   left_join(dens_depth_sum_troph, by = c("DEPTH_CAT", "TROPH_NOAA1")) %>% 
   mutate(percdens = totaldensity / trophdensity) %>% 
   arrange(DEPTH_CAT,TROPH_NOAA1, desc(totaldensity))
dens_depth_percs_cum <- dens_depth_percs %>% 
   group_by(DEPTH_CAT, TROPH_NOAA1) %>% 
   reframe(cum_perc = cumsum(percdens)) %>% 
   select(cum_perc) %>% 
   bind_cols(dens_depth_percs)
dens_depth_percs_cum_sub <- dens_depth_percs_cum %>% 
   group_by(DEPTH_CAT, TROPH_NOAA1) %>% 
   #arrange(desc(percdens)) %>% 
   slice_head(n=10) %>% 
   filter(TROPH_NOAA1 %in% c("herbivore","invertivore","piscivore","planktivore"))
dens_depth_percs_cum_sub

ggplot(data=dens_depth_percs_cum_sub, 
       aes(x=cum_perc, y=reorder(COMMON.NAME_UVI, -cum_perc), 
            shape=DEPTH_CAT)) +
   geom_point(alpha=0.5) +
   geom_vline(aes(xintercept=0.5)) +
   facet_wrap(~TROPH_NOAA1, scales="free_y") +
   scale_x_continuous(limits = c(0,1)) +
   xlab("Cumulative percentage (density)") +
   ylab("")
ggsave("outputs/accumulation_curves_troph_depth_bw.jpeg")

# The plotting order is weird. Maybe sort by total density (no depth separation)?
# look at the table
dens_depth_percs_cum_sub %>% 
   pivot_wider(id_cols =c(COMMON.NAME_UVI, TROPH_NOAA1),
               names_from = DEPTH_CAT,
               values_from = percdens) %>% 
   mutate(ratio = deep / shallow) %>% 
   print(n=71)

##### Depth only, combining trophic groups #####
dens_d_sum <- dens_all %>%
   group_by(SPECIES_CD, COMMON.NAME_UVI, DEPTH_CAT) %>% 
   summarize(totaldensity = sum(density), .groups="drop")

dens_sum_depth <- dens_d_sum %>% 
   group_by(DEPTH_CAT) %>% 
   summarize(depthdensity = sum(totaldensity))

dens_d_percs <- dens_d_sum %>% 
   left_join(dens_sum_depth) %>% 
   mutate(percdens = totaldensity / depthdensity) %>% 
   arrange(DEPTH_CAT, desc(totaldensity))
dens_d_percs_cum <- dens_d_percs %>% 
   group_by(DEPTH_CAT) %>% 
   reframe(cum_perc = cumsum(percdens)) %>% 
   select(cum_perc) %>% 
   bind_cols(dens_d_percs)
dens_d_percs_cum_sub <- dens_d_percs_cum %>% 
   group_by(DEPTH_CAT) %>% 
   arrange(cum_perc) %>% 
   slice_head(n=10)

ggplot(data=dens_d_percs_cum_sub, 
       aes(x=cum_perc, y=reorder(COMMON.NAME_UVI, -cum_perc), 
           group=1, color = DEPTH_CAT)) +
   geom_point(alpha=0.5) +
   geom_vline(aes(xintercept=0.5)) +
   scale_x_continuous(limits = c(0,1)) +
   xlab("Cumulative percentage (density)") +
   ylab("")

# look at the table
dens_d_percs_cum_sub %>% 
   pivot_wider(id_cols =c(COMMON.NAME_UVI),
               names_from = DEPTH_CAT,
               values_from = percdens) %>% 
   mutate(ratio = deep / shallow) 
