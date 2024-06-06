# Summary Stats
# Sarah Heidmann
# Created 5 April 2022
# Last modified 6 Sep 2023

# This script calculates some summary data.
# i.e., number of sites by year, number of fish species and families,
#     fish density, fish density by trophic group, and 
#     habitat characteristics, including investigating correlated factors.

# Load libraries
library(tidyverse)

##### Import the data #####
# Start with unfiltered by speies so we can get the full counts
spcs_list <- read_csv("data/NCDC_fish_species_list_2023.csv")
spcs_list_keep <- spcs_list %>% 
   filter(keep=="y")

dat <- readRDS("data/NCDC_2017_2022_full.rds")
dens_all <- readRDS("data/NCDC_2017_2022_density_full.rds")

#sitemeta <- readRDS("data/NCDC_sites_modelvars.rds")
sitemeta <- readRDS("data/NCDC_sitemeta.rds")

##### Site summary #####
# Total sites
dat$sample_data %>%
   select(PSU) %>%
   unique() %>% nrow()
# Sites by Year
dat$sample_data %>%
   select(YEAR, NCDC, PSU, DEPTH) %>%
   unique() %>%
   group_by(YEAR, NCDC) %>%
   summarise(sites = length(PSU),
             avgdepth = mean(DEPTH))
# Depth summary across sites
dat$sample_data %>%
   select(PSU, DEPTH) %>%
   unique() %>%
   summarise(sites = length(PSU),
             avgdepth = mean(DEPTH),
             mindepth = min(DEPTH),
             maxdepth = max(DEPTH))

##### Observed fish summary #####
# Fish species and families
# All (pre-filtering)
dat$sample_data %>% 
   filter(NUM>0) %>% 
   pull(SPECIES_CD) %>% 
   unique() %>% length()
dat$sample_data %>% 
   filter(NUM>0) %>%
   left_join(select(spcs_list, SPECIES_CD, FAMILY_UVI)) %>% 
   pull(FAMILY_UVI) %>% 
   unique() %>% length()
# After filtering <1% and cryptic
nrow(spcs_list_keep)
spcs_list_keep %>% 
   pull(FAMILY_UVI) %>% 
   unique() %>% length()

# Fish density summary
dens_all %>%
   filter(SPECIES_CD %in% spcs_list_keep$SPECIES_CD) %>% 
   group_by(PSU) %>%
   # convert to per 100m2
   summarize(totdens = sum(density)/1.77, .groups="drop") %>%
   summarize(meandens = mean(totdens),
             meddens = median(totdens),
             n= length(totdens),
             sd=sd(totdens)) %>%
   mutate(sem = sd/sqrt(n))

# Fish trophic groups. What are the NAs?
dens_all %>% 
   group_by(SPECIES_CD) %>% 
   summarize(totaldensity = sum(density)) %>% 
   left_join(spcs_list, by="SPECIES_CD") %>%
   select(TROPH_NOAA1, COMMON.NAME_UVI, totaldensity) %>%
   filter(is.na(TROPH_NOAA1)) %>% 
   arrange(desc(totaldensity))
# Mainly masked goby, then bar jack, silverside, grunt, scad
# What about after removing low occurrence?
dens_all %>% 
   filter(SPECIES_CD %in% spcs_list_keep$SPECIES_CD) %>% 
   group_by(SPECIES_CD) %>% 
   summarize(totaldensity = sum(density)) %>% 
   left_join(spcs_list_keep, by="SPECIES_CD") %>%
   select(TROPH_NOAA1, COMMON.NAME_UVI, totaldensity) %>%
   filter(is.na(TROPH_NOAA1)) %>% 
   arrange(desc(totaldensity))
# Mainly bar jack, then grunt, scad, others small

# What is the trophic breakdown of total and relative density?
dens_all %>% 
   filter(SPECIES_CD %in% spcs_list_keep$SPECIES_CD &!is.na(TROPH_NOAA1)) %>% 
   group_by(TROPH_NOAA1) %>% 
   summarize(totaldensity = sum(density), .groups="drop") %>% 
   arrange(desc(totaldensity)) %>% 
   mutate(reldensity = totaldensity / sum(totaldensity))

##### Habitat variable summaries and correlations #####
# Coral cover (diver estimate)
sitemeta %>%
   select(PCT_CORAL) %>%
   summary()
# What % of sites had >10% coral cover?
sitemeta %>% 
   filter(PCT_CORAL>10) %>% 
   nrow() / nrow(sitemeta)

# Depth summary
sitemeta %>% 
   select(DEPTH) %>% 
   summary()
# Rugosity summary (avg)
sitemeta %>%
   select(AVG_HARD_RELIEF) %>%
   summary()
# Rugosity summary (max)
sitemeta %>%
   select(MAX_HARD_RELIEF) %>%
   summary()

# Correlation of coral cover and rugosity?
ggplot(data=sitemeta) +
   geom_point(aes(x=log(PCT_CORAL), y=log(AVG_HARD_RELIEF)))
shapiro.test(log(sitemeta$PCT_CORAL+0.1))  #non-normal
shapiro.test(log(sitemeta$AVG_HARD_RELIEF+0.1)) #non-normal
cor.test(sitemeta$PCT_CORAL, sitemeta$AVG_HARD_RELIEF,
         method="spearman") # non-parametric test so no transformation needed
# Does it work/hold if we remove zeros?
coral_nonzero <- sitemeta %>% filter(PCT_CORAL>0 & AVG_HARD_RELIEF>0) %>% pull(PCT_CORAL)
relief_nonzero <- sitemeta %>% filter(PCT_CORAL>0 & AVG_HARD_RELIEF>0) %>% pull(AVG_HARD_RELIEF)
cor.test(coral_nonzero, relief_nonzero,method="spearman") # non-parametric test
# Yes, so stick with the untransformed test above

# Correlation of algae cover with depth?
# don't have maca cover, so use soft relief
ggplot(data=sitemeta) +
   geom_point(aes(x=DEPTH, y=MAX_SOFT_RELIEF))
# No correlation

# Correlation of depth and shelf edge distance?
ggplot(data=sitemeta) +
   geom_point(aes(x=to_shelf, y=DEPTH))
ggplot(data=sitemeta)+geom_histogram(aes(x=to_shelf))
shapiro.test(sitemeta$to_shelf)  #non-normal
ggplot(data=sitemeta)+geom_histogram(aes(x=DEPTH))
shapiro.test(sitemeta$DEPTH) #non-normal
# Do a nonparametric test
cor.test(sitemeta$to_shelf, sitemeta$DEPTH, method="spearman")
#summary(lm(to_shelf~DEPTH, sitemeta))
# Yes, they are correlated

# Correlation of depth and relief?
ggplot(data=sitemeta) +
   geom_point(aes(x=DEPTH, y=AVG_HARD_RELIEF))
cor.test(sitemeta$AVG_HARD_RELIEF, sitemeta$DEPTH, method="spearman")
# Significant p-value (0.01) but rho is small (-0.08)

# Correlation of relief and dist to shelf?
ggplot(data=sitemeta) +
   geom_point(aes(x=to_shelf, y=AVG_HARD_RELIEF))
# Two peaks (ish)


