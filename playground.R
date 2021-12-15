# playground.R
# Sarah Heidmann
# Created 15 Dec 2021
# Last modified 15 Dec 2021

# Testing using the rvc package by Jeremiah Blondeau
# https://github.com/jeremiaheb/rvc

# Data Portal and info
#https://grunt.sefsc.noaa.gov/rvc_analysis20/

# Load libraries
library(rvc) # can see all functions with rvc:::
library(tidyverse)
library(rfishbase)

##### Install #####
# install.packages('devtools')
# devtools::install_github('jeremiaheb/rvc')

##### Import fish data #####
# Benthic not available
vi17_19 <- getRvcData(years = c(2017, 2019), regions = c("STTSTJ", "STX"))

str(vi17_19)
# It's a list of 3 data frames.
# 1. sample_data : fish species, length, and numbers
# 2. stratum_data : some site metadata
# 3. taxonomic_data : fish metadata

##### Basic fish summaries from readme #####
spcs <- "yellowtail snapper"

# Domain stats
getDomainDensity(vi17_19, species = spcs)
#rvc:::getDomainDensity
getDomainAbundance(vi17_19, species = spcs)
getDomainOccurrence(vi17_19, species = spcs)

# Stratum stats
sdens <- getStratumDensity(vi17_19, species = spcs)
ggplot(data=sdens) + geom_point(aes(x=STRAT, y=density, color = REGION))

# Biomass
getDomainBiomass(vi17_19, species = spcs) # mean biomass
getDomainTotalBiomass(vi17_19, species = spcs) # total biomass
# Can modify growth parameters with library(rfishbase)

# Can check if scientific names up-to-date!!
rfishbase::validate_names("Haemulon plumierii") # returns the same
rfishbase::validate_names("Dasyatis americana") # returns new name

# Try some filters-- see more on readme
# protected areas separately- DOESN'T WORK BECAUSE NO PROTECTED PSUS
#getDomainOccurrence(vi17_19, species = spcs, merge_protected = FALSE)
# Restrict time and region
getDomainAbundance(vi17_19, species = spcs, years = 2017, region = "STTSTJ")

# Length classes
# Calculate length frequencies for Mangrove Snapper
# for lengths from 0-100cm by 1 cm bins
lglf = getDomainLengthFrequency(vi17_19, species = "Lut gris",
                                length_bins = seq(0,100,1))
ggplot(data=lglf) + geom_point(aes(x=length_class, y= frequency, color = REGION))
# Abundance of yellowtail above/below length at maturity (from database)
getDomainAbundance(vi17_19, species = "OCY CHRY", length_bins = "lm")
# Abundance of yellowtail with 26cm cutpoint
getDomainAbundance(vi17_19, species = "OCY CHRY", length_bins = 26)
# Create a custom lookup table and use it to set length classes
# for Mutton and Mangrove Snapper when calculating densities
lb = data.frame(SPECIES_CD = c("LUT ANAL", "LUT GRIS"), CUT = c(40, 30))
getDomainDensity(vi17_19, c("LUT ANAL", "LUT GRIS"), length_bins = lb)


##### Some further attempts #####
com_spec <- c("ACA SOLA", "CEP FULV", "EPI GUTT", "EPI MORI", "EPI STRI", 
              "LAC MAXI", "LUT ANAL", "LUT APOD", "LUT CAMP", "LUT CYAN",
              "LUT GRIS", "LUT JOCU", "LUT SYNA", "MYC BONA", "MYC INTE",
              "MYC MICR", "MYC PHEN", "MYC TIGR", "MYC VENE", "OCY CHRY")
summary(com_spec %in% vi17_19$sample_data$SPECIES_CD)
DomainDensity_com <- getDomainDensity(vi17_19, species = com_spec)
DomainDensity_com_pres <- filter(DomainDensity_com, density >0)
ggplot(data = DomainDensity_com_pres) + 
   geom_point(aes(x=YEAR, y = density, color = REGION)) +
   geom_line(aes(x=YEAR, y= density, group = REGION, color = REGION)) +
   geom_errorbar(aes(x=YEAR, ymin = density - var, ymax = density - var),
                 width = 0.25) +
   facet_wrap(~SPECIES_CD, scales = "free_y") +
   scale_x_continuous(breaks = c(2017, 2019))
