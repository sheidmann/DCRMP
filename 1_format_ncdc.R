# 1_format_ncdc
# Sarah Heidmann
# Created 24 Mar 2022
# Last modified 21 Mar 2023

# Load the libraries
library(rvc)
library(tidyverse)
library(openxlsx)
library(sp)
library(vegan)
library(sf)

##### Raw Sample Data #####
# Read the NCRMP data
# nc <- getRvcData(years = c(2017, 2019, 2021), regions = c("STTSTJ"))
# nc$sample_data$PRIMARY_SAMPLE_UNIT <- as.character(nc$sample_data$PRIMARY_SAMPLE_UNIT)
# write_rds(nc, "data/NCRMP_2017_2021_full.rds")
# write.xlsx(nc, file = "data/NCRMP_2017_2021_full.xlsx")
nc <- readRDS("data/NCRMP_2017_2021_full.rds")
nc$sample_data <- nc$sample_data %>% 
   add_column(NCDC="NC") %>% 
   mutate(PSU=paste(PRIMARY_SAMPLE_UNIT, YEAR, sep="_"),
          PRIMARY_SAMPLE_UNIT = as.character(PRIMARY_SAMPLE_UNIT))

# Read the DCRMP data
dc <- readRDS("data/DCRMP_2020_2022_full.rds")
dc$sample_data <- dc$sample_data %>%
   add_column(NCDC="DC") %>% 
   mutate(PSU=ifelse(YEAR==2020, PRIMARY_SAMPLE_UNIT,
                     paste(PRIMARY_SAMPLE_UNIT, YEAR, sep="_")),
          DEPTH_STRAT="GTR3")
# write.xlsx(dc, file = "data/DCRMP_2020_2022_full.xlsx")

# Combine them
dat <- list(sample_data = bind_rows(nc$sample_data, dc$sample_data),
            stratum_data = bind_rows(nc$stratum_data, dc$stratum_data),
            taxonomic_data = nc$taxonomic_data)
# The year "2020" was actually completed throughout 2018-2020. Year of completion is in the PSU. I'm going to leave it there since it looks like some sites were repeated in 2021.

# Add Sargassum Trigger
dat$taxonomic_data <- dat$taxonomic_data %>% 
   add_row(SPECIES_CD="XAN RING",FAMILY="Balistidae",
           SCINAME="Xanthichthys ringens",COMNAME="Sargassum Triggerfish",
           LC=NA,LM=NA,WLEN_A=NA,WLEN_B=NA)

# Fix longitude typo
dat$sample_data <- dat$sample_data %>% 
   mutate(LON_DEGREES= ifelse(LON_DEGREES>-64.1,LON_DEGREES-1,LON_DEGREES))

# Export
#write_rds(dat, "data/NCDC_2017_2022_full.rds")
# write.xlsx(dat, file = "data/NCDC_2017_2022_full.xlsx")

# How many sites?
dat$sample_data %>% 
   select(PSU, NCDC) %>% 
   unique() %>% 
   pull(NCDC) %>% as.factor() %>% 
   summary()
# DC  349 NC  724


##### Filter species by >1% occurrence #####
# spcs_list <- read_csv("data/data_pre2022/fish_species_list_2022.csv")
# # Which ones did we see and how common were they?
# spcs_occ <- getDomainOccurrence(dat, spcs_list$SPP_NOAA)
# spcs_occ_sum <- spcs_occ %>%
#    group_by(SPECIES_CD) %>%
#    mutate(occurrence_total = occurrence * n) %>%
#    summarize(occurrence_sum = sum(occurrence_total),
#              n_sum = sum(n)) %>%
#    mutate(occurrence_mean = occurrence_sum / n_sum)
# spcs <- spcs_occ_sum %>%
#    select(SPECIES_CD, occurrence_mean) %>%
#    right_join(spcs_list, by=c("SPECIES_CD"="SPP_NOAA")) %>%
#    arrange(occurrence_mean)
# write_csv(spcs, "data/NCDC_fish_species_list_2023_raw.csv")

# Import the updated list
spcs_list_full <- read_csv("data/NCDC_fish_species_list_2023.csv") 
spcs_list <- spcs_list_full %>% 
   filter(keep=="y")
 

# Filter out unwanted species and export again
dat_filter <- dat
dat_filter$sample_data <- dat_filter$sample_data %>% 
   filter(SPECIES_CD %in% spcs_list$SPECIES_CD)
#write_rds(dat_filter, "data/NCDC_2017_2022.rds")
# write.xlsx(dat_filter, file = "data/NCDC_2017_2022.xlsx")

##### Density #####
# Calculate density to load easily later.
dens_all_full <- getPSUDensity(dat, spcs_list_full$SPECIES_CD)
# Don't need filtered data because function filters based on spcs_list
dens_all <- getPSUDensity(dat, spcs_list$SPECIES_CD)
# Add my extra columns
siteinfo <- dat$sample_data %>% 
   select(YEAR, PRIMARY_SAMPLE_UNIT, NCDC, PSU) %>% 
   unique()
dens_all_full <- dens_all_full %>% 
   left_join(siteinfo, by=c("YEAR","PRIMARY_SAMPLE_UNIT")) %>% 
   left_join(spcs_list_full, by="SPECIES_CD")
dens_all <- dens_all %>% 
   left_join(siteinfo, by=c("YEAR", "PRIMARY_SAMPLE_UNIT")) %>% 
   left_join(spcs_list, by="SPECIES_CD")
# Export
#write_rds(dens_all_full, "data/NCDC_2017_2022_density_full.rds")
#write_rds(dens_all, "data/NCDC_2017_2022_density.rds")
# write.xlsx(dens_all_full, "data/NCDC_2017_2022_density_full.xlsx")
# write.xlsx(dens_all, "data/NCDC_2017_2022_density.xlsx")


##### Site Metadata #####
# Using diver estimate of coral cover (PCT_CORAL)
nc_sitemeta <- read_csv("data/habitat/USVI_2017_habitat_variables.csv") %>%
   bind_rows(read_csv("data/habitat/USVI_2019_habitat_variables.csv")) %>%
   bind_rows(read_csv("data/habitat/usvi_2021_habitat.csv")) %>% 
   add_column(NCDC="NC") %>% 
   mutate(PRIMARY_SAMPLE_UNIT = as.character(PRIMARY_SAMPLE_UNIT),
          PSU = paste(PRIMARY_SAMPLE_UNIT, YEAR, sep="_")) %>%
   left_join(unique(select(nc$sample_data, 
                           PSU, LAT_DEGREES, LON_DEGREES)), 
             by= "PSU") %>%
   # This introduces NAs with repeat PSUs so take them out (not all rows have metadata)
   # There are also a bunch of series that have no fish data
   # 1000 series in 2017, 4000 series in 2019, 9100 series in 2019, 4000 series in 2021
   # All others have lat/long so take those out
   filter(!is.na(STATION_NR) & !is.na(LAT_DEGREES)) %>% 
   # Fix longitude typo
   mutate(LON_DEGREES= ifelse(LON_DEGREES>-64.1,LON_DEGREES-1,LON_DEGREES))
dc_sitemeta20 <- read_csv("data/habitat/DCRMP_2020_Habitat.csv") %>%
   add_column(NCDC="DC") %>% 
   mutate(PSU = paste(PRIMARY_SAMPLE_UNIT, YEAR, sep="_")) %>% 
   mutate(PRIMARY_SAMPLE_UNIT = paste0(PRIMARY_SAMPLE_UNIT, "_", 
                                       as.character(YEAR))) %>% 
   mutate(YEAR=2020)
dc_sitemeta21 <- read_csv("data/habitat/DCRMP_2021_Habitat.csv", 
                          col_types=cols(PRIMARY_SAMPLE_UNIT=col_character())) %>%
   add_column(NCDC="DC") %>% 
   mutate(PSU = paste(PRIMARY_SAMPLE_UNIT, YEAR, sep="_")) %>% 
   left_join(unique(select(dc$sample_data, PSU, LON_DEGREES, LAT_DEGREES)))
dc_sitemeta22 <- read_csv("data/habitat/DCRMP_2022_Habitat.csv", 
                          col_types=cols(PRIMARY_SAMPLE_UNIT=col_character())) %>%
   add_column(NCDC="DC") %>% 
   mutate(PSU = paste(PRIMARY_SAMPLE_UNIT, YEAR, sep="_")) %>% 
   left_join(unique(select(dc$sample_data, PSU, LON_DEGREES, LAT_DEGREES)))
dc_sitemeta <-bind_rows(dc_sitemeta20, dc_sitemeta21, dc_sitemeta22)

# Have right number of sites?
nrow(nc_sitemeta) # 724
nrow(dc_sitemeta) # 349

sitemeta <- bind_rows(nc_sitemeta, dc_sitemeta)

# Project it
projXY <- function(dataset){
   dataset_s <- SpatialPoints(coords=dplyr::select(dataset,
                                                   LON_DEGREES,LAT_DEGREES),
                              proj4string = CRS("+proj=longlat +datum=WGS84"))
   dataset_sp <- spTransform(dataset_s, 
                             CRS("+proj=utm +zone=20 +datum=WGS84 +units=m"))
   coords <- as_tibble(dataset_sp) %>%
      rename(POINT_X=LON_DEGREES, POINT_Y=LAT_DEGREES)
   dataset <- bind_cols(dataset, coords)
   return(dataset)
}
sitemeta <- projXY(sitemeta)

# Classify as protected
steer <- read_csv("data/protected_areas/steer.csv") %>% 
   select(lat = lat_degrees, lon = lon_degrees) %>% 
   add_column(group = "STEER", MPA="STEER")
viis <- read_csv("data/protected_areas/VIIS.csv") %>% 
   select(lat  = lat_degrees, lon = lon_degrees) %>% 
   add_column(group = "VIIS", MPA="VIIS")
vicrnm <- read_csv("data/protected_areas/VICRNM.csv") %>% 
   select(lat = lat_degrees, lon = lon_degrees, group = FID) %>% 
   mutate(group = paste0("VICRNM", as.character(group))) %>% 
   add_column(MPA="VICRNM")
mcd <- read_csv("data/protected_areas/mcd_closedarea.csv") %>% 
   select(lat, lon = long) %>% 
   add_column(group = "MCD", MPA="MCD")
ghb <- read_csv("data/protected_areas/ghb_closedarea.csv") %>% 
   select(lat, lon = long) %>% 
   add_column(group = "GHB", MPA="GHB")
mpas <- bind_rows(steer, viis, vicrnm, mcd, ghb)
usvi <- read_csv("data/usvi_land.csv") %>% 
   filter(Island!="STX")

# Determine points in polygons
# Old version using sp package (outdated and tricky)
InMPA_sp <- function(dataset){
   dataset_s <- SpatialPoints(coords=dplyr::select(dataset,
                                                   LON_DEGREES,LAT_DEGREES),
                              proj4string = CRS("+proj=longlat +datum=WGS84"))
   mpas_s<-SpatialPolygons(list(
      Polygons(list(Polygon(coords=dplyr::select(steer,lon,lat))),ID="STEER"),
      Polygons(list(Polygon(coords=dplyr::select(viis,lon,lat))),ID="VIIS"),
      Polygons(list(Polygon(coords=dplyr::select(filter(vicrnm,group=="VICRNM1"),
                                                 lon,lat)),
                    Polygon(coords=dplyr::select(filter(vicrnm,group=="VICRNM2"),
                                                 lon,lat)),
                    Polygon(coords=dplyr::select(filter(vicrnm,group=="VICRNM3"),
                                                 lon,lat))),
               ID="VICRNM"),
      Polygons(list(Polygon(coords=dplyr::select(mcd,lon,lat))),ID="MCD"),
      Polygons(list(Polygon(coords=dplyr::select(ghb,lon,lat))),ID="GHB")))
   proj4string(mpas_s) <- CRS("+proj=longlat +datum=WGS84")
   # Calculate the area
   #area <- raster::area(mpas_s) / 1000000 # in km2
   ptinpoly <- sp::over(dataset_s, mpas_s)
   dataset_exp <- dataset %>% 
      mutate(MPA = case_match(ptinpoly, 1~"STEER",
                              2~"VIIS",3~"VICRNM",4~"MCD",5~"GHB"))
   return(dataset_exp)
}
# New version using sf package (works well with tidyverse)
# Include MPA with 1 km buffer
InMPA_sf <- function(dataset, mpa_dat){
   dataset_s <- st_as_sf(dataset, coords=c("LON_DEGREES","LAT_DEGREES"), 
                         crs="+proj=longlat +datum=WGS84")
   #plot(dataset_s) # plots all variables, facetted
   # plot(dataset_s["DEPTH"]) # plots depth
   # ggplot() + geom_sf(data=dataset_s, aes(color=DEPTH)) #ggplot!
   # steer_s <- st_as_sf(steer, coords=c("lon","lat"), 
   #                     crs="+proj=longlat +datum=WGS84")
   # viis_s <- st_as_sf(viis, coords=c("lon","lat"), 
   #                    crs="+proj=longlat +datum=WGS84")
   # mpas_s <- st_union(steer_s,viis_s)
   # plot(mpas_s)
   mpas_s <- mpa_dat %>% 
      st_as_sf(coords=c("lon","lat"), crs="+proj=longlat +datum=WGS84") %>% 
      group_by(group,MPA) %>% 
      summarize(geometry = st_combine(geometry)) %>% 
      st_cast("POLYGON")
   #ggplot(data=mpas_s) + geom_sf(aes(color=MPA),fill=NA)
   sf_use_s2(FALSE)
   mpas_s1km <- mpas_s %>% 
      st_transform(crs="+proj=utm +zone=20 +datum=WGS84 +units=m") %>% 
      st_buffer(dist=1000) %>% 
      st_transform(crs="+proj=longlat +datum=WGS84")
   #ggplot(data=mpas_s1km) + geom_sf(aes(color=MPA),fill=NA)
   ptinpoly <- st_within(dataset_s, mpas_s) %>% 
      sapply(., function(s) if (length(s) == 0) NA_character_ else paste(s, collapse = " "))
   ptinpoly_1km <- st_within(dataset_s, mpas_s1km) %>% 
      sapply(., function(s) if (length(s) == 0) NA_character_ else paste(s, collapse = " "))
   # mpas_s$group # gives order. some are in multiple so must substr to get 1 or will be NA
   dataset_exp <- dataset %>% 
      mutate(MPA = case_match(substr(ptinpoly,1,1), "1"~"GHB",
                              "2"~"MCD","3"~"STEER","4"~"VICRNM","5"~"VICRNM",
                              "6"~"VICRNM","7"~"VIIS"),
             MPA_1km = case_match(substr(ptinpoly_1km,1,1), "1"~"GHB",
                              "2"~"MCD","3"~"STEER","4"~"VICRNM","5"~"VICRNM",
                              "6"~"VICRNM","7"~"VIIS"))
   # ggplot(data=dataset_exp)+geom_point(aes(x=LON_DEGREES,y=LAT_DEGREES,color=MPA)) +coord_map()
   # ggplot(data=dataset_exp)+geom_point(aes(x=LON_DEGREES,y=LAT_DEGREES,color=MPA_1km)) +coord_map()
   return(dataset_exp)
}

sitemeta <- InMPA_sf(sitemeta,mpas)

# Plot it
# ggplot() +
#    geom_polygon(aes(x=long,y=lat, group = ORIG_FID), data  = usvi) +
#    geom_polygon(aes(x=lon, y=lat, group=group), data = mpas,
#                 fill = NA, color = "red") +
#    geom_point(aes(x=LON_DEGREES,y=LAT_DEGREES, color=MPA), data = sitemeta) +
#    coord_map()

# Site distances (shelfiness and shoriness)
shelfshore <- read_csv("data/all_sites_distance_to_stuff.csv")  %>% 
   mutate(PSU=ifelse(YEAR==2020, PRIMARY_SA,
                     paste(PRIMARY_SA, YEAR, sep="_"))) %>% 
   # Remove wrong calculation from longitude typo
   mutate(LON = ifelse(LON>-64.1,LON-1,LON),
          to_shelf = ifelse(to_shelf>40000,NA,to_shelf),
          to_land =ifelse(to_land>60000,NA,to_land))

# Summarize
# Total Fish Density by site
#dens_all <- readRDS("data/NCDC_2017_2022_density.rds")
totdens_site <- dens_all %>% 
   group_by(PSU) %>% 
   summarize(totdens = sum(density))

# Commercial Fish Density by Site
com_spcs <- c("CEP FULV", "EPI GUTT", "EPI STRI",
              "LAC MAXI", "LUT ANAL", "LUT APOD", "LUT CYAN",
              "LUT GRIS", "LUT JOCU", "LUT SYNA",
              #"MYC INTE", "MYC TIGR", "MYC VENE", 
              "OCY CHRY")
comdens_site <- dens_all %>% 
   filter(SPECIES_CD %in% com_spcs) %>% 
   group_by(PSU) %>% 
   summarize(comdens = sum(density))

# Total species richness
rich_site <- dens_all %>% 
   filter(density>0) %>% 
   group_by(PSU) %>% 
   summarize(richness = length(SPECIES_CD))

# Shannon and Simpson diversity indices
div_all <- dens_all %>%
   group_by(PSU) %>%
   summarize(shannon = diversity(density, index = "shannon"),
             simpson = diversity(density, index = "simpson"))

# Calculate absolute trophic group density
mat_troph <- dens_all %>%
   group_by(PSU, TROPH_NOAA1) %>%
   summarize(totdens = sum(density), .groups="drop") %>%
   filter(!(is.na(TROPH_NOAA1))) %>%
   # Need density in site (row) by species/troph (column) format
   pivot_wider(id_cols = PSU,
               names_from = TROPH_NOAA1,
               values_from = totdens) %>%
   column_to_rownames(var = "PSU")
# Calculate relative trophic group density
mat_rel_troph <- decostand(mat_troph, method = "total")
# Combine absolute and relative and format
dens_rel_troph_sites <- mat_rel_troph %>%
   rename_with(function(x) paste0(x,".rel")) %>% 
   rownames_to_column(var = "PSU") %>%
   as_tibble() %>%
   left_join(as_tibble(rownames_to_column(mat_troph, var ="PSU")),by="PSU")

# Add them all to our metadata
sitemeta_exp <- sitemeta %>% 
   select(-STATION_NR) %>% 
   left_join(select(shelfshore, PSU, to_land, to_shelf),by="PSU") %>%
   left_join(totdens_site, by="PSU") %>% 
   left_join(comdens_site, by="PSU") %>% 
   left_join(rich_site, by="PSU") %>% 
   left_join(div_all, by="PSU") %>% 
   left_join(dens_rel_troph_sites, by="PSU")

# Export
#write_rds(sitemeta_exp, "data/NCDC_sitemeta.rds")
# write.xlsx(sitemeta_exp, "data/NCDC_sitemeta.xlsx")

