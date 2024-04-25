##%######################################################%##
#                                                          #
####       Calculating occupied vs. unrestricted        ####
####           habitat estimates for species            ####
#                                                          #
##%######################################################%##


# Load required packages
require(terra)
require(dplyr)
require(tidyverse)

sp <-
data.table::fread('H:/My Drive/10_Uncertainty_manuscript/D&D_submission2/TableS5.csv',
                  skip = 25) %>% tibble()


# list of all SDMs
# occupied
list_c <-
  "G:/My Drive/Dissertation/003_Ensemble_modeling/data/00_current_habitat_maps/" %>% list.files(pattern = '.tif$',
                                                                                                full.names = TRUE,
                                                                                                recursive = T)

# unrestricted
list_ur <- "G:/My Drive/Franklin_grant/project/1-NSF_spatial_and_species_traits/2_Outputs/1_Current/Ensemble/" %>% list.files(pattern = '.tif$',
                                                                                                                          full.names = TRUE,
                                                                                                                          recursive = T)

# remove files with "coast" in file name from list_ur
list_ur <- list_ur[!grepl("coast", list_ur)]


# list of consensus models
c_models <- c('mean', 'meansup', 'meanthr', 'meanw', 'median')

# list of species
species <- sp$species %>% unique()


# loop through c_models and species to calculate the area of occupied and unrestricted habitat
for (j in species) {
  print(j)
  # read in occupied habitat
  occ <- list_c %>% grep(j, ., value = T) %>% rast()
  # set areas <= 0 to NA
  occ[occ <= 0] <- NA
  # read in unrestricted habitat
  unres <- list_ur %>% grep(j, ., value = T) %>% rast()
  # select 2,4,6,8,10 layers unrestriced (layer with threshold that maximizes sensitivity + specificity)
  unres <- unres[[c(2, 4, 6, 8, 10)]]
  # set area <= 0 to NA
  unres[unres <= 0] <- NA
  # calculate area of occupied habitat
  occ_area <- terra::expanse(occ, unit = 'km')
  # calculate area of unrestricted habitat
  unres_area <- terra::expanse(unres, unit = 'km')
  # calculate the ratio of occupied to unrestricted habitat
  ratio <- occ_area / unres_area
  # save the ratio to a csv file
  data.table::fwrite(
    data.frame(
      species = rep(j, 5),
      model = c_models,
      unres = unres_area$area,
      occ = occ_area$area,
      ratio = ratio$area
    ),
    file = paste0('G:/My Drive/Dissertation/003_Ensemble_modeling/data/06_occupied_vs_unrestriced/', j, '.csv')
  )
}

# Read in files and combine
files <- list.files('G:/My Drive/Dissertation/003_Ensemble_modeling/data/06_occupied_vs_unrestriced/', pattern = '.csv$', full.names = T)
df <- lapply(files, read.csv) %>% bind_rows() %>% tibble()

# save full data frame
data.table::fwrite(df, 'G:/My Drive/Dissertation/003_Ensemble_modeling/data/06_occupied_vs_unrestriced/occupied_vs_unrestriced.csv')

# explore ratio by model
df %>%
  ggplot(aes(x = model, y = ratio)) +
  geom_boxplot() +
  theme_minimal()

# explore ratio by occupied range size
df %>%
  ggplot(aes(x = occ, y = ratio, color = model)) +
  geom_point() +
  theme_minimal()


##%######################################################%##
#                                                          #
####            Example maps of occupied and            ####
####        unrestricted habitat for two species        ####
#                                                          #
##%######################################################%##

require(ggspatial)

cfp <- vect('G:/My Drive/Franklin_grant/project/1-NSF_spatial_and_species_traits/1_Inputs/4_Shapefiles/JepsonRegions/JepsonRegions_CFP.shp')


# occurrence records
occ <- data.table::fread('G:/My Drive/Franklin_grant/project/1-NSF_spatial_and_species_traits/1_Inputs/1_Occurrences/2_AllOccurrences/1_spp_pres_abs_cleaned.gz')

pc_occ <- occ %>% 
  filter(species == 'Pinus coulteri' &
           pr_ab == 1) %>%
  vect(geom = c('longitude_m', 'latitude_m'), crs = crs(cfp))


# Pinus coulteri
# Two panel map of unrestricted and occupied habitat with occurrence records
# read in occupied habitat
pc_c <- list_c %>% grep('Pinus coulteri', ., value = T) %>% rast()
# read in unrestricted habitat
pc_ur <- list_ur %>% grep('Pinus coulteri', ., value = T) %>% rast()
# occupied habitat
pc_map <- ggplot() +
  layer_spatial(data = cfp,
                fill = 'gray80',
                color = 'black') +
  geom_raster(data = as.data.frame(pc_c[[5]], xy = T) %>% filter(max_sens_spec > 0),
              aes(x = x, y = y, fill = max_sens_spec)) +
  layer_spatial(data = cfp,
                fill = NA,
                color = 'black') +
  # layer_spatial(data = pc_occ,
  #               color = 'blue',
  #               size = .6,
  #               alpha = .3) +
  scale_fill_gradientn(colours = rev(terrain.colors(10)),
                       guide = 'none') +
  theme_minimal() +
  labs(title = 'Occupied habitat: Pinus coulteri',
       x = 'Longitude',
       'y' = 'Latitude') +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))

# unrestricted habitat
pc_map2 <- ggplot() +
  layer_spatial(data = cfp,
                fill = 'gray80',
                color = 'black') +
  geom_raster(data = as.data.frame(pc_ur[[10]], xy = T) %>% filter(max_sens_spec > 0),
              aes(x = x, y = y, fill = max_sens_spec)) +
  layer_spatial(data = cfp,
                fill = NA,
                color = 'black') +
  layer_spatial(data = pc_occ,
                color = 'blue',
                size = .6,
                alpha = .3) +
  scale_fill_gradientn(colours = rev(terrain.colors(10)),
                       guide = 'none') +
  theme_minimal() +
  labs(title = 'Unrestricted habitat: Pinus coulteri',
       x = 'Longitude',
       'y' = 'Latitude') +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))

# arctostaphylos ruids

# occurrence records
ar_occ <- occ %>% 
  filter(species == 'Arctostaphylos rudis' &
           pr_ab == 1) %>%
  vect(geom = c('longitude_m', 'latitude_m'), crs = crs(cfp))

# Two panel map of unrestricted and occupied habitat with occurrence records
# read in occupied habitat
ar_c <- list_c %>% grep('Arctostaphylos rudis', ., value = T) %>% rast()
# read in unrestricted habitat
ar_ur <- list_ur %>% grep('Arctostaphylos rudis', ., value = T) %>% rast()
# occupied habitat
ar_map <- ggplot() +
  layer_spatial(data = cfp,
                fill = 'gray80',
                color = 'black') +
  geom_raster(data = as.data.frame(ar_c[[3]], xy = T) %>% filter(max_sens_spec > 0),
              aes(x = x, y = y, fill = max_sens_spec)) +
  layer_spatial(data = cfp,
                fill = NA,
                color = 'black') +
  scale_fill_gradientn(colours = rev(terrain.colors(10)),
                       guide = 'none') +
  theme_minimal() +
  labs(title = 'Occupied habitat: Arctostaphylos rudis',
       x = 'Longitude',
       'y' = 'Latitude') +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))

# unrestricted habitat
ar_map2 <- ggplot() +
  layer_spatial(data = cfp,
                fill = 'gray80',
                color = 'black') +
  geom_raster(data = as.data.frame(ar_ur[[6]], xy = T) %>% filter(max_sens_spec > 0),
              aes(x = x, y = y, fill = max_sens_spec)) +
  layer_spatial(data = cfp,
                fill = NA,
                color = 'black') +
  layer_spatial(data = ar_occ,
                color = 'blue',
                size = .6,
                alpha = .3) +
  scale_fill_gradientn(colours = rev(terrain.colors(10)),
                       guide = 'none') +
  theme_minimal() +
  labs(title = 'Unrestricted habitat: Arctostaphylos rudis',
       x = 'Longitude',
       'y' = 'Latitude') +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))

require(patchwork)

ggsave((pc_map2|pc_map)/(ar_map2|ar_map),
       filename = "H:/My Drive/10_Uncertainty_manuscript/D&D_submission2/figures/habitat_restriction_examples.png", 
       dpi = 500,
       height = 16,
       width = 16)

