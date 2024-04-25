##%######################################################%##
#                                                          #
####        Uncertainty manuscript plot dataset         ####
#                                                          #
##%######################################################%##

require(dplyr)
require(terra)
require(ggplot2)
require(ggspatial)
require(ggnewscale)
require(ggsci)
library(adehabitatHR)
require(plyr)
require(tidyr)
require(ggthemes)

sp <-
  data.table::fread('G:/My Drive/Dissertation/003_Ensemble_modeling/data/species_data.csv',
                    skip = 23) %>% tibble()

# Occ database
occ <- data.table::fread('G:/My Drive/Franklin_grant/project/1-NSF_spatial_and_species_traits/1_Inputs/1_Occurrences/2_AllOccurrences/1_spp_pres_abs_cleaned.gz') %>% 
  tibble() %>%
  filter(species %in% sp$species) %>%
  mutate(type = ifelse(data_base %in% c('Thorne_Releves', 'CalFishAndWildlife'), 'presence-absence', 'presence-only'))


plot_summ <- occ %>%
  dplyr::group_by(species, pr_ab, data_base) %>%
  dplyr::summarize(n = n())

data.table::fwrite(plot_summ, 'H:/My Drive/10_Uncertainty_manuscript/D&D_submission2/pa_data_sources.csv')

# calculate prevalance with and without presence-only locations

w_po <- list()
for(i in 1:length(sp$species)){
  
  sp_d <- occ %>% filter(species == sp$species[i])
  
  np <- nrow(sp_d %>% filter(pr_ab == 1))
  na <- nrow(sp_d %>% filter(pr_ab == 0))
  
  w_po[[i]] <- tibble(species = sp$species[i],
                            np = np,
                            na = na,
                            prev = np/(np+na),
                            type = 'w_po')
  
}
w_po <- bind_rows(w_po)

wo_po <- list()
for (i in 1:length(sp$species)) {
  sp_d <-
    occ %>% filter(species == sp$species[i] & type == 'presence-absence')
  
  np <- nrow(sp_d %>% filter(pr_ab == 1))
  na <- nrow(sp_d %>% filter(pr_ab == 0))
  
  wo_po[[i]] <- tibble(
    species = sp$species[i],
    np = np,
    na = na,
    prev = np / (np + na),
    type = 'wo_po'
  )
  
}
wo_po <- bind_rows(wo_po) 


comp <- bind_rows(w_po, wo_po) %>%
  na.omit()

comp2 <- comp %>%
  pivot_wider(
    names_from = type,
    values_from = c(np, na, prev)
  ) %>%
  mutate(po_p_r = np_wo_po/(np_w_po -np_wo_po))

# scatterplot comparing prevelance
prev_c <- ggplot(comp2, aes(x = prev_wo_po, y = prev_w_po)) +
  geom_point(aes(size = prev_w_po - prev_wo_po)) +
  theme_minimal() +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = 'Prevalence (without presence-only records)',
       y = 'Prevalence (with presence-only records)',
       size = 'Difference') +
  theme(
    text = element_text(size = 25),
    axis.title.y = element_text(margin = margin(
      t = 0,
      r = 20,
      b = 0,
      l = 0
    )),
    axis.title.x = element_text(margin = margin(
      t = 20,
      r = 20,
      b = 0,
      l = 0
    ))
  )

ggsave(prev_c,
       filename = "H:/My Drive/10_Uncertainty_manuscript/D&D_submission2/figures/prevalence_scatter.png", 
       dpi = 500,
       height = 8,
       width = 12)


# Compare the datasets
comp <- tibble(species = w_calflora$species,
               prev_diff = w_calflora$prev-wo_calflora$prev,
               po = w_calflora$np-wo_calflora$np) %>%
  na.omit()

data.table::fwrite(comp, 'H:/My Drive/10_Uncertainty_manuscript/D&D_submission2/prev_comparison.csv')

comp <- data.table::fread('H:/My Drive/10_Uncertainty_manuscript/D&D_submission2/prev_comparison.csv') %>% tibble()

##%######################################################%##
#                                                          #
####                 Plot database map                  ####
#                                                          #
##%######################################################%##

cfp <- vect('G:/My Drive/Franklin_grant/project/1-NSF_spatial_and_species_traits/1_Inputs/4_Shapefiles/JepsonRegions/JepsonRegions_CFP.shp')

plots <- occ %>% dplyr::select(latitude_m, longitude_m, occ_id, year, data_base) %>%
  distinct() %>%
  mutate(type = ifelse(data_base %in% c('Thorne_Releves', 'CalFishAndWildlife'), 'presence-absence', 'presence-only')) %>%
  vect(geom = c('longitude_m', 'latitude_m'), crs = crs(cfp))

plots$type <- factor(plots$type, levels = c('presence-only', 'presence-absence'))

# California floristic province
cfp <-
  terra::vect("G:/My Drive/Franklin_grant/project/1-NSF_spatial_and_species_traits/1_Inputs/4_Shapefiles/JepsonRegions/JepsonRegions_CFP.shp")

# US shapefile
us_shp <- terra::vect('G:/My Drive/Franklin_grant/project/data/shapefiles/tl_2019_us_state/tl_2019_us_state.shp') %>%
  project(crs(cfp))


# Hillshade
hillmat <- data.table::fread("G:/My Drive/Franklin_grant/project/1-NSF_spatial_and_species_traits/1_Inputs/2_Predictors/3_AdditionalPredictors/hillmat_cfp.gz") %>% tibble()


# Figure #: CFP map

# CFP map
cfp_map <- ggplot() +
  geom_raster(data = hillmat,
              aes(x = x, y = y, fill = layer),
              show.legend = FALSE) +
   scale_fill_gradient(low = "black", high = "white") +
  #new_scale_fill() +
  layer_spatial(data = subset(plots, plots$type == 'presence-only'), 
                alpha = .2,
                size = .4,
                color = "#377EB8") +
  layer_spatial(data = subset(plots, plots$type == 'presence-absence'), 
                alpha = .2,
                size = .4,
                color = "black") +
  layer_spatial(data = cfp,
                size = 0.4,
                color = 'black',
                fill = 'transparent') +
  coord_sf(expand = FALSE) +
  theme_map(base_size = 15, base_family = '') +
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = c(.75, .85)) +
  annotation_scale(location = "bl",
                   width_hint = 0.25,
                   text_family = '',
                   text_cex = 1.5,
                   height = unit(.15, "in")) +
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.4, "in"),
    pad_y = unit(0.8, "in"),
    style = north_arrow_fancy_orienteering
  ) 


ggsave(cfp_map,
       filename = "H:/My Drive/10_Uncertainty_manuscript/D&D_submission2/figures/plot_map2.png", 
       dpi = 500,
       height = 8,
       width = 8)


##%######################################################%##
#                                                          #
####         Elevational distribuiton of plots          ####
#                                                          #
##%######################################################%##

# Elevation
elev <- terra::rast("G:/My Drive/Franklin_grant/project/data/Elevation/dem_cfp.tif")
names(elev) <- 'elevation'

e_df <- as.data.frame(elev) %>%
  mutate(type = 'CFP elevation')

plots_e <- extract(elev, plots)
plots2 <- tibble(as.data.frame(plots), plots_e) %>%
  dplyr::select(type, elevation)


plots2 <- bind_rows(plots2, e_df)
plots2$type <-
  factor(plots2$type,
         levels = c('CFP elevation', 'presence-only', 'presence-absence'))

plot_elev <- ggplot(plots2, aes(x = elevation, fill = type)) +
  #stat_density(aes(y = ..count..)) +
  geom_density(position = 'stack') +
  scale_fill_manual(
    breaks = c('CFP elevation', 'presence-only', 'presence-absence'),
    labels = c('study area', 'presence-only', 'presence-absence'),
    values = c('grey', "#377EB8", 'black'),
    name = NULL
  ) +
 # scale_y_continuous(trans = 'log1p') +
  theme_minimal() +
  labs(x = 'Elevation (m)', y = 'Density') +
  theme(
    text = element_text(size = 23),
    legend.position = c(0.6, 0.75),
    legend.text = element_text(size = 35),
    legend.background = element_rect(fill = 'white', color = 'black'),
    axis.title.y = element_text(margin = margin(
      t = 0,
      r = 20,
      b = 0,
      l = 0
    )),
    axis.title.x = element_text(margin = margin(
      t = 20,
      r = 20,
      b = 0,
      l = 0
    ))
  )

ggsave(
  plot_elev,
  filename = "H:/My Drive/10_Uncertainty_manuscript/D&D_submission2/figures/plot_elevation.png",
  dpi = 500,
  height = 9,
  width = 14
)


##%######################################################%##
#                                                          #
####              Convex hull estimations               ####
#                                                          #
##%######################################################%##



plots <- occ %>%
  mutate(type = ifelse(data_base %in% c('Thorne_Releves', 'CalFishAndWildlife'), 'presence-absence', 'presence-only'))


# List of data frames with each species p/a
sp_list <- split(plots, plots$species)

# presence-absence
p_points <- list()
for(i in 1:length(sp_list)){
  p_points[[i]] <- sp_list[[i]] %>%
    dplyr::filter(pr_ab == 1 & type == 'presence-absence') %>%
    terra::vect(crs = crs(cfp),
                geom = c('longitude_m', 'latitude_m'))
}

names(p_points) <- names(sp_list)


range_size100 <- list()
range_size95 <- list()

for (i in 79:length(p_points)) {
  print(names(p_points[i]))
  range_size100[[i]] <-
    mcp(
      xy = as(p_points[[i]], "Spatial"),
      percent = 100,
      unin = c("m"),
      unout = c("km2")
    )
  range_size95[[i]] <-
    mcp(
      xy = as(p_points[[i]], "Spatial"),
      percent = 95,
      unin = c("m"),
      unout = c("km2")
    )
  range_size100[[i]] <-
    as_tibble(range_size100[[i]]) %>%
    dplyr::mutate(species = names(p_points[i])) %>%
    dplyr::rename(area100 = area)
  range_size95[[i]] <-
    as_tibble(range_size95[[i]]) %>%
    dplyr::mutate(species = names(p_points[i])) %>%
    dplyr::rename(area95 = area)
}

range_size100 <- bind_rows(range_size100) %>% dplyr::select(-id)
range_size95 <- bind_rows(range_size95) %>% dplyr::select(-id)
range_size <- left_join(range_size100, range_size95, by = 'species') %>%
  dplyr::mutate(type = 'presence-absence')


# presence-only
p_points2 <- list()
for(i in 1:length(sp_list)){
  p_points2[[i]] <- sp_list[[i]] %>%
    dplyr::filter(pr_ab == 1 & type == 'presence-only') %>%
    terra::vect(crs = crs(cfp),
                geom = c('longitude_m', 'latitude_m'))
}

names(p_points2) <- names(sp_list)


range_size100 <- list()
range_size95 <- list()

for (i in 1:length(p_points2)) {
  print(names(p_points2[i]))
  range_size100[[i]] <-
    mcp(
      xy = as(p_points2[[i]], "Spatial"),
      percent = 100,
      unin = c("m"),
      unout = c("km2")
    )
  range_size95[[i]] <-
    mcp(
      xy = as(p_points2[[i]], "Spatial"),
      percent = 95,
      unin = c("m"),
      unout = c("km2")
    )
  range_size100[[i]] <-
    as_tibble(range_size100[[i]]) %>%
    dplyr::mutate(species = names(p_points2[i])) %>%
    dplyr::rename(area100 = area)
  range_size95[[i]] <-
    as_tibble(range_size95[[i]]) %>%
    dplyr::mutate(species = names(p_points2[i])) %>%
    dplyr::rename(area95 = area)
}

range_size100 <- bind_rows(range_size100) %>% dplyr::select(-id)
range_size95 <- bind_rows(range_size95) %>% dplyr::select(-id)
range_size2 <- left_join(range_size100, range_size95, by = 'species') %>%
  dplyr::mutate(type = 'presence-only')


# Comparisons
all_ranges <- bind_rows(range_size, range_size2)



ggplot(all_ranges, aes(x = type, y = area100)) +
  geom_boxplot()

all_ranges2 <- all_ranges %>%
  pivot_wider(names_from = type,
              values_from = c(area100, area95)) %>%
  mutate(
    change_prop100 = (`area100_presence-only` - `area100_presence-absence`) /
      `area100_presence-absence`,
    change_prop95 = (`area95_presence-only` - `area95_presence-absence`) /
      `area95_presence-absence`
  )


# Map with example

ac_map <- ggplot() +
  layer_spatial(data = cfp,
                size = 0.4,
                color = 'black',
                fill = 'gray80') +
  layer_spatial(data = p_points2$`Aesculus californica`, 
                alpha = .5,
                size = 1.5,
                color = "#377EB8") +
  layer_spatial(data = a_points, 
                alpha = .3,
                size = 1,
                color = "red") +
  layer_spatial(data = p_points$`Aesculus californica`, 
                alpha = .5,
                size = 1.5,
                color = "black") +
  coord_sf(expand = FALSE) +
  theme_map(base_size = 15, base_family = '') +
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = c(.75, .85)) +
  annotation_scale(location = "bl",
                   width_hint = 0.25,
                   text_family = '',
                   text_cex = 1.5,
                   height = unit(.15, "in")) +
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.4, "in"),
    pad_y = unit(0.8, "in"),
    style = north_arrow_fancy_orienteering
  ) 


ggsave(ac_map,
       filename = "H:/My Drive/10_Uncertainty_manuscript/D&D_submission2/figures/aesculus_californica.png", 
       dpi = 500,
       height = 8,
       width = 8)

##%######################################################%##
#                                                          #
####             Compare presence-only and              ####
####      presence-absence in environmental space       ####
#                                                          #
##%######################################################%##


# Environmental variables - Current conditions
env <- file.path('1_Inputs/2_Predictors/1_Current') %>%
  list.files(., full.names = T, pattern = '.tif$') %>%
  terra::rast()
env <- homogenize_na(env)
env_names <- names(env)

pca <- correct_colinvar(env_layer = env,
                        method = c("pca"))

plots <- occ %>%
  mutate(type = ifelse(
    data_base %in% c('Thorne_Releves', 'CalFishAndWildlife'),
    'presence-absence',
    'presence-only'
  ))

plots_ex <- sdm_extract(plots,
                        x = 'longitude_m',
                        y = 'latitude_m',
                        env_layer = pca$env_layer,
                        variables = c('PC1', 'PC2'))


# example
ggplot(plots_ex %>% filter(
  species %in% c(
    'Abies magnifica',
    'Adenostoma sparsifolium',
    'Pinus quadrifolia',
    'Aesculus californica',
    'Lonicera subspicata',
    'Poa stebbinsii'
  ) & pr_ab == 1
),
aes(x = PC1, y = PC2, color = type)) +
  geom_point(alpha = .5) +
  facet_wrap(~ species)
