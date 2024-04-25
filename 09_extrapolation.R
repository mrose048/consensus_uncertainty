##%######################################################%##
#                                                          #
####              Extrapolation assessment              ####
#                                                          #
##%######################################################%##

sp <-
  data.table::fread('G:/My Drive/Dissertation/003_Ensemble_modeling/data/species_data.csv',
                    skip = 23) %>% tibble()


# Raw extrapolation
raw_extr <- "G:/My Drive/Franklin_grant/project/1-NSF_spatial_and_species_traits/2_Outputs/5_Extrapolation/" %>% list.dirs(., recursive = FALSE)
sapply(raw_extr, function(x) length(list.files(x))) %>% unique() # ok
names(raw_extr) <- basename(raw_extr)
raw_extr <- as.list(raw_extr)
raw_extr <- c(raw_extr["current"], raw_extr[names(raw_extr) != "current"])


ce <- list.files(raw_extr$current,
                 full.names = T,
                 pattern = '.tif$')
names(ce) <- basename(ce) %>% gsub(".tif$", "", .) %>% gsub('_', ' ', .) %>%  gsub(' ssp  caerulea', '', .)
ce <- ce[c(sp$species[1:82])]

# current extrapolation
ce_r <- terra::rast(ce)


# future extrapolation 

# cnrm_rcp45_2040_2069
f1 <- list.files(raw_extr$cnrm_rcp45_2040_2069,
                 full.names = T,
                 pattern = '.tif$')
names(f1) <- basename(f1) %>% gsub(".tif$", "", .) %>% gsub('_', ' ', .) %>%  gsub(' ssp  caerulea', '', .)
f1 <- f1[c(sp$species[1:82])]
f1_r <- terra::rast(f1)
f1_r <- sum(f1_r)
plot(f1_r,
     axes = F,
     col = viridis::magma(100))

# cnrm_rcp45_2070_2099
f2 <- list.files(raw_extr$cnrm_rcp45_2070_2099,
                 full.names = T,
                 pattern = '.tif$')
names(f2) <- basename(f2) %>% gsub(".tif$", "", .) %>% gsub('_', ' ', .) %>%  gsub(' ssp  caerulea', '', .)
f2 <- f2[c(sp$species[1:82])]
f2_r <- terra::rast(f2)
f2_r <- sum(f2_r)
plot(f2_r,
     axes = F,
     col = viridis::magma(100))

# rewrite for cnrm_rcp85_2040_2069
f3 <- list.files(raw_extr$cnrm_rcp85_2040_2069,
                 full.names = T,
                 pattern = '.tif$')
names(f3) <- basename(f3) %>% gsub(".tif$", "", .) %>% gsub('_', ' ', .) %>%  gsub(' ssp  caerulea', '', .)
f3 <- f3[c(sp$species[1:82])]
f3_r <- terra::rast(f3)
f3_r <- sum(f3_r)
plot(f3_r,
     axes = F,
     col = viridis::magma(100))

# rewrite for cnrm_rcp85_2070_2099
f4 <- list.files(raw_extr$cnrm_rcp85_2070_2099,
                 full.names = T,
                 pattern = '.tif$')
names(f4) <- basename(f4) %>% gsub(".tif$", "", .) %>% gsub('_', ' ', .) %>%  gsub(' ssp  caerulea', '', .)
f4 <- f4[c(sp$species[1:82])]
f4_r <- terra::rast(f4)
f4_r <- sum(f4_r)
plot(f4_r,
     axes = F,
     col = viridis::magma(100))

# rewrite for hades_rcp45_2040_2069
f5 <- list.files(raw_extr$hades_rcp45_2040_2069,
                 full.names = T,
                 pattern = '.tif$')
names(f5) <- basename(f5) %>% gsub(".tif$", "", .) %>% gsub('_', ' ', .) %>%  gsub(' ssp  caerulea', '', .)
f5 <- f5[c(sp$species[1:82])]
f5_r <- terra::rast(f5)
f5_r <- sum(f5_r)
plot(f5_r,
     axes = F,
     col = viridis::magma(100))

# rewrite for hades_rcp45_2070_2099
f6 <- list.files(raw_extr$hades_rcp45_2070_2099,
                 full.names = T,
                 pattern = '.tif$')
names(f6) <- basename(f6) %>% gsub(".tif$", "", .) %>% gsub('_', ' ', .) %>%  gsub(' ssp  caerulea', '', .)
f6 <- f6[c(sp$species[1:82])]
f6_r <- terra::rast(f6)
f6_r <- sum(f6_r)
plot(f6_r,
     axes = F,
     col = viridis::magma(100))

# rewrite for hades_rcp85_2040_2069
f7 <- list.files(raw_extr$hades_rcp85_2040_2069,
                 full.names = T,
                 pattern = '.tif$')
names(f7) <- basename(f7) %>% gsub(".tif$", "", .) %>% gsub('_', ' ', .) %>%  gsub(' ssp  caerulea', '', .)
f7 <- f7[c(sp$species[1:82])]
f7_r <- terra::rast(f7)
f7_r <- sum(f7_r)
plot(f7_r,
     axes = F,
     col = viridis::magma(100))

# rewrite for hades_rcp85_2070_2099
f8 <- list.files(raw_extr$hades_rcp85_2070_2099,
                 full.names = T,
                 pattern = '.tif$')
names(f8) <- basename(f8) %>% gsub(".tif$", "", .) %>% gsub('_', ' ', .) %>%  gsub(' ssp  caerulea', '', .)
f8 <- f8[c(sp$species[1:82])]
f8_r <- terra::rast(f8)
f8_r <- sum(f8_r)
plot(f8_r,
     axes = F,
     col = viridis::magma(100))

# combining all rasters
e_r <- scale(c(f1_r, f2_r, f3_r, f4_r, f5_r, f6_r, f7_r, f8_r))
names(e_r) <- c('CNRM-CM 5 RCP 4.5 2040-2069',
                'CNRM-CM 5 RCP 4.5 2070-2099',
                'CNRM-CM 5 RCP 8.5 2040-2069',
                'CNRM-CM 5 RCP 8.5 2070-2099',
                'HadGEM2-ES RCP 4.5 2040-2069',
                'HadGEM2-ES RCP 4.5 2070-2099',
                'HadGEM2-ES RCP 8.5 2040-2069',
                'HadGEM2-ES RCP 8.5 2070-2099')
plot(e_r,
     axes = F,
     col = rev(viridis::magma(100)),
     range = c(-2,8),
     nc = 2,
     nr = 4)

# write this plot a png file with publication quality resolution
png('G:/My Drive/Franklin_grant/project/1-NSF_spatial_and_species_traits/2_Outputs/5_Extrapolation/extrapolation.png',
    width = 8,
    height = 11,
    units = 'in',
    res = 500)
plot(e_r,
     axes = F,
     col = rev(viridis::magma(100)),
     range = c(-2,8),
     nc = 3,
     nr = 3)
dev.off()
