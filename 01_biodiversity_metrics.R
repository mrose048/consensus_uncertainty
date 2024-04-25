############################################################
#                                                          #
#    Producing different maps based on consensus/GCM/RCP    #
#                                                          #
############################################################

setwd("G:/My Drive/Franklin_grant/project/1-NSF_spatial_and_species_traits/")

# Packages
{require(dplyr)
  require(raster)
  require(reshape2)
  require(stringr)
  require(ggplot2)
  require(RColorBrewer)
  require(nlme)
  require(gstat)
  require(MuMIn)
  require(terra)
  require(ggspatial)
  require(VGAM)
}


############################################################
#                                                          #
#                     Difference maps                      #
#                                                          #
############################################################

# Only looking at climate change, raw SDM outputs

cfp <- vect('1_Inputs/4_Shapefiles/JepsonRegions/JepsonRegions_CFP.shp')

thr <-
  readr::read_tsv("./2_Outputs/0_Model_performance/00_model_performance.txt")

thr <- thr %>%
  filter(model %in% c('meanw', 'mean', 'meansup', 'median', 'meanthr'))

# range wide habitat suitability change and each future projection factor + sd across all projections for each species
df <- readr::read_csv('G:/My Drive/Dissertation/003_Ensemble_modeling/data/exposure_sd.csv')

sp <- unique(df$sp) # species list

models <- unique(thr$model)
scen <- list.dirs("./2_Outputs/2_Projection", recursive = FALSE) %>% basename()

# list of all SDMs
list_c <-
  "G:/My Drive/Dissertation/003_Ensemble_modeling/data/00_current_habitat_maps/" %>% list.files(pattern = '.tif$',
                                                                                                full.names = TRUE,
                                                                                                recursive = T) # current models
list_f <-
  "./2_Outputs/2_Projection/" %>% list.files(pattern = '.tif$',full.names = TRUE, recursive = T) # future models


for(i in 1:length(sp)) {
  print(sp[i])
  
  list_c2 <- list_c %>% grep(sp[i], ., value = TRUE)
  list_c2 <- list_c2[!grepl('wiht_coast', list_c2)]
  list_f2 <- list_f %>% grep(sp[i], ., value = TRUE)
  list_f2 <- list_f2[!grepl('with_coast', list_f2)]
  
  for (j in 1:length(models)) {
    
    print(models[j])
    
    list_c3 <- list_c2 %>% grep(models[j], ., value = TRUE)
    list_f3 <- list_f2 %>% grep(models[j], ., value = TRUE)
    
    # threshold for each model
    thr2 <- thr %>%
      dplyr::filter(model == models[j] ,
                    species == sp[i]) %>%
      dplyr::select(thr_value)
    
    
    # Current condition
    lf <- list_c3[[1]]
    r <- terra::rast(lf)
    
    # cnrm_rcp45
    lf <- list_f3  %>% grep("cnrm_rcp45", ., value = TRUE)
    
    r55 <- terra::rast(grep('2040_2069', lf, value = TRUE))
    r85 <- terra::rast(grep('2070_2099', lf, value = TRUE))
    
  
    
    # full dispersal

    cnrm45_fd_mid <- r - r55[[2]]
    cnrm45_fd_end <- r - r85[[2]]
    
    # no dispersal

    c_mask1 <-
      terra::mask(r55[[1]],
                  r,
                  maskvalue = c(0, NA),
                  updatevalue = 0) # continuous mask, null dispersal future
    
    c_mask2 <-
      terra::mask(r85[[1]],
                  r,
                  maskvalue = c(0, NA),
                  updatevalue = 0) # continuous mask, null dispersal future
    
  
    cnrm45_nd_mid <- r - c_mask1
    cnrm45_nd_end <- r - c_mask2
    
    
    
    
    # 04_hades_rcp45
    lf <- list_f3  %>% grep("hades_rcp45", ., value = TRUE)
    
    r55 <- terra::rast(grep('2040_2069', lf, value = TRUE))
    r85 <- terra::rast(grep('2070_2099', lf, value = TRUE))
    
    
    # full dispersal
    
    hades45_fd_mid <- r - r55[[2]]
    hades45_fd_end <- r - r85[[2]]
    
    # no dispersal
    
    c_mask1 <-
      terra::mask(r55[[1]],
                  r,
                  maskvalue = c(0, NA),
                  updatevalue = 0) # continuous mask, null dispersal future
    
    c_mask2 <-
      terra::mask(r85[[1]],
                  r,
                  maskvalue = c(0, NA),
                  updatevalue = 0) # continuous mask, null dispersal future
    
    
    hades45_nd_mid <- r - c_mask1
    hades45_nd_end <- r - c_mask2
    
    # 03_cnrm_rcp85
    lf <- list_f3  %>% grep("cnrm_rcp85", ., value = TRUE)
    
    r55 <- terra::rast(grep('2040_2069', lf, value = TRUE))
    r85 <- terra::rast(grep('2070_2099', lf, value = TRUE))
    
    # full dispersal
    
    cnrm85_fd_mid <- r - r55[[2]]
    cnrm85_fd_end <- r - r85[[2]]
    
    # no dispersal
    
    c_mask1 <-
      terra::mask(r55[[1]],
                  r,
                  maskvalue = c(0, NA),
                  updatevalue = 0) # continuous mask, null dispersal future
    
    c_mask2 <-
      terra::mask(r85[[1]],
                  r,
                  maskvalue = c(0, NA),
                  updatevalue = 0) # continuous mask, null dispersal future
    
    
    cnrm85_nd_mid <- r - c_mask1
    cnrm85_nd_end <- r - c_mask2
    
    # 05_hades_rcp85
    lf <- list_f3  %>% grep("hades_rcp85", ., value = TRUE)
    
    r55 <- terra::rast(grep('2040_2069', lf, value = TRUE))
    r85 <- terra::rast(grep('2070_2099', lf, value = TRUE))
    
    # full dispersal
    
    hades85_fd_mid <- r - r55[[2]]
    hades85_fd_end <- r - r85[[2]]
    
    # no dispersal
    
    c_mask1 <-
      terra::mask(r55[[1]],
                  r,
                  maskvalue = c(0, NA),
                  updatevalue = 0) # continuous mask, null dispersal future
    
    c_mask2 <-
      terra::mask(r85[[1]],
                  r,
                  maskvalue = c(0, NA),
                  updatevalue = 0) # continuous mask, null dispersal future
    
    
    hades85_nd_mid <- r - c_mask1
    hades85_nd_end <- r - c_mask2
    
    
    maps <-
      c(
        cnrm45_fd_mid,
        cnrm45_fd_end,
        cnrm45_nd_mid,
        cnrm45_nd_end,
        hades45_fd_mid,
        hades45_fd_end,
        hades45_nd_mid,
        hades45_nd_end,
        cnrm85_fd_mid,
        cnrm85_fd_end,
        cnrm85_nd_mid,
        cnrm85_nd_end,
        hades85_fd_mid,
        hades85_fd_end,
        hades85_nd_mid,
        hades85_nd_end
      )
    
    names(maps) <- c(
      'cnrm45_fd_mid',
      'cnrm45_fd_end',
      'cnrm45_nd_mid',
      'cnrm45_nd_end',
      'hades45_fd_mid',
      'hades45_fd_end',
      'hades45_nd_mid',
      'hades45_nd_end',
      'cnrm85_fd_mid',
      'cnrm85_fd_end',
      'cnrm85_nd_mid',
      'cnrm85_nd_end',
      'hades85_fd_mid',
      'hades85_fd_end',
      'hades85_nd_mid',
      'hades85_nd_end'
    )
    
    writeRaster(
      maps,
      paste0(
        'G:/My Drive/Dissertation/003_Ensemble_modeling/data/01_difference_maps/',
        sp[i],
        '_',
        models[j],
        '.tif'
      ),
      overwrite = T
    )
  }
}


############################################################
#                                                          #
#      Species richness maps for each model/scenario       #
#                                                          #
############################################################

# To calculate species richness maps, we summed the continuous habitat suitability maps (above threshold that max sen & spec) for the current time period as well as each of the future scenarios

# sum of continuous habitat suitability maps based on Guillera-Arroita, G., J. J. Lahoz-Monfort, J. Elith, A. Gordon, H. Kujala, P. E. Lentini, M. A. McCarthy, R. Tingley, and B. A. Wintle. 2015. Is my species distribution model fit for purpose? Matching data and models to applications: Matching distribution models to applications. Global ecology and biogeography: a journal of macroecology 24:276-292.


# list of all SDMs
list_c <- "G:/My Drive/Dissertation/003_Ensemble_modeling/data/00_current_habitat_maps/" %>% list.files(full.names = TRUE, recursive = T) # current models
list_f <- "./2_Outputs/2_Projection/" %>% list.files(pattern = '.tif$', full.names = TRUE, recursive = T) # future models


# current species richness
for(i in 1:length(models)) {
  print(models[i])
  
  list_c2 <-
    "G:/My Drive/Dissertation/003_Ensemble_modeling/data/00_current_habitat_maps/" %>% 
    list.files(full.names = TRUE, recursive = T,
               pattern = paste0(models[i], '_1995.tif')) # current models
  

  
  sp_maps <- list()
  
  for (j in 1:length(sp)) {
    sp_map <-  list_c2 %>% grep(sp[j], ., value = TRUE)
    sp_maps[[j]] <- terra::rast(sp_map)
  }
  
  sp_maps <- terra::rast(sp_maps) 
  sp_maps2 <- sum(sp_maps, na.rm= TRUE) 
  names(sp_maps2) <- models[i]
  
  writeRaster(sp_maps2, paste0('G:/My Drive/Dissertation/003_Ensemble_modeling/data/02_sp_rich_maps/01_current/current_', models[i], '.tif'), overwrite = T)
}

c_sp_maps <-
  list.files(
    'G:/My Drive/Dissertation/003_Ensemble_modeling/data/02_sp_rich_maps/01_current/',
    full.names = T,
    pattern = '.tif$'
  ) %>% terra::rast()

# standard deviation
sp_maps_sd <- terra::stdev(c_sp_maps) %>% 
  crop(cfp) %>%
  mask(cfp)

# species richness map
sd_map <- ggplot() +
  layer_spatial(raster(sp_maps_sd)) +
  scale_fill_gradientn(colours = rev(pals::viridis(20)),
                       na.value = NA,
                       name = 'SD') +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.title = element_blank(),
    axis.text = element_blank(),
    text = element_text(family = 'serif', size = 10),
    legend.title = element_text(size = 10)
    #  legend.key.size = unit(1, 'cm')
  ) + coord_sf() 


ggsave(
  plot = sd_map,
  filename = 'G:/My Drive/Dissertation/003_Ensemble_modeling/figures/current_sp_richness_sd.png',
  height = 8,
  width = 8,
  units = "cm",
  dpi = 500
)

# convert to data frame
df <- c_sp_maps[]
df <- data.frame(df)
df$grid_number <- 1:nrow(df)
df <- na.omit(df)
coord <- crds(c_sp_maps)
coord <- coord[df$grid_number,]
df <- data.frame(df, coord)
rm(coord)

data.table::fwrite(df, 'G:/My Drive/Dissertation/003_Ensemble_modeling/data/02_sp_rich_maps/01_current/sp_rich_data.txt')

# future richness maps
# full dispsersal
for (i in 1:length(models)) {
  print(models[i])
  
  list_f2 <- list_f %>% grep(models[i], ., value = TRUE)
  
  for (k in 1:length(scen)) {
    print(scen[k])
    
    list_f3 <- list_f2 %>% grep(scen[k], ., value = TRUE)
    
    sp_maps <- list()
    
    for (j in 1:length(sp)) {
      sp_map <-  list_f3 %>% grep(sp[j], ., value = TRUE)
      sp_maps[[j]] <- terra::rast(sp_map)[[2]]
    }
    
    sp_maps <- terra::rast(sp_maps)
    sp_maps2 <- sum(sp_maps, na.rm = TRUE)
    
    names(sp_maps2) <- paste0(scen[k], '_', models[i], '_fd')
    
    writeRaster(
      sp_maps2,
      paste0(
        'G:/My Drive/Dissertation/003_Ensemble_modeling/data/02_sp_rich_maps/02_projection/',
        scen[k],
        '_',
        models[i],
        '_full_dispersal.tif'
      ),
      overwrite = T
    )
    
  }
}

tmp_dir <- tempdir()

# null-dispersal
for (i in 4:length(models)) {
  print(models[i])
  
  list_f2 <- list_f %>% grep(models[i], ., value = TRUE)
  list_c2 <- list_c %>% grep(models[i], ., value = TRUE)
  
  for (k in 1:length(scen)) {
    print(scen[k])
    
    list_f3 <- list_f2 %>% grep(scen[k], ., value = TRUE)
    
    sp_maps <- list()
    
    
    for (j in 1:length(sp)) {
    
      # Current condition
      r <- terra::rast(list_c2 %>% grep(sp[j], ., value = TRUE))
      
      rf <- terra::rast(list_f3 %>% grep(sp[j], ., value = TRUE))[[2]]
      
      sp_maps[[j]] <-
        terra::mask(rf[[1]],
                    r[[1]],
                    maskvalue = c(0, NA),
                    updatevalue = 0) # continuous mask, null dispersal future
      
      
    }
    
    sp_maps <- terra::rast(sp_maps)
    sp_maps2 <- sum(sp_maps, na.rm = TRUE)
    
    names(sp_maps2) <- paste0(scen[k], '_', models[i], '_nd')
    
    writeRaster(
      sp_maps2,
      paste0(
        'G:/My Drive/Dissertation/003_Ensemble_modeling/data/02_sp_rich_maps/02_projection/',
        scen[k],
        '_',
        models[i],
        '_null_dispersal.tif'
      ),
      overwrite = T
    )
    
    gc()
    files <- list.files(tmp_dir, full.names = T, pattern = ".tif")
    file.remove(files)
    
  }
}


f_sp_maps <-
  list.files(
    'G:/My Drive/Dissertation/003_Ensemble_modeling/data/02_sp_rich_maps/02_projection/',
    full.names = T,
    pattern = '.tif'
  ) %>% terra::rast() %>% crop(cfp) %>% mask(cfp)


# TROUBLESHOOT
#Error in coord[df$grid_number, ] : subscript out of bounds

# convert to data frame
df <- f_sp_maps[]
df <- data.frame(df)
df[is.na(df)] = 0
df$grid_number <- 1:nrow(df)
df <- na.omit(df)
coord <- crds(f_sp_maps)
coord <- coord[df$grid_number,]
df <- data.frame(df, coord)
rm(coord)

data.table::fwrite(df, 'G:/My Drive/Dissertation/003_Ensemble_modeling/data/02_sp_rich_maps/02_projection/sp_rich_data_fut.txt')

df <- data.table::fread('G:/My Drive/Dissertation/003_Ensemble_modeling/data/02_sp_rich_maps/02_projection/sp_rich_data_fut.txt') %>% as_tibble()

# future standard deviation of species richness (null dispersal)
nd_maps <- grep('nd', names(f_sp_maps), value = TRUE)
nd_maps <- f_sp_maps[[nd_maps]]

# standard deviation
nd_maps_sd <- terra::stdev(nd_maps) %>% 
  crop(cfp) %>%
  mask(cfp)

# species richness map
sd_map_fut_nd <- ggplot() +
  layer_spatial(raster(nd_maps_sd)) +
  scale_fill_gradientn(colours = rev(pals::viridis(20)),
                       na.value = NA,
                       name = 'SD') +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.title = element_blank(),
    axis.text = element_blank(),
    text = element_text(family = 'serif', size = 10),
    legend.title = element_text(size = 10)
    #  legend.key.size = unit(1, 'cm')
  ) + coord_sf() 


ggsave(
  plot = sd_map_fut_nd,
  filename = 'G:/My Drive/Dissertation/003_Ensemble_modeling/figures/future_nd_sp_richness_sd.png',
  height = 8,
  width = 8,
  units = "cm",
  dpi = 500
)


# future standard deviation of species richness (full dispersal)
fd_maps <- grep('fd', names(f_sp_maps), value = TRUE)
fd_maps <- f_sp_maps[[fd_maps]]

# standard deviation
fd_maps_sd <- terra::stdev(fd_maps) %>% 
  crop(cfp) %>%
  mask(cfp)

# species richness map
sd_map_fut_fd <- ggplot() +
  layer_spatial(raster(fd_maps_sd)) +
  scale_fill_gradientn(colours = rev(pals::viridis(20)),
                       na.value = NA,
                       name = 'SD') +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.title = element_blank(),
    axis.text = element_blank(),
    text = element_text(family = 'serif', size = 10),
    legend.title = element_text(size = 10)
    #  legend.key.size = unit(1, 'cm')
  ) + coord_sf() 


ggsave(
  plot = sd_map_fut_fd,
  filename = 'G:/My Drive/Dissertation/003_Ensemble_modeling/figures/future_fd_sp_richness_sd.png',
  height = 8,
  width = 8,
  units = "cm",
  dpi = 500
)


##%######################################################%##
#                                                          #
####   Percent loss of species/suitability per pixel    ####
#                                                          #
##%######################################################%##

# future suitability weighted richness - current suitability-weighted richness/
# current suitability weighted richness

c_sp_maps <-
  list.files(
    'G:/My Drive/Dissertation/003_Ensemble_modeling/data/02_sp_rich_maps/01_current/',
    full.names = T,
    pattern = '.tif$'
  ) %>% terra::rast() %>%
  crop(cfp) %>%
  mask(cfp)

f_sp_maps <-
  list.files(
    'G:/My Drive/Dissertation/003_Ensemble_modeling/data/02_sp_rich_maps/02_projection/',
    full.names = T,
    pattern = '.tif$'
  ) %>% terra::rast() %>% crop(cfp) %>% mask(cfp)

# A for loop to calculate relative change in species richness

# change areas with 0 current species richness to 0
c_sp_maps[c_sp_maps == 0] <- NA

c_sd_map <- stdev(c_sp_maps)
names(c_sd_map) <- 'sd'
c_sp_maps <- c(c_sp_maps, c_sd_map)

png(
  "G:/My Drive/Dissertation/003_Ensemble_modeling/figures/current_species richness.png",
  units = "in",
  width = 8,
  height = 6,
  res = 500
)

plot(
  c_sp_maps,
  axes = F,
  col = pals::viridis(7)
 # colNA = 'gray80'
)

dev.off()

# for all ensembles except mean
big_df <- list()
for (i in c(1, 2, 4, 5)) {
  f_maps <-
    grep(models[i],
         names(f_sp_maps),
         value = T)
  
  f_maps <- f_sp_maps[[f_maps]]
  
  f_maps[f_maps < 1] <- NA
  c_sp_maps[[i]][c_sp_maps[[i]] < 1] <- NA
  
  df <- list()
  
  for (j in 1:nlyr(f_maps)) {
    sp_change <-
      ((f_maps[[j]] - c_sp_maps[[i]]) / (c_sp_maps[[i]])) 
  
    
    f_minmax <- minmax(f_maps[[j]])
    c_minmax <- minmax(c_sp_maps[[i]])
    
    chg_thr <- (f_minmax[2,] - c_minmax[1,]) / c_minmax[1,]
    
    sp_change[sp_change >= chg_thr] <- NA
    
    writeRaster(
      sp_change,
      paste0(
        "G:/My Drive/Dissertation/003_Ensemble_modeling/data/05_sp_change_maps/",
        names(sp_change),
        ".tif"
      ),
      overwrite = T
    )
    
    # convert to data frame
    df[[j]] <- as.data.frame(sp_change, xy = T, na.rm = T)
  }
  
  big_df[[i]] <- df %>% reduce(left_join, by=c('x', 'y'))
}


# for mean 
f_maps <-
  grep(paste(models[-3],collapse="|"),
       names(f_sp_maps),
       value = T,
       invert = TRUE)

f_maps <- f_sp_maps[[f_maps]]

f_maps[f_maps < 1] <- NA
c_sp_maps[[3]][c_sp_maps[[3]] < 1] <- NA

df <- list()

for (j in 1:nlyr(f_maps)) {
  sp_change <-
    ((f_maps[[j]] - c_sp_maps[[3]]) / (c_sp_maps[[3]]))
  
  f_minmax <- minmax(f_maps[[j]])
  c_minmax <- minmax(c_sp_maps[[i]])
  
  chg_thr <- (f_minmax[2, ] - c_minmax[1, ]) / c_minmax[1, ]
  
  sp_change[sp_change >= chg_thr] <- NA
  
  writeRaster(
    sp_change,
    paste0(
      "G:/My Drive/Dissertation/003_Ensemble_modeling/data/05_sp_change_maps/",
      names(sp_change),
      ".tif"
    ),
    overwrite = T
  )
  
  # convert to data frame
  df[[j]] <- as.data.frame(sp_change, xy = T, na.rm = T)
}

big_df[[3]] <- df %>% reduce(left_join, by=c('x', 'y'))


# cbind big_df into a single data frame
big_df2 <- big_df %>% reduce(left_join, by = c('x', 'y'))

data.table::fwrite(
  big_df2,
  'G:/My Drive/Dissertation/003_Ensemble_modeling/data/sp_loss_prop_df_v2.gz'
)

##%######################################################%##
#                                                          #
####     Variance partitioning of species loss maps     ####
#                                                          #
##%######################################################%##

# Functions for variance partitioning
plot_res <- function(model, type="normalized"){
  dd <- model$data
  dd$res <- resid(model, type=type)
  dd$fitted <- fitted(model)
  ggplot(dd, aes(fitted, res, col=ensemble))+geom_point()+theme_bw()+
    facet_grid(gcm~rcp, scales = 'free_x')+
    theme(legend.position = 'bottom')
}

VariRepeated <- function(data){
  sn <- unique(data$scenario)
  vari_l <- list()
  for(i in 1:length(sn)){
    mycord <- data %>% filter(scenario==sn[i])
    sn_d <- data.frame(unique(mycord[,c('ensemble', 'rcp', 'gcm')]))
    mycord <- mycord[,c('x', 'y', 'res')]
    sp::coordinates(mycord) <-  c('x', 'y')
    vari <- variogram(res ~ 1, data = mycord)
    vari_l[[i]] <- data.frame(sn_d,data.frame(vari)[,c('dist', 'gamma')])
  }
  names(vari_l) <- sn
  vari_l <- plyr::ldply(vari_l)
  colnames(vari_l)[1] <- 'scenario'
  # return(vari_l)
  ggplot(vari_l, aes(dist, gamma, group=scenario, col=AOGCM)) + geom_point(size=0.5)+geom_smooth(method = 'loess', se = F)+theme_classic()+theme(legend.position = 'none')+
    facet_grid(RCP~Algorithm)
}


# continous spatial data with species turnover for each GCM, RCP, time period, and ensemble method
big_df <- data.table::fread('G:/My Drive/Dissertation/003_Ensemble_modeling/data/sp_loss_prop_df.gz') %>% as_tibble()

# add ID column
big_df$id <- 1:nrow(big_df)
big_df$id <- as.character(big_df$id)

# systematic sampling to reduce number of data
big_df2 <- big_df[seq(1, nrow(big_df), by=500),] # reduce value for by argument at moment to perform final analysis
dim(big_df2)

#rm(big_df)
memory.limit(15000)
{
  big_df2 <- melt(big_df2, id.vars = c('id', 'x', 'y'))
  names(big_df2)[4:5] <- c('scenario', 'sp_change')
  head(big_df2)
  length(unique(big_df2$scenario)) # ok!
  
  big_df2$scenario <- as.character(big_df2$scenario)
  big_df2$gcm <-
    str_split_fixed(big_df2$scenario, '_',2)[,1]
  big_df2$dispersal <-
    ifelse(grepl('nd', big_df2$scenario), 'null','full')
  big_df2$rcp <-
    ifelse(grepl('rcp45', big_df2$scenario), 'rcp45','rcp85')
  big_df2$ensemble <-
    str_split_fixed(str_split_fixed(str_split_fixed(
      str_split_fixed(str_split_fixed(big_df2$scenario, '_', 2)[, 2], '_', 2)[, 2], '_', 2
    )[, 2], '_', 2)[, 2], '_', 2)[, 1]
  big_df2$year <-
    ifelse(grepl('2040', big_df2$scenario), '2055','2085')
  # table(df2[,c('Algorithm', 'Dispersal', 'RCP', 'AOGCM')]) %>% as.data.frame()
}


# dispersal scenarios
DIS <- c("null", "full")
d <- 1
devl <- list()

for(d in 1:2){
  print(d)
  df3 <- big_df2 %>% filter(dispersal==DIS[d])%>% dplyr::select(-dispersal)
  
  # standardization
  # df2$delta_pd <- scale(df2$delta_pd)
  head(df3)
  nrow(df3)
  
  # sum(df2$delta_pd==0)/length(df2$delta_pd)
  # hist(df2$delta_pd)
  # boxplot((df2$delta_pd))
  
  form <- as.formula(sp_change ~ ensemble + ensemble:gcm + ensemble:gcm:rcp)
  form_gam <- as.formula(sp_change ~ ensemble + ensemble:gcm + ensemble:gcm:rcp +s(x,y))
  
  model1g <- glm(form, df3, family = 'gaussian')
  dev <- anova(model1g)$Deviance[-1]
  devl[[d]] <- dev/sum(dev)
  rm(model1g)
}


dev <- do.call(rbind, devl)
colnames(dev) <- c('Ensemble','GCM', 'RCP')
rownames(dev) <- c('null', 'full')
dev <- data.frame(dev) %>% mutate(disp=rownames(.)) %>% melt
dev$value <- dev$value*100


require(readr)
# write_tsv(dev, 'Alg_Gcm_Rcp.txt')

dev2 <- dev %>% mutate(variable=factor(variable, levels=c("Ensemble", "GCM","RCP","DIS")))


P0 <-
  ggplot(dev2, aes(as.factor(disp), value)) + geom_col(aes(fill = variable), position = 'dodge') +
  scale_fill_viridis_d() + labs(x = "Dispersal scenarios ", y = "Explained deviance (%)") +
  theme_classic() + theme(
    legend.title = element_blank(),
    legend.position = c(0.9, .8),
    legend.background = element_rect(colour = NA, fill = NA)
  ) + theme(axis.text = element_text(colour = 'black'))
P0

P1 <-
  ggplot(dev2, aes(variable, value)) + geom_col(fill = viridis::cividis(2)[1], position = 'dodge') +
  scale_fill_viridis_d() + labs(x = "", y = "Explained deviance (%)") + theme_classic() +
  theme(legend.title = element_blank(), legend.position = 'none') + theme(axis.text = element_text(colour = 'black'))
P1

require(gridExtra)
require(grid)
myplot1 <-arrangeGrob(P0, top = textGrob("a)", x = unit(0, "npc"), y = unit(1, "npc"),just = c("left", "top")))
myplot2 <-arrangeGrob(P1, top = textGrob("b)", x = unit(0, "npc"), y = unit(1, "npc"),just = c("left", "top")))

grid.arrange(myplot1, myplot2, ncol=2,  widths=c(2.5/4, 1.5/4))

ggsave(plot = grid.arrange(myplot1, myplot2, ncol=2,  widths=c(2.5/4, 1.5/4)), 'VarianceExplained.png', width = 9, height = 7, scale = 1.8, units = 'cm', dpi=300)




# Santi's variance partitioning code

df <- readr::read_tsv('C:/Users/santi/OneDrive/Documentos/FORESTAL/1-Trabajos/47-PalmsNeotropics/2-DataAnalysisPalms_2019/VariancePartitioning/data_mode.txt')
(df)
df$grid_number <- as.character(df$grid_number)
df <- data.frame(df)

(nrow(df)*50)/1000

# systematic sampling to reduce number of data
df2 <- df[seq(1, nrow(df), by=20),] # reduce value for by argument at moment to perform final analysis
dim(df2)
# df2 <- df
rm(df)
memory.limit(15000)
{
  df2 <- melt(df2, id.vars = c('grid_number', 'x', 'y'))
  names(df2)[4:5] <- c('scenario', 'delta_pd')
  head(df2)
  length(unique(df2$scenario)) # ok!
  
  df2$scenario <- as.character(df2$scenario)
  df2$Algorithm <-
    str_split_fixed(df2$scenario, '_',2)[,1]
  df2$Dispersal <-
    str_split_fixed(df2$scenario, '[.]',2)[,2]
  df2$RCP <-
    ifelse(grepl('rcp4_5', df2$scenario), 'rcp4_5','rcp8_5')
  df2$AOGCM <-
    str_split_fixed(str_split_fixed(df2$scenario, '_',2)[,2], '[.]',2)[,1]
  df2$AOGCM <-
    gsub('_rcp8_5', '', gsub('_rcp4_5', '', df2$AOGCM))
  # table(df2[,c('Algorithm', 'Dispersal', 'RCP', 'AOGCM')]) %>% as.data.frame()
}

# d50km ---
DIS <- c("d0km", "d10km", "d50km")
d <- 1
devl <- list()
for(d in 1:3){
  print(d)
  df3 <- df2 %>% filter(Dispersal==DIS[d])%>% dplyr::select(-Dispersal)
  
  # standardization
  # df2$delta_pd <- scale(df2$delta_pd)
  head(df3)
  nrow(df3)
  
  # sum(df2$delta_pd==0)/length(df2$delta_pd)
  # hist(df2$delta_pd)
  # boxplot((df2$delta_pd))
  
  form <- as.formula(delta_pd ~ Algorithm + Algorithm:AOGCM + Algorithm:AOGCM:RCP)
  form_gam <- as.formula(delta_pd ~ Algorithm + Algorithm:AOGCM + Algorithm:AOGCM:RCP +s(x,y))
  
  model1g <- glm(form, df3, family = 'gaussian')
  dev <- anova(model1g)$Deviance[-1]
  devl[[d]] <- dev/sum(dev)
  rm(model1g)
}


dev <- do.call(rbind, devl)
colnames(dev) <- c('ALG','GCM', 'RPC')
rownames(dev) <- c('0', '10', '50')
dev <- data.frame(dev) %>% mutate(disp=rownames(.)) %>% melt
dev$value <- dev$value*100

setwd("C:/Users/santi/OneDrive/Documentos/FORESTAL/1-Trabajos/47-PalmsNeotropics/2-DataAnalysisPalms_2019/VariancePartitioning")
require(readr)
# write_tsv(dev, 'Alg_Gcm_Rcp.txt')
require(readr)
dev1 <- read_tsv('Alg_Gcm_Rcp.txt')
dev2 <- read_tsv('Alg_Gcm_Rcp_Dis.txt') %>% mutate(variable=factor(variable, levels=c("ALG", "GCM","RPC","DIS")))


P0 <- ggplot(dev1, aes(as.factor(disp), value)) + geom_col(aes(fill=variable), position = 'dodge')+scale_fill_viridis_d()+labs(x="Dispersal scenarios (km/d)", y="Explained deviance (%)")+theme_classic()+theme(legend.title = element_blank(), legend.position = c(0.9,.8), legend.background = element_rect(colour = NA, fill = NA))+theme(axis.text = element_text(colour = 'black'))
P0

P1 <- ggplot(dev2, aes(variable, value)) + geom_col(fill=viridis::cividis(2)[1], position = 'dodge')+scale_fill_viridis_d()+labs(x="", y="Explained deviance (%)")+theme_classic()+theme(legend.title = element_blank(), legend.position = 'none')+theme(axis.text = element_text(colour = 'black'))
P1

require(gridExtra)
require(grid)
myplot1 <-arrangeGrob(P0, top = textGrob("a)", x = unit(0, "npc"), y = unit(1, "npc"),just = c("left", "top")))
myplot2 <-arrangeGrob(P1, top = textGrob("b)", x = unit(0, "npc"), y = unit(1, "npc"),just = c("left", "top")))

grid.arrange(myplot1, myplot2, ncol=2,  widths=c(2.5/4, 1.5/4))

ggsave(plot = grid.arrange(myplot1, myplot2, ncol=2,  widths=c(2.5/4, 1.5/4)), 'VarianceExplained.png', width = 9, height = 7, scale = 1.8, units = 'cm', dpi=300)

# Another approach for gam or mixed models
# model1g <- gam(form_gam, df2, method = 'REML', family = 'gaussian')
# model2g <- update(model1g, . ~ . -Algorithm:AOGCM:RCP)
# model3g <- update(model2g, . ~ . -Algorithm:AOGCM)
# model4g <- update(model3g, . ~ . -Algorithm)
# barplot(c(deviance(model1g),
#           deviance(model2g),
#           deviance(model3g),
#           deviance(model4g)))
#
# dev <-anova(model1g, model2g, model3g, model4g)
# dev <- dev$Deviance[-1]
# (dev/sum(dev))*100


