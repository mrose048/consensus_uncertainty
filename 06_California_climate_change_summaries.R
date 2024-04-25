##%######################################################%##
#                                                          #
####        California climate change summaries         ####
#                                                          #
##%######################################################%##

setwd('G:/My Drive/')

cfp <- terra::vect('G:/My Drive/Franklin_grant/project/1-NSF_spatial_and_species_traits/1_Inputs/4_Shapefiles/JepsonRegions/JepsonRegions_CFP.shp')

# Environmental variables - Current conditions
env <- file.path('Franklin_grant/project/1-NSF_spatial_and_species_traits/1_Inputs/2_Predictors/1_Current') %>%
  list.files(., full.names = T, pattern = '.tif$') %>%
  terra::rast()
env_names <- names(env)


env_fut <- file.path("Franklin_grant/project/1-NSF_spatial_and_species_traits/1_Inputs/2_Predictors/2_Projection/") %>% list.dirs(recursive = F)
names(env_fut) <- env_fut
factor <- 'terrain'
env_fut <- as.list(env_fut)

for(i in 1:length(env_fut)) {
  env_fut[[i]] <- env_fut[[i]] %>%
    list.files(., pattern = ".tif$", full.names = TRUE) %>%
    terra::rast()
}
names(env_fut) <- basename(names(env_fut))


dif_maps <- list()

for (i in 1:length(env_fut)) {
  dif_maps[[i]] <- env_fut[[i]] - env
  
}

mean_diff <- lapply(dif_maps, terra::global, 'mean', na.rm = T)
mean_diff_df <- bind_cols(mean_diff)
colnames(mean_diff_df) <- names(env_fut)
mean_diff_df <- tibble::rownames_to_column(mean_diff_df, "variable")
df_t <- as_tibble(t(mean_diff_df[,-1])) 
names(df_t) = mean_diff_df[, 1]
df_t <- df_t %>%
  mutate(scenario = names(env_fut),
         total_ppt = ppt_djf + ppt_jja) %>%
  dplyr::select(aet, cwd, ppt_djf, ppt_jja, tmn,total_ppt, scenario)


current_df <- terra::global(env, 'mean', na.rm = T)



# convert future difference maps into data frames
dfs <- lapply(dif_maps, as.data.frame, xy = TRUE, na.rm = TRUE)
names(dfs) <- basename(names(env_fut))
big_df <- bind_rows(dfs, .id = 'model')
# add ID column
big_df$id <- 1:nrow(big_df)
big_df$id <- as.character(big_df$id)


# systematic sampling to reduce number of data
big_df <- big_df[seq(1, nrow(big_df), by=1000),] # reduce value for by argument at moment to perform final analysis
dim(big_df)

library(reshape)
library(reshape2)

# renaming columns/rearranging for easier plotting
{
  big_df <- melt(big_df, id.vars = c('id', 'x', 'y', 'model'))
  names(big_df)[5:6] <- c('scenario', 'fire_change')
  head(big_df)
  length(unique(big_df$scenario)) # ok!
  
  big_df$scenario <- as.character(big_df$scenario)
  big_df$gcm <-
    ifelse(grepl('cnrm', big_df$scenario), 'cnrm','hades')
  big_df$rcp <-
    ifelse(grepl('rcp45', big_df$scenario), 'rcp45','rcp85')
  big_df$year <-
    ifelse(grepl('2040-2069', big_df$scenario), '2055','2085')
}

# Add ecoregion to analyze uncertainty spatially
df_sp <- vect(big_df, geom = c('x','y'))
eco_v <- terra::extract(cfp, df_sp)

big_df <- cbind(big_df, eco_v %>% dplyr::select(-id.y))
filt <- !is.na(big_df$Region)
big_df <- big_df[filt,]
big_df <- big_df %>% dplyr::select(-scenario)

future_summary <-
  big_df %>% filter(gcm == 'hades' &
                      rcp == 'rcp85' &
                      year == '2085') %>% ggplot(aes(x = model, y = fire_change, fill = model)) +
  geom_violin() + geom_boxplot(width =
                                 0.1, fill = 'white') +
  scale_fill_manual(
    values = pals::viridis(3),
    limits = c('wo_eco', 'w_eco', 'eco_spec'),
    labels = c(
      'Model without ecoregion',
      'Model with ecoregion',
      'Ecoregion-specific models'
    ),
    name = NULL
  ) + theme_pub(base_size = 20, legend_size = 25) +
  labs(y = 'Change in pixel-level fire probability', x = NULL) +
  geom_hline(yintercept = 0) +
  facet_wrap(~
               Region) + theme(axis.text.x = element_blank(), legend.position = c(.7, .2))


ggsave(future_summary,
       filename = paste0("2_FireModels_fire_ms/3-FireModel_figures/boxplots_fire_prob_change_hades85_2070_2099.jpeg"),
       width = 14,
       height = 12,
       dpi = 300)