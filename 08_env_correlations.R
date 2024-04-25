##%######################################################%##
#                                                          #
####         Correlation structure of BCM data          ####
#                                                          #
##%######################################################%##

setwd("G:/My Drive/Franklin_grant/project/1-NSF_spatial_and_species_traits/")

{
  require(dplyr)
  require(terra)
  require(flexsdm)
  require(here)
  require(progress)
  require(raster)
  require(ggplot2)
  require(kernlab)
  require(foreach)
  require(ggcorrplot)
}



# Environmental variables - Current conditions
env <- file.path('1_Inputs/2_Predictors/1_Current') %>%
  list.files(., full.names = T, pattern = '.tif$') %>%
  terra::rast()
env <- homogenize_na(env)
env_names <- names(env)

env_df <- as.data.frame(env,
                        na.rm = T) %>%
  dplyr::select(-terrain)
corr <- round(cor(env_df), 1)
p.mat <- cor_pmat(env_df)


curr_p <- ggcorrplot(
  corr,
  lab = T,
  hc.order = TRUE,
  type = "lower",
  outline.col = "white",
  ggtheme = ggplot2::theme_gray,
  colors = c("#6D9EC1", "white", "#E46726")
)


# future
env_fut <-
  file.path("1_Inputs/2_Predictors/2_Projection/") %>% list.dirs(recursive = F)
names(env_fut) <- env_fut
factor <- 'terrain'
env_fut <- as.list(env_fut)

for (i in 1:length(env_fut)) {
  env_fut[[i]] <- env_fut[[i]] %>%
    list.files(., pattern = ".tif$", full.names = TRUE) %>%
    terra::rast()
}
names(env_fut) <- basename(names(env_fut))


fut_p <- list()

for (i in 1:length(env_fut)) {
  
  print(i)
  fut_df <- as.data.frame(env_fut[[i]],
                          na.rm = T) %>%
    dplyr::select(-terrain)
  corr <- round(cor(fut_df), 1)
  p.mat <- cor_pmat(fut_df)
  
  fut_p[[i]] <- ggcorrplot(
    corr,
    lab = T,
    hc.order = TRUE,
    type = "lower",
    outline.col = "white",
    ggtheme = ggplot2::theme_gray,
    colors = c("#6D9EC1", "white", "#E46726")
  )
  
}


require(patchwork)
plot <- (curr_p|fut_p[[1]]|fut_p[[2]])/(fut_p[[3]]|fut_p[[4]]|fut_p[[5]])/(fut_p[[6]]|fut_p[[7]]|fut_p[[8]]) +
  plot_layout(guides = "collect") + plot_annotation(tag_levels = 'a')

ggsave(plot,
       filename = "H:/My Drive/10_Uncertainty_manuscript/D&D_submission2/figures/correlation_plots.png",
       dpi = 500,
       height = 13,
       width = 13)

