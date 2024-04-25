##%######################################################%##
#                                                          #
####          Figures for uncertainty chapter           ####
#                                                          #
##%######################################################%##

setwd('G:/My Drive/')

# function for publication ready figures
theme_pub <- function (base_size = 12,
                       base_family = "",
                       legend_size = 12) {
  theme_grey(base_size = base_size,
             base_family = base_family) %+replace%
    
    theme(
      
      # Set text size
      plot.title = element_text(size = base_size, margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)),
      axis.title.x = element_text(size = base_size + 2, margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0)),
      axis.title.y = element_text(size = base_size + 2, margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), angle = 90),
      
      axis.text.x = element_text(size = base_size),
      axis.text.y = element_text(size = base_size),
      
      strip.text.x = element_text(size = base_size),
      strip.text.y = element_text(size = base_size),
      
      # Legend text
      legend.title = element_text(size = legend_size),
      legend.text = element_text(size = legend_size),
      
      # Configure lines and axes
      axis.ticks.x = element_line(colour = "black"),
      axis.ticks.y = element_line(colour = "black"),
      
      # Plot background
      panel.background = element_rect(fill = "white"),
      #panel.grid.major = element_line(colour = "grey83",
      #                                size = 0.2),
      # panel.grid.minor = element_line(colour = "grey88",
      #                                 size = 0.5),
      
      # Facet labels
      legend.key = element_rect(colour = "grey80"),
      strip.background = element_rect(
        fill = "grey80",
        colour = "grey50",
        size = 0.2
      )
    )
}

# publication quality theme
theme_map <- function (base_size = 12,
                       base_family = "") {
  theme_grey(base_size = base_size,
             base_family = base_family) %+replace%
    
    theme(
      
      # Set text size
      plot.title = element_text(size = 18, margin = margin(t = 0, r = 20, b = 0, l = 0)),
      axis.title.x = element_text(size = 20, margin = margin(t = 20, r = 0, b = 0, l = 0)),
      axis.title.y = element_text(size = 20,
                                  angle = 90, margin = margin(t = 0, r = 20, b = 0, l = 0)),
      
      #axis.text.x = element_text(size = 17),
      #axis.text.y = element_text(size = 17),
      
      strip.text.x = element_text(size = 18),
      strip.text.y = element_text(size = 18,
                                  angle = -90),
      
      # Legend text
      legend.title = element_text(size = base_size),
      legend.text = element_text(size = base_size),
      
      # Configure lines and axes
      # axis.ticks.x = element_line(colour = "black"),
      # axis.ticks.y = element_line(colour = "black"),
      
      # Plot background
      #  panel.background = element_rect(fill = "white"),
      #panel.grid.major = element_line(colour = "grey83",
      #                                size = 0.2),
      # panel.grid.minor = element_line(colour = "grey88",
      #                                 size = 0.5),
      
      # Facet labels
      legend.key = element_rect(colour = "grey80"),
      # strip.background = element_rect(
      #   fill = "grey80",
      #   colour = "grey50",
      #   size = 0.2
      # )
    )
}



{
  require(dplyr)
  require(raster)
  require(reshape2)
  require(stringr)
  require(ggplot2)
  require(RColorBrewer)
  require(nlme)
  require(gstat)
  require(MuMIn)
  require(parallel)
  require(doParallel)
  require(foreach)
  require(tidyr)
  require(patchwork)
  require(terra)
}


# Patterns explaining standard deviation in habitat suitability change

sp_traits <-
  data.table::fread(
    'G:/My Drive/Franklin_grant/project/1-NSF_spatial_and_species_traits/2_Outputs/3_Exposure/sp_traits_data.csv'
  ) %>% as_tibble() %>% dplyr::select(-model)


biog_traits <- data.table::fread(
  'G:/My Drive/Franklin_grant/project/1-NSF_spatial_and_species_traits/2_Outputs/3_Exposure/occ_based_biogeographic_traits_data.csv'
) %>% as_tibble() 


# range wide habitat suitability change and each future projection factor + sd across all projections for each species
df <- readr::read_csv('Dissertation/003_Ensemble_modeling/data/exposure_sd.csv')


# Habitat suitability change by ensemble method
ens_plot <-
  ggplot(data = df, aes(
    x = model,
    y = log(hs_change + 1),
    fill = model
  )) +
  geom_violin() +  geom_boxplot(width =
                                  0.1, fill = 'white') +
  labs(y = 'Habiat suitability change - log', x = NULL) +
  facet_grid(vars(year), vars(dispersal), labeller = as_labeller(big.label)) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = pals::viridis(5),
                    guide = 'none') +
  theme_pub(base_size = 15, base_family = 'serif') 

ggsave(
  plot = ens_plot,
  'G:/My Drive/Dissertation/003_Ensemble_modeling/figures/models_range_change_boxplot.png',
  dpi = 500,
  height = 5,
  width = 10
)


# Habitat suitability change by each factor
hs_rcp <-  ggplot(df,
                   aes(
                     y = log10(hs_change),
                     x = rcp,
                     fill = rcp
                   )) +
  geom_violin(width = .7) + geom_boxplot(alpha = 0.5, width = .7) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(x = 'Emissions scenario', y = 'Habitat suitability change - log10') +
  scale_fill_manual(
    limits = c('rcp45', 'rcp85'),
    values = c("#FDE725", "#3D4988"),
  #  labels = c('RCP 4.5', 'RCP 8.5'),
    name = NULL,
  guide = F
  ) +
  scale_x_discrete(labels = c('RCP 4.5', 'RCP 8.5')) +
  theme_pub(base_size = 40,
            base_family = 'serif',
            legend_size = 40) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.title.y = element_text(angle = 90))  +
  theme(legend.key.height = unit(3, 'cm'),
        legend.key.width = unit(3.5, 'cm'))

hs_model <-  ggplot(df,
                  aes(
                    y = log10(hs_change),
                    x = model,
                    fill = model
                  )) +
  geom_violin(width = .7) + geom_boxplot(alpha = 0.5, width = .7) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(x = 'Method', y = 'Habitat suitability change - log10') +
  theme_pub(base_size = 40,
            base_family = 'serif',
            legend_size = 40) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.title.y = element_text(angle = 90))  +
  theme(legend.key.height = unit(3, 'cm'),
        legend.key.width = unit(3.5, 'cm'))

ggsave(
  hs_rcp,
  filename = "/My Drive/Dissertation/003_Ensemble_modeling/figures/boxplot_rcp_hs_change.png",
  dpi = 500,
  height = 24,
  width = 35,
  unit = 'cm'
)

##%######################################################%##
#                                                          #
####                 Model performance                  ####
#                                                          #
##%######################################################%##

perf <-
  data.table::fread('Dissertation/003_Ensemble_modeling/data/model_performance.csv') %>%
  dplyr::filter(species %in% df$sp) %>% tibble()

sort(unique(perf$model))
perf$model <- factor(perf$model, levels = c(
  "gam",
  "gau",
  "gbm",
  "glm",
  "max",
  "net",
  "raf",
  "svm",
  "mean",
  'meansup',
  'meanthr',
  'meanw',
  'median'
)
)

perf2 <- perf %>%
  dplyr::group_by(species)  %>%
  mutate(rank = dense_rank(desc(AUC_mean)))

# performance by model type
mod_perf <- perf2 %>%
  group_by(model) %>%
  summarize(mean_AUC = mean(AUC_mean),
            mean_TSS = mean(TSS_mean),
            mean_BOYCE = mean(BOYCE_mean))

# average performance of individual algorithms
ind_avg <- perf2 %>%
  filter(model %in% c("gam",
                      "gau",
                      "gbm",
                      "glm",
                      "max",
                      "net",
                      "raf",
                      "svm")) %>%
  group_by() %>%
  summarize(mean_AUC = mean(AUC_mean),
            mean_TSS = mean(TSS_mean),
            mean_BOYCE = mean(BOYCE_mean))

# average performance of consensus methods
con_avg <- perf2 %>%
  filter(model %in% c("mean",
                      'meansup',
                      'meanthr',
                      'meanw',
                      'median')) %>%
  group_by() %>%
  summarize(
    mean_AUC = mean(AUC_mean),
    mean_TSS = mean(TSS_mean),
    mean_BOYCE = mean(BOYCE_mean)
  )


# histogram of each model's frequency of ranking
h0 <-
  perf2 %>% ggplot(aes(x = rank, fill = model)) + geom_histogram() + facet_wrap(~ model, ncol = 3) + labs(x = 'Model ranking (AUC)', y = 'Frequency') + scale_fill_viridis_d(guide = NULL)

# relationship between average ensemble performance and uncertainty
perf3 <-
  perf %>% filter(model %in% c('mean', 'meanw', 'median', 'meanthr', 'meansup')) %>%
  group_by(species) %>%
  summarize(
    AUC_mean2 = mean(AUC_mean),
    TSS_mean2 = mean(TSS_mean),
    Boyce_mean2 = mean(BOYCE_mean)) %>%
  left_join(exp_sd, by = c('species' = 'sp'))

# performance and prevlance
perf_prev <- ggplot(perf3, aes(x =prev, y = AUC_mean2)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm') +
  labs(y = 'Average AUC', x = 'Prevalence') +
  theme_pub(base_size = 15)

ggsave(
  perf_prev,
  filename = 'Dissertation/003_Ensemble_modeling/figures/prevalence_performance.png',
  dpi = 500,
  width = 15,
  height = 15,
  units = "cm"
)

# scatterplot of performance and uncertainty
perf_un <- ggplot(perf3, aes(x = AUC_mean2, y = log(sd_HSC))) +
  geom_point(aes(color = prev), size = 2) +
  facet_wrap(
    ~ dispersal,
    labeller =  as_labeller(c('fd_change_prop' = 'Full dispersal',
                              'nd_change_prop' = 'Null dispersal'))
  ) +
  scale_color_viridis_b(name = 'Prevalence',
                        end = .9) +
  labs(x = 'Average AUC', y = 'Standard Deviation in HSC (log)') + theme_pub(base_size = 15)

ggsave(
  perf_un,
  filename = 'Dissertation/003_Ensemble_modeling/figures/uncertainty_performance.png',
  dpi = 500,
  width = 21,
  height = 15,
  units = "cm"
)

a <- 
  perf %>% ggplot(aes(model, AUC_mean, fill =
                        model)) +  scale_fill_viridis_d(alpha = .5, guide = 'none') +
  geom_violin() + geom_boxplot(alpha = 0.5, width = 0.5) + #geom_jitter(width = 0.1) +
  theme(legend.position = "none") + labs(x = element_blank(), y = "AUC") +
  # geom_hline(yintercept = .7,
  #            col = 'black') +
  geom_vline(xintercept = 6.5,
             col = "black",
             lwd = 1.5) +
  #geom_hline(aes(yintercept = mean(AUC_mean))) +
  theme_pub(base_size = 30,
            base_family = '',
            legend_size = 25)

b <-
  perf %>% ggplot(aes(model, TSS_mean, fill =
                                                              model)) +  scale_fill_viridis_d(alpha = .5, guide = 'none') +
  geom_violin() + geom_boxplot(alpha = 0.5, width = 0.5) + #geom_jitter(width = 0.1) +
  theme(legend.position = "none") + labs(x = element_blank(), y = "TSS") +
  geom_vline(xintercept = 6.5, col = "black", lwd = 1.5) +
  theme_pub(base_size = 30,
            base_family = '',
            legend_size = 25)


c <-
  perf %>% ggplot(aes(model, BOYCE_mean, fill =
                        model)) + 
  scale_fill_viridis_d(alpha = .5, guide = 'none') +
  geom_violin() + geom_boxplot(alpha = 0.5, width = 0.5) + 
  #geom_jitter(width = 0.1) +
  theme(legend.position = "none") + labs(x = element_blank(), y = "Boyce index") +
  geom_vline(xintercept = 6.5,
             col = "black",
             lwd = 1.5) +
  theme_pub(base_size = 30,
            base_family = '',
            legend_size = 25)

d <-
  perf %>% ggplot(aes(model, FPB_mean, fill =
                        model)) +  scale_fill_viridis_d(alpha = .5, guide = 'none') +
  geom_violin() + geom_boxplot(alpha = 0.5, width = 0.5) + 
  #geom_jitter(width = 0.1) +
  theme(legend.position = "none") + labs(x = element_blank(), y = "F-measure on \n presence-background") +
  geom_vline(xintercept = 6.5,
             col = "black",
             lwd = 1.5) +
  theme_pub(base_size = 30,
            base_family = '',
            legend_size = 25) +
  theme()

e <-
  perf %>% ggplot(aes(model, SORENSEN_mean, fill =
                                                              model)) +  scale_fill_viridis_d(alpha = .5, guide = 'none') +
  geom_violin() + geom_boxplot(alpha = 0.5, width = 0.5) + 
  #geom_jitter(width = 0.1) +
  theme(legend.position = "none") + labs(x = element_blank(), y = "Sorensen index") +
  geom_vline(xintercept = 6.5, col = "black", lwd = 1.5) +
  theme_pub(base_size = 30,
            base_family = 'serif',
            legend_size = 25)


ggsave(
  plot = a/b/c + plot_layout(guides = 'collect') + plot_annotation(tag_levels = c('a')),
  filename = "G:/My Drive/Dissertation/003_Ensemble_modeling/figures/SDM_performance_plot.png",
  width = 60,
  height = 60,
  dpi = 500,
  units = "cm"
)

##%######################################################%##
#                                                          #
####               Variance partitioning                ####
#                                                          #
##%######################################################%##

df2 <- left_join(df, sp_traits, by = c('sp'='species'))

var_df <- df2 %>% dplyr::select(-sd_hsc) %>% unique() %>%
  dplyr::mutate(hs_change_log = log(hs_change +1))

# define parameter names

year.names = c('2055', '2085')
mod.names = c('mean', 'meansup', 'meanthr', 'meanw', 'median')
rcp.names = c('rcp45', 'rcp85')
gcm.names = c('cnrm', 'hades')
disp.names = c('fd_change_prop', 'nd_change_prop')
sp = unique(var_df$sp)

params = expand.grid(year = year.names, 
                    disp = disp.names,
                    sp = sp)



var1 = 'model'
var2 = 'model:scen'
var3 = 'model:scen:rcp'
var4 = 'model:scen:rcp:dispersal'

var.expl = 'hs_change'
#var.expl = 'area_km2'

res = foreach(year = params$year,
              dispersal = params$disp,
              sp = params$sp,
              .combine = 'rbind') %do%
  {
    cat('', year)
    
    # Get data corresponding to the dispersal and year
    
    tmp <- var_df[which(var_df$year == year &
                          var_df$dispersal == dispersal &
                          var_df$sp == sp), ]
    
    # compute glm
    
    mod = glm(
      paste0(var.expl, '~', var1, '+', var2, '+', var3),
      data = tmp,
      family = gaussian()
    )
    
    # get deviance values from anova
    if(mod$df.residual == 0) mod$df.residual = 1
    anov = anova(mod)
    devi = 100 * (anov$Deviance[-1]/anov$`Resid. Dev`[1])
    
    unexpl = 100 * (anov$`Resid. Dev`[1]-sum(anov$Deviance[-1]))/anov$`Resid. Dev`[1]
    devi = c(devi, unexpl)
    
    res = data.frame(
      year = year,
      dispersal = dispersal,
      sp = sp,
      RCP = 'all',
      model = devi[1],
      gcm = devi[2],
      rcp = devi[3]
     # dispersal = devi[4]
    )
  }

# pivot longer so that all deviance is in 1 column
res <- res %>% 
  pivot_longer(cols = model:rcp,
               names_to = 'factor',
               values_to = 'dev')


# Labelling for plotting
big.label <- c('fd_change_prop' = 'Full dispersal',
               'nd_change_prop' = 'No dispersal',
               '2055' = '2055',
               '2085' = '2085')

sort(unique(res$factor))
res$factor <- factor(res$factor, levels = c('model',
                                            'gcm',
                                            'rcp'))



dev_plot <-
  ggplot(data = res, aes(x = factor, y = dev, fill = factor)) +
  geom_violin() +  geom_boxplot(width =
                                  0.1, fill = 'white') +
  stat_summary(fun="mean",color="black", shape=3, size = .2) +
  labs(y = 'Explained deviance %', x = NULL) +
  facet_grid(vars(year), vars(dispersal), labeller = as_labeller(big.label)) +
  # facet_wrap(vars(year)) +
  scale_fill_manual(values = c("#30678D", "#6DCC57", "#FDE725"),
                    guide = 'none') +
  theme_pub(base_size = 15, base_family = '') +
  scale_x_discrete(labels = c('Consensus', 'GCM', 'RCP'))

ggsave(
  plot = dev_plot,
  'G:/My Drive/Dissertation/003_Ensemble_modeling/figures/deviance_boxplot.png',
  dpi = 500,
  height = 5,
  width = 8
)


# stacked barplot for a few species
sp_plot <-
  ggplot(data = res %>% dplyr::filter(
    sp %in% c(
      'Abies magnifica',
      'Pinus jeffreyi',
      'Quercus engelmannii',
      'Quercus douglasii',
      'Lupinus arboreus',
      'Ceanothus greggii'
    )
  ),
  aes(x = sp, y = dev/4, fill = factor)) +
  geom_bar(position="stack", stat="identity") +
  labs(y = 'Explained deviance %', x = NULL) +
  scale_fill_manual(values = c("#30678D", "#6DCC57", "#FDE725")) +
  theme_pub(base_size = 15, base_family = '') 

ggsave(
  plot = sp_plot,
  'G:/My Drive/Dissertation/003_Ensemble_modeling/figures/deviance_stack_plot_hsc_by_species.png',
  dpi = 500,
  height = 5,
  width = 15
)



summ_stats <- res %>% dplyr::group_by(year, dispersal, factor) %>%
  summarize(mean = mean(dev),
            median = median(dev),
            sd = sd(dev))


res2 <- left_join(res, sp_traits, by = c('sp' = 'species'))

ggplot(res2 %>% dplyr::filter(factor == 'model'),
       aes(y = dev, x = range_size)) +  +
  facet_grid(vars(year), vars(dispersal), labeller = as_labeller(big.label)) 

# what is driving differences in % deviance
nb_plot <- ggplot(res2 %>% filter(factor == 'model'),
                  aes(x = niche_breadth, y = dev)) + geom_point() +
  geom_smooth(method = lm) +
  facet_grid(vars(year), vars(dispersal), labeller = as_labeller(big.label)) +
  theme_pub(base_size = 15, base_family = 'serif') +
  labs(y = 'Explained deviance % by ensemble method', x = 'Niche breadth')


ggsave(
  plot = nb_plot,
  'G:/My Drive/Dissertation/003_Ensemble_modeling/figures/mod_deviance_niche_breadth.png',
  dpi = 500,
  height = 8,
  width = 11
)


# Patterns explaining standard deviation in habitat suitability change

sp_traits <-
  data.table::fread(
    'G:/My Drive/Franklin_grant/project/1-NSF_spatial_and_species_traits/2_Outputs/3_Exposure/sp_traits_data.csv'
  ) %>% as_tibble() %>% dplyr::select(-model)


biog_traits <- data.table::fread(
  'G:/My Drive/Franklin_grant/project/1-NSF_spatial_and_species_traits/2_Outputs/3_Exposure/occ_based_biogeographic_traits_data.csv'
) %>% as_tibble() 

full_df <- left_join(df, sp_traits, by = c('sp'= 'species')) %>%
  group_by(sp) %>%
  mutate(mean_hsc = mean(hs_change),
         min_hsc = min(hs_change),
         max_hsc = max(hs_change)) %>%
  dplyr::select(-model, -rcp, -scen, -dispersal, -year, -hs_change) %>%
  left_join(biog_traits %>% dplyr::select(species, range_topo, range_elev, range_d_coast), by = c('sp' = 'species')) %>%
  unique()

a <- ggplot(full_df, aes(x = area100, y = log10(sd))) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = 'Extent', y = 'Standard Deviation in HSC (log10)')


b <- ggplot(full_df, aes(x = niche_breadth, y = log10(sd))) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = 'Niche breadth', y = 'Standard Deviation in HSC (log10)')


c <- ggplot(full_df, aes(x = log10(mean_hsc), y = log10(sd))) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = 'Mean habitat suitability change (log10)', y = 'Standard Deviation in HSC (log10)')


d <- ggplot(full_df, aes(x = mean_elev, y = log10(sd))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = 'Elevation', y = 'Standard Deviation in HSC (log10)')

e <- ggplot(full_df, aes(x = pr, y = log10(sd))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = '# of presences', y = 'Standard Deviation in HSC (log10)')

f <- ggplot(full_df, aes(x = mean_topo, y = log10(sd))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = 'Topographic het.', y = 'Standard Deviation in HSC (log10)')

f2 <- ggplot(full_df, aes(x = mean_d_coast, y = log10(sd))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = 'Distance to coast', y = 'Standard Deviation in HSC (log10)')


f <- ggplot(full_df, aes(x = rabin_class, y = log10(sd))) +
  geom_boxplot() +
  labs(x = 'Rarity class', y = 'Standard Deviation in HSC (log10)')

g <- ggplot(full_df, aes(x = log10(max_hsc), y = log10(sd))) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = 'Max habitat suitability change (log10)', y = 'Standard Deviation in HSC (log10)')


f <- ggplot(full_df, aes(x = range_size, y = log10(sd))) +
  geom_boxplot() +
  labs(x = 'Range size class', y = 'Standard Deviation in HSC (log10)')

g <- ggplot(full_df, aes(x = range_topo, y = log10(sd))) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = 'Topo het. range', y = 'Standard Deviation in HSC (log10)')

h <- ggplot(full_df, aes(x = range_elev, y = log10(sd))) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = 'Elevation range', y = 'Standard Deviation in HSC (log10)')


k <- ggplot(full_df, aes(x = tmn_range, y = log10(sd))) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = 'Tmn range', y = 'Standard Deviation in HSC (log10)')

# Linear model
lm1 <- lm(log10(sd) ~ area100, data = full_df)
lm2 <- lm(log10(sd) ~ pr, data = full_df)
lm3 <- lm(log10(sd) ~ niche_breadth, data = full_df)

lm4 <- lm(log10(sd) ~ area100*niche_breadth, data = full_df)

lm5 <- lm(log10(sd) ~ poly(mean_d_coast, 2), data = full_df)
lm6 <- lm(log10(sd) ~ poly(mean_elev, 2), data = full_df)
lm7 <- lm(log10(sd) ~ poly(mean_topo, 2), data = full_df)


# range size and all HSC values
full_df2 <- left_join(df, sp_traits, by = c('sp'= 'species')) %>%
  left_join(biog_traits %>% dplyr::select(species, range_topo, range_elev, range_d_coast), by = c('sp' = 'species'))

f <-
  ggplot(
    full_df2 %>% dplyr::filter(dispersal == 'nd_change_prop'),
    aes(x = model, y = hs_change, fill = model)
  ) +
  geom_boxplot() +
  scale_fill_manual(values = pals::viridis(5)) +
  labs(x = 'Ensemble method', y = 'Habitat suitability change')




##%######################################################%##
#                                                          #
####    Variance partitioning for change in species     ####
#                                                          #
##%######################################################%##

# California floristic province
cfp <-
  terra::vect("G:/My Drive/Franklin_grant/project/1-NSF_spatial_and_species_traits/1_Inputs/4_Shapefiles/JepsonRegions/JepsonRegions_CFP.shp")

cfp2 <- terra::vect("G:/My Drive/07_Endemics_project/Burge_etal_2016/CFP_shapefile/CFP_GIS.shp") %>%
  project(cfp)

# continous spatial data with species turnover for each GCM, RCP, time period, and ensemble method
big_df <- data.table::fread('G:/My Drive/Dissertation/003_Ensemble_modeling/data/sp_loss_prop_df_v2.gz') %>% as_tibble()

# add ID column
big_df$id <- 1:nrow(big_df)
big_df$id <- as.character(big_df$id)
big_df <- na.omit(big_df)


# systematic sampling to reduce number of data
big_df2 <- big_df[seq(1, nrow(big_df), by=3000),] # reduce value for by argument at moment to perform final analysis
dim(big_df2)


{
  big_df2 <- melt(big_df2, id.vars = c('id', 'x', 'y'))
  names(big_df2)[4:5] <- c('scenario', 'sp_change')
  head(big_df2)
  length(unique(big_df2$scenario)) # ok!
  
  big_df2$scenario <- as.character(big_df2$scenario)
  big_df2$gcm <-
    str_split_fixed(big_df2$scenario, '_',2)[,1]
  big_df2$dispersal <-
    ifelse(grepl('nd', big_dff2$scenario), 'null','full')
  big_df2$rcp <-
    ifelse(grepl('rcp45', big_df2$scenario), 'rcp45','rcp85')
  big_df2$model <-
    str_split_fixed(str_split_fixed(str_split_fixed(
      str_split_fixed(str_split_fixed(big_df2$scenario, '_', 2)[, 2], '_', 2)[, 2], '_', 2
    )[, 2], '_', 2)[, 2], '_', 2)[, 1]
  big_df2$year <-
    ifelse(grepl('2040', big_df2$scenario), '2055','2085')
}

# Add ecoregion to analyze uncertainty spatially
df_sp <- vect(big_df2, geom = c('x','y'))
eco_v <- terra::extract(cfp, df_sp, xy = T)

big_df2 <- cbind(big_df2, eco_v %>% dplyr::select(-id.y))
filt <- !is.na(big_df2$Region)
big_df2 <- big_df2[filt,]



# histogram of sp change values by dispersal scenario

h1 <- ggplot(big_df2, aes(x = sp_change, fill = dispersal)) +
  geom_density(alpha = 0.4)


# boxplot of sp change by consensus method
b1 <- ggplot(big_df2, aes(x = model, y = log10(sp_change), fill = model)) +
  geom_violin() + geom_boxplot(width = .1, fill = 'white') +
  facet_grid(vars(year), vars(dispersal))

b2 <- ggplot(big_df2, aes(x = rcp, y = log10(sp_change), fill = rcp)) +
  geom_violin() + geom_boxplot(width = .1, fill = 'white') +
  facet_grid(vars(year), vars(dispersal))

b3 <- ggplot(big_df2, aes(x = gcm, y = log10(sp_change), fill = gcm)) +
  geom_violin() + geom_boxplot(width = .1, fill = 'white') +
  facet_grid(vars(year), vars(dispersal))



# define parameter names
year.names = c('2055', '2085')
mod.names = c('mean', 'meansup', 'meanthr', 'meanw', 'median')
rcp.names = c('rcp45', 'rcp85')
gcm.names = c('cnrm', 'hades')
disp.names = c('full', 'null')
grid.names = unique(big_df2$id)
ecoregions <- unique(big_df2$Region)

params = expand.grid(year = year.names, 
                     disp = disp.names,
                     grid = grid.names)


var1 = 'model'
var2 = 'model:gcm'
var3 = 'model:gcm:rcp'
var4 = 'model:gcm:rcp'

var.expl = 'sp_change'



res = foreach(year = params$year,
              dispersal = params$disp,
              grid = params$grid,
              .combine = 'rbind') %do%
  {
    cat('', grid)
    
    # Get data corresponding to the dispersal and year
    
    tmp <- big_df2[which(big_df2$year == year &
                           big_df2$dispersal == dispersal &
                           big_df2$id == grid),]
    
    # compute glm
    
    mod = glm(
      paste0(var.expl, '~', var1, '+', var2, '+', var3),
      data = tmp,
      family = 'gaussian'
    )
    
    # get deviance values from anova
    if (mod$df.residual == 0)
      mod$df.residual = 1
    anov = anova(mod)
    devi = 100 * (anov$Deviance[-1] / anov$`Resid. Dev`[1])
    
    unexpl = 100 * (anov$`Resid. Dev`[1] - sum(anov$Deviance[-1])) / anov$`Resid. Dev`[1]
    devi = c(devi, unexpl)
    
    res = data.frame(
      year = year,
      dispersal = dispersal,
      grid = grid,
      eco = tmp$Region,
      model = devi[1],
      gcm = devi[2],
      rcp = devi[3]
    )
  }

# pivot longer so that all deviance is in 1 column
res <- res %>% 
  pivot_longer(cols = model:rcp,
               names_to = 'factor',
               values_to = 'dev') %>% unique()

#data.table::fwrite(res, 'G:/My Drive/Dissertation/003_Ensemble_modeling/data/pixel_level_var_part.csv')
res <- data.table::fread('G:/My Drive/Dissertation/003_Ensemble_modeling/data/pixel_level_var_part.csv')


# Labelling for plotting
big.label <- c('full' = 'Full dispersal',
               'null' = 'No dispersal',
               '2055' = '2055',
               '2085' = '2085')

sort(unique(res$factor))
res$factor <- factor(res$factor, levels = c('model',
                                            'gcm',
                                            'rcp'))



dev_plot <-
  ggplot(data = res, aes(x = factor, y = dev, fill = factor)) +
  geom_violin() + geom_boxplot(width =
                                 0.1, fill = 'white') +
  stat_summary(fun="mean",color="black", shape=3, size = .2) +
  labs(y = 'Explained deviance %', x = NULL) +
  facet_grid(vars(year), vars(dispersal), labeller = as_labeller(big.label)) +
  # facet_wrap(vars(year)) +
  scale_fill_manual(values = c("#30678D", "#6DCC57", "#FDE725"),
                    guide = 'none') +
  theme_pub(base_size = 15, base_family = '') +
  scale_x_discrete(labels = c('Consensus', 'GCM', 'RCP'))


ggsave(
  plot = dev_plot,
  'G:/My Drive/Dissertation/003_Ensemble_modeling/figures/sp_turnover_deviance_boxplot.png',
  dpi = 500,
  height = 5,
  width = 8
)


# plot by ecoregion

big.label <- c('full' = 'Full dispersal',
               'null' = 'Null dispersal',
               'NorthWestern CA' = 'Northwestern',
               'Cascade Ranges' = 'Cascade Ranges',
               'Sierra Nevada' = 'Sierra Nevada',
               'Great Valley' = 'Great Valley',
               'Central Western CA' = 'Central Western',
               'Southwestern CA' = 'Southwestern')

eco_dev_plot <-
  ggplot(data = res %>% filter(year == 2085), aes(x = factor, y = dev, fill = factor)) +
  geom_violin() + geom_boxplot(width =
                                 0.1, fill = 'white') +
  stat_summary(fun="mean",color="black", shape=3, size = .2) +
  labs(y = 'Explained deviance %', x = NULL) +
  facet_grid(vars(eco), vars(dispersal), labeller = as_labeller(big.label)) +
  # facet_wrap(vars(year)) +
  scale_fill_manual(values = c("#30678D", "#6DCC57", "#FDE725"),
                    guide = 'none') +
  theme_pub(base_size = 22, base_family = '') +
  scale_x_discrete(labels = c('Consensus', 'GCM', 'RCP')) +
  scale_y_continuous(breaks = seq(0, 100, 50))

ggsave(
  plot = eco_dev_plot,
  'G:/My Drive/Dissertation/003_Ensemble_modeling/figures/sp_turnover_deviance_boxplot_by_ecoregion.png',
  dpi = 500,
  height = 15,
  width = 12
)


summ_stats3 <- res %>% dplyr::group_by(year, dispersal, factor) %>%
  summarize(mean = mean(dev),
            median = median(dev),
            sd = sd(dev))


summ_stats4 <- res %>% dplyr::group_by(year, factor, dispersal, eco) %>%
  summarize(mean = mean(dev),
            median = median(dev),
            sd = sd(dev))


summ_stats2 <- res %>% dplyr::group_by(year, dispersal, factor) %>%
  summarize(mean = mean(dev),
            median = median(dev),
            sd = sd(dev))

data.table::fwrite(summ_stats2 %>% filter(year == '2085'), 'G:/My Drive/Dissertation/003_Ensemble_modeling/data/sp_turnover_summary_stats.csv')



# sp change by ensemble
p1 <-  ggplot(data = big_df2 %>% filter(sp_change < 0), aes(x = model, y = log(sp_change+1), fill = model)) +
  geom_violin() + geom_boxplot(width =
                                 0.1, fill = 'white') +
  scale_fill_manual(values = pals::viridis(5), guide = 'none') +
  labs(y = 'log(Species turnover)', x = NULL) +
  facet_wrap(~dispersal) +
  theme_pub(base_size = 15, base_family = 'serif') 

# sp change difference by dispersal
p2 <-  ggplot(data = big_df2, aes(x = dispersal, fill = dispersal, y = log(sp_change+1))) +
  geom_violin() + geom_boxplot(width =
                                 0.1, fill = 'white') +
  scale_fill_manual(values = pals::viridis(2), guide = 'none') +
  labs(y = 'Species turnover', x = NULL) +
  theme_pub(base_size = 15, base_family = 'serif')

# sp change difference by dispersal
p3 <-  ggplot(data = big_df2, aes(x = rcp, fill = rcp, y = log(sp_change+1))) +
  geom_violin() + geom_boxplot(width =
                                 0.1, fill = 'white') +
  scale_fill_manual(values = pals::viridis(2), guide = 'none') +
  labs(y = 'Species turnover', x = NULL) +
  theme_pub(base_size = 15, base_family = 'serif')
##%######################################################%##
#                                                          #
####              Figure of pixel turnover              ####
#                                                          #
##%######################################################%##

# raster plotting function
######################################################
################ FUNCTIONS ###########################
######################################################

#This plots a single raster in the format that we want it to. 
#When making comparisons we can set limitVals which will make the color palette standard across plots
ggplotMyRaster = function(myRast,
                          myVar = NA,
                          titleString = "",
                          putColorBar = T,
                          colorMidpointValue = 0,
                          caxisname = "val",
                          colorscale = NA,
                          cols = cols) {
  #If we are putting on limits clip the data so that it doesn't show as NA. This will not affect the mean or anything other than the image.
  
  limitVals = c(quantile(myRast, 0.01, na.rm = T),
                quantile(myRast, 0.99, na.rm = T))
  
  if (length(myVar) == 1) {
    myVar = myRast
    myVar[] = NA
  }
  
  
  #convert the raster to points for plotting
  map.p <- rasterToPoints(myRast)
  #Make the points a dataframe for ggplot
  df <- data.frame(map.p)
  #Make appropriate column headings
  colnames(df) <- c("Longitude", "Latitude", "Val")
  
  df <- tibble(df)
  
  # add a column with range values
  brks <- c(0, 10, 25, 50, 75, 100, 200, 628)
  
  df <- df %>%
    mutate(val_int = cut(
      Val,
      breaks = brks,
      include.lowest = T,
      dig.lab = 3
    ))
  
  #Now make the map
  g = ggplot() +
    coord_equal() +
    geom_spatvector(
      data = cfp,
      color = "black",
      fill = "gray90",
      size = 0.2
    ) +
    geom_raster(data = df %>% na.omit(), aes(y = Latitude, x = Longitude, fill = val_int)) +
    coord_equal() +
    geom_spatvector(
      data = cfp,
      color = "black",
      fill = "gray90",
      size = 0.2,
      alpha = 0
    ) +
    theme_map() + ggtitle(titleString)
  
  
  g = g + scale_fill_manual(
    values = rev(pals::viridis(8)),
    labels = c('0-10', '10-25', '25-50', '50-75', '75-100', '100-200', '>200'),
    name = 'SD',
    guide = guide_legend(reverse = TRUE)
  )  + annotation_scale(
    location = "bl",
    width_hint = 0.3,
    text_cex = 3,
    height = unit(.7, "in")
  ) +
    annotation_north_arrow(
      location = "tr",
      which_north = "true",
      pad_x = unit(0.4, "in"),
      pad_y = unit(0.8, "in"),
      style = north_arrow_fancy_orienteering(text_size = 25),
      height = unit(2, 'in'),
      width = unit(2, 'in')
    )
  
  g = g + theme(
    legend.position = c(.84, .6),
    legend.background = element_rect(
      fill = "white",
      size = 0.5,
      color = 'black'
    ),
    legend.title = element_text(size = 40),
    legend.text = element_text(size = 38),
    legend.box.margin = margin(1, 1, 1, 1),
    plot.title = element_text(size = 50),
    axis.title = element_text(size = 45),
    axis.text = element_text(size = 40),
    plot.margin = unit(c(0, 0, 1, 0), "cm"),
    panel.grid = element_line(size = 0.1, colour = "black"),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1
    ),
    panel.background = element_rect(fill = 'white'),
    axis.title.x = element_blank(), axis.title.y = element_blank()
  ) 

    
  
  g
}

# Spatial patterns of plant species turnover projections

require(terra)

change_maps <- list.files('G:/My Drive/Dissertation/003_Ensemble_modeling/data/05_sp_change_maps/', full.names = T) %>% terra::rast()

change_maps <- change_maps *100

# Standard deviation in turnover values (%)  based on each factor by averaging projections resulting from all the modalities of the other factors

# All scenarios and projects

sd_map <- terra::stdev(change_maps)

# model averaging technique
m1 <-
  list.files(
    'G:/My Drive/Dissertation/003_Ensemble_modeling/data/05_sp_change_maps/',
    pattern = 'mean_',
    full.names = T
  ) %>% terra::rast() %>% terra::mean()
m2 <- list.files(
  'G:/My Drive/Dissertation/003_Ensemble_modeling/data/05_sp_change_maps/',
  pattern = 'median',
  full.names = T
) %>% terra::rast() %>% terra::mean()
m3 <- list.files(
  'G:/My Drive/Dissertation/003_Ensemble_modeling/data/05_sp_change_maps/',
  pattern = 'meanthr',
  full.names = T
) %>% terra::rast() %>% terra::mean()
m4 <- list.files(
  'G:/My Drive/Dissertation/003_Ensemble_modeling/data/05_sp_change_maps/',
  pattern = 'meanw',
  full.names = T
) %>% terra::rast() %>% terra::mean()
m5 <- list.files(
  'G:/My Drive/Dissertation/003_Ensemble_modeling/data/05_sp_change_maps/',
  pattern = 'meansup',
  full.names = T
) %>% terra::rast() %>% terra::mean()

model_sd <- stdev(c(m1*100, m2*100, m3*100, m4*100, m5*100))

# GCM
gcm1 <-
  list.files(
    'G:/My Drive/Dissertation/003_Ensemble_modeling/data/05_sp_change_maps/',
    pattern = 'hades',
    full.names = T
  ) %>% terra::rast() %>% terra::mean()
gcm2 <- list.files(
  'G:/My Drive/Dissertation/003_Ensemble_modeling/data/05_sp_change_maps/',
  pattern = 'cnrm',
  full.names = T
) %>% terra::rast() %>% terra::mean()

gcm_sd <- stdev(c(gcm1*100,gcm2*100))

# dispersal
dis1 <-
  list.files(
    'G:/My Drive/Dissertation/003_Ensemble_modeling/data/05_sp_change_maps/',
    pattern = 'fd',
    full.names = T
  ) %>% terra::rast() %>% terra::mean()
dis2 <- list.files(
  'G:/My Drive/Dissertation/003_Ensemble_modeling/data/05_sp_change_maps/',
  pattern = 'nd',
  full.names = T
) %>% terra::rast() %>% terra::mean()

dis_sd <- stdev(c(dis1*100,dis2*100))

# RCP 
rcp1 <-
  list.files(
    'G:/My Drive/Dissertation/003_Ensemble_modeling/data/05_sp_change_maps/',
    pattern = 'rcp45',
    full.names = T
  ) %>% terra::rast() %>% terra::mean()
rcp2 <- list.files(
  'G:/My Drive/Dissertation/003_Ensemble_modeling/data/05_sp_change_maps/',
  pattern = 'rcp85',
  full.names = T
) %>% terra::rast() %>% terra::mean()

rcp_sd <- stdev(c(rcp1*100,rcp2*100))

# year
year1 <-
  list.files(
    'G:/My Drive/Dissertation/003_Ensemble_modeling/data/05_sp_change_maps/',
    pattern = '2040',
    full.names = T
  ) %>% terra::rast() %>% terra::mean()
year2 <- list.files(
  'G:/My Drive/Dissertation/003_Ensemble_modeling/data/05_sp_change_maps/',
  pattern = '2070',
  full.names = T
) %>% terra::rast() %>% terra::mean()

year_sd <- stdev(c(year1*100,year2*100))

library(tidyterra)
library(scales)
library(ggspatial)

# MAPS FOR FIGURES
ggp1 = ggplotMyRaster(raster(sd_map), titleString = 'All Factors') 
ggp2 = ggplotMyRaster(raster(model_sd), titleString = 'Consensus Method') 
ggp3 = ggplotMyRaster(raster(rcp_sd), titleString = 'RCP')
ggp4 = ggplotMyRaster(raster(gcm_sd), titleString = 'GCM')
ggp5 = ggplotMyRaster(raster(year_sd), titleString = 'Time Period')
ggp6 = ggplotMyRaster(raster(dis_sd), titleString = 'Dispersal Assumption')

library(cowplot)

gg_uncertainty = plot_grid(
  ggp1,
  ggp2,
  ggp3,
  ggp4,
  ggp5 + theme(plot.margin = unit(c(0, 1.5, 0, 0), "cm")),
  ggp6 + theme(plot.margin = unit(c(0, 1.5, 0, 0), "cm")),
  nrow = 2,
  ncol = 3,
  rel_heights = c(1, 1, 1, 1, 1, 1),
  align = "vh",
  label_size = 50,
  labels = c("A", "B", "C", "D", "E", "F")
)

ggsave(
  gg_uncertainty,
  filename = paste0("G:/My Drive/Dissertation/003_Ensemble_modeling/figures/sd_uncertainty_maps.jpeg"),
  width = 36,
  height = 32
)


png(
  "G:/My Drive/Dissertation/003_Ensemble_modeling/figures/future_sp_rich_stand_dev.png",
  units = "in",
  width = 8,
  height = 6,
  res = 500
)

plot(
  sd_map,
  axes = F,
  col = rev(pals::viridis(10))
  # colNA = 'gray80'
)

plot(cfp, add = T)

dev.off()

# visualize where habitat loss will be

loss <- change_maps
loss[loss >= 0] <- NA


png(
  "G:/My Drive/Dissertation/003_Ensemble_modeling/figures/future_loss_prop1.png",
  units = "in",
  width = 8,
  height = 6,
  res = 500
)

plot(
  loss[[76]],
  main = '',
  axes = F,
  col = pals::viridis(10)
  # range = c(-.7,0)
  #colNA = 'gray80'
)

plot(cfp, add = T)

dev.off()


png(
  "G:/My Drive/Dissertation/003_Ensemble_modeling/figures/future_loss_prop2.png",
  units = "in",
  width = 8,
  height = 6,
  res = 500
)

plot(
  loss[[10]],
  main = '',
  axes = F,
  col = pals::viridis(10),
  range = c(-.9,0)
  #colNA = 'gray80'
)

plot(cfp, add = T)

dev.off()


############################################################
#                                                          #
#                         Figure 1                         #
#                                                          #
############################################################

# A map that shows the CFP + inset map and an example different ensemble predictions 
# for one species

library(terra)
library(sf)
library(ggsci)
library(ggnewscale)
library(ggspatial)
library(cowplot)

# California floristic province
cfp <-
  terra::vect("G:/My Drive/Franklin_grant/project/1-NSF_spatial_and_species_traits/1_Inputs/4_Shapefiles/JepsonRegions/JepsonRegions_CFP.shp")

# US shapefile
us_shp <- terra::vect('G:/My Drive/Franklin_grant/project/data/shapefiles/tl_2019_us_state/tl_2019_us_state.shp') %>%
  project(crs(cfp))


# Hillshade
hillmat <- data.table::fread("G:/My Drive/Franklin_grant/project/1-NSF_spatial_and_species_traits/1_Inputs/2_Predictors/3_AdditionalPredictors/hillmat_cfp.gz") %>% tibble()


# Figure 1: CFP map

cfp_bb = st_as_sfc(st_bbox(st_as_sf(cfp2)))
cali <- terra::vect('G:/My Drive/02_Vulcan_project/03_Data/CA_shapefile/cnty24k09_1_poly.shp') %>%
  aggregate(dissolve = T)
  

us_map = ggplot() +
  layer_spatial(
    terra::subset(
      us_shp,
      us_shp$NAME != "United States Virgin Islands" &
        us_shp$NAME !=  "Commonwealth of the Northern Mariana Islands" &
        us_shp$NAME != "Alaska" &
        us_shp$NAME != "Hawaii" &
        us_shp$NAME != "American Samoa" &
        us_shp$NAME != "Puerto Rico" &
        us_shp$NAME != "Guam"
    ),
    fill = "grey80"
  ) +
  geom_sf(
    data = cfp_bb,
    fill = NA,
    color = "black",
    size = 1
  ) +
  theme_void()

# ecoregion names corrected
#cfp$Region2 <- c('Central Western', 'Cascade Ranges', 'Great Valley', 'Northwestern', 'Sierra Nevada', 'Southwestern')

# CFP map
cfp_map <- ggplot() +
  # geom_raster(data = hillmat,
  #             aes(x = x, y = y, fill = layer),
  #             show.legend = FALSE) +
#  scale_fill_gradient(low = "black", high = "white") +
 # new_scale_fill() +
  layer_spatial(data = cali, size = 0.4, fill = 'gray50') +
  layer_spatial(data = cfp2,
                size = 0.4,
                aes(fill = JEP_REG)) +
  scale_fill_manual(
    values = c(
      'tomato',
      'darkgreen',
      'aquamarine3',
      'greenyellow',
      'mediumblue',
      'plum',
      'darkseagreen',
      'darkolivegreen',
      'deepskyblue4',
      'orchid',
      'mediumpurple1'
    ),
    name = 'CFP Ecoregion',
    labels = c(
      'Baja CFP',
      'Cascade Ranges',
      'Central Western CA',
      'Great Central Valley',
      'Nevada CFP',
      'Northern Channel Islands',
      'Northwestern CA',
      'Oregon CFP',
      'Sierra Nevada',
      'Southern Channel Islands',
      'Southwestern CA'
   
    )
  ) +
  coord_sf(expand = FALSE) +
  theme_map(base_size = 11, base_family = '') +
  labs(x = "Longitude", y = "Latitude") +
  #theme(legend.position = c(.82, .65)) +
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


gg_inset_map1 = ggdraw() +
  draw_plot(cfp_map) +
  draw_plot(
    us_map,
    x = 0.4,
    y = 0.79,
    width = 0.23,
    height = 0.23
  )

#gg_inset_map1


ggsave(gg_inset_map1,
       filename = "G:/My Drive/Dissertation/003_Ensemble_modeling/figures/Figure1_inset_map.png", 
       dpi = 500,
       height = 8,
       width = 8)

# Current habitat maps using different ensemble methods
c_maps <-  list.files(
  'G:/My Drive/Dissertation/003_Ensemble_modeling/data/00_current_habitat_maps/',
  pattern = 'Quercus engelmannii',
  full.names = T,
  recursive = T
)

r <- terra::rast(c_maps)
sw <- subset(cfp, cfp$Region == 'Southwestern CA')

# convert hillmat back to raster and crop to extent of southwestern California
hillmat_r <- rasterFromXYZ(hillmat)
hillmat_r <- terra::rast(hillmat_r)
hillmat_r <- hillmat_r %>%
  crop(sw) %>%
  mask(sw)

hillmat_r2 <- as.data.frame(hillmat_r, xy = T)

r2 <- r %>% crop(sw) %>% mask(sw)
r2[r2 == 0] <- NA

g <- list()

for(i in 1:nlyr(r2)) {
  print(i)
  g[[i]] <- ggplot() +
    geom_raster(data = hillmat_r2,
                aes(x = x, y = y, fill = layer),
                show.legend = FALSE) +
    scale_fill_gradient(low = "black", high = "white") +
    new_scale_fill() +
    layer_spatial(sw,
                  fill = 'grey80',
                  alpha = .2) +
    layer_spatial(raster(r2[[i]]),
                  alpha = .8) +
    scale_fill_gradientn(
      colours = rev(pals::viridis(20)),
      na.value = NA,
      name = "Habitat suitability",
      breaks = c(0, .2, .4, .6, .8, 1),
      limits = c(0, 1)
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      axis.title = element_blank(),
      axis.text = element_blank(),
      text = element_text(family = '', size = 30),
      legend.title = element_text(size = 30),
      legend.key.size = unit(1, 'cm')
    ) + coord_sf()
}


# Q. engelmannii plots
plot <- g[[1]] + g[[2]]+g[[3]]+g[[4]]+g[[5]]+plot_layout(guides = 'collect')

ggsave(plot,
       filename = "G:/My Drive/Dissertation/003_Ensemble_modeling/figures/Figure1_Qengelmannii_current.png", 
       dpi = 500,
       height = 15,
       width = 16)


# Ensemble maps for one species
dif_maps <-
  list.files(
    'G:/My Drive/Dissertation/003_Ensemble_modeling/data/01_difference_maps/',
    pattern = 'Quercus kelloggii',
    full.names = T,
    recursive = T
  )

r <- list()

for(i in 1:length(dif_maps)){
  r[[i]] <- terra::rast(dif_maps[i])
}


for(i in 1:length(r)){
  print(i)
  r2 <- r[[i]]$hades85_nd_end  %>% crop(cfp) %>% mask(cfp)
  
  r_df <- as.data.frame(r2, xy = TRUE, na.rm = TRUE) %>% dplyr::filter(layer <0)
  
  gp <- ggplot(r_df) +
    geom_spatvector(
      data = cfp,
      color = "black",
      fill = "beige",
      size = 0.2
    )  +
    geom_raster(data = r_df,
                aes(x = x, y = y, fill = layer)) +
    scale_fill_gradientn(
      colours = pals::inferno(10),
      na.value = 'grey50',
      breaks = c(-.99, .3),
      labels = c('Decrease', 'Increase'),
      name = 'Habitat Change'
    ) +
    annotation_scale(
      location = "bl",
      width_hint = 0.35,
      # text_family = 'serif',
      text_cex = 1,
      height = unit(.1, "in")
    ) +
    labs(x = NULL, y = NULL) +
    theme(
      legend.position = c(.8 , .75))
  
  ggsave(
    gp,
    filename = paste0(
      "2_Outputs/3_Exposure/difference_maps/04_hades_rcp85/figures/",
      basename(maps2[i]),
      ".png"
    ),
    dpi = 500,
    height = 5,
    width = 5
  )
}

##%######################################################%##
#                                                          #
####      Species data to include with manuscript       ####
#                                                          #
##%######################################################%##

# 1 Species
# 2 # of pr
# 3 average AUC, Boyce, and TSS
# 4 Range size
# 5 Prevalence
# 6 niche breadth
# 7 topo het, elevation, ranges
# 8 standard deviation in habitat suitability change
# 9 average habitat change 
# 2 files 1 for null and full dispersal

full_df <- perf3 %>%
  pivot_wider(
    id_cols = c(
      species,
      AUC_mean2,
      TSS_mean2,
      Boyce_mean2,
      area100,
      niche_breadth,
      range_topo,
      mean_topo,
      range_elev,
      mean_elev,
      prev,
      pr
    ),
    names_from = dispersal,
    values_from = c(sd_HSC, mean_HSC)
  ) %>%
  rename(AUC_mean = AUC_mean2,
         TSS_mean = TSS_mean2,
         Boyce_mean = Boyce_mean2,
         range_size = area100,
         full_disp_hsc_SD = sd_HSC_fd_change_prop,
         full_disp_hsc_mean = mean_HSC_fd_change_prop,
         no_disp_hsc_SD = sd_HSC_nd_change_prop,
         no_disp_hsc_mean = mean_HSC_nd_change_prop)


data.table::fwrite(full_df, 'G:/My Drive/Dissertation/003_Ensemble_modeling/data/species_data.csv')



