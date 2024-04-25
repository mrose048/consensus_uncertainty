##%######################################################%##
#                                                          #
####             Generalized linear models              ####
#                                                          #
##%######################################################%##

{
  require(dplyr)
  require(tidyverse)
  require(dotwhisker)
  library(tidyverse)
  library(here)
  library(glue)
  library(officer)
  library(rvg)
  library(viridis)
}

#setwd('Google Drive/')
setwd('G:/My Drive/')

##%######################################################%##
#                                                          #
####                Variable importance                 ####
#                                                          #
##%######################################################%##

# species variable importance information
var_imp <- data.table::fread('H:/My Drive/08_Chapter3/sdm_var_imp.csv') %>% tibble()
var_imp <- var_imp %>% 
  dplyr::select(species, Variable, avg_rank) %>%
  pivot_wider(names_from = Variable,
              values_from = avg_rank) %>%
  dplyr::select(-terrain, -topo_hetero)

spp_prev <- data.table::fread('Dissertation/003_Ensemble_modeling/data/species_prevalence.csv') %>% tibble()

geog_traits <-
  data.table::fread(
    'Dissertation/002_Biogeography_exposure/data/sp_geographic_traits.csv'
  ) %>% tibble()


# species traits
sp_traits <-
  data.table::fread(
    'Franklin_grant/project/1-NSF_spatial_and_species_traits/2_Outputs/3_Exposure/sp_traits_data.csv'
  )


exp <- "Dissertation/003_Ensemble_modeling/data/03_exposure/" %>% list.files(pattern = ".txt", full.names = TRUE)
exp <-
  lapply(exp,
         readr::read_tsv,
         col_types = list(
           .default = "d",
           sp = "c",
           model = 'c',
           scen = 'c',
           rcp = 'c',
           change_driver = 'c'
         ))
exp <- bind_rows(exp) 
exp <-
  exp %>% filter(
    sp != 'Hazardia squarrosa' &  sp != "Rhamnus rubra" &
      sp != "Achillea millefolium" &
      sp != "Arctostaphylos pungens" &
      sp != "Mimulus lewisii" &
      sp != "Mimulus guttatus" &
      sp != 'Abies concolor' &
      sp!= 'Pinus ponderosa' &
      sp != 'Muhlenbergia rigens' &
      sp != 'Prunus virginiana' &
      sp != 'Rhus trilobata'
  )

exp2 <- left_join(exp, sp_traits, by = c('sp' = 'species')) %>%
  dplyr::select(sp,
                model = model.x,
                year,
                scen,
                rcp,
                pr,
                area100,
                cwd_range,
                ppt_djf_range,
                ppt_jja_range,
                tmn_range,
                niche_breadth,
                fd_change_prop,
                nd_change_prop) %>%
  left_join(geog_traits, by = c('sp' = 'species')) %>%
  left_join(spp_prev, by = c('sp' = 'species')) %>%
  pivot_longer(cols = fd_change_prop:nd_change_prop,
               names_to = 'dispersal',
               values_to = 'HSC') %>%
  left_join(var_imp, by = c('sp'='species'))


# Figure showing boxplot of HSC value ranges for Pinus and Quercus species
exp2 %>% filter(
  sp %in% c(
    'Abies magnifica',
    'Quercus agrifolia',
    'Pinus balfouriana',
    'Pinus coulteri',
    'Pinus lambertiana',
    'Pinus quadrifolia',
    'Pinus sabiniana',
    'Pinus torreyana',
    'Sequoia sempervirens',
    'Salvia mellifera',
    'Sequoiadendron giganteum'
  )
) %>% ggplot(aes(x = sp, y = log(HSC + 1), fill = dispersal)) + geom_boxplot() + coord_flip() + theme_bw() + scale_fill_grey(
  name = "Dispersal",
  breaks = c("fd_change_prop", "nd_change_prop"),
  labels = c("Full", "No")
) + 
  labs(x = '') + theme(
  plot.title = element_text(
    size = 20,
    margin = ggplot2::margin(
      t = 0,
      r = 20,
      b = 0,
      l = 0
    )
  ),
  axis.title.x = element_text(
    size = 20 + 2,
    margin = ggplot2::margin(
      t = 20,
      r = 0,
      b = 0,
      l = 0
    )
  ),
  axis.text.x = element_text(size = 20),
  axis.text.y = element_text(size = 20),
  
  strip.text.x = element_text(size = 20),
  strip.text.y = element_text(size = 20),
  
  # Legend text
  legend.title = element_text(size = 20),
  legend.text = element_text(size = 20)
)


# Figure showing boxplot of HSC value ranges ordered by standard deviation
uncertainty_gg <-
  exp2 %>%
  filter(dispersal == 'nd_change_prop',
    year == 2085 &
      sp %in% c(
        'Arctostaphylos rudis',
        'Quercus wislizeni',
        'Abies magnifica',
        'Salvia leucophylla',
        'Sequoiadendron giganteum',
        'Ceanothus verrucosus',
        'Quercus sadleriana',
        'Pinus torreyana'
      )
  ) %>%
  ggplot(aes(
    x = reorder(sp, area100),
    y = HSC
  )) + geom_hline(yintercept = 0) + geom_violin(aes(fill = sp)) + geom_boxplot(fill = 'white', width = .1) +  coord_flip() + theme_bw() + scale_fill_uchicago(guide = NULL) +
  labs(x = '', y = 'Exposure') + theme(
    plot.title = element_text(
      size = 20,
      margin = ggplot2::margin(
        t = 0,
        r = 20,
        b = 0,
        l = 0
      )
    ),
    axis.title.x = element_text(
      size = 20 + 2,
      margin = ggplot2::margin(
        t = 20,
        r = 0,
        b = 0,
        l = 0
      )
    ),
    
    
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    
    strip.text.x = element_text(size = 20),
    strip.text.y = element_text(size = 20),
    
    # Legend text
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  )

# Figure showing boxplot of HSC value ranges under null dispersal ordered by range size
null_gg <-
  exp2 %>% filter(dispersal == 'nd_change_prop') %>% ggplot(aes(
    x = reorder(sp, mean_elev),
    y = HSC,
    fill = mean_elev
  )) + geom_boxplot() + coord_flip() + theme_bw() + scale_fill_viridis_b() +
  labs(x = '') + theme(
    plot.title = element_text(
      size = 20,
      margin = ggplot2::margin(
        t = 0,
        r = 20,
        b = 0,
        l = 0
      )
    ),
    axis.title.x = element_text(
      size = 20 + 2,
      margin = ggplot2::margin(
        t = 20,
        r = 0,
        b = 0,
        l = 0
      )
    ),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    
    strip.text.x = element_text(size = 20),
    strip.text.y = element_text(size = 20),
    
    # Legend text
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  )

ggsave(
  null_gg,
  filename = '/My Drive/Dissertation/003_Ensemble_modeling/figures/null_boxplots_species_HSC.png',
  height = 70,
  width = 30,
  dpi = 500,
  unit = 'cm'
)


exp_sd <- exp2 %>%
  filter(year == 2085) %>%
  dplyr::group_by(
    sp,
    dispersal,
    area100,
    niche_breadth,
    range_topo,
    mean_topo,
    range_elev,
    mean_elev,
    prev,
    pr,
    aet,
    awc,
    cwd,
    depth,
    pct_clay,
    ph,
    ppt_djf,
    ppt_jja,
    tmn
  ) %>%
  summarise(sd_HSC = sd(HSC),
            mean_HSC = mean(HSC))

# Correlation plot of predictor variables
# all correlations less that .7, so I think we can do a full glm
library(ggcorrplot)
pred <-
  exp2 %>% dplyr::select(
    prev,
    area100,
    niche_breadth,
    range_topo,
    mean_topo,
    range_elev,
    mean_elev,
    aet:tmn
    
  ) %>% unique()
corr <- round(cor(pred), 2)
p.mat <- cor_pmat(pred)


corr_plot <- ggcorrplot(
  digits = 2,
  corr,
  hc.order = TRUE,
  type = "lower",
  outline.col = "white",
  colors = c("#6D9EC1", "white", "#E46726"),
  tl.cex = 20,
  lab = TRUE,
  ggtheme = theme_pub(base_family = '', base_size = 30))

full_corr <- corr_plot +
  scale_x_discrete(
    labels = c(
      "Topo. het. (range)",
      "Soil depth",
      "PPT DJF",
      "Niche breadth",
      "Range size",
      "Elevation (range)",
      "Prevalence",
      "Clay %",
      "CWD",
      "PPT JJA",
      "TMN",
      'Topo. het. (mean)',
      "Elevation (mean)",
      "AET",
      "AWC"
    )
  ) +
  scale_y_discrete(
    labels = rev(c(
      "AET",
      "Elevation (mean)",
      'Topo. het. (mean)',
      "TMN",
      "PPT JJA",
      "Clay %",
      "CWD",
      "Prevalence",
      "Elevation (range)",
      "Range size",
      "Niche breadth",
      "PPT DJF",
      "Soil depth",
      "Topo. het. (range)",
      "Soil pH"
    ))
  )

ggsave(
  full_corr,
  filename = "Dissertation/003_Ensemble_modeling/figures/correlation_all_predictors.png",
  dpi = 500,
  height = 25,
  width = 25,
  unit = 'cm'
)



##%######################################################%##
#                                                          #
####        Uncertainty due to consensus method         ####
#                                                          #
##%######################################################%##

exp_sd_model <- exp2 %>%
  dplyr::filter(year == 2085) %>%
  dplyr::group_by(
    sp,
    dispersal,
    area100,
    niche_breadth,
    range_topo,
    mean_topo,
    range_elev,
    mean_elev,
    prev,
    pr,
    model
  ) %>%
  summarise(model_HSC = mean(HSC)) %>%
  dplyr::group_by(
    sp,
    dispersal,
    area100,
    niche_breadth,
    range_topo,
    mean_topo,
    range_elev,
    mean_elev,
    prev,
    pr
  ) %>%
  summarise(sd_HSC = sd(model_HSC)) 


p0 <- exp_sd_model %>%
  split(.$dispersal) %>%
  purrr::map(
    ~ glm(
      sd_HSC~ prev + area100 + niche_breadth + mean_elev + range_elev + mean_topo + range_topo,
      data = .x
    )
  ) %>%
  dwplot(whisker_args = c(size = 1.5), dot_args = c(size = 3)) %>%
  relabel_predictors(
    prev = "Prevalence",
    `log10(area100)` = "Range size (log10)",
    `log(niche_breadth)` = "Niche breadth",
    `log(mean_elev)` = "Elevation (log)",
    `log(range_elev)` = "Range in elevation (log)",
    mean_topo = "Topo. het.",
    range_topo = "Range in topo. het."
  ) +
  theme_bw(base_size = 20) + xlab("Coefficient") + ylab("") +
  geom_vline(xintercept = 0,
             colour = "grey60",
             linetype = 2) +
  theme(
    legend.position = c(.995, .6),
    legend.justification = c(1, 1),
    legend.background = element_rect(colour = "grey80"),
    legend.title.align = .5
  ) +
  scale_colour_viridis_d(
    begin= .2,
    end = .8,
    name = "Dispersal",
    breaks = c("fd_change_prop", "nd_change_prop"),
    labels = c("Full", "Null")
  ) +
  scale_colour_viridis_d(
    begin = .2,
    end = .8,
    name = "Dispersal",
    breaks = c("fd_change_prop", "nd_change_prop"),
    labels = c("Full", "Null")
  ) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) + ggtitle('Consensus method uncertainty')


p0 <- ggplot(data = exp_sd_model, aes(x = prev, y = sd_HSC)) +
  geom_point() +
  geom_smooth(method = 'glm') +
  labs(y = 'HSC Standard Deviation (log10)', x = 'Prevalence') +
  facet_wrap(vars(dispersal), labeller = as_labeller(big.label)) +
 # theme_pub(base_size = 15, base_family = 'serif') +
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))


p1 <-
  ggplot(data = exp_sd_model, aes(x = area100, y = log10(sd_HSC))) +
  geom_point() +
  geom_smooth(method = 'glm') +
  scale_x_continuous(labels = label_number_si(), breaks = c(0, 75000, 175000, 275000)) +
  labs(y = 'HSC Standard Deviation (log10)', x = expression('Range size' ~ (km ^ 2))) +
  facet_wrap(vars(dispersal)) +
  theme_pub(base_size = 15, base_family = 'serif') +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) 

p2 <-
  ggplot(data = exp_sd_model, aes(x = niche_breadth, y = log10(sd_HSC))) +
  geom_point() +
  geom_smooth(method = 'glm') +
  labs(y = 'HSC Standard Deviation (log10)', x = 'Niche breadth') +
  facet_wrap(vars(dispersal)) +
  theme_pub(base_size = 15, base_family = 'serif') +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())

p3 <-
  ggplot(data = exp_sd_model, aes(x = range_elev, y = log10(sd_HSC))) +
  geom_point() +
  geom_smooth(method = 'glm') +
  labs(y = 'HSC Standard Deviation (log10)', x = 'Elevation range') +
  facet_wrap(vars(dispersal)) +
  theme_pub(base_size = 15, base_family = 'serif') +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())

p4 <-
  ggplot(data = exp_sd_model, aes(x = range_topo, y = log10(sd_HSC))) +
  geom_point() +
  geom_smooth(method = 'glm') +
  labs(y = 'HSC Standard Deviation (log10)', x = 'Topography range') +
  facet_wrap(vars(dispersal)) +
  theme_pub(base_size = 15, base_family = 'serif') +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())


ggsave(
  p0 / p1 / p2 / p3 / p4,
  height = 60,
  width = 25,
  dpi = 500,
  unit = 'cm',
  file = '/My Drive/Dissertation/003_Ensemble_modeling/figures/rarity_geog_and_model_based_uncertainty.png'
)


##%######################################################%##
#                                                          #
####               GCM based uncertainty                ####
#                                                          #
##%######################################################%##


exp_sd_gcm <- exp2 %>%
  filter(year == 2085) %>%
  dplyr::group_by(
    sp,
    dispersal,
    area100,
    niche_breadth,
    range_topo,
    mean_topo,
    range_elev,
    mean_elev,
    prev,
    pr,
    scen
  ) %>%
  summarise(model_HSC = mean(HSC)) %>%
  dplyr::group_by(
    sp,
    dispersal,
    area100,
    niche_breadth,
    range_topo,
    mean_topo,
    range_elev,
    mean_elev,
    prev,
    pr
  ) %>%
  summarise(sd_HSC = sd(model_HSC))



p1 <- exp_sd_gcm %>%
  split(.$dispersal) %>%
  purrr::map(
    ~ glm(
      sd_HSC ~ prev + area100 + niche_breadth + mean_elev + range_elev + mean_topo + range_topo,
      data = .x
    )
  ) %>%
  dwplot(whisker_args = c(size = 1.5),
         dot_args = c(size = 3)) %>%
  relabel_predictors(
    prev = "Prevalence",
    area100 = "Range size",
    niche_breadth = "Niche breadth",
    mean_elev = "Elevation",
    range_elev = "Range in elevation",
    mean_topo = "Topo. het.",
    range_topo = "Range in topo. het."
  ) +
  theme_bw(base_size = 20) + xlab("Coefficient") + ylab("") +
  geom_vline(xintercept = 0,
             colour = "grey60",
             linetype = 2) +
  theme(
    legend.position = c(.995, .6),
    legend.justification = c(1, 1),
    legend.background = element_rect(colour = "grey80"),
    legend.title.align = .5
  ) +
  scale_colour_viridis_d(
    begin= .2,
    end = .8,
    name = "Dispersal",
    breaks = c("fd_change_prop", "nd_change_prop"),
    labels = c("Full", "Null")
  ) +
  scale_colour_viridis_d(
    begin = .2,
    end = .8,
    name = "Dispersal",
    breaks = c("fd_change_prop", "nd_change_prop"),
    labels = c("Full", "Null")
  ) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  ggtitle('GCM uncertainty')


p0 <- ggplot(data = exp_sd_gcm %>% filter(year == 2085), aes(x = prev, y = sd_HSC)) +
  geom_point() +
  geom_smooth(method = 'glm') +
  labs(y = 'HSC Standard Deviation (log10)', x = 'Prevalence') +
  facet_wrap(vars(dispersal), labeller = as_labeller(big.label)) +
  theme_pub(base_size = 15, base_family = 'serif') +
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))


p1 <-
  ggplot(data = exp_sd_gcm %>% filter(year == 2085), aes(x = area100, y = log10(sd_HSC))) +
  geom_point() +
  geom_smooth(method = 'glm') +
  scale_x_continuous(labels = label_number_si(), breaks = c(0, 75000, 175000, 275000)) +
  labs(y = 'HSC Standard Deviation (log10)', x = expression('Range size' ~ (km ^ 2))) +
  facet_wrap(vars(dispersal)) +
  theme_pub(base_size = 15, base_family = 'serif') +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) 

p2 <-
  ggplot(data = exp_sd_gcm %>% filter(year == 2085), aes(x = niche_breadth, y = log10(sd_HSC))) +
  geom_point() +
  geom_smooth(method = 'glm') +
  labs(y = 'HSC Standard Deviation (log10)', x = 'Niche breadth') +
  facet_wrap(vars(dispersal)) +
  theme_pub(base_size = 15, base_family = 'serif') +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())

p3 <-
  ggplot(data = exp_sd_gcm %>% filter(year == 2085), aes(x = range_elev, y = log10(sd_HSC))) +
  geom_point() +
  geom_smooth(method = 'glm') +
  labs(y = 'HSC Standard Deviation (log10)', x = 'Elevation range') +
  facet_wrap(vars(dispersal)) +
  theme_pub(base_size = 15, base_family = 'serif') +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())

p4 <-
  ggplot(data = exp_sd_gcm %>% filter(year == 2085), aes(x = range_topo, y = log10(sd_HSC))) +
  geom_point() +
  geom_smooth(method = 'glm') +
  labs(y = 'HSC Standard Deviation (log10)', x = 'Topography range') +
  facet_wrap(vars(dispersal)) +
  theme_pub(base_size = 15, base_family = 'serif') +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())

ggsave(
  p0 / p1 / p2 / p3 / p4,
  height = 50,
  width = 15,
  dpi = 500,
  unit = 'cm',
  file = '/My Drive/Dissertation/003_Ensemble_modeling/figures/rarity_geog_and_gcm_based_uncertainty.png'
)


##%######################################################%##
#                                                          #
####               RCP based uncertainty                ####
#                                                          #
##%######################################################%##


exp_sd_rcp <- exp2 %>%
  filter(year == 2085) %>%
  dplyr::group_by(
    sp,
    dispersal,
    area100,
    niche_breadth,
    range_topo,
    mean_topo,
    range_elev,
    mean_elev,
    prev,
    pr,
    rcp
  ) %>%
  summarise(model_HSC = mean(HSC)) %>%
  dplyr::group_by(
    sp,
    dispersal,
    area100,
    niche_breadth,
    range_topo,
    mean_topo,
    range_elev,
    mean_elev,
    prev,
    pr
  ) %>%
  summarise(sd_HSC = sd(model_HSC))


##%######################################################%##
#                                                          #
####    Stepwise model selection & dot-whisker plots    ####
#                                                          #
##%######################################################%##


# all uncertainty data frames
all_sd <- bind_rows(exp_sd_model %>% mutate(source = 'model'),
                    exp_sd_gcm %>% mutate(source = 'gcm'),
                    exp_sd_rcp %>% mutate(source = 'rcp')) 

# initial model formula
mod_form <-
  formula(
    log(sd_HSC) ~ 
      scale(prev) + 
      scale(area100) + 
      scale(niche_breadth) + 
      scale(mean_elev) + 
      scale(mean_topo) + 
      scale(range_elev) + 
      scale(range_topo) 
  )

# stepwise model selection
step1 <-
  step(lm(
    mod_form,
    data = all_sd %>% filter(source == 'model' &
                               dispersal == 'nd_change_prop')
  ))

plot(density(resid(step1)))

step2 <-
  step(lm(
    mod_form,
    data = all_sd %>% filter(source == 'model' &
                               dispersal == 'fd_change_prop')
  ))

plot(density(resid(step2)))

step3 <-
  step(lm(
    mod_form,
    data = all_sd %>% filter(source == 'gcm' &
                               dispersal == 'nd_change_prop')
  ))

plot(density(resid(step3)))

step4 <- step(lm(
  mod_form,
  data = all_sd %>% filter(source == 'gcm' &
                             dispersal == 'fd_change_prop')
))

plot(density(resid(step4)))

step5 <- step(lm(
  mod_form,
  data = all_sd %>% filter(source == 'rcp' &
                             dispersal == 'nd_change_prop')
))

plot(density(resid(step5)))

step6 <- step(lm(
  mod_form,
  data = all_sd %>% filter(source == 'rcp' &
                             dispersal == 'fd_change_prop')
))

plot(density(resid(step6)))



# dot and whisker plots
dw1 <- list(step1, step3, step5) %>%
  dwplot(whisker_args = list(size = 4),
         dot_args = list(size = 10)) %>%
  relabel_predictors(
    `scale(prev)` = "Prevalence",
    `scale(area100)` = "Range size",
    `scale(niche_breadth)` = "Niche breadth",
    `scale(mean_elev)` = "Elevation",
    `scale(range_elev)` = "Range in elevation",
    `scale(mean_topo)` = "Topo. het."
  ) +
  theme_bw(base_size = 43) + xlab("Standardized Estimate") + ylab("") +
  geom_vline(xintercept = 0,
             colour = "black",
             linetype = 2) +
  theme(
  #  legend.position = c(.995, .3),
  #  legend.position = 'bottom',
    legend.justification = c(1, 1),
    legend.background = element_rect(colour = "grey80"),
    legend.title.align = .5
  ) +
  scale_colour_viridis_d(
    begin = .2,
    end = .8,
    name = "Dispersal",
    breaks = c("Model 1", "Model 2", "Model 3"),
    labels = c("Consensus method", "GCM", "RCP")
  ) +
  scale_colour_viridis_d(
    begin = .2,
    end = .8,
    name = "Uncertainty source",
    breaks = c("Model 1", "Model 2", "Model 3"),
    labels = c("Consensus method", "GCM ", "RCP")
  ) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  ggtitle('No dispersal')

# full dispersal
dw2 <- list(step2, step4, step6) %>%
  dwplot(whisker_args = list(size = 4),
         dot_args = list(size = 10)) %>%
  relabel_predictors(
    `scale(prev)` = "Prevalence",
    `scale(area100)` = "Range size",
    `scale(niche_breadth)` = "Niche breadth",
    `scale(mean_elev)` = "Elevation",
    `scale(range_elev)` = "Range in elevation",
    `scale(mean_topo)` = "Topo. het.",
    `scale(range_topo)` = "Range in topo. het."
  ) +
  theme_bw(base_size = 43) + xlab("Standardized Estimate") + ylab("") +
  geom_vline(xintercept = 0,
             colour = "black",
             linetype = 2) +
  theme(
    #legend.position = c(.995, .3),
  #  legend.position = 'bottom',
    legend.justification = c(1, 1),
    legend.background = element_rect(colour = "grey80"),
    legend.title.align = .5
  ) +
  scale_colour_viridis_d(
    begin = .2,
    end = .8,
    name = "Dispersal",
    breaks = c("Model 1", "Model 2", "Model 3"),
    labels = c("Consensus method", "GCM", "RCP")
  ) +
  scale_colour_viridis_d(
    begin = .2,
    end = .8,
    name = "Uncertainty source",
    breaks = c("Model 1", "Model 2", "Model 3"),
    labels = c("Consensus method", "GCM ", "RCP")
  ) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  ggtitle('Full dispersal')

library(patchwork)

full_dw <-
  (dw1 / dw2) +
  plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect") &
  theme(legend.position = 'bottom')

ggsave(
  full_dw,
  file = 'H:/My Drive/10_Uncertainty_manuscript/D&D_submission2/figures/full_dot_and_whisker.png',
  height = 70,
  width = 57,
  unit = 'cm',
  dpi = 300
)


# models with soil and climate variable importance

# initial model formula
mod_form2 <-
  formula(
    log(sd_HSC) ~ 
      scale(aet) +
      scale(awc) +
      scale(cwd) + 
      scale(depth) +
      scale(pct_clay) +
      scale(ph) +
      scale(ppt_djf) + 
      scale(ppt_jja) +
      scale(tmn)
  )

# stepwise model selection
step1 <-
  step(lm(
    mod_form2,
    data = all_sd %>% filter(source == 'model' &
                               dispersal == 'nd_change_prop')
  ))

plot(density(resid(step1)))

step2 <-
  step(lm(
    mod_form2,
    data = all_sd %>% filter(source == 'model' &
                               dispersal == 'fd_change_prop')
  ))

plot(density(resid(step2)))

step3 <-
  step(lm(
    mod_form2,
    data = all_sd %>% filter(source == 'gcm' &
                               dispersal == 'nd_change_prop')
  ))

plot(density(resid(step3)))

step4 <- step(lm(
  mod_form2,
  data = all_sd %>% filter(source == 'gcm' &
                             dispersal == 'fd_change_prop')
))

plot(density(resid(step4)))

step5 <- step(lm(
  mod_form2,
  data = all_sd %>% filter(source == 'rcp' &
                             dispersal == 'nd_change_prop')
))

plot(density(resid(step5)))

step6 <- step(lm(
  mod_form2,
  data = all_sd %>% filter(source == 'rcp' &
                             dispersal == 'fd_change_prop')
))

plot(density(resid(step6)))



# dot and whisker plots
dw3 <- list(step1, step3, step5) %>%
  dwplot(whisker_args = list(size = 4),
         dot_args = list(size = 10)) %>%
  relabel_predictors(
    `scale(aet)` = "AET (Imp.)",
    `scale(depth)` = "Soil Depth (Imp.)",
    `scale(ph)` = "Soil pH (Imp.)",
    `scale(cwd)` = "CWD (Imp.)",
    `scale(awc)` = "AWC (Imp.)",
    `scale(ppt_djf)` = "PPT DJF (Imp.)",
    `scale(ppt_jja)` = "PPT JJA (Imp.)",
    `scale(tmn)` = "TMN (Imp.)",
  ) +
  theme_bw(base_size = 43) + xlab("Standardized Estimate") + ylab("") +
  geom_vline(xintercept = 0,
             colour = "black",
             linetype = 2) +
  theme(
    #  legend.position = c(.995, .3),
    #  legend.position = 'bottom',
    legend.justification = c(1, 1),
    legend.background = element_rect(colour = "grey80"),
    legend.title.align = .5
  ) +
  scale_colour_viridis_d(
    begin = .2,
    end = .8,
    name = "Dispersal",
    breaks = c("Model 1", "Model 2", "Model 3"),
    labels = c("Consensus method", "GCM", "RCP")
  ) +
  scale_colour_viridis_d(
    begin = .2,
    end = .8,
    name = "Uncertainty source",
    breaks = c("Model 1", "Model 2", "Model 3"),
    labels = c("Consensus method", "GCM ", "RCP")
  ) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  ggtitle('No dispersal')

# full dispersal
dw4 <- list(step2, step4, step6) %>%
  dwplot(whisker_args = list(size = 4),
         dot_args = list(size = 10)) %>%
  relabel_predictors(
    `scale(aet)` = "AET (Imp.)",
    `scale(ppt_djf)` = "PPT DJF (Imp.)",
    `scale(ppt_jja)` = "PPT JJA (Imp.)",
    `scale(tmn)` = "TMN (Imp.)",
    `scale(ph)` = "Soil pH (Imp.)",
    `scale(cwd)` = "CWD (Imp.)",
    `scale(awc)` = "AWC (Imp.)"
  ) +
  theme_bw(base_size = 43) + xlab("Standardized Estimate") + ylab("") +
  geom_vline(xintercept = 0,
             colour = "black",
             linetype = 2) +
  theme(
    #legend.position = c(.995, .3),
    #  legend.position = 'bottom',
    legend.justification = c(1, 1),
    legend.background = element_rect(colour = "grey80"),
    legend.title.align = .5
  ) +
  scale_colour_viridis_d(
    begin = .2,
    end = .8,
    name = "Dispersal",
    breaks = c("Model 1", "Model 2", "Model 3"),
    labels = c("Consensus method", "GCM", "RCP")
  ) +
  scale_colour_viridis_d(
    begin = .2,
    end = .8,
    name = "Uncertainty source",
    breaks = c("Model 1", "Model 2", "Model 3"),
    labels = c("Consensus method", "GCM ", "RCP")
  ) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  ggtitle('Full dispersal')

library(patchwork)

clim_soil_dw <-
  (dw3 / dw4) +
  plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect") &
  theme(legend.position = 'bottom')

ggsave(
  clim_soil_dw,
  file = 'H:/My Drive/10_Uncertainty_manuscript/D&D_submission2/figures/clim&soil_dot_and_whisker.png',
  height = 55,
  width = 57,
  unit = 'cm',
  dpi = 300
)


##%######################################################%##
#                                                          #
####        Simple linear regressions (explore)         ####
#                                                          #
##%######################################################%##


# uncertainty due to model type, null dispersal
{
  lm1 <-
    lm(sd_HSC ~ area100,
       all_sd %>% filter(source == 'model' &
                           dispersal == 'nd_change_prop'))
  lm2 <-
    lm(sd_HSC ~ prev,
       all_sd %>% filter(source == 'model' &
                           dispersal == 'nd_change_prop'))
  lm3 <-
    lm(
      sd_HSC ~ niche_breadth,
      all_sd %>% filter(source == 'model' &
                          dispersal == 'nd_change_prop')
    )
  lm4 <-
    lm(sd_HSC ~ mean_elev,
       all_sd %>% filter(source == 'model' &
                           dispersal == 'nd_change_prop'))
  
  lm5 <-
    lm(sd_HSC ~ range_elev,
       all_sd %>% filter(source == 'model' &
                           dispersal == 'nd_change_prop'))
  lm6 <-
    lm(sd_HSC ~ mean_topo,
       all_sd %>% filter(source == 'model' &
                           dispersal == 'nd_change_prop'))
  lm7 <-
    lm(sd_HSC ~ range_topo,
       all_sd %>% filter(source == 'model' &
                           dispersal == 'nd_change_prop'))
}

# uncertainty due to model type, full dispersal

{
  lm1 <-
    lm(log(sd_HSC) ~ area100,
       all_sd %>% filter(source == 'model' &
                           dispersal == 'fd_change_prop'))
  lm2 <-
    lm(log(sd_HSC) ~ prev,
       all_sd %>% filter(source == 'model' &
                           dispersal == 'fd_change_prop'))
  lm3 <-
    lm(
      log(sd_HSC) ~ niche_breadth,
      all_sd %>% filter(source == 'model' &
                          dispersal == 'fd_change_prop')
    )
  lm4 <-
    lm(log(sd_HSC) ~ mean_elev,
       all_sd %>% filter(source == 'model' &
                           dispersal == 'fd_change_prop'))
  
  lm5 <-
    lm(log(sd_HSC) ~ range_elev,
       all_sd %>% filter(source == 'model' &
                           dispersal == 'fd_change_prop'))
  lm6 <-
    lm(log(sd_HSC) ~ mean_topo,
       all_sd %>% filter(source == 'model' &
                           dispersal == 'fd_change_prop'))
  lm7 <-
    lm(log(sd_HSC) ~ range_topo,
       all_sd %>% filter(source == 'model' &
                           dispersal == 'fd_change_prop'))
}

# uncertainty due to gcm, null dispersal
{
  lm1 <-
    lm(sd_HSC ~ area100,
       all_sd %>% filter(source == 'gcm' &
                           dispersal == 'nd_change_prop'))
  lm2 <-
    lm(sd_HSC ~ prev,
       all_sd %>% filter(source == 'gcm' &
                           dispersal == 'nd_change_prop'))
  lm3 <-
    lm(
      sd_HSC ~ niche_breadth,
      all_sd %>% filter(source == 'gcm' &
                          dispersal == 'nd_change_prop')
    )
  lm4 <-
    lm(sd_HSC ~ mean_elev,
       all_sd %>% filter(source == 'gcm' &
                           dispersal == 'nd_change_prop'))
  
  lm5 <-
    lm(sd_HSC ~ range_elev,
       all_sd %>% filter(source == 'gcm' &
                           dispersal == 'nd_change_prop'))
  lm6 <-
    lm(sd_HSC ~ mean_topo,
       all_sd %>% filter(source == 'gcm' &
                           dispersal == 'nd_change_prop'))
  lm7 <-
    lm(sd_HSC ~ range_topo,
       all_sd %>% filter(source == 'gcm' &
                           dispersal == 'nd_change_prop'))
}

# uncertainty due to gcm, full dispersal
{
  lm1 <-
    lm(log(sd_HSC) ~ area100,
       all_sd %>% filter(source == 'gcm' &
                           dispersal == 'fd_change_prop'))
  lm2 <-
    lm(log(sd_HSC) ~ prev,
       all_sd %>% filter(source == 'gcm' &
                           dispersal == 'fd_change_prop'))
  lm3 <-
    lm(
      log(sd_HSC) ~ niche_breadth,
      all_sd %>% filter(source == 'gcm' &
                          dispersal == 'fd_change_prop')
    )
  lm4 <-
    lm(log(sd_HSC) ~ mean_elev,
       all_sd %>% filter(source == 'gcm' &
                           dispersal == 'fd_change_prop'))
  
  lm5 <-
    lm(log(sd_HSC) ~ range_elev,
       all_sd %>% filter(source == 'gcm' &
                           dispersal == 'fd_change_prop'))
  lm6 <-
    lm(log(sd_HSC) ~ mean_topo,
       all_sd %>% filter(source == 'gcm' &
                           dispersal == 'fd_change_prop'))
  lm7 <-
    lm(log(sd_HSC) ~ range_topo,
       all_sd %>% filter(source == 'gcm' &
                           dispersal == 'fd_change_prop'))
}

# uncertainty due to rcp, null dispersal
{
  lm1 <-
    lm(sd_HSC ~ area100,
       all_sd %>% filter(source == 'rcp' &
                           dispersal == 'nd_change_prop'))
  lm2 <-
    lm(sd_HSC ~ prev,
       all_sd %>% filter(source == 'rcp' &
                           dispersal == 'nd_change_prop'))
  lm3 <-
    lm(
      sd_HSC ~ niche_breadth,
      all_sd %>% filter(source == 'rcp' &
                          dispersal == 'nd_change_prop')
    )
  lm4 <-
    lm(sd_HSC ~ mean_elev,
       all_sd %>% filter(source == 'rcp' &
                           dispersal == 'nd_change_prop'))
  
  lm5 <-
    lm(sd_HSC ~ range_elev,
       all_sd %>% filter(source == 'rcp' &
                           dispersal == 'nd_change_prop'))
  lm6 <-
    lm(sd_HSC ~ mean_topo,
       all_sd %>% filter(source == 'rcp' &
                           dispersal == 'nd_change_prop'))
  lm7 <-
    lm(sd_HSC ~ range_topo,
       all_sd %>% filter(source == 'rcp' &
                           dispersal == 'nd_change_prop'))
}

# uncertainty due to rcp, full dispersal
{
  lm1 <-
    lm(log(sd_HSC) ~ area100,
       all_sd %>% filter(source == 'rcp' &
                           dispersal == 'fd_change_prop'))
  lm2 <-
    lm(log(sd_HSC) ~ prev,
       all_sd %>% filter(source == 'rcp' &
                           dispersal == 'fd_change_prop'))
  lm3 <-
    lm(
      log(sd_HSC) ~ niche_breadth,
      all_sd %>% filter(source == 'rcp' &
                          dispersal == 'fd_change_prop')
    )
  lm4 <-
    lm(log(sd_HSC) ~ mean_elev,
       all_sd %>% filter(source == 'rcp' &
                           dispersal == 'fd_change_prop'))
  
  lm5 <-
    lm(log(sd_HSC) ~ range_elev,
       all_sd %>% filter(source == 'rcp' &
                           dispersal == 'fd_change_prop'))
  lm6 <-
    lm(log(sd_HSC)~ mean_topo,
       all_sd %>% filter(source == 'rcp' &
                           dispersal == 'fd_change_prop'))
  lm7 <-
    lm(log(sd_HSC) ~ range_topo,
       all_sd %>% filter(source == 'rcp' &
                           dispersal == 'fd_change_prop'))
}


# Exploratory scatterplots for rarity/geographic traits and uncertainty from multiple sources

predictors <-
  c(
    'area100',
    'niche_breadth',
    'prev',
    'mean_elev',
    'range_elev',
    'mean_topo',
    'range_topo',
    'aet',
    'cwd',
    'ppt_djf',
    'ppt_jja',
    'tmn',
    'awc',
    'depth',
    'pct_clay',
    'ph'
  )


for (i in 1:length(predictors)) {
  p <- ggplot(
    all_sd %>% filter(dispersal == 'nd_change_prop'),
    aes_string(predictors[i],
               'sd_HSC')
  ) +
    geom_point(color = 'black', size = 5) +
    geom_smooth(method = "glm", colour = 'black') +
    geom_hline(yintercept = 0, linetype = "dotted") +
    labs(x = predictors[i], y = 'HSC SD') +
    facet_wrap(vars(source),
               scales = 'free_y',
               labeller = as_labeller(c(
                 'gcm' = 'gcm',
                 'rcp' = 'rcp',
                 'model' = 'model'
               ))) + theme_pub(base_size = 32) + theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
  
  ggsave(
    p,
    file = paste0(
      'Dissertation/003_Ensemble_modeling/figures/scatterplots/',
      predictors[i],
      '_nd.png'
    ),
    height = 15,
    width = 36,
    dpi = 300
  )
}

all_sd <- all_sd %>% mutate(log_sd_HSC = log(sd_HSC))

for (i in 1:length(predictors)) {
  p <- ggplot(
    all_sd %>% filter(dispersal == 'fd_change_prop'),
    aes_string(predictors[i],
               'log_sd_HSC')
  ) +
    geom_point(color = 'black', size = 5) +
    geom_smooth(method = "glm", colour = 'black') +
    geom_hline(yintercept = 0, linetype = "dotted") +
    labs(x = predictors[i], y = 'log(HSC SD)') +
    facet_wrap(vars(source),
               scales = 'free_y',
               labeller = as_labeller(c(
                 'gcm' = 'gcm',
                 'rcp' = 'rcp',
                 'model' = 'model'
               ))) + theme_pub(base_size = 32) + theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
  
  ggsave(
    p,
    file = paste0(
      'Dissertation/003_Ensemble_modeling/figures/scatterplots/',
      predictors[i],
      '_fd.png'
    ),
    height = 15,
    width = 36,
    dpi = 300
  )
}


##%######################################################%##
#                                                          #
####           Dot and whisker for Powerpoint           ####
#                                                          #
##%######################################################%##


# dot and whisker plots (RCP only)
dw1 <- list(step1, step3, step5) %>%
  dwplot(whisker_args = list(size = 4),
         dot_args = list(size = 10)) %>%
  relabel_predictors(
    `scale(prev)` = "Prevalence",
    `scale(area100)` = "Range size",
    `scale(niche_breadth)` = "Niche breadth",
    `scale(mean_elev)` = "Elevation",
    `scale(range_elev)` = "Range in elevation",
    `scale(mean_topo)` = "Topo. het.",
    `scale(range_topo)` = "Range in topo. het."
  ) +
  theme_bw(base_size = 43) + xlab("Standardized Estimate") + ylab("") +
  geom_vline(xintercept = 0,
             colour = "black",
             linetype = 2) +
  theme(
    #  legend.position = c(.995, .3),
    #  legend.position = 'bottom',
    legend.justification = c(1, 1),
    legend.background = element_rect(colour = "grey80"),
    legend.title.align = .5
  ) +
  scale_colour_manual(
    values = "#6DCC57",
    breaks = c("Model 3"),
    labels = c("RCP")
  ) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  ggtitle('No dispersal')

d# full dispersal
dw2 <- list(step6) %>%
  dwplot(whisker_args = list(size = 4),
         dot_args = list(size = 10)) %>%
  relabel_predictors(
    `scale(prev)` = "Prevalence",
    `scale(area100)` = "Range size",
    `scale(niche_breadth)` = "Niche breadth",
    `scale(mean_elev)` = "Elevation",
    `scale(range_elev)` = "Range in elevation",
    `scale(mean_topo)` = "Topo. het.",
    `scale(range_topo)` = "Range in topo. het."
  ) +
  theme_bw(base_size = 43) + xlab("Standardized Estimate") + ylab("") +
  geom_vline(xintercept = 0,
             colour = "black",
             linetype = 2) +
  theme(
    #legend.position = c(.995, .3),
    #  legend.position = 'bottom',
    legend.justification = c(1, 1),
    legend.background = element_rect(colour = "grey80"),
    legend.title.align = .5
  ) +
  scale_colour_manual(
    values = "#6DCC57",
    breaks = c("Model 1"),
    labels = c("RCP")
  ) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  ggtitle('Full dispersal')

library(patchwork)

full_dw <-
  (dw1 / dw2) +
  plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect") &
  theme(legend.position = 'bottom')

ggsave(
  full_dw,
  file = 'Dissertation/003_Ensemble_modeling/figures/full_dot_and_whisker.png',
  height = 55,
  width = 57,
  unit = 'cm',
  dpi = 300
)



##%######################################################%##
#                                                          #
####         Maps of agreement for two species          ####
#                                                          #
##%######################################################%##


# Mapped agreement for two species with contrasting rarity and geography across
# 1 consensus methods
# 2 GCMs
# 3 RCPs
# under no dispersal for end of century

models <- c('mean', 'meanw', 'meanthr', 'meansup', 'median')


sp1_cur <- list_c <-
  "G:/My Drive/Dissertation/003_Ensemble_modeling/data/00_current_habitat_maps/" %>% list.files(pattern = 'Arctostaphylos rudis',
                                                                                                full.names = TRUE,
                                                                                                recursive = T) 



sp1_fut <- list.files(
  '2_Outputs/2_Projection/',
  recursive = TRUE,
  full.names = TRUE,
  pattern = '.tif$'
)

sp1_fut2 <- sp1_fut  %>% grep("Arctostaphylos rudis", ., value = TRUE)
sp1_fut2 <- sp1_fut2  %>% grep("Ensemble", ., value = TRUE)
sp1_fut2 <- sp1_fut2  %>% grep("2070_2099", ., value = TRUE)

sp1_stack <- list()


for (j in 1:length(models)) {
  
  print(models[j])
  
  c <- sp1_cur %>% grep(models[j], ., value = TRUE)
  f <- sp1_fut2 %>% grep(models[j], ., value = TRUE)
  
  # Current condition
  lf <- c[[1]]
  r <- terra::rast(lf)
  
  # cnrm_rcp45
  lf <- f  %>% grep("cnrm_rcp45", ., value = TRUE) 
  
  
  # no dispersal
  scen1 <-
    terra::mask(terra::rast(lf[[1]])[[2]],
                r,
                maskvalue = c(0, NA),
                updatevalue = 0) # continuous mask, null dispersal future
  
  # hades_rcp45
  lf <- f  %>% grep("hades_rcp45", ., value = TRUE) 
  
  
  # no dispersal
  scen2 <-
    terra::mask(terra::rast(lf[[1]])[[2]],
                r,
                maskvalue = c(0, NA),
                updatevalue = 0) # continuous mask, null dispersal future
  
  # cnrm_rcp85
  lf <- f  %>% grep("cnrm_rcp85", ., value = TRUE) 
  
  
  # no dispersal
  scen3 <-
    terra::mask(terra::rast(lf[[1]])[[2]],
                r,
                maskvalue = c(0, NA),
                updatevalue = 0) # continuous mask, null dispersal future
  
  # hades_rcp85
  lf <- f  %>% grep("hades_rcp85", ., value = TRUE) 
  
  
  # no dispersal
  scen4 <-
    terra::mask(terra::rast(lf[[1]])[[2]],
                r,
                maskvalue = c(0, NA),
                updatevalue = 0) # continuous mask, null dispersal future
  
  sp1_stack[[j]] <- c(scen1, scen2, scen3, scen4)
  names(sp1_stack[[j]]) <- c('cnrm_rcp45', 'hades_rcp45', 'cnrm_rcp85', 'hades_rcp85')
}
  

names(sp1_stack) <- models


# variation by consensus
model_mean <- lapply(sp1_stack, mean)
model_mean <- terra::rast(model_mean)
model_sd <- stdev(model_mean)


writeRaster(model_sd, 'G:/My Drive/Dissertation/003_Ensemble_modeling/Map_visualization/Arctostaphylos rudis_sd_models.tif')


# agreement by RCP
rcp45 <- mean(
  sp1_stack[[1]]$cnrm_rcp45,
  sp1_stack[[1]]$hades_rcp45,
  sp1_stack[[2]]$cnrm_rcp45,
  sp1_stack[[2]]$hades_rcp45,
  sp1_stack[[3]]$cnrm_rcp45,
  sp1_stack[[3]]$hades_rcp45,
  sp1_stack[[4]]$cnrm_rcp45,
  sp1_stack[[4]]$hades_rcp45,
  sp1_stack[[5]]$cnrm_rcp45,
  sp1_stack[[5]]$hades_rcp45
)
rcp85 <- mean(
  sp1_stack[[1]]$cnrm_rcp85,
  sp1_stack[[1]]$hades_rcp85,
  sp1_stack[[2]]$cnrm_rcp85,
  sp1_stack[[2]]$hades_rcp85,
  sp1_stack[[3]]$cnrm_rcp85,
  sp1_stack[[3]]$hades_rcp85,
  sp1_stack[[4]]$cnrm_rcp85,
  sp1_stack[[4]]$hades_rcp85,
  sp1_stack[[5]]$cnrm_rcp85,
  sp1_stack[[5]]$hades_rcp85
)

rcps <- c(rcp45, rcp85)
rcps_sd <- stdev(rcps)
writeRaster(rcps_sd, 'G:/My Drive/Dissertation/003_Ensemble_modeling/Map_visualization/Arctostaphylos rudis_sd_rcp.tif')


# agreement by GCM
cnrm <- mean(
  sp1_stack[[1]]$cnrm_rcp45,
  sp1_stack[[1]]$cnrm_rcp85,
  sp1_stack[[2]]$cnrm_rcp45,
  sp1_stack[[2]]$cnrm_rcp85,
  sp1_stack[[3]]$cnrm_rcp45,
  sp1_stack[[3]]$cnrm_rcp85,
  sp1_stack[[4]]$cnrm_rcp45,
  sp1_stack[[4]]$cnrm_rcp85,
  sp1_stack[[5]]$cnrm_rcp85,
  sp1_stack[[5]]$cnrm_rcp85
)
hades <- mean(
  sp1_stack[[1]]$hades_rcp45,
  sp1_stack[[1]]$hades_rcp85,
  sp1_stack[[2]]$hades_rcp45,
  sp1_stack[[2]]$hades_rcp85,
  sp1_stack[[3]]$hades_rcp45,
  sp1_stack[[3]]$hades_rcp85,
  sp1_stack[[4]]$hades_rcp45,
  sp1_stack[[4]]$hades_rcp85,
  sp1_stack[[5]]$hades_rcp45,
  sp1_stack[[5]]$hades_rcp85
)

gcms <- c(cnrm, hades)
gcms_sd <- stdev(gcms)
writeRaster(gcms_sd, 'G:/My Drive/Dissertation/003_Ensemble_modeling/Map_visualization/Arctostaphylos rudis_sd_gcm.tif')


# Abies magnifica
sp2_cur <- list_c <-
  "G:/My Drive/Dissertation/003_Ensemble_modeling/data/00_current_habitat_maps/" %>% list.files(pattern = 'Abies magnifica',
                                                                                                full.names = TRUE,
                                                                                                recursive = T) 

sp2_fut <- sp1_fut  %>% grep("Abies magnifica", ., value = TRUE)
sp2_fut <- sp2_fut  %>% grep("Ensemble", ., value = TRUE)
sp2_fut <- sp2_fut  %>% grep("2070_2099", ., value = TRUE)

sp2_stack <- list()


for (j in 1:length(models)) {
  
  print(models[j])
  
  c <- sp2_cur %>% grep(models[j], ., value = TRUE)
  f <- sp2_fut %>% grep(models[j], ., value = TRUE)
  
  # Current condition
  lf <- c[[1]]
  r <- terra::rast(lf)
  
  # cnrm_rcp45
  lf <- f  %>% grep("cnrm_rcp45", ., value = TRUE) 
  
  
  # no dispersal
  scen1 <-
    terra::mask(terra::rast(lf[[1]])[[2]],
                r,
                maskvalue = c(0, NA),
                updatevalue = 0) # continuous mask, null dispersal future
  
  # hades_rcp45
  lf <- f  %>% grep("hades_rcp45", ., value = TRUE) 
  
  
  # no dispersal
  scen2 <-
    terra::mask(terra::rast(lf[[1]])[[2]],
                r,
                maskvalue = c(0, NA),
                updatevalue = 0) # continuous mask, null dispersal future
  
  # cnrm_rcp85
  lf <- f  %>% grep("cnrm_rcp85", ., value = TRUE) 
  
  
  # no dispersal
  scen3 <-
    terra::mask(terra::rast(lf[[1]])[[2]],
                r,
                maskvalue = c(0, NA),
                updatevalue = 0) # continuous mask, null dispersal future
  
  # hades_rcp85
  lf <- f  %>% grep("hades_rcp85", ., value = TRUE) 
  
  
  # no dispersal
  scen4 <-
    terra::mask(terra::rast(lf[[1]])[[2]],
                r,
                maskvalue = c(0, NA),
                updatevalue = 0) # continuous mask, null dispersal future
  
  sp2_stack[[j]] <- c(scen1, scen2, scen3, scen4)
  names(sp2_stack[[j]]) <- c('cnrm_rcp45', 'hades_rcp45', 'cnrm_rcp85', 'hades_rcp85')
}


names(sp2_stack) <- models


# variation by consensus
model_mean <- lapply(sp2_stack, mean)
model_mean <- terra::rast(model_mean)
model_sd <- stdev(model_mean)


writeRaster(model_sd, 'G:/My Drive/Dissertation/003_Ensemble_modeling/Map_visualization/Abies magnifica_sd_models.tif')


# agreement by RCP
rcp45 <- mean(
  sp2_stack[[1]]$cnrm_rcp45,
  sp2_stack[[1]]$hades_rcp45,
  sp2_stack[[2]]$cnrm_rcp45,
  sp2_stack[[2]]$hades_rcp45,
  sp2_stack[[3]]$cnrm_rcp45,
  sp2_stack[[3]]$hades_rcp45,
  sp2_stack[[4]]$cnrm_rcp45,
  sp2_stack[[4]]$hades_rcp45,
  sp2_stack[[5]]$cnrm_rcp45,
  sp2_stack[[5]]$hades_rcp45
)
rcp85 <- mean(
  sp2_stack[[1]]$cnrm_rcp85,
  sp2_stack[[1]]$hades_rcp85,
  sp2_stack[[2]]$cnrm_rcp85,
  sp2_stack[[2]]$hades_rcp85,
  sp2_stack[[3]]$cnrm_rcp85,
  sp2_stack[[3]]$hades_rcp85,
  sp2_stack[[4]]$cnrm_rcp85,
  sp2_stack[[4]]$hades_rcp85,
  sp2_stack[[5]]$cnrm_rcp85,
  sp2_stack[[5]]$hades_rcp85
)

rcps <- c(rcp45, rcp85)
rcps_sd <- stdev(rcps)
writeRaster(rcps_sd, 'G:/My Drive/Dissertation/003_Ensemble_modeling/Map_visualization/Abies magnifica_sd_rcp.tif')


# agreement by GCM
cnrm <- mean(
  sp2_stack[[1]]$cnrm_rcp45,
  sp2_stack[[1]]$cnrm_rcp85,
  sp2_stack[[2]]$cnrm_rcp45,
  sp2_stack[[2]]$cnrm_rcp85,
  sp2_stack[[3]]$cnrm_rcp45,
  sp2_stack[[3]]$cnrm_rcp85,
  sp2_stack[[4]]$cnrm_rcp45,
  sp2_stack[[4]]$cnrm_rcp85,
  sp2_stack[[5]]$cnrm_rcp85,
  sp2_stack[[5]]$cnrm_rcp85
)
hades <- mean(
  sp2_stack[[1]]$hades_rcp45,
  sp2_stack[[1]]$hades_rcp85,
  sp2_stack[[2]]$hades_rcp45,
  sp2_stack[[2]]$hades_rcp85,
  sp2_stack[[3]]$hades_rcp45,
  sp2_stack[[3]]$hades_rcp85,
  sp2_stack[[4]]$hades_rcp45,
  sp2_stack[[4]]$hades_rcp85,
  sp2_stack[[5]]$hades_rcp45,
  sp2_stack[[5]]$hades_rcp85
)

gcms <- c(cnrm, hades)
gcms_sd <- stdev(gcms)
writeRaster(gcms_sd, 'G:/My Drive/Dissertation/003_Ensemble_modeling/Map_visualization/Abies magnifica_sd_gcm.tif')



