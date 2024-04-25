##%######################################################%##
#                                                          #
####            Chi-squared goodness of fit             ####
####            tests for model comparisons             ####
#                                                          #
##%######################################################%##

setwd("G:/My Drive/Franklin_grant/project/1-NSF_spatial_and_species_traits/")

# Written by Brooke Rose
{
  require(terra)
  require(dplyr)
  require(flexsdm)
  require(here)
  require(ggplot2)
  require(viridis)
  require(tidyr)
  require(landscapemetrics)
  require(raster)
  require(landscapetools)
  require(data.table)
  require(parallel)
  require(doParallel)
  require(raster)
  require(BAMMtools)
}


# species traits
sp_traits <- data.table::fread('G:/My Drive/Franklin_grant/project/1-NSF_spatial_and_species_traits/2_Outputs/3_Exposure/sp_traits_data.csv') %>% as_tibble() %>% dplyr::select(-model)


exp <- "/Volumes/GoogleDrive/My Drive/Dissertation/003_Ensemble_modeling/data/03_exposure/" %>% list.files(pattern = ".txt", full.names = TRUE)
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

exp <- left_join(exp, sp_traits, by = c('sp' = 'species'))

# variation in full and null dispersal change (proportional to current range)
getJenksBreaks(exp$nd_change_prop, k = 5)
getJenksBreaks(exp$fd_change_prop, k = 5)


# Categories based on Beaumont et al. 2016
# No future habitat (range extinction)
# Low stability of current habitat (<10% remains)
# No gain of habitat in new locations
# All future habitat is in new locations 
# Substantial increase in new habitat (future habitat is >100% larger than current habitat)

# Because we are looking at changes in habitat suitability, species can have increases in suitable habitat within the current range ("stable" current habitat is hard to measure)
# so instead, we look at insitu vs. exsitu habitat suitability proportional to current range
# Proposed categories based on measures of continuous habitat suitability proportions
# Low stability of current habitat (insitu prop <= 10%)
# Very high stability/gain of current habitat (insitu prop > 100%)
# Very little gain of habitat in new locations (exsitu prop <= 10 %)
# Substantial increase in new habitat (exsitu prop >100%)


exp <- exp %>%
  mutate(insitu_prop = nd_future_km2/current_km2,
         exsitu_prop = (fd_future_km2-nd_future_km2)/current_km2)


# adding change categories based on breaks above
exp <- exp %>%
  mutate(change_cat = ifelse(
    insitu_prop <= .4,
    'a',
    ifelse(
      insitu_prop >= 1,
      'c',
      ifelse(
        exsitu_prop <= .4,
        'b',
        ifelse(exsitu_prop >= 1, 'd', 'Not extreme')
      )
    )
  ))


# adding change categories based on breaks above
exp <- exp %>%
  mutate(
    a = ifelse(insitu_prop <= .4,
               'yes', 'no'),
    c = ifelse(insitu_prop >= 1,
               'yes', 'no'),
    b = ifelse(exsitu_prop <= .4,
               'yes', 'no'),
    d = ifelse(exsitu_prop >= 1,
               'yes', 'no')
  )



sum_exp <- exp %>%
  group_by(model, change_cat) %>%
  dplyr::summarise(count = n())

sum_exp <- exp %>%
  filter(year == 2085) %>%
  group_by(model, scen, rcp, a, b, c, d) %>%
  dplyr::summarise(count = n())

# Calculate the number of extreme scenarios within each of the four climate futures (end up century)
sum_exp2 <- exp %>%
  filter(year == 2085) %>%
  group_by(scen, rcp, a, b, c, d) %>%
  dplyr::summarise(count = n())

# Chi-square goodness of fit for each change cateogry
# expected frequencies for categories
# number of predictions made by each consensus/total number of predictions = 656/3280 = .2
# are the methods equally represented within the categories?

exp_prob <- rep(.2, 5)

# for the global chi-square test statistics

# category a
exp_prob <- rep(.2, 5)
act_prob <- c(21, 14, 20, 35, 30)
a_chi <-
  chisq.test(act_prob, p = exp_prob)
a_res <- a_chi$stdres
names(a_res) <- c('mean', 'meanw', 'meansup', 'meanthr', 'median')

# Test for category b, low gain
act_prob <- c(67, 35, 43, 91, 137)
b_chi <-
  chisq.test(act_prob, p = exp_prob)
b_res <- b_chi$stdres
names(b_res) <- c('mean', 'meanw', 'meansup', 'meanthr', 'median')

# Test for category c, high stability
act_prob <- c(138, 145, 96, 105, 144)
c_chi <-
  chisq.test(act_prob, p = exp_prob)
c_res <- c_chi$stdres
names(c_res) <- c('mean', 'meanw', 'meansup', 'meanthr', 'median')

# Test for category d, high stability
act_prob <- c(401, 423, 445, 377, 318)
d_chi <-
  chisq.test(act_prob, p = exp_prob)
d_res <- d_chi$stdres
names(d_res) <- c('mean', 'meanw', 'meansup', 'meanthr', 'median')

# data frame of residuals
res <- list(a_res, b_res, c_res, d_res)
names(res) <- c('a', 'b', 'c', 'd')
res_df <- bind_rows(res, .id = 'category') %>%
  pivot_longer(cols = mean:median,
               names_to = 'models',
               values_to = 'residuals')

# plot of residuals by model and change category
p_res <-
  ggplot(res_df, aes(x = models, fill = category, y = residuals)) +
  geom_col(position = 'dodge') +
  scale_fill_manual(
    values = c("#36B677", "#25818E", "#3D4988", "#440154"),
    labels = c(
      'Low stability (a)',
      'Low gain (b)',
      'High stability (c)',
      'High gain (d)'
    ),
    name = NULL
  ) +
  theme_pub(base_size = 30,
            base_family = '',
            legend_size = 30) +
  labs(x = '', y = 'Standardized Residuals') +
  geom_hline(yintercept = 2, linetype = 'dotted', size = 1) +
  geom_hline(yintercept = -2, linetype = 'dotted', size = 1) +
  geom_hline(yintercept = 0)

ggsave(
  p_res,
  file = 'G:/My Drive/Dissertation/003_Ensemble_modeling/figures/standardize_res.jpeg',
  height = 10,
  width = 14,
  dpi = 300
)


# by climate change scenario

exp_prob <- rep(.2, 5)

# CNRM CM5 RCP 4.5
# Test for category a, low stability
act_prob <- c(5, 3, 4, 7, 7)
a_chi <-
  chisq.test(act_prob, p = exp_prob)
a_res <- a_chi$stdres
names(a_res) <- c('mean', 'meanw', 'meansup', 'meanthr', 'median')

# Test for category b, low gain
act_prob <- c(20, 15, 13, 24, 30)
b_chi <-
  chisq.test(act_prob, p = exp_prob)
b_res <- b_chi$stdres
names(b_res) <- c('mean', 'meanw', 'meansup', 'meanthr', 'median')

# Test for category c, high stability
act_prob <- c(17, 18, 14, 16, 18)
c_chi <-
  chisq.test(act_prob, p = exp_prob)
c_res <- c_chi$stdres
names(c_res) <- c('mean', 'meanw', 'meansup', 'meanthr', 'median')

# Test for category d, high stability
act_prob <- c(53, 55, 57, 50, 41)
d_chi <-
  chisq.test(act_prob, p = exp_prob)
d_res <- d_chi$stdres
names(d_res) <- c('mean', 'meanw', 'meansup', 'meanthr', 'median')

# data frame of residuals
cnrm45_res <- list(a_res, b_res, c_res, d_res)
names(cnrm45_res) <- c('a', 'b', 'c', 'd')
cnrm45_res_res_df <- bind_rows(cnrm45_res, .id = 'category') %>%
  pivot_longer(cols = mean:median,
               names_to = 'models',
               values_to = 'residuals') %>%
  mutate(scen = 'cnrm',
         rcp = 'rcp45')


# CNRM CM5 RCP 8.5
# Test for category a, low stability
act_prob <- c(12, 10, 12, 23, 20)
a_chi <-
  chisq.test(act_prob, p = exp_prob)
a_res <- a_chi$stdres
names(a_res) <- c('mean', 'meanw', 'meansup', 'meanthr', 'median')

# Test for category b, low gain
act_prob <- c(19, 14, 13, 21, 27)
b_chi <-
  chisq.test(act_prob, p = exp_prob)
b_res <- b_chi$stdres
names(b_res) <- c('mean', 'meanw', 'meansup', 'meanthr', 'median')

# Test for category c, high stability
act_prob <- c(18, 19, 11, 12, 19)
c_chi <-
  chisq.test(act_prob, p = exp_prob)
c_res <- c_chi$stdres
names(c_res) <- c('mean', 'meanw', 'meansup', 'meanthr', 'median')

# Test for category d, high stability
act_prob <- c(53, 53, 54, 49, 42)
d_chi <-
  chisq.test(act_prob, p = exp_prob)
d_res <- d_chi$stdres
names(d_res) <- c('mean', 'meanw', 'meansup', 'meanthr', 'median')

# data frame of residuals
cnrm85_res <- list(a_res, b_res, c_res, d_res)
names(cnrm85_res) <- c('a', 'b', 'c', 'd')
cnrm85_res_res_df <- bind_rows(cnrm85_res, .id = 'category') %>%
  pivot_longer(cols = mean:median,
               names_to = 'models',
               values_to = 'residuals') %>%
  mutate(scen = 'cnrm',
         rcp = 'rcp85')


# Hades RCP 4.5
# Test for category a, low stability
act_prob <- c(6, 4, 4, 9, 10)
a_chi <-
  chisq.test(act_prob, p = exp_prob)
a_res <- a_chi$stdres
names(a_res) <- c('mean', 'meanw', 'meansup', 'meanthr', 'median')

# Test for category b, low gain
act_prob <- c(20, 16, 16, 25, 34)
b_chi <-
  chisq.test(act_prob, p = exp_prob)
b_res <- b_chi$stdres
names(b_res) <- c('mean', 'meanw', 'meansup', 'meanthr', 'median')

# Test for category c, high stability
act_prob <- c(16, 15, 10, 12, 19)
c_chi <-
  chisq.test(act_prob, p = exp_prob)
c_res <- c_chi$stdres
names(c_res) <- c('mean', 'meanw', 'meansup', 'meanthr', 'median')

# Test for category d, high stability
act_prob <- c(46, 50, 54, 45, 37)
d_chi <-
  chisq.test(act_prob, p = exp_prob)
d_res <- d_chi$stdres
names(d_res) <- c('mean', 'meanw', 'meansup', 'meanthr', 'median')

# data frame of residuals
hades45_res <- list(a_res, b_res, c_res, d_res)
names(hades45_res) <- c('a', 'b', 'c', 'd')
hades45_res_res_df <- bind_rows(hades45_res, .id = 'category') %>%
  pivot_longer(cols = mean:median,
               names_to = 'models',
               values_to = 'residuals')  %>%
  mutate(scen = 'hades',
         rcp = 'rcp45')

# Hades RCP 8.5
# Test for category a, low stability
act_prob <- c(14, 14, 15, 18, 20)
a_chi <-
  chisq.test(act_prob, p = exp_prob)
a_res <- a_chi$stdres
names(a_res) <- c('mean', 'meanw', 'meansup', 'meanthr', 'median')

# Test for category b, low gain
act_prob <- c(16, 13, 14, 27, 28)
b_chi <-
  chisq.test(act_prob, p = exp_prob)
b_res <- b_chi$stdres
names(b_res) <- c('mean', 'meanw', 'meansup', 'meanthr', 'median')

# Test for category c, high stability
act_prob <- c(27, 29, 16, 19, 23)
c_chi <-
  chisq.test(act_prob, p = exp_prob)
c_res <- c_chi$stdres
names(c_res) <- c('mean', 'meanw', 'meansup', 'meanthr', 'median')

# Test for category d, high gain
act_prob <- c(54, 53, 58, 51, 45)
d_chi <-
  chisq.test(act_prob, p = exp_prob)
d_res <- d_chi$stdres
names(d_res) <- c('mean', 'meanw', 'meansup', 'meanthr', 'median')

# data frame of residuals
hades85_res <- list(a_res, b_res, c_res, d_res)
names(hades85_res) <- c('a', 'b', 'c', 'd')
hades85_res_res_df <- bind_rows(hades85_res, .id = 'category') %>%
  pivot_longer(cols = mean:median,
               names_to = 'models',
               values_to = 'residuals') %>%
  mutate(scen = 'hades',
         rcp = 'rcp85')


# combining residual data frames from each scenario
full_res_df <- bind_rows(cnrm45_res_res_df, cnrm85_res_res_df, hades45_res_res_df, hades85_res_res_df)

# plot of residuals by model and change category

big.label <- c(
  'cnrm' = 'CNRM-CM5',
  'hades' = 'HadGEM2-ES',
  'rcp45' = 'RCP 4.5',
  'rcp85' = 'RCP 8.5'
)

p_res <-
  ggplot(full_res_df, aes(x = models, fill = category, y = residuals)) +
  geom_col(position = 'dodge') +
  scale_fill_manual(
    values = c("#36B677", "#25818E", "#3D4988", "#440154"),
    labels = c(
      'Low stability (a)',
      'Low gain (b)',
      'High stability (c)',
      'High gain (d)'
    ),
    name = NULL
  ) +
  theme_pub(base_size = 40,
            base_family = '',
            legend_size = 50) +
  labs(x = '', y = 'Standardized Residuals') +
  geom_hline(yintercept = 2, linetype = 'dotted', size = 1) +
  geom_hline(yintercept = -2, linetype = 'dotted', size = 1) +
  geom_hline(yintercept = 0) +
  theme(legend.direction = 'horizontal',
        legend.position = 'bottom') +
  facet_grid(vars(rcp), vars(scen), labeller = as_labeller(big.label)) + coord_flip()

ggsave(
  p_res,
  file = 'G:/My Drive/Dissertation/003_Ensemble_modeling/figures/standardize_res_by_cc_scen.jpeg',
  height = 15,
  width = 25,
  dpi = 300
)


ggsave(
  p_res,
  file = '/Volumes/GoogleDrive/My Drive/Dissertation/003_Ensemble_modeling/figures/standardize_res_by_cc_scen.jpeg',
  height = 15,
  width = 25,
  dpi = 300
)

