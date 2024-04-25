############################################################
#                                                          #
#        Data Analysis for Uncertainty in Exposure         #
#                                                          #
############################################################


{
  require(dplyr)
  require(ggplot2)
  require(viridis)
  require(tidyr)
  require(data.table)
  require(parallel)
  require(doParallel)
  require(readr)
  require(BAMMtools)
}



# species traits calculated for chapter 1 (range size, niche breadth, prevalenc, geographic traits, etc.)
sp_traits <- data.table::fread('G:/My Drive/Franklin_grant/project/1-NSF_spatial_and_species_traits/2_Outputs/3_Exposure/sp_traits_data.csv') %>% as_tibble() %>% dplyr::select(-model)

# .txt files with exposure measures for all species * GCMs * RCPs * ensemble method * dispersal scenario
exp <- "G:/My Drive/Dissertation/003_Ensemble_modeling/data/03_exposure/" %>% list.files(pattern = ".txt", full.names = TRUE)
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

# data frame for including with manuscript materials
exp2 <- exp %>% dplyr::select(sp, model, year, scen, rcp, current_km2, nd_future_km2, fd_future_km2, nd_change_prop, fd_change_prop) %>%
  rename(species = sp,
         gcm = scen,
         nd_hs_change = nd_change_prop,
         fd_hs_change = fd_change_prop)

writexl::write_xlsx(exp2, 'exposure_data.xls')

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
    insitu_prop <= .2,
    'a',
    ifelse(
      insitu_prop >= 1,
      'c',
      ifelse(
        exsitu_prop <= .2,
        'b',
        ifelse(exsitu_prop >= 1, 'd', 'Not extreme')
      )
    )
  ))


sum_exp <- exp %>%
  group_by(model, change_cat) %>%
  dplyr::summarise(count = n())

# pivot longer to have one habitat suitability change column and 5 factor columns
exp2 <- exp %>%
  pivot_longer(cols = nd_change_prop:fd_change_prop,
               names_to = 'dispersal',
               values_to = 'hs_change') %>%
  dplyr::select(sp, model, rcp, scen, dispersal, year, hs_change) 

exp2_sd <- exp2 %>%
  dplyr::group_by(sp, dispersal) %>%
  dplyr::summarize(sd = sd(hs_change))

exp2 <- left_join(exp2, exp2_sd, by = c('sp', 'dispersal'))

aov1 <- aov(hs_change ~ model / rcp, exp2)

write_csv(exp2 %>% dplyr::select(-sd), 'G:/My Drive/Dissertation/003_Ensemble_modeling/data/hs_change.csv')

exp_traits <- left_join(exp2, sp_traits, by = c('sp'= 'species'))


############################################################
#                                                          #
#                 Some exploratory figures                 #
#                                                          #
############################################################

# Labelling for plotting
big.label <- c('fd_change_prop' = 'Full dispersal',
               'nd_change_prop' = 'Null dispersal',
               '2055' = '2055',
               '2085' = '2085')

fig1 <-
  ggplot(
    exp_traits %>% filter(
      sp %in% c(
        'Arctostaphylos rudis',
      #  'Abies magnifica',
      #  'Pinus jeffreyi',
        'Quercus engelmannii'
      ) & dispersal == 'nd_change_prop'
    ),
    aes(
      x = sp,
      y = hs_change,
      fill = scen
    )
  ) + geom_boxplot() +
  facet_wrap(vars(model))




fig2 <-
  ggplot(
    exp_traits %>% filter(
      sp %in% c(
        'Arctostaphylos rudis',
        #  'Abies magnifica',
        #  'Pinus jeffreyi',
        'Quercus engelmannii'
      ) & dispersal == 'nd_change_prop'
    ),
    aes(
      x = model,
      y = hs_change
    )
  ) + geom_boxplot() + facet_wrap(vars(sp))



fig3 <-
  ggplot(exp_traits %>% filter(dispersal == 'nd_change_prop'),
         aes(x = model,
             y = hs_change)) + geom_boxplot() + facet_wrap(vars(range_size))

