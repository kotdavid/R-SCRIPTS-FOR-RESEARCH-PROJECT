remove(list = ls())
# Load libraries
library(sf)
library(dplyr)
library(haven)
library(ggplot2)
library(elevatr)
library(cubelyr)
library(elevatr)
library(parallel)
library(pbapply)
library(stars)
library(starsExtra)
library(terra)
library(viridis)
library(fixest)
library(tidyr)
library(starsExtra)

# Load map of South African communities
setwd("")  ### I HAVE REMOVED THE WORKING DIRECTORY (DATA SOURCE)  ##########

zaf <- read_sf("")  ### I HAVE REMOVED THE WORKING DIRECTORY (DATA SOURCE)  ##########

ggplot() + geom_sf(data = zaf)
# Select the communities in the province of KwaZulu-Natal
shp <- zaf %>% filter(PROVINCE %in% "KZN")
ggplot() + geom_sf(data = zaf) + geom_sf(data = shp, fill = "red")

################################################################################
# 1) Download world elevation data and select South Africa
################################################################################
# Note larger values of z correspond to smaller size of grid cells
# We set z = 3 to reduce computational time
# To get more accurate statistics, set z >= 5 (the larger the better)
elev <- get_elev_raster(shp, z = 3, verbose = T, clip = "bbox")
# Have a look to how a raster looks like
sp::plot(elev)
# Compute elevation of a cell with respect to all neighboring cells (8 - queen contiguity)
slope <- terrain(elev, "slope", neighbors=8, unit="degrees")
# Transform slope into an object compatible with sf
star_slope <- stars::st_as_stars(slope)
# Transform slope into a sf object
sf_star_slope <- st_as_sf(star_slope) 

################################################################################
# 2) Compute the average slope of each district
################################################################################
# Find which cell is in each district
shp_s <- st_join(shp, sf_star_slope)
# Average value of slope in each community
shp_avg_s <- shp_s %>%
  st_drop_geometry() %>%
  group_by(DISTRICT, FID) %>%
  summarise(slope = mean(slope, na.rm = T)) %>%
  ungroup; shp_avg_s

shp <- left_join(shp, shp_avg_s)

ggplot() + geom_sf(data = shp, aes(fill = slope))
# Higher gradient raises the average cost of a household connection, 
# making gradient an important factor in prioritizing areas for electrification.

################################################################################
# 3) Find roads in each district and compute their length
################################################################################
# Find where water basins are located
roads <- st_read("roads.shp"); roads
p <- ggplot() + geom_sf(data = zaf) + geom_sf(data = roads)
ggsave(filename = "roads.pdf", p)
roads_shp <- st_join(roads, shp) %>% 
  filter(!is.na(FID)); roads_shp
# collapse roads within a community into one object
roads_shp <- roads_shp %>% 
  group_by(DISTRICT, FID) %>% 
  summarize(geometry = st_union(geometry)) %>%
  ungroup()
# Compute basins length
# Warning: for large areas such as India, no single map projection preserves distance in every direction 
# The correct approach would be to project each district to appropriate UTM projection
length_roads <- st_length(roads_shp)
length_roads <- as.numeric(length_roads)
roads_shp <- roads_shp %>%
  mutate(length_roads = length_roads)

shp <- left_join(shp, st_drop_geometry(roads_shp) )
shp <- shp %>% 
  mutate(length_roads = ifelse(is.na(length_roads), 0, length_roads))
ggplot() + geom_sf(data = shp, aes(fill = length_roads))

################################################################################
# 4) Descriptive analysis
################################################################################
remove(list = ls())
db <- read_dta("matched_censusdata.dta")

# Motivating evidence
eskom <- db %>% filter(T==1)
noproj <- db %>% filter(T==0)

# Observe the difference between employment rates in 
# communities involved/not involved in the eskom project
# in specific years
# For female
t.test(eskom$prop_emp_f0, noproj$prop_emp_f0) # in 1996
t.test(eskom$prop_emp_f1, noproj$prop_emp_f1) # in 2001
# For Male
t.test(eskom$prop_emp_m0, noproj$prop_emp_m0) # in 1996
t.test(eskom$prop_emp_m1, noproj$prop_emp_m1) # in 2001

# Takeaway
# Employment rates are very low for men and women 

# Observe the difference between employment rates in 
# communities involved/not involved in the eskom project
# across time
t.test(eskom$d_prop_emp_f, noproj$d_prop_emp_f)
t.test(eskom$d_prop_emp_m, noproj$d_prop_emp_m)

# Takeaway
# Employment rates falling—and falling faster—for men in electrified areas
# between 1996 and 2001

# Observe the difference in household home consumption in communities involved in
# eskom project between 1996 and 2001
eskom1996 <- eskom %>% 
  select(spcode,
         prop_candles0,
         prop_wood0,
         prop_eleccook0,
         prop_elec0) %>% 
  pivot_longer(!spcode) %>% 
  mutate(name = case_when(name %in% "prop_elec0" ~ "Electric lighting",
                          name %in% "prop_wood0" ~ "Wood cooking",
                          name %in% "prop_eleccook0" ~ "Electric cooking",
                          name %in% "prop_candles0" ~ "Candles"),
         year = 1996,
         community = "Eskom") %>% 
  rename(`type of fuel in houshold consumtion` = name)

eskom2001 <- eskom %>% 
  select(spcode,
         prop_candles1,
         prop_wood1,
         prop_eleccook1,
         prop_elec1) %>% 
  pivot_longer(!spcode) %>% 
  mutate(name = case_when(name %in% "prop_elec1" ~ "Electric lighting",
                          name %in% "prop_wood1" ~ "Wood cooking",
                          name %in% "prop_eleccook1" ~ "Electric cooking",
                          name %in% "prop_candles1" ~ "Candles"),
         year = 2001,
         community = "Eskom") %>% 
  rename(`type of fuel in houshold consumtion` = name)

# Observe the difference in household home consumption in communities not involved in
# eskom project between 1996 and 2001
noproj1996 <- noproj %>% 
  select(spcode,
         prop_candles0,
         prop_wood0,
         prop_eleccook0,
         prop_elec0) %>% 
  pivot_longer(!spcode) %>% 
  mutate(name = case_when(name %in% "prop_elec0" ~ "Electric lighting",
                          name %in% "prop_wood0" ~ "Wood cooking",
                          name %in% "prop_eleccook0" ~ "Electric cooking",
                          name %in% "prop_candles0" ~ "Candles"),
         year = 1996,
         community = "No Eskom") %>% 
  rename(`type of fuel in houshold consumtion` = name)

noproj2001 <- noproj %>% 
  select(spcode,
         prop_candles1,
         prop_wood1,
         prop_eleccook1,
         prop_elec1) %>% 
  pivot_longer(!spcode) %>% 
  mutate(name = case_when(name %in% "prop_elec1" ~ "Electric lighting",
                          name %in% "prop_wood1" ~ "Wood cooking",
                          name %in% "prop_eleccook1" ~ "Electric cooking",
                          name %in% "prop_candles1" ~ "Candles"),
         year = 2001,
         community = "No Eskom") %>% 
  rename(`type of fuel in houshold consumtion` = name)


diff_consum <- bind_rows(eskom1996, eskom2001, noproj1996, noproj2001)
diff_consum <- diff_consum %>% 
  group_by(year, community, `type of fuel in houshold consumtion`) %>%
  summarise(`% of household` = mean(value, na.rm = T)) %>% 
  ungroup %>% 
  mutate(year = as.factor(year))
  
ggplot(diff_consum, aes(y = `% of household`, x = `type of fuel in houshold consumtion`, fill = year)) +
  geom_bar(position="dodge", stat="identity") +
  facet_grid(. ~ community)

# It seems that households make large adjustments to their home production technologies 
# in the wake of household electrification.
# In combination with the finding on the rising female employment,
# it seems that an important channel through
# which electricity affects the rural labor market is by 
# "freeing up" women's time for the market. 

################################################################################
# 5) First stage analysis: Assignment of Electricity Projects to Communities
################################################################################

db_cens <- db %>% filter(largeareas == 1)

# Interpretation of variables
# Dependent variable
# T: Eskom project (1 = Yes; 0 = No)

# Target variable
# mean_grad_new: avg gradient in the community

# Control variables
# kms_to_subs0: Kilometers to grid
# baseline_hhdens0: Household density
# base_hhpovrate0: Poverty rate
# prop_head_f_a0: Female-headed HHs
# sexratio0_a: Adult sex ratio
# prop_indianwhite0: Proportion of Indian and white adults in the population
# kms_to_road0: Kilometers to road
# kms_to_town0: Kilometers to town
# prop_matric_m0: Men with high school
# prop_matric_f0: Women with high school
# d_prop_waterclose: Change in water access from 1996 to 2001
# d_prop_flush: Change in toilet access from 1996 to 2001

fit1_1 <- feols(T ~ mean_grad_new, data = db_cens, cluster = "placecode0")
summary(fit1_1)
fit1_2 <- feols(T ~ mean_grad_new + kms_to_subs0 + baseline_hhdens0 + base_hhpovrate0 + prop_head_f_a0 + sexratio0_a + prop_indianwhite0 + kms_to_road0 + kms_to_town0 + prop_matric_m0 + prop_matric_f0, data = db_cens, cluster = "placecode0")
summary(fit1_2)

# Remember: The inclusion of district fixed effects in this first stage is important, 
# as a large amount of the variation in gradient in communities comes from cross-district variation.
# By controlling for district fixed effects,
# we compare communities that are in the same local labor market, 
# but which are slightly flatter or steeper.
fit1_3 <- feols(T ~ mean_grad_new + kms_to_subs0 + baseline_hhdens0 + base_hhpovrate0 + prop_head_f_a0 + sexratio0_a + prop_indianwhite0 + kms_to_road0 + kms_to_town0 + prop_matric_m0 + prop_matric_f0| dccode0, data = db_cens, cluster = "placecode0")
summary(fit1_3)
fit1_4 <- feols(T ~ mean_grad_new + kms_to_subs0 + baseline_hhdens0 + base_hhpovrate0 + prop_head_f_a0 + sexratio0_a + prop_indianwhite0 + kms_to_road0 + kms_to_town0 + prop_matric_m0 + prop_matric_f0 + d_prop_waterclose + d_prop_flush| dccode0, data = db_cens, cluster = "placecode0")
summary(fit1_4)

################################################################################
# 6) Second stage analysis: treatment effect on employment
################################################################################

# Employment results for women: OLS
fit2_1 <- feols(d_prop_emp_f ~ T, data = db_cens, cluster = "placecode0")
summary(fit2_1) # this is in line with descriptive statistics:
# there is no significant growth in female employment across project and nonproject areas
fit2_2 <- feols(d_prop_emp_f ~ T + base_hhpovrate0 + prop_head_f_a0 + sexratio0_a + kms_to_subs0 + baseline_hhdens0 + prop_indianwhite0 + kms_to_road0 + kms_to_town0 + prop_matric_m0 + prop_matric_f0, data = db_cens, cluster = "placecode0")
summary(fit2_2)
# We include district fixed effects allows us to identify the effect of 
# electrification to compare slightly steeper to slightly
# flatter areas within the same local labor market.
fit2_3 <- feols(d_prop_emp_f ~ T + base_hhpovrate0 + prop_head_f_a0 + sexratio0_a + kms_to_subs0 + baseline_hhdens0 + prop_indianwhite0 + kms_to_road0 + kms_to_town0 + prop_matric_m0 + prop_matric_f0| dccode0, data = db_cens, cluster = "placecode0")
summary(fit2_3)
# Employment results for women: IV
fit2_4 <- feols(d_prop_emp_f ~ 1|T ~ mean_grad_new, data = db_cens, cluster = "placecode0")
summary(fit2_4)
fit2_5 <- feols(d_prop_emp_f ~ base_hhpovrate0 + prop_head_f_a0 + sexratio0_a + kms_to_subs0 + baseline_hhdens0 + prop_indianwhite0 + kms_to_road0 + kms_to_town0 + prop_matric_m0 + prop_matric_f0|T ~ mean_grad_new, data = db_cens, cluster = "placecode0")
summary(fit2_5)
fit2_6 <- feols(d_prop_emp_f ~ base_hhpovrate0 + prop_head_f_a0 + sexratio0_a + kms_to_subs0 + baseline_hhdens0 + prop_indianwhite0 + kms_to_road0 + kms_to_town0 + prop_matric_m0 + prop_matric_f0| dccode0|T ~ mean_grad_new, data = db_cens, cluster = "placecode0")
summary(fit2_6)
fit2_7 <- feols(d_prop_emp_f ~ base_hhpovrate0 + prop_head_f_a0 + sexratio0_a + kms_to_subs0 + baseline_hhdens0 + prop_indianwhite0 + kms_to_road0 + kms_to_town0 + prop_matric_m0 + prop_matric_f0 + d_prop_waterclose + d_prop_flush| dccode0|T ~ mean_grad_new, data = db_cens, cluster = "placecode0")
summary(fit2_7)
# Note: IV estimates of electrification are substantially larger than 
# OLS estimates and significantly positive

# The IV results suggest that in a nonelectrified community with 
# the median number of adult women in 1996 
# N = 285
# a 9 percentage point increase in female employment raises 
# the number of women working by a number of woment equal to
# 285/100*9
# If we assume this 9 percentage point increase applies to the entire group of 
# electrified communities (rather than marginal communities only, i.e. those treated in the considered period), 
# this translates into an increase of approximately 15,000 newly employed women out of the baseline
# female population of 165,637.

# Employment results for men: OLS
fit3_1 <- feols(d_prop_emp_m ~ T, data = db_cens, cluster = "placecode0")
summary(fit3_1) # this is in line with descriptive statistics
# there is a significant decrease in male employment across project and nonproject areas
fit3_2 <- feols(d_prop_emp_m ~ T + base_hhpovrate0 + prop_head_f_a0 + sexratio0_a + kms_to_subs0 + baseline_hhdens0 + prop_indianwhite0 + kms_to_road0 + kms_to_town0 + prop_matric_m0 + prop_matric_f0, data = db_cens, cluster = "placecode0")
summary(fit3_2)
fit3_3 <- feols(d_prop_emp_m ~ T + base_hhpovrate0 + prop_head_f_a0 + sexratio0_a + kms_to_subs0 + baseline_hhdens0 + prop_indianwhite0 + kms_to_road0 + kms_to_town0 + prop_matric_m0 + prop_matric_f0| dccode0, data = db_cens, cluster = "placecode0")
summary(fit3_3)
# Employment results for men: IV
fit3_4 <- feols(d_prop_emp_m ~ 1|T ~ mean_grad_new, data = db_cens, cluster = "placecode0")
summary(fit3_4)
fit3_5 <- feols(d_prop_emp_m ~ base_hhpovrate0 + prop_head_f_a0 + sexratio0_a + kms_to_subs0 + baseline_hhdens0 + prop_indianwhite0 + kms_to_road0 + kms_to_town0 + prop_matric_m0 + prop_matric_f0|T ~ mean_grad_new, data = db_cens, cluster = "placecode0")
summary(fit3_5)
fit3_6 <- feols(d_prop_emp_m ~ base_hhpovrate0 + prop_head_f_a0 + sexratio0_a + kms_to_subs0 + baseline_hhdens0 + prop_indianwhite0 + kms_to_road0 + kms_to_town0 + prop_matric_m0 + prop_matric_f0| dccode0|T ~ mean_grad_new, data = db_cens, cluster = "placecode0")
summary(fit3_6)
fit3_7 <- feols(d_prop_emp_m ~ base_hhpovrate0 + prop_head_f_a0 + sexratio0_a + kms_to_subs0 + baseline_hhdens0 + prop_indianwhite0 + kms_to_road0 + kms_to_town0 + prop_matric_m0 + prop_matric_f0 + d_prop_waterclose + d_prop_flush| dccode0|T ~ mean_grad_new, data = db_cens, cluster = "placecode0")
summary(fit3_7)
# Note: Male employment increases by a substantially smaller value with respect to 
# female employment when using IV estimates, and this is not significantly different from zero

################################################################################
# 7) Ruling out demand shocks
################################################################################

# If employment rates in steep and flat areas evolve differently 
# even in the absence of new electricity, the gradient IV would be invalid. 
# This is difficult to check directly. 
# Instead, we implement an indirect placebo test using 
# historical administrative data on electricity projects:
# areas that are electrified prior to 1996, which were excluded from the main analysis.
# For these areas, there should be no relationship between 
# gradient and employment growth between 1996 and 2001,
# since they have already received an electricity project.
db_plac <- read_dta("placebodata.dta")
db_plac_cens <- db_plac %>% filter(largeareas==1)
feols(d_prop_emp_f ~ mean_grad_new + base_hhpovrate0 + prop_head_f_a0 + sexratio0_a + kms_to_subs0 + baseline_hhdens0 + prop_indianwhite0 + kms_to_road0 + kms_to_town0 + prop_matric_m0 + prop_matric_f0| dccode0, data = db_plac)

# Takeaway
# when communities get new access to household electricity, employment
# on the extensive margin increases for women and possibly for men, although
# male effects are difficult to estimate precisely.
# Given the results of the placebo test, there
# is no strong evidence that contemporaneous expansions in sources of demand for
# female work confound these employment results

# A second potential threat to the validity of the IV strategy 
# arises if flatter communities received positive labor demand shocks 
# concurrent with electricity projects. 
# We test whether there are larger increases in the major sources of
# female labor demand in flatter communities. 
# Individual-level census data suggest that most women in these areas 
# work as teachers or domestic workers. 
# We test whether gradient is negatively correlated with growth in
# new schools or with the growth in new
# employer households 
# (proxied for by the change in fraction of Indian and white adults in the population).
feols(d_schools ~ mean_grad_new + base_hhpovrate0 + prop_head_f_a0 + sexratio0_a + kms_to_subs0 + baseline_hhdens0 + prop_indianwhite0 + kms_to_road0 + kms_to_town0 + prop_matric_m0 + prop_matric_f0| dccode0, data = db_cens)
feols(prop_indianwhite0 ~ mean_grad_new + base_hhpovrate0 + prop_head_f_a0 + sexratio0_a + kms_to_subs0 + baseline_hhdens0 + kms_to_road0 + kms_to_town0 + prop_matric_m0 + prop_matric_f0| dccode0, data = db_cens)

################################################################################
# 8) Channels
################################################################################

# Electrification and home production: A Labor Supply channel
# In order for electrification to affect employment through the channel of reduced
# time in home production, households must switch out of traditional fuels when their
# communities are connected to the grid and spend less time in home production. 

# Interpretation of variables
# Dependent variable
# d_prop_elec: Lighting with electricity
# d_prop_wood: Cooking with wood
# d_prop_eleccook: Cooking with electricity
# d_prop_waterclose: Water nearby
# d_prop_flush: Flush toilet

# OLS: no controls
feols(d_prop_elec ~ T, data = db_cens)
feols(d_prop_wood ~ T, data = db_cens)
feols(d_prop_eleccook ~ T, data = db_cens)
feols(d_prop_waterclose ~ T, data = db_cens)
feols(d_prop_flush ~ T, data = db_cens)

# OLS: with controls
feols(d_prop_elec ~ T + kms_to_subs0 + baseline_hhdens0 + base_hhpovrate0 + prop_head_f_a0 + sexratio0_a + prop_indianwhite0 + kms_to_road0 + kms_to_town0 + prop_matric_m0 + prop_matric_f0| dccode0, data = db_cens)
feols(d_prop_wood ~ T + kms_to_subs0 + baseline_hhdens0 + base_hhpovrate0 + prop_head_f_a0 + sexratio0_a + prop_indianwhite0 + kms_to_road0 + kms_to_town0 + prop_matric_m0 + prop_matric_f0| dccode0, data = db_cens)
feols(d_prop_eleccook ~ T + kms_to_subs0 + baseline_hhdens0 + base_hhpovrate0 + prop_head_f_a0 + sexratio0_a + prop_indianwhite0 + kms_to_road0 + kms_to_town0 + prop_matric_m0 + prop_matric_f0| dccode0, data = db_cens)
feols(d_prop_waterclose ~ T + kms_to_subs0 + baseline_hhdens0 + base_hhpovrate0 + prop_head_f_a0 + sexratio0_a + prop_indianwhite0 + kms_to_road0 + kms_to_town0 + prop_matric_m0 + prop_matric_f0| dccode0, data = db_cens)
feols(d_prop_flush ~ T + kms_to_subs0 + baseline_hhdens0 + base_hhpovrate0 + prop_head_f_a0 + sexratio0_a + prop_indianwhite0 + kms_to_road0 + kms_to_town0 + prop_matric_m0 + prop_matric_f0| dccode0, data = db_cens)

# IV: no controls
feols(d_prop_elec ~ 1|T ~ mean_grad_new, data = db_cens)
feols(d_prop_wood ~ 1|T ~ mean_grad_new, data = db_cens)
feols(d_prop_eleccook ~ 1|T ~ mean_grad_new, data = db_cens)
feols(d_prop_waterclose ~ 1|T ~ mean_grad_new, data = db_cens)
feols(d_prop_flush ~ 1|T ~ mean_grad_new, data = db_cens)

# IV: with controls
feols(d_prop_elec ~ kms_to_subs0 + baseline_hhdens0 + base_hhpovrate0 + prop_head_f_a0 + sexratio0_a + prop_indianwhite0 + kms_to_road0 + kms_to_town0 + prop_matric_m0 + prop_matric_f0| dccode0|T ~ mean_grad_new, data = db_cens)
feols(d_prop_wood ~ kms_to_subs0 + baseline_hhdens0 + base_hhpovrate0 + prop_head_f_a0 + sexratio0_a + prop_indianwhite0 + kms_to_road0 + kms_to_town0 + prop_matric_m0 + prop_matric_f0| dccode0|T ~ mean_grad_new, data = db_cens)
feols(d_prop_eleccook ~ kms_to_subs0 + baseline_hhdens0 + base_hhpovrate0 + prop_head_f_a0 + sexratio0_a + prop_indianwhite0 + kms_to_road0 + kms_to_town0 + prop_matric_m0 + prop_matric_f0| dccode0|T ~ mean_grad_new, data = db_cens)
feols(d_prop_waterclose ~ kms_to_subs0 + baseline_hhdens0 + base_hhpovrate0 + prop_head_f_a0 + sexratio0_a + prop_indianwhite0 + kms_to_road0 + kms_to_town0 + prop_matric_m0 + prop_matric_f0| dccode0|T ~ mean_grad_new, data = db_cens)
feols(d_prop_flush ~ kms_to_subs0 + baseline_hhdens0 + base_hhpovrate0 + prop_head_f_a0 + sexratio0_a + prop_indianwhite0 + kms_to_road0 + kms_to_town0 + prop_matric_m0 + prop_matric_f0| dccode0|T ~ mean_grad_new, data = db_cens)

# Takeaway
# Households do make large adjustments to their home production technologies 
# in the wake of household electrification.
# Results illustrate substantial shifts towards using
# electricity for home production,

# In combination with the main results —rising female employment—
# the results on changing home production suggest that one important channel through
# which electricity affects the rural labor market is by 
# "freeing up" women's time for the market. 
# This is, of course, unlikely to be the only way in which this infrastructure
# roll-out affects rural areas.

