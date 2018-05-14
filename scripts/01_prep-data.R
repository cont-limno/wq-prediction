# Prepare data for predicting water quality

# ----load_packages ----
library(dplyr)
library(ggplot2)

# ----load_lagos ----
library(LAGOSNE)

lg <- lagosne_load("1.087.1")

# ----select_epi_nutr_variables ----
# variables: TP, TN, chlorophyll, color(true), Secchi


# ----filter_to_common_time_period----
# time period: 1980 - 2011


# ----calculate_tn----
# from componenent variables


# ----calculate_medians ----
# median of each summer period
# median of the summer periods
# number of years (summer periods)


# ----pull_geo_predictors ----
# max depth
# lulc
# hydrology
# watershed/lake morphometry
# connectivity

