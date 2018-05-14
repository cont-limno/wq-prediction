# Prepare data for predicting water quality

# ----load_packages ----
library(dplyr)
library(ggplot2)

# ----load_lagos ----
library(LAGOSNE)

lg <- lagosne_load("1.087.1")

# ----select_epi_nutr_variables ----
# variables: TP, TN, chlorophyll, color(true), Secchi
epi_nutr <- lg$epi_nutr
secchi <- lg$secchi

# ----filter_to_common_time_period----
# time period: 1980 - 2011, using samples collected between Jun 15 and Sep 15 (stratification period)
# define time parameters
first_year <- 1980
last_year <- 2010
first_day <- '0615' #i.e., '0615' for Jun 15
last_day <- '0915'

# convert sampledate to date format and create "monthday" column to filter by day
epi_nutr$sampledate <- as.Date(epi_nutr$sampledate, format="%m/%d/%Y")
secchi$sampledate <- as.Date(secchi$sampledate, format="%m/%d/%Y")

epi_nutr$monthday <- format(epi_nutr$sampledate, format="%m%d")
secchi$monthday <- format(secchi$sampledate, format="%m%d")

# ----calculate_tn----
# from componenent variables and join with tn into a new column called "tn_combined"
epi_nutr$tn_calculated <- epi_nutr$tkn + epi_nutr$no2no3
epi_nutr$tn_combined <- epi_nutr$tn
epi_nutr$tn_combined[which(is.na(epi_nutr$tn_combined) == TRUE)] = epi_nutr$tn_calculated[which(is.na(epi_nutr$tn_combined) == TRUE)]

# subset by sample date and year cutoffs specified above
epi_nutr_subset <- epi_nutr[epi_nutr$monthday >= first_day & epi_nutr$monthday <= last_day,]
epi_nutr_subset <- subset(epi_nutr_subset, sampleyear >= first_year & sampleyear <= last_year)

secchi_subset <- secchi[secchi$monthday >= first_day & secchi$monthday <= last_day,]
secchi_subset <- subset(secchi_subset, sampleyear >= first_year & sampleyear <= last_year)


# get rid of duplicate values
epi_nutr_dTN<- epi_nutr_subset[!duplicated(paste(epi_nutr_subset$lagoslakeid, epi_nutr_subset$sampledate, epi_nutr_subset$tn_combined)),]
epi_nutr_dTP<- epi_nutr_dTN[!duplicated(paste(epi_nutr_dTN$lagoslakeid, epi_nutr_dTN$sampledate, epi_nutr_dTN$tp)),]
epi_nutr_dchl<- epi_nutr_dTP[!duplicated(paste(epi_nutr_dTP$lagoslakeid, epi_nutr_dTP$sampledate, epi_nutr_dTP$chla)),]
epi_nutr_d<- epi_nutr_dchl[!duplicated(paste(epi_nutr_dchl$lagoslakeid, epi_nutr_dchl$sampledate, epi_nutr_dchl$colort)),]

secchi_subset_d<- secchi_subset[!duplicated(paste(secchi_subset$lagoslakeid, secchi_subset$sampledate, secchi_subset$secchi)),]

# ----calculate_medians ----

secchi_subset_medians <- secchi_subset_d %>%
  group_by(lagoslakeid, sampleyear) %>%
  summarise(nSamples_secchi = n(), secchi=median(secchi))

epi_nutr_subset_medians <- epi_nutr_d %>%
  group_by(lagoslakeid, sampleyear) %>%
  summarise(TP=median(tp), chla=median(chla), color=median(colort), TN=median(tn_combined))

# get number of samples for non-Secchi variables
# TP_counts <- aggregate(epi_nutr_subset$tp, by=list(epi_nutr_subset$lagoslakeid, epi_nutr_subset$sampleyear),
#                        FUN='length')
# colnames(TP_counts) <- c('lagoslakeid', 'sampleyear', 'nSamples_TP')
#
# TN_counts <- aggregate(epi_nutr_subset$tn, by=list(epi_nutr_subset$lagoslakeid, epi_nutr_subset$sampleyear),
#                        FUN='length')
# colnames(TN_counts) <- c('lagoslakeid', 'sampleyear', 'nSamples_TN')
# color_counts <- aggregate(epi_nutr_subset$colort, by=list(epi_nutr_subset$lagoslakeid, epi_nutr_subset$sampleyear),
#                           FUN='length')
# colnames(color_counts) <- c('lagoslakeid', 'sampleyear', 'nSamples_color')
# chla_counts <- aggregate(epi_nutr_subset$chla, by=list(epi_nutr_subset$lagoslakeid, epi_nutr_subset$sampleyear),
#                          FUN='length')
# colnames(chla_counts) <- c('lagoslakeid', 'sampleyear', 'nSamples_chla')

# calculate median of the medians
secchi_subset_full_record_medians <- secchi_subset_medians %>%
  group_by(lagoslakeid) %>%
  summarise(nYears_secchi = n(), secchi=median(secchi))

epi_nutr_subset_full_record_medians <- epi_nutr_subset_medians %>%
  group_by(lagoslakeid) %>%
  summarise(TP=median(TP), chla=median(chla), color=median(color), TN=median(TN))

# get number of years of data for non-Secchi variables
TP_years <- aggregate(epi_nutr_subset_medians$TP, by=list(epi_nutr_subset_medians$lagoslakeid),
                        FUN='length')
colnames(TP_years) <- c('lagoslakeid', 'nYears_TP')

TN_years <- aggregate(epi_nutr_subset_medians$TN, by=list(epi_nutr_subset_medians$lagoslakeid),
                       FUN='length')
colnames(TN_years) <- c('lagoslakeid', 'nYears_TN')

color_years <- aggregate(epi_nutr_subset_medians$color, by=list(epi_nutr_subset_medians$lagoslakeid),
                          FUN='length')
colnames(color_years) <- c('lagoslakeid', 'nYears_color')

chla_years <- aggregate(epi_nutr_subset_medians$chla, by=list(epi_nutr_subset_medians$lagoslakeid),
                         FUN='length')
colnames(chla_years) <- c('lagoslakeid', 'nYears_chla')


## create final table of limno data
limno_data_table <- Reduce(inner_join, list(secchi_subset_full_record_medians, epi_nutr_subset_full_record_medians,
                                            TP_years, TN_years, color_years, chla_years))

limno_data_table <- limno_data_table[,c(1,3:7,2,8:11)] #rearrange column by number

# ----pull_geo_predictors (for all lakes)----
# max depth
# lulc
# hydrology
# watershed/lake morphometry
# connectivity

