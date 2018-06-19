# Prepare data for predicting water quality

# ----load_packages ----
library(dplyr)
library(ggplot2)

# ----load_lagos ----
library(LAGOSNE)

lg <- lagosne_load("1.087.1")

# set working directory (comment out others' and insert your own)
working_directory <- setwd("C:/Users/FWL/Documents/wq-prediction")

# ----select_epi_nutr_variables ----
# variables: TP, TN, chlorophyll, Secchi
epi_nutr <- lg$epi_nutr
secchi <- lg$secchi

# ----filter_to_common_time_period----
# time period: 1990 - 2011, using samples collected between Jun 15 and Sep 15 (stratification period)
# define time parameters
first_year <- 1990
last_year <- 2011
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

secchi_subset_d<- secchi_subset[!duplicated(paste(secchi_subset$lagoslakeid, secchi_subset$sampledate, secchi_subset$secchi)),]

#select only useful columns for analysis (this is a final dataset for WQ1, all observations for temporal study)
epi_nutr_tptnchl<-select(epi_nutr_dchl, lagoslakeid, tp, tn_combined, chla, sampledate, sampleyear, samplemonth, monthday, eventida1087, programname)
sec<-select(secchi_subset_d, lagoslakeid, sampledate, secchi)

#merge tp,tn,chla to the secchi table
limno_data_WQ1 <- left_join(epi_nutr_tptnchl, sec, by=c("lagoslakeid","sampledate" ))

#add a column that counts up the number of variables present
limno_data_WQ1$var_num<- apply(limno_data_WQ1, 1, function(x) {
  sum(!is.na(x['tp']), !is.na(x['tn_combined']), !is.na(x['chla']), !is.na(x['secchi']) )
  }
    )

#save observations with at least 1 variable
limno_vars_WQ1<-filter(limno_data_WQ1, var_num >= 1)

#----subset of data to get one date per lake with the most variables for WQ2 -----
#give preference to the observation that has the most variables



# ----calculate_medians ---- Note: decision was made not to do this -----

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
max_depth <- data.frame(lagoslakeid=lg$lakes_limno$lagoslakeid, maxdepth_m=lg$lakes_limno$maxdepth)

# lulc (2001 NLCD)
#11=openwater
#21=developed open space
#22=developed low intensity
#23=developed medium intensity
#24=developed high intensity
#31=bare rock
#41=deciduous
#42=evergreen
#43=mixed forest
#52=scrub/shrub
#71=grasslands/herbaceous
#81=pasture/hay
#82=row crops
# Watershed: IWS
IWS_LULC <- lg$iws.lulc
IWS_LULC <- data.frame(lagoslakeid=IWS_LULC$lagoslakeid,
                       openwater2001_pct=IWS_LULC$iws_nlcd2001_pct_11,
                       developed_open2001_pct=IWS_LULC$iws_nlcd2001_pct_21,
                       developed_low2001_pct=IWS_LULC$iws_nlcd2001_pct_22,
                       developed_med2001_pct=IWS_LULC$iws_nlcd2001_pct_23,
                       developed_high2001_pct=IWS_LULC$iws_nlcd2001_pct_24,
                       barerock2001_pct=IWS_LULC$iws_nlcd2001_pct_31,
                       deciduous2001_pct=IWS_LULC$iws_nlcd2001_pct_41,
                       evergreen2001_pct=IWS_LULC$iws_nlcd2001_pct_42,
                       mixedforest2001_pct=IWS_LULC$iws_nlcd2001_pct_43,
                       scrubshrub2001_pct=IWS_LULC$iws_nlcd2001_pct_52,
                       grasslands_herbaceous2001_pct=IWS_LULC$iws_nlcd2001_pct_71,
                       pasture_hay2001_pct=IWS_LULC$iws_nlcd2001_pct_81,
                       rowcrops2001_pct=IWS_LULC$iws_nlcd2001_pct_82,
                       topo_rough_index_mean=IWS_LULC$iws_tri_mean,
                       topo_rough_index_max=IWS_LULC$iws_tri_max,
                       roaddensity_mperha=IWS_LULC$iws_roaddensity_density_mperha)

# Local buffer around lakes (100 m)
Buff100_LULC <- lg$buffer100m.lulc
Buff100_LULC <- data.frame(lagoslakeid=Buff100_LULC$lagoslakeid,
                           openwater2001_pct=Buff100_LULC$buffer100m_nlcd2001_pct_11,
                           developed_open2001_pct=Buff100_LULC$buffer100m_nlcd2001_pct_21,
                           developed_low2001_pct=Buff100_LULC$buffer100m_nlcd2001_pct_22,
                           developed_med2001_pct=Buff100_LULC$buffer100m_nlcd2001_pct_23,
                           developed_high2001_pct=Buff100_LULC$buffer100m_nlcd2001_pct_24,
                           barerock2001_pct=Buff100_LULC$buffer100m_nlcd2001_pct_31,
                           deciduous2001_pct=Buff100_LULC$buffer100m_nlcd2001_pct_41,
                           evergreen2001_pct=Buff100_LULC$buffer100m_nlcd2001_pct_42,
                           mixedforest2001_pct=Buff100_LULC$buffer100m_nlcd2001_pct_43,
                           scrubshrub2001_pct=Buff100_LULC$buffer100m_nlcd2001_pct_52,
                           grasslands_herbaceous2001_pct=Buff100_LULC$buffer100m_nlcd2001_pct_71,
                           pasture_hay2001_pct=Buff100_LULC$buffer100m_nlcd2001_pct_81,
                           rowcrops2001_pct=Buff100_LULC$buffer100m_nlcd2001_pct_82,
                           topo_rough_index_mean=Buff100_LULC$buffer100m_tri_mean,
                           topo_rough_index_max=Buff100_LULC$buffer100m_tri_max,
                           roaddensity_mperha=Buff100_LULC$buffer100m_roaddensity_density_mperha)

# Regional watershed (HU4)
# get table of HU4 and lagoslakeid
hu4_lagoslakeid_table <- data.frame(lagoslakeid=lg$lakes.geo$lagoslakeid, hu4_zoneid=lg$lakes.geo$hu4_zoneid)

HU4_LULC <- lg$hu4.lulc
HU4_LULC <- data.frame(hu4_zoneid=HU4_LULC$hu4_zoneid,
                       openwater2001_pct=HU4_LULC$hu4_nlcd2001_pct_11,
                       developed_open2001_pct=HU4_LULC$hu4_nlcd2001_pct_21,
                       developed_low2001_pct=HU4_LULC$hu4_nlcd2001_pct_22,
                       developed_med2001_pct=HU4_LULC$hu4_nlcd2001_pct_23,
                       developed_high2001_pct=HU4_LULC$hu4_nlcd2001_pct_24,
                       barerock2001_pct=HU4_LULC$hu4_nlcd2001_pct_31,
                       deciduous2001_pct=HU4_LULC$hu4_nlcd2001_pct_41,
                       evergreen2001_pct=HU4_LULC$hu4_nlcd2001_pct_42,
                       mixedforest2001_pct=HU4_LULC$hu4_nlcd2001_pct_43,
                       scrubshrub2001_pct=HU4_LULC$hu4_nlcd2001_pct_52,
                       grasslands_herbaceous2001_pct=HU4_LULC$hu4_nlcd2001_pct_71,
                       pasture_hay2001_pct=HU4_LULC$hu4_nlcd2001_pct_81,
                       rowcrops2001_pct=HU4_LULC$hu4_nlcd2001_pct_82,
                       topo_rough_index_mean=HU4_LULC$hu4_tri_mean,
                       topo_rough_index_max=HU4_LULC$hu4_tri_max,
                       roaddensity_mperha=HU4_LULC$hu4_roaddensity_density_mperha)

# hydrology and deposition (at HU 12)
# get table of hu12 and lagoslakeid (seems to have more than just lakes > 4ha; OK)
hu12_lagoslakeid_table <- data.frame(lagoslakeid=lg$lakes.geo$lagoslakeid, hu12_zoneid=lg$lakes.geo$hu12_zoneid)

HU12_hydro_dep <- lg$hu12.chag
HU12_hydro_dep <- data.frame(hu12_zoneid=HU12_hydro_dep$hu12_zoneid,
                             baseflow_mean=HU12_hydro_dep$hu12_baseflowindex_mean,
                             runoff_mean=HU12_hydro_dep$hu12_runoff_mean,
                             groundwater_recharge=HU12_hydro_dep$hu12_groundwaterrecharge_mean,
                             so4dep_1990_mean=HU12_hydro_dep$hu12_dep_so4_1990_mean,
                             so4dep_2010_mean=HU12_hydro_dep$hu12_dep_so4_2010_mean,
                             totalNdep_1990_mean=HU12_hydro_dep$hu12_dep_totaln_1990_mean,
                             totalNdep_2010_mean=HU12_hydro_dep$hu12_dep_totaln_2010_mean,
                             no3dep_1990_mean=HU12_hydro_dep$hu12_dep_no3_1990_mean,
                             no3dep_2010_mean=HU12_hydro_dep$hu12_dep_no3_2010_mean,
                             prism_ppt_mean=HU12_hydro_dep$hu12_prism_ppt_30yr_normal_800mm2_annual_mean,
                             prism_tmean_mean=HU12_hydro_dep$hu12_prism_tmean_30yr_normal_800mm2_annual_mean,
                             prism_tmax_mean=HU12_hydro_dep$hu12_prism_tmax_30yr_normal_800mm2_annual_mean,
                             prism_tmin_mean=HU12_hydro_dep$hu12_prism_tmin_30yr_normal_800mm2_annual_mean,
                             alluvial_pct=HU12_hydro_dep$hu12_surficialgeology_alluv_pct,
                             colluvial_pct=HU12_hydro_dep$hu12_surficialgeology_colluv_pct,
                             glaciofluvial_pct=HU12_hydro_dep$hu12_surficialgeology_gf_out_pct,
                             till_loam_pct=HU12_hydro_dep$hu12_surficialgeology_till_loam_pct,
                             till_sand_pct=HU12_hydro_dep$hu12_surficialgeology_till_sand_pct,
                             till_clay_pct=HU12_hydro_dep$hu12_surficialgeology_till_clay_pct)

# calculate deposition differences 1990-2010
HU12_hydro_dep$so4dep_19902010_diff <- HU12_hydro_dep$so4dep_1990_mean-HU12_hydro_dep$so4dep_2010_mean
HU12_hydro_dep$totalNdep_19902010_diff <- HU12_hydro_dep$totalNdep_1990_mean-HU12_hydro_dep$totalNdep_2010_mean
HU12_hydro_dep$no3dep_19902010_diff <- HU12_hydro_dep$no3dep_1990_mean-HU12_hydro_dep$no3dep_2010_mean

# merge in lagoslakeid
HU12_hydro_dep <- merge(HU12_hydro_dep, hu12_lagoslakeid_table, by='hu12_zoneid', all.x=F)

# bring in pre-calculated PRISM normals for other climate variables not in LAGOS
PRISM_normals <- read.csv(paste0(getwd(), "/data/lagoslakeid_PRISM_Normals_1981_2010.csv"))
PRISM_normals_t <- as.data.frame(t(PRISM_normals)) #transpose (to make columns climate variables)
colnames(PRISM_normals_t) <- PRISM_normals$Var #rename columns by climate variables
temp_lagoslakeids <- names(PRISM_normals)[3:ncol(PRISM_normals)] #get string of lagoslakeids
temp_lagoslakeids <- sub("X*", "", temp_lagoslakeids) #remove "X" from string (put in by Excel)
PRISM_normals_t <- PRISM_normals_t[-c(1,2),] #delete first two rows
PRISM_normals_t$lagoslakeid <- temp_lagoslakeids #create lagoslakeid column

hu12_prism_normals <- merge(hu12_lagoslakeid_table, PRISM_normals_t, by='lagoslakeid',all.x=F)

HU12_hydro_dep_climate <- merge(HU12_hydro_dep, hu12_prism_normals, by='hu12_zoneid',all.x=F)
HU12_hydro_dep_climate <- HU12_hydro_dep_climate[!duplicated(HU12_hydro_dep_climate$hu12_zoneid),]
vars_keep <- c(names(HU12_hydro_dep), 'lagoslakeid','winter_ppt', 'winter_tmax', 'winter_tmin',
                    'summer_ppt', 'summer_tmax', 'summer_tmin', 'spring_ppt', 'spring_tmax', 'spring_tmin')

HU12_hydro_dep_climate <- HU12_hydro_dep_climate[,vars_keep] #eliminate some climate variables
colnames(HU12_hydro_dep_climate) <- c(names(HU12_hydro_dep_climate)[1:24], 'prism_winter_ppt_mean',
                                      'prism_winter_tmax_mean','prism_winter_tmin_mean',
                                      'prism_summer_ppt_mean','prism_summer_tmax_mean',
                                      'prism_summer_tmin_mean','prism_spring_ppt_mean',
                                      'prism_spring_tmax_mean','prism_spring_tmin_mean')

# rearrange columns
HU12_hydro_dep_climate <- HU12_hydro_dep_climate[,c(24,1:23,25:33)]

# get extra climate for IWS (based on lagoslakeid in PRISM_normals_t)
IWS_extra_climate <- PRISM_normals_t[,c('lagoslakeid','winter_ppt', 'winter_tmax', 'winter_tmin',
                    'summer_ppt', 'summer_tmax', 'summer_tmin', 'spring_ppt', 'spring_tmax', 'spring_tmin')]
colnames(IWS_extra_climate) <- c('lagoslakeid', 'prism_winter_ppt_mean',
                           'prism_winter_tmax_mean','prism_winter_tmin_mean',
                           'prism_summer_ppt_mean','prism_summer_tmax_mean',
                           'prism_summer_tmin_mean','prism_spring_ppt_mean',
                           'prism_spring_tmax_mean','prism_spring_tmin_mean')
IWS_extra_climate$lagoslakeid <- as.integer(IWS_extra_climate$lagoslakeid) #convert to integer for merging later

# HU4 climate, hydrology, deposition, geology
HU4_hydro_dep <- lg$hu4.chag
HU4_hydro_dep <- data.frame(hu4_zoneid=HU4_hydro_dep$hu4_zoneid,
                            baseflow_mean=HU4_hydro_dep$hu4_baseflowindex_mean,
                            runoff_mean=HU4_hydro_dep$hu4_runoff_mean,
                            groundwater_recharge=HU4_hydro_dep$hu4_groundwaterrecharge_mean,
                            so4dep_1990_mean=HU4_hydro_dep$hu4_dep_so4_1990_mean,
                            so4dep_2010_mean=HU4_hydro_dep$hu4_dep_so4_2010_mean,
                            totalNdep_1990_mean=HU4_hydro_dep$hu4_dep_totaln_1990_mean,
                            totalNdep_2010_mean=HU4_hydro_dep$hu4_dep_totaln_2010_mean,
                            no3dep_1990_mean=HU4_hydro_dep$hu4_dep_no3_1990_mean,
                            no3dep_2010_mean=HU4_hydro_dep$hu4_dep_no3_2010_mean,
                            prism_ppt_mean=HU4_hydro_dep$hu4_prism_ppt_30yr_normal_800mm2_annual_mean,
                            prism_tmean_mean=HU4_hydro_dep$hu4_prism_tmean_30yr_normal_800mm2_annual_mean,
                            prism_tmax_mean=HU4_hydro_dep$hu4_prism_tmax_30yr_normal_800mm2_annual_mean,
                            prism_tmin_mean=HU4_hydro_dep$hu4_prism_tmin_30yr_normal_800mm2_annual_mean,
                            alluvial_pct=HU4_hydro_dep$hu4_surficialgeology_alluv_pct,
                            colluvial_pct=HU4_hydro_dep$hu4_surficialgeology_colluv_pct,
                            glaciofluvial_pct=HU4_hydro_dep$hu4_surficialgeology_gf_out_pct,
                            till_loam_pct=HU4_hydro_dep$hu4_surficialgeology_till_loam_pct,
                            till_sand_pct=HU4_hydro_dep$hu4_surficialgeology_till_sand_pct,
                            till_clay_pct=HU4_hydro_dep$hu4_surficialgeology_till_clay_pct)

# calculate deposition differences 1990-2010
HU4_hydro_dep$so4dep_19902010_diff <- HU4_hydro_dep$so4dep_1990_mean-HU4_hydro_dep$so4dep_2010_mean
HU4_hydro_dep$totalNdep_19902010_diff <- HU4_hydro_dep$totalNdep_1990_mean-HU4_hydro_dep$totalNdep_2010_mean
HU4_hydro_dep$no3dep_19902010_diff <- HU4_hydro_dep$no3dep_1990_mean-HU4_hydro_dep$no3dep_2010_mean

# watershed/lake morphometry
lake_morphometry <- data.frame(lagoslakeid=lg$iws$lagoslakeid, lakearea_ha=lg$iws$iws_lakeareaha,
                               iwsarea_ha=lg$iws$iws_ha)
lake_morphometry$IWS_ratio <- lake_morphometry$iwsarea_ha/lake_morphometry$lakearea_ha

# connectivity
IWS_conn <- lg$iws.conn
IWS_conn <- data.frame(lagoslakeid=IWS_conn$lagoslakeid, streamdensity_mperha=IWS_conn$iws_streamdensity_streams_density_mperha,
                       lakes_overlapping_area_pct=IWS_conn$iws_lakes_overlapping_area_pct,
                       wetlands_overlapping_area_pct=IWS_conn$iws_wl_allwetlandsdissolved_overlapping_area_pct)

HU4_conn <- lg$hu4.conn
HU4_conn <- data.frame(hu4_zoneid=HU4_conn$hu4_zoneid, streamdensity_mperha=HU4_conn$hu4_streamdensity_streams_density_mperha,
                       lakes_overlapping_area_pct=HU4_conn$hu4_lakes_overlapping_area_pct,
                       wetlands_overlapping_area_pct=HU4_conn$hu4_wl_allwetlandsdissolved_overlapping_area_pct)

# add hu12 and hu4 zoneid to limno data table
limno_data_table_final <- Reduce(inner_join, list(limno_data_table, hu12_lagoslakeid_table, hu4_lagoslakeid_table))



#### Save final output tables ####
# limno data
write.csv(limno_data_table_final, file=paste0(getwd(), '/data/limno_data.csv'))

# IWS (note: climate, geology, deposition and hydrology at HU12)
IWS_final_table <- Reduce(inner_join, list(IWS_LULC, IWS_conn, lake_morphometry, IWS_extra_climate, HU12_hydro_dep))
write.csv(IWS_final_table, file=paste0(getwd(), '/data/IWS_data.csv'))

# HU4 (note: does not contain extra climate variables that were calculated at hu12)
HU4_final_table <- Reduce(inner_join, list(HU4_LULC, HU4_conn, HU4_hydro_dep))
write.csv(HU4_final_table, file=paste0(getwd(), '/data/HU4_data.csv'))

# 100m lake buffer
Buffer100_final_table <- Reduce(inner_join, list(Buff100_LULC, lake_morphometry[1:2], max_depth)) #excluding IWS area and IWS_ratio
write.csv(Buffer100_final_table, file=paste0(getwd(), '/data/Buffer100m_data.csv'))
