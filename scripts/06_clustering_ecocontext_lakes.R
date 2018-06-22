setwd("~/Documents/GitHub/wq-prediction")
library(LAGOSNE)
library(tidyverse)

dat_full <-lagosne_load(version = "1.087.1")
names(dat_full)
dat_geo <- dat_full$lakes.geo %>% select(lagoslakeid,upstream_lakes_4ha_area_ha,upstream_lakes_4ha_count,wlconnections_allwetlands_shoreline_km,lakeconnection,latewisconsinglaciation_glacial)
dat_locus <- dat_full$locus
dat_iws <- dat_full$iws.conn %>% select(lagoslakeid,iws_wl_allwetlandsdissolved_overlapping_area_pct,iws_streamdensity_streams_density_mperha)
dat_iws_lulc <-dat_full$iws %>% select(lagoslakeid,iws_ha,iws_perimkm) %>% 
    mutate(iws_km2 = iws_ha*0.01) %>% 
    mutate(iws_sdf = iws_perimkm/(2*(pi*iws_km2)^1/2)) %>% 
    select(-iws_perimkm-iws_ha)
dat <- dat_locus %>% select(lagoslakeid,lake_area_ha,lake_perim_meters,iws_zoneid,elevation_m) %>% 
    filter(lake_area_ha>=4) %>% 
    mutate(lake_area_km2 = lake_area_ha*0.01) %>% 
    mutate(lake_perim_km = lake_perim_meters/1000) %>% 
    mutate(lake_sdf = lake_perim_km/(2*(pi*lake_area_km2)^1/2)) %>% 
    select(-lake_area_ha,-lake_perim_meters)
dat <- dat %>% 
    left_join(dat_iws) %>% 
    left_join(dat_iws_lulc) %>% 
    mutate(iws_lk_ratio = iws_km2/lake_area_km2) %>% 
    left_join(dat_geo) %>% 
    mutate(wlconnections_allwetlands_shoreline_pctperim = wlconnections_allwetlands_shoreline_km/lake_perim_km) %>% 
    select(-lake_perim_km,wlconnections_allwetlands_shoreline_km)

write_csv(dat,"data/cluster_localcontext.csv")

dat.clust <- dat %>% select(-lagoslakeid,-iws_zoneid,-latewisconsinglaciation_glacial,-lakeconnection) 

  