setwd("~/Documents/GitHub/wq-prediction")
library(LAGOSNE)
library(tidyverse)
library(cluster)
library(factoextra)
library(fastcluster)
library(sf)
library(magrittr)
library(cowplot)
library(USAboundaries)
library(USAboundariesData)


dat_full <-lagosne_load(version = "1.087.1")
names(dat_full)
dat_geo <- dat_full$lakes.geo %>% select(lagoslakeid,upstream_lakes_4ha_area_ha,upstream_lakes_4ha_count,wlconnections_allwetlands_shoreline_km,lakeconnection,latewisconsinglaciation_glacial)
dat_locus <- dat_full$locus
dat_iws <- dat_full$iws.conn %>% select(lagoslakeid,iws_wl_allwetlandsdissolved_overlapping_area_pct,iws_streamdensity_streams_density_mperha)
dat_iws_lulc <-dat_full$iws %>% select(lagoslakeid,iws_ha,iws_perimkm) %>%
    mutate(iws_km2 = iws_ha*0.01) %>%
    mutate(iws_sdf = iws_perimkm/(2*(pi*iws_km2)^1/2)) %>%
    select(-iws_perimkm,-iws_ha)
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
    select(-lake_perim_km,-wlconnections_allwetlands_shoreline_km,-iws_km2)

write_csv(dat,"data/cluster_localcontext.csv")

dat.clust <- dat %>%
  select(-iws_zoneid,-latewisconsinglaciation_glacial,-lakeconnection,-elevation_m,-lake_area_km2) %>%
  na.omit() %>%
  mutate(lake_sdf = sqrt(lake_sdf)) %>%
  mutate(iws_wl_allwetlandsdissolved_overlapping_area_pct = sqrt(iws_wl_allwetlandsdissolved_overlapping_area_pct)) %>%
  mutate(iws_streamdensity_streams_density_mperha = log10(iws_streamdensity_streams_density_mperha+1)) %>%
  mutate(iws_sdf = log10(iws_sdf)) %>%
  mutate(iws_lk_ratio = log10(iws_lk_ratio)) %>%
  mutate(upstream_lakes_4ha_area_ha = log10(upstream_lakes_4ha_area_ha+1)) %>%
  mutate(upstream_lakes_4ha_count = log10(upstream_lakes_4ha_count+1)) %>%
  mutate(wlconnections_allwetlands_shoreline_pctperim = sqrt(wlconnections_allwetlands_shoreline_pctperim))

summary(dat.clust)

plot(density(sqrt(dat.clust$wlconnections_allwetlands_shoreline_pctperim)))


dat.s <- apply(as.matrix(dat.clust[,2:9]),2,scale,center=TRUE,scale=TRUE)
colnames(dat.s) <- colnames(dat.clust[,2:9])

avgsil <- seq(from = 1,to = 50,by=1)
for(i in 2:50) {
clara.res <- clara(x = dat.s,k = 19,metric = "manhattan",samples = 100,sampsize = 3000,pamLike = TRUE,correct.d=TRUE)
avgsil[i] <-clara.res$silinfo$avg.width
}

hc <- hclust.vector(dat.s,method = "ward")

plot(hc)
rect.hclust(hc, k=10, border="red")
groups <- cutree(hc, k=10)
barplot(table(groups),xlab="Cluster",ylab="Number of Lakes")

{pdf("cluster_sizes.pdf",width=8,height=10.5,onefile = TRUE)
  par(mfrow=c(5,3))
  for (i in 2:50){
    t.groups <- cutree(hc, k=i)
    barplot(table(t.groups),xlab="Cluster",ylab="Number of Lakes",
            main=paste("# of clusters:",i))
  }
  dev.off()
  }


dat.plot <- data.frame(lagoslakeid=dat.clust$lagoslakeid,cluster=groups)
dat.plot <- dat_locus %>% select(lagoslakeid,nhd_long,nhd_lat) %>% right_join(dat.plot)
dat.plot <- coordinatize(dat.plot)
options(device="quartz")
dev.new()

state_names= c('wisconsin','minnesota','vermont','maine','michigan','missouri','rhode island','new york','iowa','illinois','indiana','ohio','new hampshire','pennsylvania','connecticut','massachusetts','new jersey')
lagos_states<-us_states(resolution = "high", states = state_names) %>%
  st_transform(crs = 4326)

p1 <- ggplot() +
  geom_sf(data=lagos_states, color = "gray30", lwd=.5, fill=NA) +
  geom_sf(data = dat.plot %>% filter(cluster==1),
          aes(color=cluster), alpha = 0.4,show.legend=F)
p2 <- ggplot() +
  geom_sf(data=lagos_states, color = "gray30", lwd=.5, fill=NA) +
  geom_sf(data = dat.plot %>% filter(cluster==2), aes(color = cluster),
          alpha = 0.4,show.legend=F)
p3 <- ggplot() +
  geom_sf(data=lagos_states, color = "gray30", lwd=.5, fill=NA) +
  geom_sf(data = dat.plot %>% filter(cluster==3),
          aes(color = cluster), alpha = 0.4,show.legend=F)
p4 <- ggplot() +
  geom_sf(data=lagos_states, color = "gray30", lwd=.5, fill=NA) +
  geom_sf(data = dat.plot %>% filter(cluster==4), aes(color = cluster),
          alpha = 0.4,show.legend=F)
p5 <- ggplot() +
  geom_sf(data=lagos_states, color = "gray30", lwd=.5, fill=NA) +
  geom_sf(data = dat.plot %>% filter(cluster==5),
          aes(color = cluster), alpha = 0.4,show.legend=F)
p6 <- ggplot() +
  geom_sf(data=lagos_states, color = "gray30", lwd=.5, fill=NA) +
  geom_sf(data = dat.plot %>% filter(cluster==6),
          aes(color = cluster), alpha = 0.4,show.legend=F)
p7 <- ggplot() +
  geom_sf(data=lagos_states, color = "gray30", lwd=.5, fill=NA) +
  geom_sf(data = dat.plot %>% filter(cluster==7),
          aes(color = cluster), alpha = 0.4,show.legend=F)
p8 <- ggplot() +
  geom_sf(data=lagos_states, color = "gray30", lwd=.5, fill=NA) +
  geom_sf(data = dat.plot %>% filter(cluster==8),
          aes(color = cluster), alpha = 0.4,show.legend=F)
p9 <- ggplot() +
  geom_sf(data=lagos_states, color = "gray30", lwd=.5, fill=NA) +
  geom_sf(data = dat.plot %>% filter(cluster==9),
          aes(color = cluster), alpha = 0.4,show.legend=F)
p10 <- ggplot() +
  geom_sf(data=lagos_states, color = "gray30", lwd=.5, fill=NA) +
  geom_sf(data = dat.plot %>% filter(cluster==10),
          aes(color = cluster), alpha = 0.4,show.legend=F)
p11 <- ggplot() +
  geom_sf(data=lagos_states, color = "gray30", lwd=.5, fill=NA) +
  geom_sf(data = dat.plot %>% filter(cluster==11),
          aes(color = cluster), alpha = 0.4,show.legend=F)
p12 <- ggplot() +
  geom_sf(data=lagos_states, color = "gray30", lwd=.5, fill=NA) + geom_sf(data = dat.plot %>% filter(cluster==12), aes(color = cluster), alpha = 0.4,show.legend=F)
p13 <- ggplot() +
  geom_sf(data=lagos_states, color = "gray30", lwd=.5, fill=NA) +
  geom_sf(data = dat.plot %>% filter(cluster==13),
          aes(color = cluster), alpha = 0.4,show.legend=F)
p14 <- ggplot() + geom_sf(data=lagos_states, color = "gray30", lwd=.5, fill=NA) + geom_sf(data = dat.plot %>% filter(cluster==14), aes(color = cluster), alpha = 0.4,show.legend=F)
p15 <- ggplot() + geom_sf(data=lagos_states, color = "gray30", lwd=.5, fill=NA) + geom_sf(data = dat.plot %>% filter(cluster==15), aes(color = cluster), alpha = 0.4,show.legend=F)
p16 <- ggplot() + geom_sf(data=lagos_states, color = "gray30", lwd=.5, fill=NA) + geom_sf(data = dat.plot %>% filter(cluster==16), aes(color = cluster), alpha = 0.4,show.legend=F)
p17 <- ggplot() +
  geom_sf(data=lagos_states, color = "gray30", lwd=.5, fill=NA) +
  geom_sf(data = dat.plot %>% filter(cluster==17),
          aes(color = cluster), alpha = 0.4,show.legend=F)
p18 <- ggplot() +
  geom_sf(data=lagos_states, color = "gray30", lwd=.5, fill=NA) +
  geom_sf(data = dat.plot %>% filter(cluster==18),
          aes(color = cluster), alpha = 0.4,show.legend=F)
p19 <- ggplot() +
  geom_sf(data=lagos_states, color = "gray30", lwd=.5, fill=NA) +
  geom_sf(data = dat.plot %>% filter(cluster==19),
          aes(color = cluster), alpha = 0.4,show.legend=F)
p20 <- ggplot() +
  geom_sf(data=lagos_states, color = "gray30", lwd=.5, fill=NA) +
  geom_sf(data = dat.plot %>% filter(cluster==20),
          aes(color = cluster), alpha = 0.4,show.legend=F)


clusterplot <- plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,labels=seq(1,10,1))
save_plot(filename = "lake_clusters_10.png",
          plot = clusterplot,
          ncol=4,
          nrow=3,
          base_aspect_ratio = 1.3)

clusterplot <- plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,labels=seq(1,20,1))
save_plot("lake_clusters_20.png",clusterplot,ncol=5,nrow=4,base_aspect_ratio = 1.3)


############voilin to look at paramter distributions
dat.clust <- cbind(dat.clust,groups)
names(dat.clust) <- c("lagoslakeid",
                      "lake_sdf",
                      "wet_pct",
                      "streamdensity",
                      "iws_sdf",
                      "iws_lk_ratio",
                      "us_lakes_area",
                      "us_lake_count",
                      "shoreline_wet_pct",
                      "groups" )
dat.s <- apply(as.matrix(dat.clust[,2:9]),2,scale,center=TRUE,scale=TRUE)
colnames(dat.s) <- colnames(dat.clust[,2:9])
dat.violin <- cbind(dat.clust[,c(1,10)],as.data.frame(dat.s))
unq.groups <- unique(groups)

myplots <- list()
count <-1
for(i in unq.groups){
  dat.temp <- dat.violin %>% filter(groups==i) %>%
    gather(key = "key",value = "value",-lagoslakeid,-groups)
  myplots[[count]] <- ggplot(dat.temp, aes(key, value)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    geom_hline(yintercept = 0,color="red",alpha=0.4)
  count <- count+1
}

eco_context <- plot_grid(plotlist=myplots,labels=seq(1,10,1))
save_plot("eco_context.png",eco_context,ncol=4,nrow=3,base_aspect_ratio = 1)
