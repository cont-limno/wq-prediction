################# LPEP2: Map figures ###########################################################
# Date: 3-14-19
# updated: 11-1-19
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(raster)
library(ggplot2)
library(gridExtra)
library(grid)

#### input data ####
# setwd('C:/Users/FWL/Documents/wq-prediction')
# GIS data downloaded and stored locally from:
# Soranno P., K. Cheruvelil. (2017c). LAGOS-NE-GIS v1.0: A module for LAGOS-NE,
# a multi-scaled geospatial and temporal database of lake ecological context and water
# quality for thousands of U.S. Lakes: 2013-1925. Environmental Data Initiative.
# Package ID: edi.98.1
# http://dx.doi.org/10.6073/pasta/fb4f5687339bec467ce0ed1ea0b5f0ca. Dataset accessed 9/26/2017.
lakes_4ha_pts <- shapefile("C:/Ian_GIS/LAGOS-NE-GISv1.0/LAGOS_NE_All_Lakes_4ha_POINTS/LAGOS_NE_All_Lakes_4ha_POINTS.shp")
HU4 <- shapefile("C:/Ian_GIS/LAGOS-NE-GISv1.0/HU4/HU4.shp")

# water quality/ecological context for different model scenarios
#datatab <- read.csv("data/raw_data_LPEP2_03082019.csv") #original submission
datatab <- read.csv("data/revision_datasets/wq2_single_run1.csv")[,c(2:22)] #revision

########### Main program ##################
## Map of Secchi

# partition secchi data into groups based on percentile
secchi_cutoffs <- quantile(datatab$secchi, na.rm=T, c(0.25,0.50,0.75))
datatab$Secchi_group <- NA
datatab$Secchi_group <- ifelse(datatab$secchi <= secchi_cutoffs[1], 'Low', NA)
datatab$Secchi_group <- ifelse(datatab$secchi <= secchi_cutoffs[3] & datatab$secchi >= secchi_cutoffs[1], 'Medium', datatab$Secchi_group)
datatab$Secchi_group <- ifelse(datatab$secchi >= secchi_cutoffs[3], 'High', datatab$Secchi_group)
datatab$Secchi_group <- as.factor(datatab$Secchi_group)
datatab$Secchi_group <- factor(datatab$Secchi_group,levels(datatab$Secchi_group)[c(2,3,1)]) #reorder factor levels: low, med, high

# join secchi data to points for mapping
lakes_4ha_pts_secchi <- merge(lakes_4ha_pts, datatab, by.x='lagoslakei', by.y='lagoslakeid', all.x=F)

# get xy coordinates for mapping
lakes_4ha_pts_secchi_df <- as.data.frame(lakes_4ha_pts_secchi@data)
lakes_4ha_pts_secchi_df$xCor <- lakes_4ha_pts_secchi@coords[,1]
lakes_4ha_pts_secchi_df$yCor <- lakes_4ha_pts_secchi@coords[,2]
lakes_4ha_pts_secchi_df <- lakes_4ha_pts_secchi_df[!is.na(lakes_4ha_pts_secchi_df$Secchi_group),] #remove rows with NA

# map
jpeg('graphics/secchi_map.jpeg',width = 6,height = 4,units = 'in',res=600)
secchi.point1<-ggplot(lakes_4ha_pts_secchi_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=lakes_4ha_pts_secchi_df$Secchi_group), size=0.9) +
  ggtitle('Secchi (m)')+
  geom_path(data=HU4,aes(long,lat,group=group),colour='black', size=0.2) + coord_equal()+
  scale_color_manual(values=c("olivedrab1", "dodgerblue", "navy"),
                     labels=c('Low (< 25%)','Medium (25-75%)','High (> 75%)'),
                     name='Percentile group')+
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.position=c(0.82,0.2),
        legend.text=element_text(colour='black', size=9),
        plot.title=element_text(hjust=0, vjust=0, face='bold'))+
  geom_segment(arrow=arrow(length=unit(4,"mm")), aes(x=1012336,xend=1012336,y=1608736,yend=1608739),
               colour="black") +
  annotate(x=1012393, y=1481399, label="N", colour="black", geom="text", size=6) +
  guides(color = guide_legend(override.aes = list(size=1.5)))#increase legend point size
secchi.point1
dev.off()

## Maps of training/test datasets
# get xy coordinates for mapping
test_train_df <- as.data.frame(lakes_4ha_pts@data)
test_train_df$xCor <- lakes_4ha_pts@coords[,1]
test_train_df$yCor <- lakes_4ha_pts@coords[,2]
test_train_df <- merge(test_train_df, datatab, by.x='lagoslakei', by.y='lagoslakeid', all.x=F)

test_train_df <- test_train_df[,c(1,33,34,48:54)] #remove unused columns; make dataframe more workable

# assign "Training" to 1, "Testing" to 0, then reorder factor levels
# Test=TRUE, TRAIN=FALSE
test_train_df$random25_holdout <- as.factor(ifelse(test_train_df$random25_holdout==FALSE, 'Training','Testing'))
#test_train_df$random25_holdout <- factor(test_train_df$random25_holdout,levels(test_train_df$random25_holdout)[c(2,1)])

test_train_df$random75_holdout <- as.factor(ifelse(test_train_df$random75_holdout==FALSE, 'Training','Testing'))
#test_train_df$random75_holdout <- factor(test_train_df$random75_holdout,levels(test_train_df$random75_holdout)[c(2,1)])

test_train_df$hu4_ag50_holdout <- as.factor(ifelse(test_train_df$hu4_ag50_holdout==FALSE, 'Training','Testing'))
#test_train_df$hu4_ag50_holdout <- factor(test_train_df$hu4_ag50_holdout,levels(test_train_df$hu4_ag50_holdout)[c(2,1)])

test_train_df$hu4_strat75_holdout <- as.factor(ifelse(test_train_df$hu4_strat75_holdout==FALSE, 'Training','Testing'))
#test_train_df$hu4_strat75_holdout <- factor(test_train_df$hu4_strat75_holdout,levels(test_train_df$hu4_strat75_holdout)[c(2,1)])

test_train_df$hu4_random50_holdout <- as.factor(ifelse(test_train_df$hu4_random50_holdout==FALSE,'Training','Testing'))
#test_train_df$hu4_random50_holdout <- factor(test_train_df$hu4_random50_holdout,levels(test_train_df$hu4_random50_holdout)[c(2,1)])

test_train_df$cluster_strat75_holdout <- as.factor(ifelse(test_train_df$cluster_strat75_holdout==FALSE,'Training','Testing'))
#test_train_df$cluster_strat75_holdout <- factor(test_train_df$cluster_strat75_holdout,levels(test_train_df$cluster_strat75_holdout)[c(2,1)])

test_train_df$cluster_random50_holdout <- as.factor(ifelse(test_train_df$cluster_random50_holdout==FALSE,'Training','Testing'))
#test_train_df$cluster_random50_holdout <- factor(test_train_df$cluster_random50_holdout,levels(test_train_df$cluster_random50_holdout)[c(2,1)])

# random25_holdout
mappoint_size <- 0.15
title_size <- 9
random25_holdout.point1<-ggplot(test_train_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=test_train_df$random25_holdout), size=mappoint_size) +
  #ggtitle('a) Random_Lakelarge')+
  geom_path(data=HU4,aes(long,lat,group=group),colour='black', size=0.2) + coord_equal()+
  scale_color_manual(values=c("orange", "royalblue"),
                     labels=c('Training','Testing'),
                     name='')+
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.position='none',
        legend.text=element_text(colour='black', size=11),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))+
  guides(color = guide_legend(override.aes = list(size=2.5)))#increase legend point size
#random25_holdout.point1

# random75_holdout
random75_holdout.point1<-ggplot(test_train_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=test_train_df$random75_holdout), size=mappoint_size) +
  #ggtitle('b) Random_Lakesmall')+
  geom_path(data=HU4,aes(long,lat,group=group),colour='black', size=0.2) + coord_equal()+
  scale_color_manual(values=c("orange", "royalblue"),
                     labels=c('Training','Testing'),
                     name='')+
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.position='none',
        #legend.text=element_text(colour='black', size=11),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))+
  guides(color = guide_legend(override.aes = list(size=2.5)))#increase legend point size
#random75_holdout.point1

# cluster_strat75_holdout
cluster_strat75_holdout.point1<-ggplot(test_train_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=test_train_df$cluster_strat75_holdout), size=mappoint_size) +
  #ggtitle('c) Local-EC_Lakesmall')+
  geom_path(data=HU4,aes(long,lat,group=group),colour='black', size=0.2) + coord_equal()+
  scale_color_manual(values=c("orange", "royalblue"),
                     labels=c('Training','Testing'),
                     name='')+
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.position='none',
        legend.text=element_text(colour='black', size=11),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))+
  guides(color = guide_legend(override.aes = list(size=2.5)))#increase legend point size
#cluster_strat75_holdout.point1

# hu4_strat75_holdout
hu4_strat75_holdout.point1<-ggplot(test_train_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=test_train_df$hu4_strat75_holdout), size=mappoint_size) +
  #ggtitle('d) Regional-EC_Regionsmall')+
  geom_path(data=HU4,aes(long,lat,group=group),colour='black', size=0.2) + coord_equal()+
  scale_color_manual(values=c("orange", "royalblue"),
                     labels=c('Training','Testing'),
                     name='')+
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.position='none',
        legend.text=element_text(colour='black', size=11),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))+
  guides(color = guide_legend(override.aes = list(size=2.5)))#increase legend point size
#hu4_strat75_holdout.point1

# cluster_random50_holdout
cluster_random50_holdout.point1<-ggplot(test_train_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=test_train_df$cluster_random50_holdout), size=mappoint_size) +
  #ggtitle('e) Local-EC_Lakemoderate')+
  geom_path(data=HU4,aes(long,lat,group=group),colour='black', size=0.2) + coord_equal()+
  scale_color_manual(values=c("orange", "royalblue"),
                     labels=c('Training','Testing'),
                     name='')+
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.position='none',
        legend.text=element_text(colour='black', size=11),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))+
  guides(color = guide_legend(override.aes = list(size=2.5)))#increase legend point size
#cluster_random50_holdout.point1

# hu4_random50_holdout
hu4_random50_holdout.point1<-ggplot(test_train_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=test_train_df$hu4_random50_holdout), size=mappoint_size) +
  #ggtitle('f) Random_Regionmoderate')+
  geom_path(data=HU4,aes(long,lat,group=group),colour='black', size=0.2) + coord_equal()+
  scale_color_manual(values=c("orange", "royalblue"),
                     labels=c('Training','Testing'),
                     name='')+
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.position='none',
        legend.text=element_text(colour='black', size=11),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))+
  guides(color = guide_legend(override.aes = list(size=2.5)))#increase legend point size
#hu4_random50_holdout.point1

# hu4_ag50_holdout
hu4_ag50_holdout.point1<-ggplot(test_train_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=test_train_df$hu4_ag50_holdout), size=mappoint_size) +
  #ggtitle('g) Regional-LU_Regionlarge')+
  geom_path(data=HU4,aes(long,lat,group=group),colour='black', size=0.2) + coord_equal()+
  scale_color_manual(values=c("orange", "royalblue"),
                     labels=c('Training','Testing'),
                     name='')+
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.position='none',
        legend.text=element_text(colour='black', size=11),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))+
  guides(color = guide_legend(override.aes = list(size=2.5)))#increase legend point size
#hu4_ag50_holdout.point1

blank.plot <- ggplot(test_train_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=test_train_df$random25_holdout), size=mappoint_size) +
  scale_color_manual(values=c("white", "white"),
                     labels=c('Training','Testing'),
                     name='')+
  ggtitle('')+
  geom_blank()+
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.position='none',
        legend.text=element_text(colour='black', size=11),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# legend plot (created so can extract legend from it; this plot not plotted)
legend.point1<-ggplot(test_train_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=test_train_df$random25_holdout), size=mappoint_size) +
  #ggtitle('Random25_holdout')+
  geom_path(data=HU4,aes(long,lat,group=group),colour='black', size=0.2) + coord_equal()+
  scale_color_manual(values=c("orange", "royalblue"),
                     labels=c('Testing','Training'),
                     name='')+
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.position=c(0.5,0.5),
        legend.text=element_text(colour='black', size=11),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))+
  guides(color = guide_legend(override.aes = list(size=3.5)))#increase legend point size

# with help from: https://stackoverflow.com/questions/12041042/how-to-plot-just-the-legends-in-ggplot2
g_legend <- function(legend.point1){
  tmp <- ggplot_gtable(ggplot_build(legend.point1))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

legend <- g_legend(legend.point1)
#grid.draw(legend)

## create multi-panel plot (warning, takes a couple mins)
jpeg('graphics/train_test_map_revised.jpeg',width = 6,height = 5,units = 'in',res=600)
  # plotting positions!
  # (upper left, upper mid, uppper right,
  # mid left, mid mid, mid right,
  # bot left, bot mid, bot right)
  grid.arrange(random25_holdout.point1, cluster_strat75_holdout.point1, cluster_random50_holdout.point1,
               random75_holdout.point1, hu4_strat75_holdout.point1, hu4_random50_holdout.point1,
               blank.plot, legend, hu4_ag50_holdout.point1, nrow=3)
dev.off()
