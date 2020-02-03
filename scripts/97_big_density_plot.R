################# LPEP2: Density plots ##########################################################
# Date: 7-1-19 (Happy Canada Day)
# updated: 11-1-19
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(gridExtra)
library(ggplot2)
library(grid)

#### input data ####
# setwd('C:/Users/FWL/Documents/wq-prediction')

# water quality/ecological context for different model scenarios
#datatab <- read.csv("data/raw_data_LPEP2_03082019.csv") #original submission
datatab <- read.csv("data/revision_datasets/wq2_single_run1.csv")[,c(2:22)] #revision
local_predictors <- read.csv("data/local_predictors.csv") #for max depth and % forest variables

################## Main program #################
datatab <- datatab[,c(1:4,11,15:21)] #keep only some columns
local_predictors <- local_predictors[,c('lagoslakeid','maxdepth_m','iws_nlcd2006_for')]
datatab <- merge(datatab, local_predictors, by='lagoslakeid', all.x=T) #merge in maxdepth and forest variables

# Test=TRUE, TRAIN=FALSE
# assign "Training" to 1, "Testing" to 0, then reorder factor levels
datatab$random25_holdout <- as.factor(ifelse(datatab$random25_holdout==TRUE, 'Testing','Training'))
datatab$random25_holdout <- factor(datatab$random25_holdout,levels(datatab$random25_holdout)[c(2,1)])

datatab$random75_holdout <- as.factor(ifelse(datatab$random75_holdout==TRUE, 'Testing','Training'))
datatab$random75_holdout <- factor(datatab$random75_holdout,levels(datatab$random75_holdout)[c(2,1)])

datatab$hu4_ag50_holdout <- as.factor(ifelse(datatab$hu4_ag50_holdout==TRUE, 'Testing','Training'))
datatab$hu4_ag50_holdout <- factor(datatab$hu4_ag50_holdout,levels(datatab$hu4_ag50_holdout)[c(2,1)])

datatab$hu4_strat75_holdout <- as.factor(ifelse(datatab$hu4_strat75_holdout==TRUE, 'Testing','Training'))
datatab$hu4_strat75_holdout <- factor(datatab$hu4_strat75_holdout,levels(datatab$hu4_strat75_holdout)[c(2,1)])

datatab$hu4_random50_holdout <- as.factor(ifelse(datatab$hu4_random50_holdout==TRUE,'Testing','Training'))
datatab$hu4_random50_holdout <- factor(datatab$hu4_random50_holdout,levels(datatab$hu4_random50_holdout)[c(2,1)])

datatab$cluster_strat75_holdout <- as.factor(ifelse(datatab$cluster_strat75_holdout==TRUE,'Testing','Training'))
datatab$cluster_strat75_holdout <- factor(datatab$cluster_strat75_holdout,levels(datatab$cluster_strat75_holdout)[c(2,1)])

datatab$cluster_random50_holdout <- as.factor(ifelse(datatab$cluster_random50_holdout==TRUE,'Testing','Training'))
datatab$cluster_random50_holdout <- factor(datatab$cluster_random50_holdout,levels(datatab$cluster_random50_holdout)[c(2,1)])

plot_colorz <- c('royalblue','orange')
title_size = 7

# create separate dataframe for 'plot E' (cluster random 50 holdout; NAs need to be removed; otherwise messes up plots)
plotE_data <- subset(datatab, cluster_random50_holdout=='Training' | cluster_random50_holdout=='Testing')

#### TP ####
TP_xlims <- c(0,200)
TP_ylims <- c(0,0.05)
variable_label <- grobTree(textGrob("TP", x=0.8,  y=0.9, hjust=0,
                                    gp=gpar(col="black", fontsize=title_size)))

# random25_holdout
plotA_TP <- ggplot(datatab, aes(x=tp, fill=factor(random25_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TP_xlims)+
  scale_y_continuous(limits=TP_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(a) Random-Large')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# random75_holdout
plotB_TP <- ggplot(datatab, aes(x=tp, fill=factor(random75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TP_xlims)+
  scale_y_continuous(limits=TP_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(b) Random-Small')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# cluster_strat75_holdout
plotC_TP <- ggplot(datatab, aes(x=tp, fill=factor(cluster_strat75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TP_xlims)+
  scale_y_continuous(limits=TP_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(c) Stratified-Type')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# hu4_strat75_holdout
plotD_TP <- ggplot(datatab, aes(x=tp, fill=factor(hu4_strat75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TP_xlims)+
  scale_y_continuous(limits=TP_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(d) Stratified-Region')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# cluster_random50_holdout
plotE_TP <- ggplot(plotE_data, aes(x=tp, fill=factor(cluster_random50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TP_xlims)+
  scale_y_continuous(limits=TP_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(e) Targeted-Type')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# hu4_random50_holdout
plotF_TP <- ggplot(datatab, aes(x=tp, fill=factor(hu4_random50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TP_xlims)+
  scale_y_continuous(limits=TP_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(f) Targeted-Region')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# hu4_ag50_holdout
plotG_TP <- ggplot(datatab, aes(x=tp, fill=factor(hu4_ag50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TP_xlims)+
  scale_y_continuous(limits=TP_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(g) Targeted-AgRegion')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# blank plot for spacing in multi-panel plot
# blank.plot <- ggplot(datatab, aes(x=tp,y=chal))+ geom_blank() +
#   #ggtitle('')+
#   theme_bw() +
#   theme(axis.text = element_blank(),
#         axis.line = element_blank(),
#         axis.ticks = element_blank(),
#         panel.border = element_blank(),
#         panel.grid = element_blank(),
#         axis.title = element_blank(),
#         legend.position='none')

# legend plot (created so can extract legend from it; this plot not plotted)
# legend.point1<-ggplot(datatab, aes(x=tp, fill=factor(hu4_ag50_holdout))) + geom_density(alpha=0.75)+
#   scale_fill_manual(values=plot_colorz, name='')+
#   theme_bw() +
#   theme(axis.text = element_blank(),
#         axis.line = element_blank(),
#         axis.ticks = element_blank(),
#         #panel.border = element_blank(),
#         panel.grid = element_blank(),
#         axis.title = element_blank(),
#         legend.position=c(0.5,0.5),
#         legend.text=element_text(colour='black', size=11),
#         plot.title=element_text(hjust=0, vjust=0, face='bold', size=9))+
#   guides(color = guide_legend(override.aes = list(size=3.5)))#increase legend point size
#
# # with help from: https://stackoverflow.com/questions/12041042/how-to-plot-just-the-legends-in-ggplot2
# g_legend <- function(legend.point1){
#   tmp <- ggplot_gtable(ggplot_build(legend.point1))
#   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#   legend <- tmp$grobs[[leg]]
#   return(legend)}
#
# legend <- g_legend(legend.point1)
# #grid.draw(legend)

#### Chla ####
chla_xlims <- c(0,100)
chla_ylims <- c(0,0.1)
variable_label <- grobTree(textGrob("CHL", x=0.8,  y=0.9, hjust=0,
                                    gp=gpar(col="black", fontsize=title_size)))

# random25_holdout
plotA_chla <- ggplot(datatab, aes(x=chla, fill=factor(random25_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=chla_xlims)+
  scale_y_continuous(limits=chla_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(a) Random-Large')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# random75_holdout
plotB_chla <- ggplot(datatab, aes(x=chla, fill=factor(random75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=chla_xlims)+
  scale_y_continuous(limits=chla_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(b) Random-Small')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# cluster_strat75_holdout
plotC_chla <- ggplot(datatab, aes(x=chla, fill=factor(cluster_strat75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=chla_xlims)+
  scale_y_continuous(limits=chla_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(c) Stratified-Type')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# hu4_strat75_holdout
plotD_chla <- ggplot(datatab, aes(x=chla, fill=factor(hu4_strat75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=chla_xlims)+
  scale_y_continuous(limits=chla_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(d) Stratified-Region')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# cluster_random50_holdout
plotE_chla <- ggplot(plotE_data, aes(x=chla, fill=factor(cluster_random50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=chla_xlims)+
  scale_y_continuous(limits=chla_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(e) Targeted-Type')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# hu4_random50_holdout
plotF_chla <- ggplot(datatab, aes(x=chla, fill=factor(hu4_random50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=chla_xlims)+
  scale_y_continuous(limits=chla_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(f) Targeted-Region')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# hu4_ag50_holdout
plotG_chla <- ggplot(datatab, aes(x=chla, fill=factor(hu4_ag50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=chla_xlims)+
  scale_y_continuous(limits=chla_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(g) Targeted-AgRegion')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

#### TN ####
TN_xlims <- c(0,7000)
TN_ylims <- c(0,0.0015)
variable_label <- grobTree(textGrob("TN", x=0.8,  y=0.9, hjust=0,
                                    gp=gpar(col="black", fontsize=title_size)))

# random25_holdout
plotA_TN <- ggplot(datatab, aes(x=tn_combined, fill=factor(random25_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TN_xlims)+
  scale_y_continuous(limits=TN_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(a) Random-Large')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# random75_holdout
plotB_TN <- ggplot(datatab, aes(x=tn_combined, fill=factor(random75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TN_xlims)+
  scale_y_continuous(limits=TN_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(b) Random-Small')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# cluster_strat75_holdout
plotC_TN <- ggplot(datatab, aes(x=tn_combined, fill=factor(cluster_strat75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TN_xlims)+
  scale_y_continuous(limits=TN_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(c) Stratified-Type')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# hu4_strat75_holdout
plotD_TN <- ggplot(datatab, aes(x=tn_combined, fill=factor(hu4_strat75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TN_xlims)+
  scale_y_continuous(limits=TN_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(d) Stratified-Region')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# cluster_random50_holdout
plotE_TN <- ggplot(plotE_data, aes(x=tn_combined, fill=factor(cluster_random50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TN_xlims)+
  scale_y_continuous(limits=TN_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(e) Targeted-Type')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# hu4_random50_holdout
plotF_TN <- ggplot(datatab, aes(x=tn_combined, fill=factor(hu4_random50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TN_xlims)+
  scale_y_continuous(limits=TN_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(f) Targeted-Region')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# hu4_ag50_holdout
plotG_TN <- ggplot(datatab, aes(x=tn_combined, fill=factor(hu4_ag50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TN_xlims)+
  scale_y_continuous(limits=TN_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(g) Targeted-AgRegion')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

#### Secchi ####
secchi_xlims <- c(0,15)
secchi_ylims <- c(0,0.35)
variable_label <- grobTree(textGrob("Clarity", x=0.8,  y=0.9, hjust=0,
                                    gp=gpar(col="black", fontsize=title_size)))

# random25_holdout
plotA_Secchi <- ggplot(datatab, aes(x=secchi, fill=factor(random25_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=secchi_xlims)+
  scale_y_continuous(limits=secchi_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(a) Random-Large')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# random75_holdout
plotB_Secchi <- ggplot(datatab, aes(x=secchi, fill=factor(random75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=secchi_xlims)+
  scale_y_continuous(limits=secchi_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(b) Random-Small')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# cluster_strat75_holdout
plotC_Secchi <- ggplot(datatab, aes(x=secchi, fill=factor(cluster_strat75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=secchi_xlims)+
  scale_y_continuous(limits=secchi_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(c) Stratified-Type')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# hu4_strat75_holdout
plotD_Secchi <- ggplot(datatab, aes(x=secchi, fill=factor(hu4_strat75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=secchi_xlims)+
  scale_y_continuous(limits=secchi_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(d) Stratified-Region')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# cluster_random50_holdout
plotE_Secchi <- ggplot(plotE_data, aes(x=secchi, fill=factor(cluster_random50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=secchi_xlims)+
  scale_y_continuous(limits=secchi_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(e) Targeted-Type')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# hu4_random50_holdout
plotF_Secchi <- ggplot(datatab, aes(x=secchi, fill=factor(hu4_random50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=secchi_xlims)+
  scale_y_continuous(limits=secchi_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(f) Targeted-Region')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# hu4_ag50_holdout
plotG_Secchi <- ggplot(datatab, aes(x=secchi, fill=factor(hu4_ag50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=secchi_xlims)+
  scale_y_continuous(limits=secchi_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(g) Targeted-AgRegion')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())


#### Max depth ####
depth_xlims <- c(0,60)
depth_ylims <- c(0,0.08)
variable_label <- grobTree(textGrob("Max depth", x=0.65,  y=0.9, hjust=0,
                                    gp=gpar(col="black", fontsize=title_size)))

# random25_holdout
plotA_depth <- ggplot(datatab, aes(x=maxdepth_m, fill=factor(random25_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=depth_xlims)+
  scale_y_continuous(limits=depth_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(a) Random-Large')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# random75_holdout
plotB_depth <- ggplot(datatab, aes(x=maxdepth_m, fill=factor(random75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=depth_xlims)+
  scale_y_continuous(limits=depth_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(b) Random-Small')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# cluster_strat75_holdout
plotC_depth <- ggplot(datatab, aes(x=maxdepth_m, fill=factor(cluster_strat75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=depth_xlims)+
  scale_y_continuous(limits=depth_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(c) Stratified-Type')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# hu4_strat75_holdout
plotD_depth <- ggplot(datatab, aes(x=maxdepth_m, fill=factor(hu4_strat75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=depth_xlims)+
  scale_y_continuous(limits=depth_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(d) Stratified-Region')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# cluster_random50_holdout
plotE_depth <- ggplot(plotE_data, aes(x=maxdepth_m, fill=factor(cluster_random50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=depth_xlims)+
  scale_y_continuous(limits=depth_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(e) Targeted-Type')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# hu4_random50_holdout
plotF_depth <- ggplot(datatab, aes(x=maxdepth_m, fill=factor(hu4_random50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=depth_xlims)+
  scale_y_continuous(limits=depth_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(f) Targeted-Region')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# hu4_ag50_holdout
plotG_depth <- ggplot(datatab, aes(x=maxdepth_m, fill=factor(hu4_ag50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=depth_xlims)+
  scale_y_continuous(limits=depth_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(g) Targeted-AgRegion')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

#### IWS_forest ####
forest_xlims <- c(0,100)
forest_ylims <- c(0,0.025)
variable_label <- grobTree(textGrob("IWS forest", x=0.65,  y=0.9, hjust=0,
                                    gp=gpar(col="black", fontsize=title_size)))

# random25_holdout
plotA_forest <- ggplot(datatab, aes(x=iws_nlcd2006_for, fill=factor(random25_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=forest_xlims)+
  scale_y_continuous(limits=forest_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(a) Random-Large')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# random75_holdout
plotB_forest <- ggplot(datatab, aes(x=iws_nlcd2006_for, fill=factor(random75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=forest_xlims)+
  scale_y_continuous(limits=forest_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(b) Random-Small')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# cluster_strat75_holdout
plotC_forest <- ggplot(datatab, aes(x=iws_nlcd2006_for, fill=factor(cluster_strat75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=forest_xlims)+
  scale_y_continuous(limits=forest_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(c) Stratified-Type')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# hu4_strat75_holdout
plotD_forest <- ggplot(datatab, aes(x=iws_nlcd2006_for, fill=factor(hu4_strat75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=forest_xlims)+
  scale_y_continuous(limits=forest_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(d) Stratified-Region')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# cluster_random50_holdout
plotE_forest <- ggplot(plotE_data, aes(x=iws_nlcd2006_for, fill=factor(cluster_random50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=forest_xlims)+
  scale_y_continuous(limits=forest_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(e) Targeted-Type')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# hu4_random50_holdout
plotF_forest <- ggplot(datatab, aes(x=iws_nlcd2006_for, fill=factor(hu4_random50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=forest_xlims)+
  scale_y_continuous(limits=forest_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(f) Targeted-Region')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

# hu4_ag50_holdout
plotG_forest <- ggplot(datatab, aes(x=iws_nlcd2006_for, fill=factor(hu4_ag50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=forest_xlims)+
  scale_y_continuous(limits=forest_ylims)+
  #annotation_custom(variable_label)+
  #ggtitle('(g) Targeted-AgRegion')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0.05,0.05,0.05,0.05), "cm"),
        plot.title=element_blank())

tiff(filename="graphics/density_plots/all_vars_scenarios_revised.tif",height=5600,width=5200,units="px",res=800,compression="lzw")
grid.arrange(plotA_TP, plotA_TN, plotA_chla, plotA_Secchi, plotA_depth, plotA_forest,
             plotB_TP, plotB_TN, plotB_chla, plotB_Secchi, plotB_depth, plotB_forest,
             plotC_TP, plotC_TN, plotC_chla, plotC_Secchi, plotC_depth, plotC_forest,
             plotD_TP, plotD_TN, plotD_chla, plotD_Secchi, plotD_depth, plotD_forest,
             plotE_TP, plotE_TN, plotE_chla, plotE_Secchi, plotE_depth, plotE_forest,
             plotF_TP, plotF_TN, plotF_chla, plotF_Secchi, plotF_depth, plotF_forest,
             plotG_TP, plotG_TN, plotG_chla, plotG_Secchi, plotG_depth, plotG_forest, nrow=7)
dev.off()
