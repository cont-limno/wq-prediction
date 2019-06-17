################# LPEP2: TP frequency distributions ###########################################################
# Date: 3-15-19
# updated: 6-17-19
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(gridExtra)
library(ggplot2)
library(grid)

#### input data ####
setwd('C:/Users/FWL/Documents/wq-prediction')

# water quality/ecological context for different model scenarios
datatab <- read.csv("data/raw_data_LPEP2_03082019.csv")


################## Main program #################
datatab <- datatab[,c(1,5:16,18)] #keep only some columns

# assign "Training" to 1, "Testing" to 0, then reorder factor levels
datatab$random25_holdout <- as.factor(ifelse(datatab$random25_holdout==1, 'Testing','Training'))
datatab$random25_holdout <- factor(datatab$random25_holdout,levels(datatab$random25_holdout)[c(2,1)])

datatab$random75_holdout <- as.factor(ifelse(datatab$random75_holdout==1, 'Testing','Training'))
datatab$random75_holdout <- factor(datatab$random75_holdout,levels(datatab$random75_holdout)[c(2,1)])

datatab$hu4_ag50_holdout <- as.factor(ifelse(datatab$hu4_ag50_holdout==1, 'Testing','Training'))
datatab$hu4_ag50_holdout <- factor(datatab$hu4_ag50_holdout,levels(datatab$hu4_ag50_holdout)[c(2,1)])

datatab$hu4_strat75_holdout <- as.factor(ifelse(datatab$hu4_strat75_holdout==1, 'Testing','Training'))
datatab$hu4_strat75_holdout <- factor(datatab$hu4_strat75_holdout,levels(datatab$hu4_strat75_holdout)[c(2,1)])

datatab$hu4_random50_holdout <- as.factor(ifelse(datatab$hu4_random50_holdout==1,'Testing','Training'))
datatab$hu4_random50_holdout <- factor(datatab$hu4_random50_holdout,levels(datatab$hu4_random50_holdout)[c(2,1)])

datatab$cluster_strat75_holdout <- as.factor(ifelse(datatab$cluster_strat75_holdout==1,'Testing','Training'))
datatab$cluster_strat75_holdout <- factor(datatab$cluster_strat75_holdout,levels(datatab$cluster_strat75_holdout)[c(2,1)])

datatab$cluster_random50_holdout <- as.factor(ifelse(datatab$cluster_random50_holdout==1,'Testing','Training'))
datatab$cluster_random50_holdout <- factor(datatab$cluster_random50_holdout,levels(datatab$cluster_random50_holdout)[c(2,1)])

plot_colorz <- c('royalblue','orange')
title_size = 7

#### TP ####
TP_xlims <- c(0,1250)
TP_ylims <- c(0,0.05)
variable_label <- grobTree(textGrob("TP", x=0.8,  y=0.9, hjust=0,
                                    gp=gpar(col="black", fontsize=title_size)))

# random25_holdout
plotA <- ggplot(datatab, aes(x=tp, fill=factor(random25_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TP_xlims)+
  scale_y_continuous(limits=TP_ylims)+
  annotation_custom(variable_label)+
  ggtitle('a) Small test dataset')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# random75_holdout
plotB <- ggplot(datatab, aes(x=tp, fill=factor(random75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TP_xlims)+
  scale_y_continuous(limits=TP_ylims)+
  annotation_custom(variable_label)+
  ggtitle('b) Large test dataset')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# cluster_strat75_holdout
plotC <- ggplot(datatab, aes(x=tp, fill=factor(cluster_strat75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TP_xlims)+
  scale_y_continuous(limits=TP_ylims)+
  annotation_custom(variable_label)+
  ggtitle('c) Stratified by ecosystem type')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# hu4_strat75_holdout
plotD <- ggplot(datatab, aes(x=tp, fill=factor(hu4_strat75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TP_xlims)+
  scale_y_continuous(limits=TP_ylims)+
  annotation_custom(variable_label)+
  ggtitle('d) Stratified by region')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# cluster_random50_holdout
plotE <- ggplot(datatab, aes(x=tp, fill=factor(cluster_random50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TP_xlims)+
  scale_y_continuous(limits=TP_ylims)+
  annotation_custom(variable_label)+
  ggtitle('e) Targeted by ecosystem type')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# hu4_random50_holdout
plotF <- ggplot(datatab, aes(x=tp, fill=factor(hu4_random50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TP_xlims)+
  scale_y_continuous(limits=TP_ylims)+
  annotation_custom(variable_label)+
  ggtitle('f) Targeted by region')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# hu4_ag50_holdout
plotG <- ggplot(datatab, aes(x=tp, fill=factor(hu4_ag50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TP_xlims)+
  scale_y_continuous(limits=TP_ylims)+
  annotation_custom(variable_label)+
  ggtitle('g) Targeted by land use regions')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# blank plot for spacing in multi-panel plot
blank.plot <- ggplot(datatab, aes(x=tp,y=chal))+ geom_blank() +
  ggtitle('')+
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.position='none')

# legend plot (created so can extract legend from it; this plot not plotted)
legend.point1<-ggplot(datatab, aes(x=tp, fill=factor(hu4_ag50_holdout))) + geom_density(alpha=0.75)+
  scale_fill_manual(values=plot_colorz, name='')+
  theme_bw() + 
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.position=c(0.5,0.5),
        legend.text=element_text(colour='black', size=11),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=9))+
  guides(color = guide_legend(override.aes = list(size=3.5)))#increase legend point size

# with help from: https://stackoverflow.com/questions/12041042/how-to-plot-just-the-legends-in-ggplot2
g_legend <- function(legend.point1){ 
  tmp <- ggplot_gtable(ggplot_build(legend.point1)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

legend <- g_legend(legend.point1) 
#grid.draw(legend)

jpeg('graphics/density_plots/tp.jpeg',width = 6,height = 5,units = 'in',res=600)
# plotting positions!
# (upper left, upper mid, uppper right,
# mid left, mid mid, mid right,
# bot left, bot mid, bot right)
grid.arrange(plotA, plotB, plotC,
             plotD, plotE, plotF,
             legend, blank.plot, plotG, nrow=3)
dev.off()

#### Chla ####
chla_xlims <- c(0,400)
chla_ylims <- c(0,0.1)
variable_label <- grobTree(textGrob("CHL", x=0.8,  y=0.9, hjust=0,
                                    gp=gpar(col="black", fontsize=title_size)))

# random25_holdout
plotA <- ggplot(datatab, aes(x=chal, fill=factor(random25_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=chla_xlims)+
  scale_y_continuous(limits=chla_ylims)+
  annotation_custom(variable_label)+
  ggtitle('a) Small test dataset')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# random75_holdout
plotB <- ggplot(datatab, aes(x=chal, fill=factor(random75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=chla_xlims)+
  scale_y_continuous(limits=chla_ylims)+
  annotation_custom(variable_label)+
  ggtitle('b) Large test dataset')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# cluster_strat75_holdout
plotC <- ggplot(datatab, aes(x=chal, fill=factor(cluster_strat75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=chla_xlims)+
  scale_y_continuous(limits=chla_ylims)+
  annotation_custom(variable_label)+
  ggtitle('c) Stratified by ecosystem type')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# hu4_strat75_holdout
plotD <- ggplot(datatab, aes(x=chal, fill=factor(hu4_strat75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=chla_xlims)+
  scale_y_continuous(limits=chla_ylims)+
  annotation_custom(variable_label)+
  ggtitle('d) Stratified by region')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# cluster_random50_holdout
plotE <- ggplot(datatab, aes(x=chal, fill=factor(cluster_random50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=chla_xlims)+
  scale_y_continuous(limits=chla_ylims)+
  annotation_custom(variable_label)+
  ggtitle('e) Targeted by ecosystem type')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# hu4_random50_holdout
plotF <- ggplot(datatab, aes(x=chal, fill=factor(hu4_random50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=chla_xlims)+
  scale_y_continuous(limits=chla_ylims)+
  annotation_custom(variable_label)+
  ggtitle('f) Targeted by region')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# hu4_ag50_holdout
plotG <- ggplot(datatab, aes(x=chal, fill=factor(hu4_ag50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=chla_xlims)+
  scale_y_continuous(limits=chla_ylims)+
  annotation_custom(variable_label)+
  ggtitle('g) Targeted by land use regions')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

jpeg('graphics/density_plots/chla.jpeg',width = 6,height = 5,units = 'in',res=600)
# plotting positions!
# (upper left, upper mid, uppper right,
# mid left, mid mid, mid right,
# bot left, bot mid, bot right)
grid.arrange(plotA, plotB, plotC,
             plotD, plotE, plotF,
             legend, blank.plot, plotG, nrow=3)
dev.off()

#### TN ####
TN_xlims <- c(0,20000)
TN_ylims <- c(0,0.0015)
variable_label <- grobTree(textGrob("TN", x=0.8,  y=0.9, hjust=0,
                                    gp=gpar(col="black", fontsize=title_size)))

# random25_holdout
plotA <- ggplot(datatab, aes(x=tn, fill=factor(random25_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TN_xlims)+
  scale_y_continuous(limits=TN_ylims)+
  annotation_custom(variable_label)+
  ggtitle('a) Small test dataset')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# random75_holdout
plotB <- ggplot(datatab, aes(x=tn, fill=factor(random75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TN_xlims)+
  scale_y_continuous(limits=TN_ylims)+
  annotation_custom(variable_label)+
  ggtitle('b) Large test dataset')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# cluster_strat75_holdout
plotC <- ggplot(datatab, aes(x=tn, fill=factor(cluster_strat75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TN_xlims)+
  scale_y_continuous(limits=TN_ylims)+
  annotation_custom(variable_label)+
  ggtitle('c) Stratified by ecosystem type')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# hu4_strat75_holdout
plotD <- ggplot(datatab, aes(x=tn, fill=factor(hu4_strat75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TN_xlims)+
  scale_y_continuous(limits=TN_ylims)+
  annotation_custom(variable_label)+
  ggtitle('d) Stratified by region')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# cluster_random50_holdout
plotE <- ggplot(datatab, aes(x=tn, fill=factor(cluster_random50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TN_xlims)+
  scale_y_continuous(limits=TN_ylims)+
  annotation_custom(variable_label)+
  ggtitle('e) Targeted by ecosystem type')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# hu4_random50_holdout
plotF <- ggplot(datatab, aes(x=tn, fill=factor(hu4_random50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TN_xlims)+
  scale_y_continuous(limits=TN_ylims)+
  annotation_custom(variable_label)+
  ggtitle('f) Targeted by region')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# hu4_ag50_holdout
plotG <- ggplot(datatab, aes(x=tn, fill=factor(hu4_ag50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=TN_xlims)+
  scale_y_continuous(limits=TN_ylims)+
  annotation_custom(variable_label)+
  ggtitle('g) Targeted by land use regions')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

jpeg('graphics/density_plots/tn.jpeg',width = 6,height = 5,units = 'in',res=600)
# plotting positions!
# (upper left, upper mid, uppper right,
# mid left, mid mid, mid right,
# bot left, bot mid, bot right)
grid.arrange(plotA, plotB, plotC,
             plotD, plotE, plotF,
             legend, blank.plot, plotG, nrow=3)
dev.off()

#### Secchi ####
secchi_xlims <- c(0,20)
secchi_ylims <- c(0,0.35)
variable_label <- grobTree(textGrob("Clarity", x=0.8,  y=0.9, hjust=0,
                                    gp=gpar(col="black", fontsize=title_size)))

# random25_holdout
plotA <- ggplot(datatab, aes(x=secchi, fill=factor(random25_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=secchi_xlims)+
  scale_y_continuous(limits=secchi_ylims)+
  annotation_custom(variable_label)+
  ggtitle('a) Small test dataset')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# random75_holdout
plotB <- ggplot(datatab, aes(x=secchi, fill=factor(random75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=secchi_xlims)+
  scale_y_continuous(limits=secchi_ylims)+
  annotation_custom(variable_label)+
  ggtitle('b) Large test dataset')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# cluster_strat75_holdout
plotC <- ggplot(datatab, aes(x=secchi, fill=factor(cluster_strat75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=secchi_xlims)+
  scale_y_continuous(limits=secchi_ylims)+
  annotation_custom(variable_label)+
  ggtitle('c) Stratified by ecosystem type')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# hu4_strat75_holdout
plotD <- ggplot(datatab, aes(x=secchi, fill=factor(hu4_strat75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=secchi_xlims)+
  scale_y_continuous(limits=secchi_ylims)+
  annotation_custom(variable_label)+
  ggtitle('d) Stratified by region')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# cluster_random50_holdout
plotE <- ggplot(datatab, aes(x=secchi, fill=factor(cluster_random50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=secchi_xlims)+
  scale_y_continuous(limits=secchi_ylims)+
  annotation_custom(variable_label)+
  ggtitle('e) Targeted by ecosystem type')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# hu4_random50_holdout
plotF <- ggplot(datatab, aes(x=secchi, fill=factor(hu4_random50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=secchi_xlims)+
  scale_y_continuous(limits=secchi_ylims)+
  annotation_custom(variable_label)+
  ggtitle('f) Targeted by region')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# hu4_ag50_holdout
plotG <- ggplot(datatab, aes(x=secchi, fill=factor(hu4_ag50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=secchi_xlims)+
  scale_y_continuous(limits=secchi_ylims)+
  annotation_custom(variable_label)+
  ggtitle('g) Targeted by land use regions')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

jpeg('graphics/density_plots/clarity.jpeg',width = 6,height = 5,units = 'in',res=600)
# plotting positions!
# (upper left, upper mid, uppper right,
# mid left, mid mid, mid right,
# bot left, bot mid, bot right)
grid.arrange(plotA, plotB, plotC,
             plotD, plotE, plotF,
             legend, blank.plot, plotG, nrow=3)
dev.off()

#### Max depth ####
depth_xlims <- c(0,200)
depth_ylims <- c(0,0.08)
variable_label <- grobTree(textGrob("Max depth", x=0.65,  y=0.9, hjust=0,
                                    gp=gpar(col="black", fontsize=title_size)))

# random25_holdout
plotA <- ggplot(datatab, aes(x=maxdepth_m, fill=factor(random25_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=depth_xlims)+
  scale_y_continuous(limits=depth_ylims)+
  annotation_custom(variable_label)+
  ggtitle('a) Small test dataset')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# random75_holdout
plotB <- ggplot(datatab, aes(x=maxdepth_m, fill=factor(random75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=depth_xlims)+
  scale_y_continuous(limits=depth_ylims)+
  annotation_custom(variable_label)+
  ggtitle('b) Large test dataset')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# cluster_strat75_holdout
plotC <- ggplot(datatab, aes(x=maxdepth_m, fill=factor(cluster_strat75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=depth_xlims)+
  scale_y_continuous(limits=depth_ylims)+
  annotation_custom(variable_label)+
  ggtitle('c) Stratified by ecosystem type')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# hu4_strat75_holdout
plotD <- ggplot(datatab, aes(x=maxdepth_m, fill=factor(hu4_strat75_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=depth_xlims)+
  scale_y_continuous(limits=depth_ylims)+
  annotation_custom(variable_label)+
  ggtitle('d) Stratified by region')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# cluster_random50_holdout
plotE <- ggplot(datatab, aes(x=maxdepth_m, fill=factor(cluster_random50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=depth_xlims)+
  scale_y_continuous(limits=depth_ylims)+
  annotation_custom(variable_label)+
  ggtitle('e) Targeted by ecosystem type')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# hu4_random50_holdout
plotF <- ggplot(datatab, aes(x=maxdepth_m, fill=factor(hu4_random50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=depth_xlims)+
  scale_y_continuous(limits=depth_ylims)+
  annotation_custom(variable_label)+
  ggtitle('f) Targeted by region')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

# hu4_ag50_holdout
plotG <- ggplot(datatab, aes(x=maxdepth_m, fill=factor(hu4_ag50_holdout))) + geom_density(alpha=0.75)+
  theme_bw()+
  scale_x_continuous(limits=depth_xlims)+
  scale_y_continuous(limits=depth_ylims)+
  annotation_custom(variable_label)+
  ggtitle('g) Targeted by land use regions')+
  scale_fill_manual(values=plot_colorz)+
  theme(panel.grid = element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=title_size, color='black'),
        plot.title=element_text(hjust=0, vjust=0, face='bold', size=title_size))

jpeg('graphics/density_plots/maxdepth.jpeg',width = 6,height = 5,units = 'in',res=600)
# plotting positions!
# (upper left, upper mid, uppper right,
# mid left, mid mid, mid right,
# bot left, bot mid, bot right)
grid.arrange(plotA, plotB, plotC,
             plotD, plotE, plotF,
             legend, blank.plot, plotG, nrow=3)
dev.off()



#### histograms: old  and unmaintained ####
# ## random75_holdout
# random75_holdout_train <- subset(datatab, random75_holdout=='Training')
# random75_holdout_test <- subset(datatab, random75_holdout=='Testing')
# jpeg('graphics/tp_histograms/Random_Lakesmall.jpeg',width = 5,height = 4,units = 'in',res=300)
# par(mfrow=c(1,2))
# hist(random75_holdout_train$tp, xlab='TP', ylim=ylimz, xlim=xlimz, las=1, breaks=seq(0,1200,10),
#      main='Random_Lakesmall', col='orange')
# mtext(side=3, 'Training')
# hist(random75_holdout_test$tp, xlab='TP', ylim=ylimz, xlim=xlimz, las=1, breaks=seq(0,1200,10),
#      main='Random_Lakesmall', col='royalblue')
# mtext(side=3, 'Testing')
# dev.off()
# 
# ## cluster_strat75_holdout
# cluster_strat75_holdout_train <- subset(datatab, cluster_strat75_holdout=='Training')
# cluster_strat75_holdout_test <- subset(datatab, cluster_strat75_holdout=='Testing')
# jpeg('graphics/tp_histograms/Local-EC_Lakesmall.jpeg',width = 5,height = 4,units = 'in',res=300)
# par(mfrow=c(1,2))
# hist(cluster_strat75_holdout_train$tp, xlab='TP', ylim=ylimz, xlim=xlimz, las=1, breaks=seq(0,1200,10),
#      main='Local-EC_Lakesmall', col='orange')
# mtext(side=3, 'Training')
# hist(cluster_strat75_holdout_test$tp, xlab='TP', ylim=ylimz, xlim=xlimz, las=1, breaks=seq(0,1200,10),
#      main='Local-EC_Lakesmall', col='royalblue')
# mtext(side=3, 'Testing')
# dev.off()
# 
# ## hu4_strat75_holdout
# hu4_strat75_holdout_train <- subset(datatab, hu4_strat75_holdout=='Training')
# hu4_strat75_holdout_test <- subset(datatab, hu4_strat75_holdout=='Testing')
# jpeg('graphics/tp_histograms/Regional-EC_Regionsmall.jpeg',width = 5,height = 4,units = 'in',res=300)
# par(mfrow=c(1,2))
# hist(hu4_strat75_holdout_train$tp, xlab='TP', ylim=ylimz, xlim=xlimz, las=1, breaks=seq(0,1200,10),
#      main='Regional-EC_Regionsmall', col='orange')
# mtext(side=3, 'Training')
# hist(hu4_strat75_holdout_test$tp, xlab='TP', ylim=ylimz, xlim=xlimz, las=1, breaks=seq(0,1200,10),
#      main='Regional-EC_Regionsmall', col='royalblue')
# mtext(side=3, 'Testing')
# dev.off()
# 
# ## cluster_random50_holdout
# cluster_random50_holdout_train <- subset(datatab, cluster_random50_holdout=='Training')
# cluster_random50_holdout_test <- subset(datatab, cluster_random50_holdout=='Testing')
# jpeg('graphics/tp_histograms/Local-EC_Lakemoderate.jpeg',width = 5,height = 4,units = 'in',res=300)
# par(mfrow=c(1,2))
# hist(cluster_random50_holdout_train$tp, xlab='TP', ylim=ylimz, xlim=xlimz, las=1, breaks=seq(0,1200,10),
#      main='Local-EC_Lakemoderate', col='orange')
# mtext(side=3, 'Training')
# hist(cluster_random50_holdout_test$tp, xlab='TP', ylim=ylimz, xlim=xlimz, las=1, breaks=seq(0,1200,10),
#      main='Local-EC_Lakemoderate', col='royalblue')
# mtext(side=3, 'Testing')
# dev.off()
# 
# ## hu4_random50_holdout
# hu4_random50_holdout_train <- subset(datatab, hu4_random50_holdout=='Training')
# hu4_random50_holdout_test <- subset(datatab, hu4_random50_holdout=='Testing')
# jpeg('graphics/tp_histograms/Random_Regionmoderate.jpeg',width = 5,height = 4,units = 'in',res=300)
# par(mfrow=c(1,2))
# hist(hu4_random50_holdout_train$tp, xlab='TP', ylim=ylimz, xlim=xlimz, las=1, breaks=seq(0,1200,10),
#      main='Random_Regionmoderate', col='orange')
# mtext(side=3, 'Training')
# hist(hu4_random50_holdout_test$tp, xlab='TP', ylim=ylimz, xlim=xlimz, las=1, breaks=seq(0,1200,10),
#      main='Random_Regionmoderate', col='royalblue')
# mtext(side=3, 'Testing')
# dev.off()
# 
# ## hu4_ag50_holdout
# hu4_ag50_holdout_train <- subset(datatab, hu4_ag50_holdout=='Training')
# hu4_ag50_holdout_test <- subset(datatab, hu4_ag50_holdout=='Testing')
# jpeg('graphics/tp_histograms/Regional-LU_Regionlarge.jpeg',width = 5,height = 4,units = 'in',res=300)
# par(mfrow=c(1,2))
# hist(hu4_ag50_holdout_train$tp, xlab='TP', ylim=ylimz, xlim=xlimz, las=1, breaks=seq(0,1200,10),
#      main='Regional-LU_Regionlarge', col='orange')
# mtext(side=3, 'Training')
# hist(hu4_ag50_holdout_test$tp, xlab='TP', ylim=ylimz, xlim=xlimz, las=1, breaks=seq(0,1200,10),
#      main='Regional-LU_Regionlarge', col='royalblue')
# mtext(side=3, 'Testing')
# dev.off()