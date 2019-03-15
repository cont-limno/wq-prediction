################# LPEP2: TP frequency distributions ###########################################################
# Date: 3-15-19
# updated: 
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####


#### input data ####
setwd('C:/Users/FWL/Documents/wq-prediction')

# water quality/ecological context for different model scenarios
datatab <- read.csv("data/raw_data_LPEP2_03082019.csv")


################## Main program #################
datatab <- datatab[,c(1,5:12)] #keep only some columns

# assign "Training" to 1, "Testing" to 0, then reorder factor levels
datatab$random25_holdout <- as.factor(ifelse(datatab$random25_holdout==1, 'Training','Testing'))
datatab$random25_holdout <- factor(datatab$random25_holdout,levels(datatab$random25_holdout)[c(2,1)])

datatab$random75_holdout <- as.factor(ifelse(datatab$random75_holdout==1, 'Training','Testing'))
datatab$random75_holdout <- factor(datatab$random75_holdout,levels(datatab$random75_holdout)[c(2,1)])

datatab$hu4_ag50_holdout <- as.factor(ifelse(datatab$hu4_ag50_holdout==1, 'Training','Testing'))
datatab$hu4_ag50_holdout <- factor(datatab$hu4_ag50_holdout,levels(datatab$hu4_ag50_holdout)[c(2,1)])

datatab$hu4_strat75_holdout <- as.factor(ifelse(datatab$hu4_strat75_holdout==1, 'Training','Testing'))
datatab$hu4_strat75_holdout <- factor(datatab$hu4_strat75_holdout,levels(datatab$hu4_strat75_holdout)[c(2,1)])

datatab$hu4_random50_holdout <- as.factor(ifelse(datatab$hu4_random50_holdout==1,'Training','Testing'))
datatab$hu4_random50_holdout <- factor(datatab$hu4_random50_holdout,levels(datatab$hu4_random50_holdout)[c(2,1)])

datatab$cluster_strat75_holdout <- as.factor(ifelse(datatab$cluster_strat75_holdout==1,'Training','Testing'))
datatab$cluster_strat75_holdout <- factor(datatab$cluster_strat75_holdout,levels(datatab$cluster_strat75_holdout)[c(2,1)])

datatab$cluster_random50_holdout <- as.factor(ifelse(datatab$cluster_random50_holdout==1,'Training','Testing'))
datatab$cluster_random50_holdout <- factor(datatab$cluster_random50_holdout,levels(datatab$cluster_random50_holdout)[c(2,1)])

### Histograms
xlimz <- c(0,300)
ylimz <- c(0,1500)

## random25_holdout
random25_holdout_train <- subset(datatab, random25_holdout=='Training')
random25_holdout_test <- subset(datatab, random25_holdout=='Testing')
jpeg('graphics/tp_histograms/Random_Lakelarge.jpeg',width = 5,height = 4,units = 'in',res=300)
par(mfrow=c(1,2))
hist(random25_holdout_train$tp, xlab='TP', ylim=ylimz, xlim=xlimz, las=1, breaks=seq(0,1200,10),
     main='Random_Lakelarge', col='orange')
mtext(side=3, 'Training')
hist(random25_holdout_test$tp, xlab='TP', ylim=ylimz, xlim=xlimz, las=1, breaks=seq(0,1200,10),
     main='Random_Lakelarge', col='royalblue')
mtext(side=3, 'Testing')
dev.off()

## random75_holdout
random75_holdout_train <- subset(datatab, random75_holdout=='Training')
random75_holdout_test <- subset(datatab, random75_holdout=='Testing')
jpeg('graphics/tp_histograms/Random_Lakesmall.jpeg',width = 5,height = 4,units = 'in',res=300)
par(mfrow=c(1,2))
hist(random75_holdout_train$tp, xlab='TP', ylim=ylimz, xlim=xlimz, las=1, breaks=seq(0,1200,10),
     main='Random_Lakesmall', col='orange')
mtext(side=3, 'Training')
hist(random75_holdout_test$tp, xlab='TP', ylim=ylimz, xlim=xlimz, las=1, breaks=seq(0,1200,10),
     main='Random_Lakesmall', col='royalblue')
mtext(side=3, 'Testing')
dev.off()

## cluster_strat75_holdout
cluster_strat75_holdout_train <- subset(datatab, cluster_strat75_holdout=='Training')
cluster_strat75_holdout_test <- subset(datatab, cluster_strat75_holdout=='Testing')
jpeg('graphics/tp_histograms/Local-EC_Lakesmall.jpeg',width = 5,height = 4,units = 'in',res=300)
par(mfrow=c(1,2))
hist(cluster_strat75_holdout_train$tp, xlab='TP', ylim=ylimz, xlim=xlimz, las=1, breaks=seq(0,1200,10),
     main='Local-EC_Lakesmall', col='orange')
mtext(side=3, 'Training')
hist(cluster_strat75_holdout_test$tp, xlab='TP', ylim=ylimz, xlim=xlimz, las=1, breaks=seq(0,1200,10),
     main='Local-EC_Lakesmall', col='royalblue')
mtext(side=3, 'Testing')
dev.off()

## hu4_strat75_holdout
hu4_strat75_holdout_train <- subset(datatab, hu4_strat75_holdout=='Training')
hu4_strat75_holdout_test <- subset(datatab, hu4_strat75_holdout=='Testing')
jpeg('graphics/tp_histograms/Regional-EC_Regionsmall.jpeg',width = 5,height = 4,units = 'in',res=300)
par(mfrow=c(1,2))
hist(hu4_strat75_holdout_train$tp, xlab='TP', ylim=ylimz, xlim=xlimz, las=1, breaks=seq(0,1200,10),
     main='Regional-EC_Regionsmall', col='orange')
mtext(side=3, 'Training')
hist(hu4_strat75_holdout_test$tp, xlab='TP', ylim=ylimz, xlim=xlimz, las=1, breaks=seq(0,1200,10),
     main='Regional-EC_Regionsmall', col='royalblue')
mtext(side=3, 'Testing')
dev.off()

## cluster_random50_holdout
cluster_random50_holdout_train <- subset(datatab, cluster_random50_holdout=='Training')
cluster_random50_holdout_test <- subset(datatab, cluster_random50_holdout=='Testing')
jpeg('graphics/tp_histograms/Local-EC_Lakemoderate.jpeg',width = 5,height = 4,units = 'in',res=300)
par(mfrow=c(1,2))
hist(cluster_random50_holdout_train$tp, xlab='TP', ylim=ylimz, xlim=xlimz, las=1, breaks=seq(0,1200,10),
     main='Local-EC_Lakemoderate', col='orange')
mtext(side=3, 'Training')
hist(cluster_random50_holdout_test$tp, xlab='TP', ylim=ylimz, xlim=xlimz, las=1, breaks=seq(0,1200,10),
     main='Local-EC_Lakemoderate', col='royalblue')
mtext(side=3, 'Testing')
dev.off()

## hu4_random50_holdout
hu4_random50_holdout_train <- subset(datatab, hu4_random50_holdout=='Training')
hu4_random50_holdout_test <- subset(datatab, hu4_random50_holdout=='Testing')
jpeg('graphics/tp_histograms/Random_Regionmoderate.jpeg',width = 5,height = 4,units = 'in',res=300)
par(mfrow=c(1,2))
hist(hu4_random50_holdout_train$tp, xlab='TP', ylim=ylimz, xlim=xlimz, las=1, breaks=seq(0,1200,10),
     main='Random_Regionmoderate', col='orange')
mtext(side=3, 'Training')
hist(hu4_random50_holdout_test$tp, xlab='TP', ylim=ylimz, xlim=xlimz, las=1, breaks=seq(0,1200,10),
     main='Random_Regionmoderate', col='royalblue')
mtext(side=3, 'Testing')
dev.off()

## hu4_ag50_holdout
hu4_ag50_holdout_train <- subset(datatab, hu4_ag50_holdout=='Training')
hu4_ag50_holdout_test <- subset(datatab, hu4_ag50_holdout=='Testing')
jpeg('graphics/tp_histograms/Regional-LU_Regionlarge.jpeg',width = 5,height = 4,units = 'in',res=300)
par(mfrow=c(1,2))
hist(hu4_ag50_holdout_train$tp, xlab='TP', ylim=ylimz, xlim=xlimz, las=1, breaks=seq(0,1200,10),
     main='Regional-LU_Regionlarge', col='orange')
mtext(side=3, 'Training')
hist(hu4_ag50_holdout_test$tp, xlab='TP', ylim=ylimz, xlim=xlimz, las=1, breaks=seq(0,1200,10),
     main='Regional-LU_Regionlarge', col='royalblue')
mtext(side=3, 'Testing')
dev.off()