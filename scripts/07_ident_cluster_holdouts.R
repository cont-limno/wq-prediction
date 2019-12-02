library(splitstackshape)
library(dplyr)
library(readr)

#read in data
lake_clusters_4groups <- read_csv("data/lake_clusters_4groups.csv")
wq2_single <- read_csv("data/wq2_single.csv")

#merge clusters with wq data and exclude any lakes that weren't clustered
cluster_sample <- wq2_single %>% select(lagoslakeid) %>%
  left_join(lake_clusters_4groups) %>% mutate(groups = as.character(groups)) %>%
  na.omit()

#assess data sampling bias by calcing sampling freq for each cluster
#full dataset (unbiased sample)
full <- table(lake_clusters_4groups$groups)/sum(table(lake_clusters_4groups$groups))
#observed dataset (what we sampled)
obs <- table(cluster_sample$groups)/sum(table(cluster_sample$groups))

#plot biases
dat.overview = data.frame(cluster=1:4,full=as.vector(full),obs=as.vector(obs))
dat.overview <- dat.overview %>% gather(key = "key",value = "value",-cluster)
ggplot(dat.overview, aes(fill=key, y=value, x=cluster)) +
  geom_bar(position="dodge", stat="identity") +labs(y="percent")

#assign holdout data
#cluster_strat75_holdout
#Training data is 25% of water chem samples taken stratfied by true distribution
#of data for all 50,000 lakes based on clusters identified using hclust cut at
#k=4

num.samples = round(nrow(wq2_single)*.25,digits=0)  #how many total samples to get
pop.samples = round(as.vector(full)*num.samples,digits=0) #how many to sample from each cluster
names(pop.samples) = c("1","2","3","4") #name vector
select.lakes = stratified(indt = cluster_sample, #random stratification
                          group = "groups",
                          size = pop.samples)

#assigned holdout values (TRUE == Training data, FALSE == validation data)
cluster_sample$cluster_strat75_holdout <- NA
cluster_sample$cluster_strat75_holdout[which(cluster_sample$lagoslakeid %in%
                                               select.lakes$lagoslakeid)] <- TRUE
cluster_sample$cluster_strat75_holdout[which(is.na(cluster_sample$cluster_strat75_holdout))] = FALSE

#cluster_random50_holdout
#hold out all samples from 2 clusters. Decided to hold out clusters 2 and 4
#because they are undersampled and represent hydrologically disconnected lakes
names(cluster_sample)[2] = "assigned_cluster"
cluster_sample$cluster_random50_holdout <- NA
cluster_sample$cluster_random50_holdout[which(cluster_sample$assigned_cluster=="2")] = FALSE
cluster_sample$cluster_random50_holdout[which(cluster_sample$assigned_cluster=="4")] = FALSE
cluster_sample$cluster_random50_holdout[which(is.na(cluster_sample$cluster_random50_holdout))] = TRUE


#write file
write_csv(cluster_sample,"data/cluster_holdout.csv")
