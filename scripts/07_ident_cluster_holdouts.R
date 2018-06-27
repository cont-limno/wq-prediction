lake_clusters_4groups <- read_csv("data/lake_clusters_4groups.csv")
wq2_single <- read_csv("data/wq2_single.csv")

cluster_sample <- wq2_single %>% select(lagoslakeid) %>%
  left_join(lake_clusters_4groups) %>% mutate(groups = as.character(groups)) %>%
  na.omit()

#assess data sampling bias
full <- table(lake_clusters_4groups$groups)/sum(table(lake_clusters_4groups$groups))
obs <- table(cluster_sample$groups)/sum(table(cluster_sample$groups))

dat.overview = data.frame(cluster=1:4,full=as.vector(full),obs=as.vector(obs))
dat.overview <- dat.overview %>% gather(key = "key",value = "value",-cluster)
ggplot(dat.overview, aes(fill=key, y=value, x=cluster)) +
  geom_bar(position="dodge", stat="identity") +labs(y="percent")

#assign holdout data
#cluster_strat75_holdout
num.samples = nrow(cluster_sample)*.25
pop.samples = round(as.vector(full)*num.samples,digits=0)
names(pop.samples) = c("1","2","3","4")
select.lakes = stratified(indt = cluster_sample, #random stratification
                          group = "groups",
                          size = pop.samples)

cluster_sample$cluster_strat75_holdout <- NA
cluster_sample$cluster_strat75_holdout[which(cluster_sample$lagoslakeid %in%
                                               select.lakes$lagoslakeid)] <- TRUE
cluster_sample$cluster_strat75_holdout[which(is.na(cluster_sample$cluster_strat75_holdout))] = FALSE

#cluster_random50_holdout
cluster_sample$cluster_random50_holdout <- NA
cluster_sample$cluster_random50_holdout[which(cluster_sample$groups==2)] = FALSE
cluster_sample$cluster_random50_holdout[which(cluster_sample$groups==4)] = FALSE
cluster_sample$cluster_random50_holdout[which(is.na(cluster_sample$cluster_random50_holdout))] = TRUE

names(cluster_sample)[2] = "assigned_cluster"

write_csv(cluster_sample,"data/cluster_holdout.csv")
