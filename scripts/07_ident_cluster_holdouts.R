lake_clusters_4groups <- read_csv("data/lake_clusters_4groups.csv")
wq2_single <- read_csv("data/wq2_single.csv")

cluster_sample <- wq2_single %>% select(lagoslakeid) %>%
  left_join(lake_clusters_4groups)
