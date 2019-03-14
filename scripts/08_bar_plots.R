library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)

# ---- rmse ----
raw           <- readxl::read_excel(
  "data/bar_plot_data/2019.03.08LPEP2-RMSE-results_dec2018.xlsx",
                          sheet = "conditional-combined plot")
names(raw)[1] <- "variable"
raw <- tidyr::gather(raw, key = set, value = value, -variable)

clean <- raw %>%
  filter(!(set %in% c("random25", "random75"))) %>%
  mutate(set = factor(set, levels = c("cluster_strat75", "hu4_strat",
                                         "cluster_random50", "hu4_random",
                                         "hu4_ago")))

ggplot(data = clean) +
  geom_segment(aes(xend = set, yend = 0, x = set, y = value),
               lineend = "butt", size = 8) +
  # geom_hline() +
  # ylim(0.3, 1) +
  facet_wrap(~variable) +
  theme(axis.text.x = element_text(angle = 90))




# ---- median_errors ----


# ---- r2 ----

