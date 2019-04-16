library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(janitor)
library(cowplot)
library(latex2exp)
library(colorblindr)

# Interpolation scenarios:
# (2) random25 : Random-Small
# (1) random75: Random-Large
# (3) Local-EC_Lakesmall = Cluster_strat75_holdout : Stratified-Type
# (4) Regional-EC_Regionsmall = Hu4_strat75_holdout : Stratified-Region

# Then Extrapolation scenarios:
# (1) Local-EC_Lakemoderate = Cluster_random50_holdout :  Targeted-Type
# (2) Random_Regionmoderate = Hu4_random50_holdout : Targeted-Region
# (3) Regional-LU_Regionlarge = Hu4_ag50_holdout : Targeted-AgRegion

theme_opts <- theme(axis.text.x = element_text(angle = 90),
                    legend.title = element_blank(),
                    panel.border = element_rect(fill = "transparent", size = 1.2),
                    legend.position = "none")

bar_size <- 4.8

(color_d <- c(
  rep("#b2df8a", 4),
  rep("#1f78b4", 3)
  ))
# scales::show_col(color_d)

label_key <- data.frame(
  set = factor(c(
    "random25", "random75", "cluster_strat75", "hu4_strat",
    "cluster_random50", "hu4_random",
    "hu4_ago"
  ), levels = c(
    "random25", "random75", "cluster_strat75", "hu4_strat",
    "cluster_random50", "hu4_random",
    "hu4_ago"
  )),
  set_parsed = factor(c(
    "Random-Small", "Random-Large", "Stratified-Type",
    "Stratified-Region", "Targeted-Type", "Targeted-Region",
    "Targeted-AgRegion"
  ), levels = c(
    "Random-Small", "Random-Large", "Stratified-Type",
    "Stratified-Region", "Targeted-Type", "Targeted-Region",
    "Targeted-AgRegion")),
  stringsAsFactors = FALSE
)

bar_plot_clean <- function(raw){
  random_lines <- raw %>%
    tidyr::spread(set, value)

  clean <- raw %>%
    dplyr::filter(!(variable %in% "secchi")) %>%
    left_join(label_key, by = "set") %>%
    left_join(random_lines) %>%
    mutate(variable = factor(variable, levels = c("TP", "TN", "Chla", "secchi"))) %>%
    mutate(polation = case_when(
      set %in% c("random25", "random75",
                 "cluster_strat75", "hu4_strat") ~ "interpolation",
      TRUE ~ "extrapolation"))

  clean
}

# ---- rmse ----
raw           <- readxl::read_excel(
  "data/bar_plot_data/2019.03.08LPEP2-RMSE-results_dec2018.xlsx",
                          sheet = "conditional-combined plot")
names(raw)[1] <- "variable"
raw <- tidyr::gather(raw, key = set, value = value, -variable)

clean <- bar_plot_clean(raw)

(gg_rmse <- ggplot(data = clean) +
  geom_segment(aes(xend = set_parsed, yend = 0, x = set_parsed,
                   y = value, color = set_parsed),
               lineend = "butt", size = bar_size) +
  facet_wrap(~variable) +
  scale_color_manual(values = color_d,
                     labels = parse(text = TeX(levels(clean$set_parsed)))) +
  theme_minimal() +
    theme_opts +
  ylab("RMSE") + xlab(""))

# ---- median_errors ----

raw           <- readxl::read_excel(
  "data/bar_plot_data/median_error_03082019Qi.xlsx",
  sheet = "conditional")[,1:4] %>%
  setNames(c("variable", "set", "error_relative", "error_nonrelative")) %>%
  janitor::remove_empty(which = "rows") %>%
  tidyr::fill(variable, .direction = "down") %>%
  select(-error_relative) %>%
  rename(value = error_nonrelative) %>%
  mutate(set = case_when(set %in% c("cluster_random50_holdout") ~ "cluster_random50",
                              TRUE ~ set)) %>%
  mutate(variable = case_when(variable %in% c("chla") ~ "Chla",
                             TRUE ~ variable))

clean <- bar_plot_clean(raw)

(gg_mrae <- ggplot(data = clean) +
      geom_segment(aes(xend = set_parsed, yend = 0, x = set_parsed,
                       y = value, color = set_parsed),
                   lineend = "butt", size = bar_size) +
      facet_wrap(~variable, scales = "free", labeller = label_value) +
      scale_color_manual(values = color_d,
                         labels = parse(text = TeX(levels(clean$set_parsed)))) +
      theme_minimal() +
      theme_opts +
      ylab("MRAE") + xlab(""))

# ---- r2 ----

raw           <- readxl::read_excel(
  "data/bar_plot_data/R_squared_03072019Qi.xlsx",
  sheet = "conditional")[1:35,1:3] %>%
  setNames(c("variable", "set", "value")) %>%
  mutate(value = as.numeric(value)) %>%
  janitor::remove_empty(which = "rows") %>%
  tidyr::fill(variable, .direction = "down") %>%
  mutate(set = case_when(set %in% c("cluster_random50_holdout") ~ "cluster_random50",
                         TRUE ~ set)) %>%
  mutate(variable = case_when(variable %in% c("chla") ~ "Chla",
                              TRUE ~ variable))

clean      <- bar_plot_clean(raw)

(gg_r2 <- ggplot(data = clean) +
    geom_segment(aes(xend = set_parsed, yend = 0, x = set_parsed,
                     y = value, color = set_parsed),
                 lineend = "butt", size = bar_size) +
    facet_wrap(~variable, labeller = label_value) +
    scale_color_manual(values = color_d,
                       labels = parse(text = TeX(levels(clean$set_parsed)))) +
    theme_minimal() +
    theme_opts +
    ylab(expression(R^{2})) + xlab(""))

ggsave("graphics/rmse_bar.png", gg_rmse, height = 5)
ggsave("graphics/mrae_bar.png", gg_mrae, height = 5)
ggsave("graphics/r2_bar.png", gg_r2)

# https://stackoverflow.com/a/20502085/3362993
pdf("graphics/bar_plots.pdf")
invisible(lapply(list(gg_rmse, gg_mrae, gg_r2), print))
dev.off()
