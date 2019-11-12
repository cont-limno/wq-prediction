library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(janitor)
library(cowplot)
library(latex2exp)
library(colorblindr) # devtools::install_github("clauswilke/colorblindr")
library(extrafont)
font_import()
View(fonttable())

# Interpolation scenarios:
# (2) random25 : Random-Large
# (1) random75: Random-Small
# (3) Local-EC_Lakesmall = Cluster_strat75_holdout : Stratified-Type
# (4) Regional-EC_Regionsmall = Hu4_strat75_holdout : Stratified-Region

# Then Extrapolation scenarios:
# (1) Local-EC_Lakemoderate = Cluster_random50_holdout :  Targeted-Type
# (2) Random_Regionmoderate = Hu4_random50_holdout : Targeted-Region
# (3) Regional-LU_Regionlarge = Hu4_ag50_holdout : Targeted-AgRegion

theme_opts <- theme(#axis.text.x = element_text(angle = 45, hjust = 0.98),
                    axis.text.x = element_text(angle = 90,
                    family = "Liberation Mono"),
                    legend.title = element_blank(),
                    panel.border = element_rect(fill = "transparent", size = 1.2),
                    legend.position = "none")

bar_size      <- 4.8
facet_spacing <- 1.85

(color_d <- c(
  rep("#ffef77", 2),
  rep("#b2df8a", 2),
  rep("#1f78b4", 3)
  ))
# scales::show_col(color_d)

label_names <- c(
  "(a) Random-Large", "(b) Random-Small", "(c) Stratified-Type",
  "(d) Stratified-Region", "(e) Targeted-Type", "(f) Targeted-Region",
  "(g) Targeted-AgRegion"
)
label_names <- stringr::str_pad(label_names, max(nchar(label_names)),
                                side = "right")

label_key <- data.frame(
  set = factor(c(
    "random25", "random75", "cluster_strat75",
    "hu4_strat", "cluster_random50", "hu4_random",
    "hu4_ago"
  ), levels = c(
    "random25", "random75", "cluster_strat75",
    "hu4_strat", "cluster_random50", "hu4_random",
    "hu4_ago"
  )),
  set_parsed = factor(label_names, levels = label_names),
  stringsAsFactors = FALSE
)

zero_pad <- function(x, width = 3){
  sapply(x, function(y){
    if(!is.na(y)){
      if(nchar(y) < width){
        stringr::str_pad(paste0(y, "."), width = 4, side = "right",
                         pad = "0")
      }else{
        y
      }
    }else{
      y
    }
  })
}

zero_pad2 <- function(x, width = 1){
  sapply(x, function(y){
    if(!is.na(y)){
      if(nchar(y) < width){
        stringr::str_pad(paste0(y, "."), width = 4, side = "right",
                         pad = "0")
      }else{
        y
      }
    }else{
      y
    }
  })
}

my_breaks <- function(x) {
  if(max(x) < 11){
    seq(0, 5, 1)
  }else{
    if(max(x) < 200){
      seq(0, 12, 3)
    }else{
      seq(0, 300, 50)
    }
  }
}

bar_plot_clean <- function(x){
  x <- x %>%
    mutate(variable = case_when(variable %in% c("chla") ~ "CHL",
                                TRUE ~ variable),
           set = case_when(set == "cluster_random50_holdout" ~ "cluster_random50",
                           TRUE ~ set)) %>%

    arrange(set) %>%
    dplyr::select(variable, set, value, stdev)

  clean <- x %>%
    dplyr::filter(!(variable %in% "secchi")) %>%
    left_join(label_key, by = "set") %>%
    # left_join(random_lines) %>%
    mutate(variable = factor(variable, levels = c("TP", "TN", "CHL", "secchi"))) %>%
    mutate(polation = case_when(
      set %in% c("random25", "random75",
                 "cluster_strat75", "hu4_strat") ~ "interpolation",
      TRUE ~ "extrapolation"))

  clean
}

# ---- rmse ----
raw <- read.csv("data/revision_datasets/mean_variance_statistics/MeanPerformanceAcross10.csv",
                 stringsAsFactors = FALSE) %>%
  janitor::clean_names() %>%
  rename(variable = target, set = splitting, value = avg_rmse) %>%
  mutate(stdev = replace_na(sqrt(variance_rmse), 0))

clean <- bar_plot_clean(raw) %>%
  mutate(ymin = value - (stdev / sqrt(10)), # we ran 10 replicate models
         ymax = value + (stdev / sqrt(10)))

(gg_rmse <-  ggplot(data = clean) +
  geom_segment(aes(xend = set_parsed, yend = 0, x = set_parsed,
                   y = value, color = set_parsed),
               lineend = "butt", size = bar_size) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax, x = set_parsed)) +
  ylim(0, 1) +
  facet_wrap(~variable) +
  scale_color_manual(values = color_d,
                     labels = parse(text = TeX(levels(clean$set_parsed)))) +
  theme_minimal() +
    theme_opts +
    theme(panel.spacing.x = unit(facet_spacing, "lines")) +
  ylab("RMSE") + xlab(""))

# ---- median_errors ----
raw <- read.csv("data/revision_datasets/mean_variance_statistics/MeanPerformanceAcross10.csv",
                stringsAsFactors = FALSE) %>%
  janitor::clean_names() %>%
  rename(variable = target, set = splitting, value = avg_median_error_abs) %>%
  mutate(stdev = replace_na(sqrt(variance_median_error_abs), 0))

clean <- bar_plot_clean(raw) %>%
  mutate(ymin = value - (stdev / sqrt(10)), # we ran 10 replicate models
         ymax = value + (stdev / sqrt(10)))

(gg_mrae <- ggplot(data = clean) +
      geom_segment(aes(xend = set_parsed, yend = 0, x = set_parsed,
                       y = value, color = set_parsed),
                   lineend = "butt", size = bar_size) +
    geom_errorbar(aes(ymin = ymin, ymax = ymax, x = set_parsed)) +
      scale_y_continuous(breaks = my_breaks,
                         labels = zero_pad) +
      facet_wrap(~variable, scales = "free", labeller = label_value) +
      scale_color_manual(values = color_d,
                         labels = parse(text = TeX(levels(clean$set_parsed)))) +
      theme_minimal() +
      theme_opts +
      ylab("MRAE") + xlab(""))

# ---- r2 ----
raw <- read.csv("data/revision_datasets/mean_variance_statistics/MeanPerformanceAcross10.csv",
                stringsAsFactors = FALSE) %>%
  janitor::clean_names() %>%
  rename(variable = target, set = splitting, value = avg_r2) %>%
  mutate(stdev = replace_na(sqrt(variance_r2), 0))

clean <- bar_plot_clean(raw) %>%
  mutate(ymin = value - (stdev / sqrt(10)), # we ran 10 replicate models
         ymax = value + (stdev / sqrt(10)))

(gg_r2 <- ggplot(data = clean) +
    geom_segment(aes(xend = set_parsed, yend = 0, x = set_parsed,
                     y = value, color = set_parsed),
                 lineend = "butt", size = bar_size) +
    geom_errorbar(aes(ymin = ymin, ymax = ymax, x = set_parsed)) +
    scale_y_continuous(breaks = seq(0.10, 0.60, by = 0.10),
                       labels = zero_pad2) +
    facet_wrap(~variable, labeller = label_value) +
    scale_color_manual(values = color_d,
                       labels = parse(text = TeX(levels(clean$set_parsed)))) +
    theme_minimal() +
    theme_opts +
    theme(panel.spacing.x = unit(facet_spacing, "lines")) +
    ylab(expression(R^{2})) + xlab(""))

(gg_all <- plot_grid(
  gg_r2  + theme(axis.text.x = element_blank()),
  gg_rmse + theme(axis.text.x = element_blank(),
                  strip.text = element_blank()),
  gg_mrae + theme(plot.title = element_blank(),
                  strip.text = element_blank()),
                                ncol = 1, rel_heights = c(0.59, 0.53, 0.85),
                                labels = c("A.", "B.", "C.")))

# plot_grid(ggplot() + geom_blank(color = "white"),
#           gg_all,
#           ncol = 2, rel_widths = c(0.01, 1))

ggsave("graphics/bar_plots.png", gg_all, height = 8)

# https://stackoverflow.com/a/20502085/3362993
# pdf("graphics/bar_plots.pdf")
# invisible(lapply(list(gg_all), print))
# dev.off()
