library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(janitor)
library(cowplot)
library(latex2exp)

# Interpolation scenarios:
# (1) Local-EC_Lakesmall = Cluster_strat75_holdout and
# (2) Regional-EC_Regionsmall = Hu4_strat75_holdout

# Then Extrapolation scenarios:
# (1) Local-EC_Lakemoderate = Cluster_random50_holdout
# (2) Random_Regionmoderate = Hu4_random50_holdout
# (3) Regional-LU_Regionlarge = Hu4_ag50_holdout

label_key <- data.frame(
  set = factor(c(
    "hu4_ago", "hu4_strat",
    "hu4_random", "cluster_strat75",
    "cluster_random50"
  ), levels = c(
    "cluster_strat75", "hu4_strat",
    "cluster_random50", "hu4_random",
    "hu4_ago"
  )),
  set_parsed = factor(c(
    "B-Regional-LU Region_{large}",
    "SR-Regional-EC Region_{small}", "B-Random Region_{moderate}", "SR-Local-EC Lake_{small}", "B-Local-EC Lake_{moderate}"
  ), levels = c("SR-Local-EC Lake_{small}", "SR-Regional-EC Region_{small}", "B-Local-EC Lake_{moderate}", "B-Random Region_{moderate}", "B-Regional-LU Region_{large}")),
  stringsAsFactors = FALSE
)

bar_plot_clean <- function(raw){
  random_lines <- raw %>%
    filter(set %in% c("random25", "random75")) %>%
    tidyr::spread(set, value)

  clean <- raw %>%
    filter(!(set %in% c("random25", "random75"))) %>%
    left_join(label_key, by = "set") %>%
    left_join(random_lines) %>%
    mutate(variable = factor(variable, levels = c("TP", "TN", "Chla", "secchi"))) %>%
    mutate(polation = case_when(set %in% c("cluster_strat75", "hu4_strat") ~ "interpolation",
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
                   y = value, color = polation),
               lineend = "butt", size = 8) +
  geom_hline(aes(yintercept = random25), linetype = "dashed") +
  geom_hline(aes(yintercept = random75)) +
  facet_wrap(~variable) +
  scale_color_manual(values = c("royalblue", "orange")) +
  scale_x_discrete(labels = parse(text = TeX(levels(clean$set_parsed)))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.title = element_blank()) +
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

(gg_mrae <- plot_grid(
    ggplot(data = dplyr::filter(clean, variable %in% c("TP", "TN"))) +
      geom_segment(aes(xend = set_parsed, yend = 0, x = set_parsed,
                       y = value, color = polation),
                   lineend = "butt", size = 8) +
      geom_hline(aes(yintercept = random25), linetype = "dashed") +
      geom_hline(aes(yintercept = random75)) +
      facet_wrap(~variable, scales = "free", labeller = label_value) +
      scale_color_manual(values = c("royalblue", "orange")) +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            legend.title=element_blank()) +
      ylab("MRAE") + xlab(""),
    ggplot(data = dplyr::filter(clean, !(variable %in% c("TP", "TN")))) +
      geom_segment(aes(xend = set_parsed, yend = 0, x = set_parsed,
                       y = value, color = polation),
                   lineend = "butt", size = 8) +
      geom_hline(aes(yintercept = random25), linetype = "dashed") +
      geom_hline(aes(yintercept = random75)) +
      facet_wrap(~variable, scales = "free", labeller = label_value) +
      scale_color_manual(values = c("royalblue", "orange")) +
      scale_x_discrete(labels = parse(text = TeX(levels(clean$set_parsed)))) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90),
            legend.title=element_blank()) +
      ylab("MRAE") + xlab(""),
    ncol = 1, rel_heights = c(0.55, 1)))


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
clean$x    <- rep(seq(0.5, 4), each = 5)
clean$xend <- rep(seq(1.5, 5), each = 5)

(gg_r2 <- ggplot(data = clean, aes(variable, value)) +
  geom_bar(aes(fill = set_parsed), stat = "identity", position = position_dodge2()) +
  geom_segment(aes(x = x, xend = xend, y = random25, yend = random25),
               linetype = "dashed") +
  geom_segment(aes(x = x, xend = xend, y = random75, yend = random75)) +
  scale_fill_manual(values = c("orange", "#f4b183", "royalblue", "#9dc3e6", "#bdd7ee"),
                    labels = parse(text = TeX(levels(clean$set_parsed)))) +
  theme_minimal() +
  theme(legend.title=element_blank()) +
  ylab(expression(R^{2})) + xlab(""))

ggsave("graphics/rmse_bar.png", gg_rmse, height = 7)
ggsave("graphics/mrae_bar.png", gg_mrae, height = 7)
ggsave("graphics/r2_bar.png", gg_r2)

# https://stackoverflow.com/a/20502085/3362993
pdf("graphics/bar_plots.pdf")
invisible(lapply(list(gg_rmse, gg_mrae, gg_r2), print))
dev.off()
