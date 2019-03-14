library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(janitor)

bar_plot_clean <- function(raw){
  random_lines <- raw %>%
    filter(set %in% c("random25", "random75")) %>%
    tidyr::spread(set, value)

  clean <- raw %>%
    filter(!(set %in% c("random25", "random75"))) %>%
    mutate(set = factor(set, levels = c("cluster_strat75", "hu4_strat",
                                        "cluster_random50", "hu4_random",
                                        "hu4_ago"))) %>%
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

gg_rmse <- ggplot(data = clean) +
  geom_segment(aes(xend = set, yend = 0, x = set, y = value, color = polation),
               lineend = "butt", size = 8) +
  geom_hline(aes(yintercept = random25), linetype = "dashed") +
  geom_hline(aes(yintercept = random75)) +
  facet_wrap(~variable) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.title=element_blank()) +
  ylab("RMSE") + xlab("")

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

gg_mrae <- ggplot(data = clean) +
  geom_segment(aes(xend = set, yend = 0, x = set, y = value, color = polation),
               lineend = "butt", size = 8) +
  geom_hline(aes(yintercept = random25), linetype = "dashed") +
  geom_hline(aes(yintercept = random75)) +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.title=element_blank()) +
  ylab("MRAE") + xlab("")

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

gg_r2 <- ggplot(data = clean, aes(variable, value)) +
  geom_bar(aes(fill = set), stat = "identity", position = position_dodge2()) +
  geom_segment(aes(x = x, xend = xend, y = random25, yend = random25),
               linetype = "dashed") +
  geom_segment(aes(x = x, xend = xend, y = random75, yend = random75)) +
  theme_minimal() +
  theme(legend.title=element_blank()) +
  ylab("R2") + xlab("")

ggsave("graphics/rmse_bar.png", gg_rmse, height = 7)
ggsave("graphics/mrae_bar.png", gg_mrae, height = 7)
ggsave("graphics/r2_bar.png", gg_r2)

# https://stackoverflow.com/a/20502085/3362993
pdf("graphics/bar_plots.pdf")
invisible(lapply(list(gg_rmse, gg_mrae, gg_r2), print))
dev.off()
