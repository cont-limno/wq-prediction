# Explore LAGOSNE color data considering its spatial distribution

# ---- load_color_data ----

library(LAGOSNE)
library(tibble)
library(dplyr)
library(magrittr)
library(tmap)
library(USAboundaries)
library(sf)

lg  <- lagosne_load("1.087.1")

epi <- lg$epi_nutr
# filter between june 15 to sep 15
epi <- dplyr::filter(epi, samplemonth >= 6 & samplemonth <= 9)
epi <- dplyr::select(epi, lagoslakeid, sampledate,
                     chla, tp, tn, colora, colort)

# ---- how_many_lakes_have_color ----

have_color <- epi %>%
  group_by(lagoslakeid) %>%
  summarize(any(!is.na(colora)))
names(have_color) <- c("lagoslakeid", "has_color")

paste(sum(have_color$has_color), "lakes or",
  round(sum(have_color$has_color) / nrow(have_color) , 2) * 100, # 22
  "percent of lakes have some color data")

# ---- do_color_lakes_have_other_response_variables ----

epi <- dplyr::filter(epi, lagoslakeid %in%
                       dplyr::filter(have_color, has_color)$lagoslakeid)
epi <- group_by(epi, lagoslakeid, sampledate)
epi <- dplyr::filter(epi, any(!is.na(tp) | !is.na(tn) | !is.na(chla)))

paste(length(unique(epi$lagoslakeid)), "lakes or",
  round(length(unique(epi$lagoslakeid)) /
          length(unique(lg$epi_nutr$lagoslakeid)), 2) * 100, # 21.5
  "percent of lakes with color data have at least on additional response variable")

# table by state

epi <- ungroup(epi) %>%
  dplyr::filter(!is.na(colora) | !is.na(colort)) %>%
  group_by(lagoslakeid) %>%
  dplyr::filter(abs(sampledate - as.Date("2006-07-31")) ==
                  min(abs(sampledate - as.Date("2006-07-31"))))

# sum(!is.na(epi$colora))
# sum(!is.na(epi$colort))
#
# plot(epi$colort, epi$colora)
# plot(epi$chla, epi$colora)
# plot(epi$tp, epi$colora)
# plot(epi$tn, epi$colora)
# hist(log(epi$colora))

epi <- left_join(epi,
                 dplyr::select(lg$locus, lagoslakeid, state_zoneid, hu4_zoneid))
epi <- left_join(epi,
                 dplyr::select(lg$state, state_zoneid, state))

knitr::kable(t(table(epi$state)))

# ---- mapping ----

epi <- left_join(epi,
                 dplyr::select(lg$locus, lagoslakeid, nhd_long, nhd_lat))
epi <- coordinatize(epi)

us_states <- st_intersects(us_states(), epi)
us_states <- us_states()[unlist(lapply(us_states, function(x) length(x) > 0)),]
us <- USAboundaries::us_boundaries()
us <- us[unlist(lapply(st_intersects(us, epi), function(x) length(x) > 0)),]
us <- filter(us, stusps != "SD")

epi_extent <- group_by(us, jurisdiction_type)
epi_extent <- st_union(epi_extent)
epi_extent <- as_Spatial(epi_extent)

epi$logcolora <- log(epi$colora)

tm_shape(us) + tm_polygons() +
  tm_shape(epi_extent) + tm_borders(lwd = 3) +
  tm_shape(epi) + tm_dots("logcolora",
                          title = "log(Apparent Color)",
                          size = 0.6) +
  tm_layout(legend.outside = FALSE,
             frame = FALSE,
            legend.position = c(0.65, 0.55))
