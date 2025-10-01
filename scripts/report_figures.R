#-------------------------------------------------------
# Code to produce figures for SACN redundancy analysis
#   Written by Kate Miller, 20250930
#-------------------------------------------------------
library(tidyverse)
library(waterGLKN)

core_sites <- c("METC_STCR_23.3", "SACN_STCR_20.0", "SACN_STCR_15.8",
                "SACN_STCR_2.0", "METC_STCR_0.3")

dat <- read.csv("./data/local/GLKN_METC_combined_data.csv") |>
  filter(Location_ID %in% core_sites)
dat$year_cen <- dat$year - min(dat$year)
dat$date <- as.Date(dat$sample_date, format = "%Y-%m-%d")
dat$site <- factor(gsub("_STCR", "", dat$Location_ID),
                   levels = c("METC_23.3", "SACN_20.0",  "SACN_15.8", "SACN_2.0", "METC_0.3"))
format_data <- function(dat){dat |>
    mutate(ylength = ifelse(leap_year(year), 366, 365),
           wcos = cos(2*pi*doy/ylength),
           wsin = sin(2*pi*doy/ylength),
           numday = as.numeric(date)/1000,
           cont_year = year+(doy/ylength), # same as cont_year in wqtrends package
           nonzero = ifelse(value > 0, 1, 0))}

dat2 <- format_data(dat) |> filter(month %in% c(4:11)) |>
  filter(depth_cat %in% c("surface", "Surface") | is.na(depth_cat))

core_sites_abbr <- c("METC_23.3", "SACN_20.0", "SACN_15.8", "SACN_2.0", "METC_0.3")
core_params <-  c("Alkalinity_mgL", "ChlA_ugL", "Cl_mgL", "DOC_mgL", "DO_mgL", "NO2+NO3_ugL",
                  "P_ugL", "pH", "Secchi_m", "SO4_mgL", "SpecCond_uScm", "TempWater_C", "TSS_mgL")
param_labs = c("Alkalinity (mg/L)", "Chlorophyll-a (ug/L)", "Chloride (mg/L)",
               "Dissolved Organic Carbon (mg/L)", "Dissolved Oxygen (mg/L)",
               "Nitrite + Nitrate (ug/L)", "Total Phosphorous (ug/L)", "pH",
               "Secchi Depth (m)",
               "Sulfate (mg/L)", "Specific Conductance (uS/cm)", "Water Temp. (C)",
               "Total Suspended Solids (mg/L)")

label_df <- data.frame(param_abbr = core_params, label = param_labs)
dat3 <- left_join(dat2, label_df, by = c("param_name" = "param_abbr"))

plot_water_series <- function(df = dat2, param, sites = core_sites_abbr, y_range = NA,
                              file_suffix = "_full_axis"){
  y_lab = label_df$label[label_df$param_abbr %in% param]

  p <-
    ggplot(df |> filter(param_name %in% param) |> filter(site %in% sites) |>
             filter(depth_cat %in% c("Surface", "surface") | is.na(depth_cat)) |> droplevels(),
           aes(x = cont_year, y = value, group = site, color = site)) +
    geom_point(aes(shape = site), alpha = 0.3, size = 2) +
    geom_smooth(se = F) +
    scale_color_brewer(guide = "legend", palette = "Set1") +
    scale_shape_manual(values = c(16, 19, 19, 19, 16)) +
    facet_wrap(~site, nrow = 1) +
    scale_x_continuous(breaks = seq(2006, 2025, 3),
                      limits = c(2006, 2025)) +
    theme(axis.text.x = element_text(size = 13, face = 'bold', angle = 90, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(size = 12, face = 'bold'),
          axis.title.y = element_text(size = 12, face = 'bold'),
          strip.text = element_text(size = 12, face = 'bold'),
          legend.text = element_text(size = 12, face = 'bold'),
          legend.title = element_text(size = 12, face = 'bold'),
          legend.position = 'bottom',
          legend.key = element_blank(),
          strip.background = element_rect(color = '#696969', fill = 'grey90', linewidth = 0.4),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(color = '#696969', fill = 'white', linewidth = 0.4)) +
    labs(y = y_lab, x = NULL, color = "Site", shape = "Site") +
    {if(all(!is.na(y_range))) ylim(y_range)} +
    guides(shape = guide_legend(override.aes = list(size = 2.5)))
  print(p)
  ggsave(paste0("./figures/Loess_smoothed_", param, file_suffix, ".png"), height = 6, width = 8)
}

lapply(core_params[4], function(p){plot_water_series(param = p)})

# param specific plots
plot_water_series(param = "P_ugL", y_range = c(0, 150), file_suffix = "_reduced_y_axis")
plot_water_series(param = "Secchi_m", sites = c("SACN_20.0", "SACN_15.8", "SACN_2.0"),
                  file_suffix = "_GLKN_only")
plot_water_series(param = "Cl_mgL", y_range = c(0, 20), file_suffix = "_reduced_y_axis")
plot_water_series(param = "SO4_mgL", y_range = c(0, 15), file_suffix = "_reduced_y_axis")

dat4 <- dat3 |> filter(param_name %in% c("TempWater_C", "DO_mgL", "pH", "SpecCond_uScm")) #|>
  #filter(!(param_name %in% "SpecCond_uScm" & value > 300))
table(dat4$label)

dat4$label_ord <-
  factor(dat4$label, levels = c("Water Temp. (C)", "pH",
                                "Dissolved Oxygen (mg/L)", "Specific Conductance (uS/cm)"))

ggplot(dat4 |> filter(site %in% core_sites_abbr) |>
               filter(depth_cat %in% c("Surface", "surface") | is.na(depth_cat)) |>
         droplevels(),
           aes(x = cont_year, y = value, group = site, color = site)) +
    geom_point(aes(shape = site), alpha = 0.1, size = 2) +
    geom_smooth(se = F) +
    scale_color_brewer(guide = "legend", palette = "Set1") +
    scale_shape_manual(values = c(16, 19, 19, 19, 16)) +
    facet_wrap(~label_ord, nrow = 2, scales = "free_y") +
    scale_x_continuous(breaks = seq(2006, 2025, 3),
                       limits = c(2006, 2025)) +
    theme(axis.text.x = element_text(size = 13, face = 'bold', angle = 90, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(size = 12, face = 'bold'),
          axis.title.y = element_text(size = 12, face = 'bold'),
          strip.text = element_text(size = 12, face = 'bold'),
          legend.text = element_text(size = 12, face = 'bold'),
          legend.title = element_text(size = 12, face = 'bold'),
          legend.position = 'bottom',
          legend.key = element_blank(),
          strip.background = element_rect(color = '#696969', fill = 'grey90', linewidth = 0.4),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(color = '#696969', fill = 'white', linewidth = 0.4)) +
    labs(x = NULL, color = "Site", shape = "Site") +
    #{if(all(!is.na(y_range))) ylim(y_range)} +
    guides(shape = guide_legend(override.aes = list(size = 2.5)))

ggsave("./figures/Loess_smoothed_temp_pH_DO_SpCon_4panel_full_axis.png",
       height = 6, width = 8)

