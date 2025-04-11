#---------------------------------------
# Checking parameters b/t GLKN and MECS
#---------------------------------------
# GLKN sites vs MCES closest sites
# SACN_STCR_20.0 = 82000100-02; 1.2km
#                  82000100-03
# SACN_STCR_15.8 = 82000100-04; 0.25km
#                  82000100-05, 82000100-06
# SACN_STCR_2.0 =  82000100-07; 0.33km

library(tidyverse)
library(waterGLKN)

params <- read.csv("./data/GLKN_vs_MECS_params_list_final.csv")

# GLKN data compile
river_zip = ("../data/GLKN_water/records-2309369.zip")
lake_zip = ("../data/GLKN_water/records-2306516.zip")
importData(type = 'zip', filepath = c(river_zip, lake_zip))

lksc <- c("SACN_STCR_20.0", "SACN_STCR_15.8", "SACN_STCR_2.0")
sacn <- getResults(park = "SACN", site = lksc, sample_type = "VS",
                   parameter = 'all',
                   months = 1:12,
                   sample_depth = 'all',
                   include_censored = T) |>
  select(Org_Code, Park_Code, Location_ID, Location_Name, sample_date, doy, year, month,
         depth_cat = Activity_Relative_Depth, depth = Activity_Depth, depth_unit = Activity_Depth_Unit,
         PARAMETER = Characteristic_Name, param_name, value, unit = Result_Unit)

sacn$param_name[sacn$param_name == "ChlA_ppb"] <- "ChlA_ugL"
sacn$unit[sacn$param_name == "ChlA_ppb"] <- "ug/l"

# MECS data compile
mecs <- read.csv("../data/GLKN_water/MECS/GLKN_MCES_20060101_20250331.csv") # data from Rick
mecs$Units[mecs$PARAMETER == "pH"] <- "None"
mecs$Units[mecs$Units == "mg/L"] <- "mg/l"
mecs$Units[mecs$Units == "ug/L"] <- "ug/l"
mecs$Units[mecs$Units == "mg/L_CaCO3"] <- "mg/l_CaCO3"

mecs$Org_Code <- "METC"
mecs$Park_Code <- "METC"
mecs$sample_date <- format(as.Date(mecs$DateTime, format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d")
mecs$year <- as.numeric(substr(mecs$sample_date, 1, 4))
mecs$month <- as.numeric(substr(mecs$sample_date, 6, 7))
mecs$doy <- as.numeric(format(as.Date(mecs$sample_date, format = "%Y-%m-%d"), "%j"))
mecs$depth_unit = "m"
mecs$depth_cat <- ifelse(mecs$Depth_m <= 1, "surface", NA_character_)
head(mecs)
mecs1 <- mecs |> mutate(RESULT = ifelse(PARAMETER == "Chlorophyll-a, Pheo-Corrected", Value/1000, Value)) |>
  select(Org_Code, Park_Code = Park, Location_ID = StationID, Location_Name = Name, sample_date, doy, year, month,
         depth_cat, depth = Depth_m, depth_unit, PARAMETER, value = Value, units = Units)

mecs_join <- left_join(mecs1, params, by = c("PARAMETER" = "Parameter", "units")) |>
  filter(!is.na(New_Name)) |>
  select(Org_Code, Park_Code, Location_ID, Location_Name, sample_date, doy, year, month, depth_cat, depth,
         depth_unit, PARAMETER, param_name = New_Name, value, unit= units)
mecs_keep <- mecs_join |> group_by(param_name) |> summarize(num_samps = sum(!is.na(value))) |> select(param_name) |> unique() |> c()

sacn2 <- sacn |> filter(param_name %in% mecs_keep$param_name)

full_dat <- rbind(sacn2, mecs_join)
table(full_dat$Org_Code, full_dat$param_name) # only params with samples in common across both
range(full_dat$sample_date[full_dat$Org_Code == "METC"])
range(full_dat$sample_date[full_dat$Org_Code == "GLKN"])
# GLKN data stops at 11/2023

# METC_STCR_23.4 had very narrow periods of record and weren't replacement sites
# for others with longer records (eg 23.6). Dropping for analysis.
table(full_dat$Location_ID, full_dat$year)
full_dat <- full_dat |> filter(!Location_ID %in% c("METC_STCR_23.4")) |>
  filter(year < 2024)

table(full_dat$Location_ID, full_dat$param_name)
# It appears that METC_STCR_12.4, METC_STCR_15.3, METC_STCR_16.6, METC_STCR_22.3, METC_STCR_22.5,
# only ever collect SecchiDepth


write.csv(full_dat, "./data/GLKN_MCES_combined_data.csv", row.names = F)
