#---------------------------------------
# Checking parameters b/t GLKN and metc
#---------------------------------------
# GLKN sites vs MCES closest sites
# SACN_STCR_20.0 = 82000100-02; 1.2km
#                  82000100-03
# SACN_STCR_15.8 = 82000100-04; 0.25km
#                  82000100-05, 82000100-06
# SACN_STCR_2.0 =  82000100-07; 0.33km

library(tidyverse)
library(waterGLKN)

params <- read.csv("C:/NETN/R_Dev/data/GLKN_water/GLKN_vs_METC_params_list_final.csv")

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

# metc data from webserver
metc <- read.csv("C:/NETN/R_Dev/data/GLKN_water/MECS/2025-03-18_1359_56_MCES_EIMS_data_GLKN_params.csv")
head(metc)

metc1 <- metc %>%
  mutate(Location_ID = case_when(grepl("SC0003", STATION_ID) ~ as.character("METC_STCR_0.1"),
                                 grepl("SC0233", STATION_ID) ~ as.character("METC_STCR_23.6"),
                                 grepl("SC0234", STATION_ID) ~ as.character("METC_STCR_23.4"),
                                 grepl("82000100-08", STATION_ID) ~ as.character("METC_STCR_22.5"),
                                 grepl("82000100-06", STATION_ID) ~ as.character("METC_STCR_11.3"),
                                 grepl("82000100-03", STATION_ID) ~ as.character("METC_STCR_16.6"),
                                 grepl("82000100-05", STATION_ID) ~ as.character("METC_STCR_12.4"),
                                 grepl("82000100-04", STATION_ID) ~ as.character("METC_STCR_15.3"),
                                 grepl("82000100-01", STATION_ID) ~ as.character("METC_STCR_22.3"))) |>
  filter(!is.na(Location_ID)) # UM8128 is teh only station that doesn't get matched with Rick's new LocIDs

metc1$Units[metc1$PARAMETER == "pH"] <- "None"
metc1$Units[metc1$UNITS == "mg/L"] <- "mg/l"
metc1$Units[metc1$UNITS == "ug/L"] <- "ug/l"
metc1$Units[metc1$UNITS == "mg/L_CaCO3"] <- "mg/l_CaCO3"

metc1$Org_Code <- "METC"
metc1$Park_Code <- "METC"
metc1$sample_date <- format(as.Date(metc1$START_DATE_TIME, format = "%m/%d/%Y %H:%M"), format = "%Y-%m-%d")
metc1$year <- as.numeric(substr(metc1$sample_date, 1, 4))
metc1$month <- as.numeric(substr(metc1$sample_date, 6, 7))
metc1$doy <- as.numeric(format(as.Date(metc1$sample_date, format = "%Y-%m-%d"), "%j"))
metc1$depth_unit = "m"
metc1$depth_cat <- ifelse(metc1$SAMPLE_DEPTH_m <= 1, "surface", NA_character_)
head(metc1)

metc2 <- metc1 |> mutate(RESULT = ifelse(PARAMETER == "Chlorophyll-a, Pheo-Corrected", RESULT/1000, RESULT)) |>
  select(Org_Code, Park_Code, Location_ID, Location_Name = NAME, sample_date, doy, year, month,
         depth_cat, depth = SAMPLE_DEPTH_m, depth_unit, PARAMETER, value = RESULT, units = UNITS)

head(metc2)
View(metc2)

table(metc2$PARAMETER, metc2$Location_ID)

# metc data compile
metc <- read.csv("../data/GLKN_water/metc/GLKN_MCES_20060101_20250331.csv") # data from Rick
metc$Units[metc$PARAMETER == "pH"] <- "None"
metc$Units[metc$Units == "mg/L"] <- "mg/l"
metc$Units[metc$Units == "ug/L"] <- "ug/l"
metc$Units[metc$Units == "mg/L_CaCO3"] <- "mg/l_CaCO3"

orig_params <-
c("Alkalinity, total", "Alkalinity, Total (total hydroxide+carbonate+bicarbonate)", "Calcium",
  "Chlorophyll a, free of pheophytin", "Chlorophyll a, free of pheophytin", "Chlorophyll a, free of pheophytin",
  "Chloride", "Dissolved Oxygen", "Dissolved oxygen (DO)", "Carbon, organic", "Organic carbon", "Dissolved Oxygen Saturation",
"Dissolved oxygen saturation", "Potassium", "Magnesium", "Sodium", "Nitrate + Nitrite", "Nitrogen, Nitrite (NO2) + Nitrate (NO3) as N",
"Phosphorus", "Phosphorus as P", "pH", "pH", "Depth, Secchi Disk Depth", "Depth, Secchi disk depth", "Secchi Depth", "Silicate",
"Sulfate", "Sulfur, sulfate (SO4) as SO4", "Specific conductance", "Temperature, water", "Transparency, tube with disk",
'Solids, Suspended (TSS)', 'Total suspended solids', "Ammonium", "Calcium, Filtered", "Chloride, Filtered",
"Chlorophyll-a, Pheo-Corrected", "Magnesium, Filtered", "Nitrogen", "Nitrogen, Ammonium (NH4) as N",
"Potassium, Filtered", "Sodium, Filtered", "Total Alkalinity, Unfiltered", "Total Phosphorus, Unfiltered", "Total Phosphorus, Unfiltered, Low Level Detection")


metc2 <- metc |> filter(PARAMETER %in% orig_params)
table(metc2$StationID, metc2$PARAMETER)

metc$Org_Code <- "METC"
metc$Park_Code <- "METC"
metc$sample_date <- format(as.Date(metc$DateTime, format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d")
metc$year <- as.numeric(substr(metc$sample_date, 1, 4))
metc$month <- as.numeric(substr(metc$sample_date, 6, 7))
metc$doy <- as.numeric(format(as.Date(metc$sample_date, format = "%Y-%m-%d"), "%j"))
metc$depth_unit = "m"
metc$depth_cat <- ifelse(metc$Depth_m <= 1, "surface", NA_character_)
head(metc)
metc1 <- metc |> mutate(RESULT = ifelse(PARAMETER == "Chlorophyll-a, Pheo-Corrected", Value/1000, Value)) |>
  select(Org_Code, Park_Code = Park, Location_ID = StationID, Location_Name = Name, sample_date, doy, year, month,
         depth_cat, depth = Depth_m, depth_unit, PARAMETER, value = Value, units = Units)

metc_join <- left_join(metc1, params, by = c("PARAMETER" = "Parameter", "units")) |>
  filter(!is.na(New_Name)) |>
  select(Org_Code, Park_Code, Location_ID, Location_Name, sample_date, doy, year, month, depth_cat, depth,
         depth_unit, PARAMETER, param_name = New_Name, value, unit= units)
metc_keep <- metc_join |> group_by(param_name) |> summarize(num_samps = sum(!is.na(value))) |> select(param_name) |> unique() |> c()

sacn2 <- sacn |> filter(param_name %in% metc_keep$param_name)

full_dat <- rbind(sacn2, metc_join)
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
