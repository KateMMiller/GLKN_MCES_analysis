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

params <- read.csv("./data/GLKN_vs_METC_params_list_final.csv")

# GLKN data compile
river_zip = ("../data/GLKN_water/records-2309369.zip")
lake_zip = ("../data/GLKN_water/records-2306516.zip")
importData(type = 'zip', filepath = c(river_zip, lake_zip))
list.files("../data/GLKN_water/2309369")

res <- read.csv("../data/GLKN_Water/2309369/Results.csv") |>
  select(param = Characteristic_Name, Location_ID, Activity_Start_Date, Activity_Group_Type, units = Result_Unit)

sacn <- res |> filter(Activity_Group_Type == "Field Set") |> filter(Location_ID %in% lksc)
df <- unique(data.frame(sacn$param, sacn$Location_ID, sacn$units))

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
data.frame(table(sacn$PARAMETER))

# metc data from webserver
metc <- read.csv("./data/2025-03-18_1359_56_MCES_EIMS_data_GLKN_params.csv")
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

# metc1$UNITS[metc1$PARAMETER == "pH"] <- "None"
# metc1$UNITS[metc1$UNITS == "mg/L"] <- "mg/l"
# metc1$UNITS[metc1$UNITS == "ug/L"] <- "ug/l"
# metc1$UNITS[metc1$UNITS == "mg/L_CaCO3"] <- "mg/l_CaCO3"

metc1$Org_Code <- "METC"
metc1$Park_Code <- "METC"
metc1$sample_date <- format(as.Date(metc1$START_DATE_TIME, format = "%m/%d/%Y %H:%M"), format = "%Y-%m-%d")
metc1$year <- as.numeric(substr(metc1$sample_date, 1, 4))
metc1$month <- as.numeric(substr(metc1$sample_date, 6, 7))
metc1$doy <- as.numeric(format(as.Date(metc1$sample_date, format = "%Y-%m-%d"), "%j"))
metc1$depth_unit = "m"
metc1$depth_cat <- ifelse(metc1$SAMPLE_DEPTH_m <= 1, "surface", NA_character_)
head(metc1)

metc2 <- metc1 |>
  mutate(value = ifelse(PARAMETER %in% c( "Chlorophyll-a, Pheo-Corrected",
                                           "Total Nitrate/Nitrite N, Unfiltered",
                                           "Total Phosphorus, Unfiltered"),
                                           RESULT/1000,
                                           RESULT)) |>
  select(Org_Code, Park_Code, Location_ID, Location_Name = NAME, sample_date, doy, year, month,
         depth_cat, depth = SAMPLE_DEPTH_m, depth_unit, PARAMETER, value, units = UNITS)

head(metc2)
head(params)
table(metc2$PARAMETER)

metc_join <- left_join(metc2, params, by = c("PARAMETER" = "Parameter", "units" = "Units")) |>
  filter(!is.na(New_Name)) |>
  select(Org_Code, Park_Code, Location_ID, Location_Name, sample_date, doy, year, month, depth_cat, depth,
         depth_unit, PARAMETER, param_name = New_Name, value, unit= units)

table(metc_join$param_name, metc_join$Location_ID)
metc_keep <- metc_join |> group_by(param_name) |> summarize(num_samps = sum(!is.na(value))) |> select(param_name) |> unique() |> c()

sacn2 <- sacn |> filter(param_name %in% metc_keep$param_name)

full_dat <- rbind(sacn2, metc_join)
table(full_dat$Org_Code, full_dat$param_name) # only params with samples in common across both
range(full_dat$sample_date[full_dat$Org_Code == "METC"])
range(full_dat$sample_date[full_dat$Org_Code == "GLKN"])
# GLKN STCR data stops at 11/2023

# METC_STCR_23.4 had very narrow periods of record and weren't replacement sites
# for others with longer records (eg 23.6). Dropping for analysis.
table(full_dat$Location_ID, full_dat$year)
full_dat2 <- full_dat |> #filter(!Location_ID %in% c("METC_STCR_23.4")) |>
  filter(year < 2024)

table(full_dat$Location_ID, full_dat$param_name)
# It appears that METC_STCR_12.4, METC_STCR_15.3, METC_STCR_16.6, METC_STCR_22.3, METC_STCR_22.5,
# only ever collect SecchiDepth
write.csv(full_dat2, "./data/GLKN_METC_combined_data.csv", row.names = F)

