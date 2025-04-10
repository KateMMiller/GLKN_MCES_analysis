#---------------------------------------
# Checking parameters b/t GLKN and MECS
#---------------------------------------
# GLKN sites vs MCES closest sites
# SACN_STCR_20.0   = 82000100-02; 1.2km
#                  82000100-03
# SACN_STCR_15.8 = 82000100-04; 0.25km
#                  82000100-05, 82000100-06
# SACN_STCR_2.0 =  82000100-07; 0.33km

library(tidyverse)
library(waterGLKN)

params <- read.csv("../data/GLKN_water/MECS/GLKN_vs_MECS_params_list_final.csv")

river_zip = ("../data/GLKN_water/records-2309369.zip")
lake_zip = ("../data/GLKN_water/records-2306516.zip")
importData(type = 'zip', filepath = c(river_zip, lake_zip))

res <- GLKN_WQ$Results
res$park <- substr(res$Location_ID, 1, 4)
res$year <- as.numeric(substr(res$Activity_Start_Date, 1, 4))
lksc <- c("SACN_STCR_20.0", "SACN_STCR_15.8", "SACN_STCR_2.0")
sacn <- res[res$park == "SACN" & res$Location_ID %in% lksc & res$Activity_Group_Type == "Field Set",]
table(sacn$Location_ID, sacn$year)
#table(sacn$param_name, sacn$year)

sacn_loc <- getLocations(park = "SACN", active = T)

sacn <- getResults(park = "SACN", site = lksc, #sample_type = "VS",
                   parameter = 'all',
                   months = 1:12,
                   sample_depth = 'all',
                   include_censored = T) |>
  select(Org_Code, Park_Code, Location_ID, Location_Name, sample_date, doy, year, month,
         depth_cat = Activity_Relative_Depth, depth = Activity_Depth, depth_unit = Activity_Depth_Unit,
         PARAMETER = Characteristic_Name, param_name, value, unit = Result_Unit)

table(sacn$Location_ID)

# Missing about 33 observations between original resutls and getResults.

sacn$param_name[sacn$param_name == "ChlA_ppb"] <- "ChlA_ugL"
sacn$unit[sacn$param_name == "ChlA_ppb"] <- "ug/l"

table(sacn$year)

mecs <- read.csv("../data/GLKN_water/MECS/GLKN_MCES_20060101_20250331.csv")
mecs$Units[mecs$PARAMETER == "pH"] <- "None"
mecs$Units[mecs$Units == "mg/L"] <- "mg/l"
mecs$Units[mecs$Units == "ug/L"] <- "ug/l"
mecs$Units[mecs$Units == "mg/L_CaCO3"] <- "mg/l_CaCO3"

mecs$Org_Code <- "MECS"
mecs$sample_date <- format(as.Date(mecs$DateTime, format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d")
mecs$year <- as.numeric(substr(mecs$sample_date, 1, 4))
mecs$month <- as.numeric(substr(mecs$sample_date, 6, 7))
mecs$doy <- as.numeric(format(as.Date(mecs$sample_date, format = "%Y-%m-%d"), "%j"))
mecs$depth_unit = "m"
mecs$depth_cat <- ifelse(mecs$Depth_m <= 1, "surface", NA_character_)
mecs1 <- mecs |> select(Org_Code, Park_Code = Park, Location_ID = StationID, Location_Name = Name, sample_date, doy, year, month,
                       depth_cat, depth = Depth_m, depth_unit, PARAMETER, value = Value, units = Units)

mecs_join <- left_join(mecs1, params, by = c("PARAMETER" = "Parameter", "units")) |>
  filter(!is.na(New_Name)) |>
  select(Org_Code, Park_Code, Location_ID, Location_Name, sample_date, doy, year, month, depth_cat, depth,
         depth_unit, PARAMETER, param_name = New_Name, value, unit= units)
mecs_keep <- mecs_join |> group_by(param_name) |> summarize(num_samps = sum(!is.na(value))) |> select(param_name) |> unique() |> c()

sacn2 <- sacn |> filter(param_name %in% mecs_keep$param_name)

full_dat <- rbind(sacn2, mecs_join)
table(full_dat$Org_Code, full_dat$param_name) # only params with samples in common across both

write.csv(full_dat, "./data/GLKN_water/MECS/GLKN_MCES_combined_data.csv", row.names = F)


# sacn_riv <- read.csv("../data/GLKN_water/Results_River_SACN.csv")
# sacn_riv$year <- substr(as.Date(sacn_riv$Activity_Start_Date, format = "%m/%d/%Y"), 1, 4)
# head(sacn_riv)
#
# table(sacn_riv$year, useNA = 'always')

