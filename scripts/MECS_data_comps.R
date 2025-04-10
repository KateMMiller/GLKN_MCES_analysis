#---------------------------------------
# Checking parameters b/t GLKN and MECS
#---------------------------------------
# GLKN sites vs MCES closest sites
# SACN_STCR_20   = 82000100-02; 1.2km
#                  82000100-03
# SACN_STCR_15.8 = 82000100-04; 0.25km
#                  82000100-05, 82000100-06
# SACN_STCR_2.0 =  82000100-07; 0.33km

library(tidyverse)
library(waterGLKN)

params <- read.csv("../data/GLKN_water/MECS/GLKN_vs_MECS_params_list_final.csv")

importData(type = 'zip', filepath = ("../data/GLKN_water/records-2309369.zip"))
lksc <- c("SACN_STCR_20", "SACN_STCR_15.8", "SACN_STCR_2.0")
sacn <- getResults(park = "SACN", site = lksc) |>
  select(Org_Code, Park_Code, Location_ID, Location_Name, sample_date, doy, year, month,
         depth_cat = Activity_Relative_Depth, depth = Activity_Depth, depth_unit = Activity_Depth_Unit,
         param_name, value, unit = Result_Unit)
head(sacn)
table(sacn$samp_type)

mecs <- read.csv("../data/GLKN_water/MECS/GLKN_MCES_20060101_20250331.csv")
mecs$Org_Code <- "MCES"
mecs$sample_date <- format(as.Date(mecs$DateTime, format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d")
mecs$year <- as.numeric(substr(mecs$sample_date, 1, 4))
mecs$month <- as.numeric(substr(mecs$sample_date, 6, 7))
mecs$doy <- as.numeric(format(as.Date(mecs$sample_date, format = "%Y-%m-%d"), "%j"))
mecs$units2 <- tolower(mecs$Units)
mecs$depth_unit = "m"
mecs$depth_cat <- ifelse(mecs$Depth_m <= 1, "surface", NA_character_)
mecs1 <- mecs |> select(Org_Code, Park_Code = Park, Location_ID = StationID, Location_Name = Name, sample_date, doy, year, month,
                       depth_cat, depth = Depth_m, depth_unit, PARAMETER, value = Value, units = units2)

mecs_join <- left_join(mecs1, params, by = )

head(params)
head(mecs1)




mecs_raw <- read.csv("../data/GLKN_water/MECS/2025-03-18_1359_56_MCES_EIMS_data.csv")
mecs_dat <- read.csv("../data/GLKN_water/MECS/GLKN_MCES_20060101_20250331.csv")

glkn_dat <- GLKN_rivers$Results
glkn_dat$park <- substr(glkn_dat$Location_ID, 1, 4)

sacn <- glkn_dat |> filter(park == "SACN") |>
  select(Location_ID,
         date = Activity_Start_Date,
         sample_type = Activity_Group_Type,
         rel_depth = Activity_Relative_Depth,
         sample_depth = Activity_Depth,
         param = Characteristic_Name,
         value = Result_Text,
         units = Result_Unit)

sacn_mat <- sacn |> filter(sample_type == "Field Set") |>
  group_by(param, units) |> summarize(num_samps = sum(!is.na(value)), .groups = 'drop')

mecs_mat <- mecs_dat |> group_by(param = PARAMETER, units = Units) |>
  summarize(num_samps = sum(!is.na(Value)), .groups = 'drop')

mecs_mat$units[mecs_mat$units == "mg/L"] <- "mg/l"
mecs_mat$units[mecs_mat$units == "ug/L"] <- "ug/l"


comp <- full_join(sacn_mat, mecs_mat, by = c("param", "units"),
                  suffix = c("_glkn", "_mecs")) |>
  arrange(param)

write.csv(comp, "../data/GLKN_water/MECS/GLKN_vs_MECS_params_list.csv", row.names = F)

comp

head(mecs_dat)






