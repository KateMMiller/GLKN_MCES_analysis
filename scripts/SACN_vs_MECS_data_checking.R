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
table(res$park)

res$year <- as.numeric(substr(res$Activity_Start_Date, 1, 4))
lksc <- c("SACN_STCR_20.0", "SACN_STCR_15.8", "SACN_STCR_2.0")
sacn <- res[res$park == "SACN" & res$Location_ID %in% lksc & res$Activity_Group_Type == "Field Set" & !is.na(res$param_name),]
sacn$sample_date <- format(as.Date(sacn$Activity_Start_Date, format = "%Y-%m-%d"), "%Y-%m-%d")
sacn$year <- as.numeric(substr(sacn$sample_date, 1, 4))
head(sacn)
table(sacn$Location_ID, sacn$year)
#table(sacn$param_name, sacn$year)
head(sacn)
sacn2 <- read.csv("../data/GLKN_water/Results_River_SACN.csv")
sacn2$sample_date <- format(as.Date(sacn2$Activity_Start_Date, format = "%m/%d/%Y"), "%Y-%m-%d")
sacn2$year <- as.numeric(substr(sacn2$sample_date, 1, 4))
sacn3 <- sacn2[sacn2$Activity_Group_Type == "Field Set",]
head(sacn2)
table(sacn3$Location_ID, sacn3$year)

sacn4 <- getResults(park = "SACN", site = lksc, months = 1:12, sample_type = 'VS', include_censored = T)
table(sacn4$Location_ID, sacn4$year)
sacn_loc <- getLocations(park = "SACN", active = T)

unmatched <-
anti_join(sacn|> select(Location_ID, sample_date, year, param_name, Characteristic_Name, Result_Text, Result_Detection_Condition,
                        Activity_Group_Type),
          sacn4 |> select(Location_ID, sample_date, year, param_name, value, Activity_Group_Type),
          by = c("Location_ID", "sample_date", "year", "param_name", "Activity_Group_Type"))

head(sacn)
table(sacn$Location_ID, sacn$param_name)

table(sacn4$Location_ID, sacn4$param_name)

sacn <- getResults(park = "SACN", site = lksc, #sample_type = "VS",
                   parameter = 'all',
                   months = 1:12,
                   sample_depth = 'all',
                   include_censored = T) |>
  select(Org_Code, Park_Code, Location_ID, Location_Name, sample_date, doy, year, month,
         depth_cat = Activity_Relative_Depth, depth = Activity_Depth, depth_unit = Activity_Depth_Unit,
         PARAMETER = Characteristic_Name, param_name, value, unit = Result_Unit)

table(sacn$Location_ID)

# Missing about 33 observations between original results and getResults.
# These are all accounted for either as Non-detects, Non reports, or observations like cloud cover.
