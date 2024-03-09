rm(list = ls())

library(tidyverse)
library(usdarnass)
my_nass_key <- "02065135-7387-30C4-929C-D2963D721E02"


dat_2022 <- AquaReveal::get_nass_data(2022, "south carolina", my_nass_key)
dat_2017 <- AquaReveal::get_nass_data(2017, "south carolina", my_nass_key)
dat_2012 <- AquaReveal::get_nass_data(2012, "south carolina", my_nass_key)
dat_2007 <- AquaReveal::get_nass_data(2007, "south carolina", my_nass_key)
dat_2002 <- AquaReveal::get_nass_data(2002, "south carolina", my_nass_key)


dat_2022 <- AquaReveal::get_nass_data(2022, "virginia", my_nass_key)
dat_2017 <- AquaReveal::get_nass_data(2017, "virginia", my_nass_key)
dat_2012 <- AquaReveal::get_nass_data(2012, "virginia", my_nass_key)
dat_2007 <- AquaReveal::get_nass_data(2007, "virginia", my_nass_key)
dat_2002 <- AquaReveal::get_nass_data(2002, "virginia", my_nass_key)


nass_binned_va <- rbind.data.frame(dat_2022,dat_2017,dat_2012,dat_2007,dat_2002)
# nass_binned_sc <- nass_binned

 usethis::use_data(nass_binned_va, overwrite = TRUE)

nass_binned <- AquaReveal::nass_binned_sc
unique_fips_county <- nass_binned %>%
  select(fips, County_Name) %>%
  distinct()

# nass_binned <- AquaReveal::nass_binned_va
YEAR =2017

#####################################################################
# Both these function should be run together. The final file from fn_Area_TH is the Non-Reported.


Year <- 2017

fn_Unreported <- function(Year) {
  QS_data(Year)
  fn_Area_TH(threshold = 25, YEAR = Year)

  Non.Reported$Year <- Year

  unique_fips_county <- nass_binned %>%
    select(fips, County_Name) %>%
    distinct()

    merged_data <- merge(Non.Reported, unique_fips_county, by.x = "County", by.y = "County_Name", all.x = TRUE)

  # Dynamically name the variable based on the Year and assign the merged data to it in the global environment
  var_name <- paste("Small_farm_unreported", Year, sep = "_")
  assign(var_name, merged_data, envir = .GlobalEnv)
}


fn_Unreported(2022)
fn_Unreported(2017)
fn_Unreported(2012)
fn_Unreported(2007)
fn_Unreported(2002)

Small_farm_unreported <- rbind.data.frame(Small_farm_unreported_2002,Small_farm_unreported_2007,
                                          Small_farm_unreported_2012,Small_farm_unreported_2017,
                                          Small_farm_unreported_2022)



library(sf)
library(tmap)
library(tmaptools)

shape <- AquaReveal::shape

shape_sc <- shape %>%
  dplyr::filter(Name == "SOUTH CAROLINA")

rm(shape)

merged_data <- merge(shape_sc, Small_farm_unreported, by.x = "GEOID", by.y="fips" )


merged_data_sf <- st_as_sf(merged_data)
# Save as .rds
saveRDS(merged_data_sf, "./data-raw/App/merged_data_sf.rds")

merged_data_sf <- readRDS("./data-raw/App/merged_data_sf.rds")

map1 <- tm_shape(merged_data_sf) +
  tm_polygons("AcreageUnderTh", title = "Acreage Under Threshold") +
  tm_facets(by = "Year", free.coords = FALSE, ncol = 2) +
  tm_layout(frame = FALSE)

tmap_save(map1, "./data-raw/Unreported_acres.png",width = 8,height = 8, units = "in", dpi = 600)

