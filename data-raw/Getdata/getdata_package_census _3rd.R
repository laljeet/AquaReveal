# install.packages("devtools")
# devtools::install_github("rdinter/usdarnass")
WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)

library("usdarnass")
library("tidyverse", "stringr")
nass_set_key("02065135-7387-30C4-929C-D2963D721E02")
# First time, reload your enviornment so you can use the key without restarting R.
# readRenviron("~/.Renviron")
# You can check it with:
Sys.getenv("NASS_KEY")

# Year = c(1997,2002,2007,2012,2017)
 # Year = 2017
nass.ag.data <- function (Year) {nass_data(year = Year,
                          group_desc ="FIELD CROPS",
                       # short_desc = "AREA HARVESTED",
                        # domain_desc =c("AREA OPERATED","TOTAL"),
                       # domain_desc =c("TOTAL"),
                       agg_level_desc = "COUNTY",
                       state_name = "Virginia")
}



dat_2017 <- nass.ag.data(2017)
dat_2012 <- nass.ag.data(2012)
dat_2007<-nass.ag.data(2007)
dat_2002 <- nass.ag.data(2002)

dat_all <- rbind.data.frame(dat_2002,dat_2007,dat_2012,dat_2017)

Irrigated_dat_all <- dplyr::filter(dat_all, prodn_practice_desc == "IRRIGATED")
Irrigated_dat_all <- Irrigated_dat_all[,c(1,4,6,7,9,10,11,21,22,31,38)]

Irrigated_Area_all<- Irrigated_dat_all %>%
  filter(str_detect(short_desc, "IRRIGATED - ACRES HARVESTED"))

Irrigated_ops_all<- Irrigated_dat_all %>%
  filter(str_detect(short_desc, "IRRIGATED - OPERATIONS WITH AREA HARVESTED"))

d2017 <- Irrigated_Area_all %>% 
  dplyr::filter(year == 2017)

##
write.csv(Irrigated_Area_all, paste0(WUDR_github,"/csv_files/Coefficient3/Irirgated_area_crops.csv"))
write.csv(Irrigated_ops_all, paste0(WUDR_github,"/csv_files/Coefficient3/Irirgated_ops_crops.csv"))

#######################################################################################
#State level
nass.ag.data <- function (Year) {nass_data(year = Year,
                                           group_desc ="FIELD CROPS",
                                            # short_desc = "AREA HARVESTED",
                                            # domain_desc =c("AREA OPERATED","TOTAL"),
                                            
                                           domain_desc =c("TOTAL"),
                                           agg_level_desc = "STATE",
                                           state_name = "Virginia")
}



dat_2017 <- nass.ag.data(2017)
dat_2012 <- nass.ag.data(2012)
dat_2007<-nass.ag.data(2007)
dat_2002 <- nass.ag.data(2002)

dat_all <- rbind.data.frame(dat_2002,dat_2007,dat_2012,dat_2017)

County_level_dat_all <- dplyr::filter(dat_all, prodn_practice_desc == "IRRIGATED")
County_level_dat_all <- County_level_dat_all[,c(1,4,6,7,9,10,11,21,22,31,38)]

County_level_Area_all<- County_level_dat_all %>%
  filter(str_detect(short_desc, "IRRIGATED - ACRES HARVESTED"))

County_level_ops_all<- County_level_dat_all %>%
  filter(str_detect(short_desc, "IRRIGATED - OPERATIONS WITH AREA HARVESTED"))



Barley <- dat_2017 %>%
  filter(str_detect(short_desc, "BARLEY, IRRIGATED"))

Barley <- Barley[,-c(13:30)]
Barley <- Barley[,-c(14:19)]
  ##
write.csv(County_level_Area_all, paste0(WUDR_github,"/csv_files/Coefficient3/County_level_crop_area.csv"))
write.csv(County_level_ops_all, paste0(WUDR_github,"/csv_files/Coefficient3/County_level_crop_ops.csv"))


####################################################################
#Acreas Harvested

nass.ag.data <- function (Year) {nass_data(year = Year,
                                           group_desc ="FIELD CROPS",
                                           # short_desc = "AREA HARVESTED",
                                           # domain_desc =c("AREA OPERATED","TOTAL"),
                                           # domain_desc =c("TOTAL"),
                                           agg_level_desc = "COUNTY",
                                           state_name = "Virginia")
}



dat_2017 <- nass.ag.data(2017)
dat_2012 <- nass.ag.data(2012)
dat_2007<-nass.ag.data(2007)
dat_2002 <- nass.ag.data(2002)

dat_all <- rbind.data.frame(dat_2002,dat_2007,dat_2012,dat_2017)

Area_harvested_dat_all <- dplyr::filter(dat_all, prodn_practice_desc == "ALL PRODUCTION PRACTICES")
Area_harvested_dat_all <- Area_harvested_dat_all[,c(1,4,6,7,9,10,11,21,22,31,38)]

Area_harvested_dat_all<- Area_harvested_dat_all %>%
  filter(str_detect(short_desc, "- ACRES HARVESTED"))



##
write.csv(Area_harvested_dat_all, paste0(WUDR_github,"/csv_files/Coefficient3/Area_harvested_dat_all.csv"))

nass.ag.data <- function (Year) {nass_data(year = Year,
                                           group_desc ="FIELD CROPS",
                                           # short_desc = "AREA HARVESTED",
                                           # domain_desc =c("AREA OPERATED","TOTAL"),
                                           # domain_desc =c("TOTAL"),
                                           agg_level_desc = "STATE",
                                           state_name = "Virginia")
}
write.csv(County_level_Area_all, paste0(WUDR_github,"/csv_files/Coefficient3/state_level_crop_area.csv"))
