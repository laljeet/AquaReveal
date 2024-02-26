WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)

pacman::p_load(dplyr, rgdal, tmap,tidyverse, gridExtra)
VA_counties<-readOGR(paste0(WUDR_github,"/VA_counties_sp"), layer="VA_counties_new")
# year=2002

common_data_fn <- function(year){
deq_dat<-read.csv(paste0(WUDR_github,"/csv_files/", year,"Summary_DEQ_withdarwals.csv"))

data_deq<-deq_dat[complete.cases(deq_dat),]


# _____________________________________________________________________________________________

census_dat<-read.csv(paste0(WUDR_github,"/csv_files/",year, "County_data_NASS.csv"))


census_dat<-merge.data.frame(census_dat,deq_dat[,c("GEOID","COUNTYFP")], by.x = "County_Code", by.y ="COUNTYFP", all.x = TRUE )

data_census<-census_dat[complete.cases(census_dat),]

#________________________________________________________________________________________________



data_deq$data_status<- data_deq$GEOID %in% data_census$GEOID
data_deq$data_status<-ifelse(data_deq$data_status == TRUE, "Available in both datasets", "Available in DEQ dataset")


#_________________________________________________________________________________________________________

data_census$data_status<-data_census$GEOID%in% data_deq$GEOID
data_census$data_status<-ifelse(data_census$data_status == TRUE, "Available in both datasets", "Available in USDA dataset")


#__________________________________________________________________________________________________________

dat1<-merge.data.frame(data_census[,c("GEOID", "data_status")],
                       data_deq[,c("GEOID", "data_status")], by.x="GEOID", by.y="GEOID", all.x = TRUE, all.y=TRUE)

dat1$data_status.x <- ifelse(is.na(dat1$data_status.x), dat1$data_status.y, dat1$data_status.x)

#____________________________________________________________________________________________________________________

deq_dat$status<-deq_dat$GEOID %in% dat1$GEOID
deq_dat_missing<-filter(deq_dat, status== FALSE)
deq_dat_missing$status[deq_dat_missing$status==FALSE] <- c("Missing in both datsets")

dat_final<-as.data.frame(mapply(c,dat1[,c("GEOID", "data_status.x")],deq_dat_missing[,c("GEOID","status")]))
colnames(dat_final)[2]<- "Data Status"



plotdat<-sp::merge(VA_counties,dat_final, 
                   by.x = "GEOID", by.y = "GEOID", all.x=TRUE)

summary<-plotdat@data[,c(1,3,10,11)]
write.csv(summary,paste0(WUDR_github,"/csv_files/deqdat_avaliability_summary", year,".csv"))

}

common_data_fn(2002)
common_data_fn(2007)
common_data_fn(2012)
common_data_fn(2017)
