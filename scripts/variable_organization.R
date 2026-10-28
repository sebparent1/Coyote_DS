library(tidyverse)
library(ggplot2)
library(skimr)
library(dplyr)
library(lubridate)
rm(list=ls())


#---Read data----

daily_climate<-read.csv("data/raw/OSM_DAYMET_daily_site_covariates.csv")

timelapse_21<-read.csv('data/raw/OSM_timelapse_2021.csv') |>select(-leftantler, -rightantler)
timelapse_22<-read.csv('data/raw/OSM_timelapse_2022.csv')|>select(-leftantler, -rightantler)
timelapse_23<-read.csv('data/raw/OSM_timelapse_2023.csv')|>select(-leftantler, -rightantler)
timelapse_24<-read.csv('data/raw/OSM_timelapse_2024.csv')|>select(-leftantler, -rightantler)

detect<-read.csv("data/raw/OSM_independent_detections_2021-2024.csv")

#----DAYMET sorting----

#Trim extra years
daily_climate<-daily_climate|>
  filter(year>=2021,year<=2025)

#Add date column
daily_climate<-daily_climate|>
  mutate(date=as.Date(paste(year,day),format="%Y%j"))

#Normalize site names
daily_climate<-daily_climate|>
  mutate(
    site=sub("^(LU[0-9]_)([0-9])$","\\10\\2",site),
    site=trimws(toupper(site))
  )


#----Snow sorting----

#Combine csv
timelapse_21_24<-bind_rows(timelapse_21,timelapse_22,timelapse_23,timelapse_24)

#Extract snow, site and date info
snow<-timelapse_21_24|>
  select(site,array,snow,datetime,month,year)

#Normalize site names
snow<-snow|>
  mutate(
    site=sub("^(LU[0-9]_)([0-9])$","\\10\\2",site),
    site=trimws(toupper(site))
  )

#Create date column and get one snow value per day
snow<-snow|>
  mutate(
    datetime=as.POSIXct(datetime,
                        format="%Y-%m-%dT%H:%M:%SZ",
                        tz="UTC"),
    date=as.Date(datetime))

snow<-snow|>
  filter(format(datetime,"%H:%M:%S")=="12:00:00")|>
  distinct(site,date,.keep_all=TRUE)


#----Combine snow and DAYMET----

daymet_snow<-snow|>
  left_join(daily_climate,by=c("site","date"))


#----Trim data to first/last snow (>50 or 100)

#Sort from first 100 to last 100
daymet_snow_100<-daymet_snow|>
  group_by(site)|>
  mutate(
    first_100=min(datetime[snow==100|snow=="Deep"],na.rm=TRUE),
    last_100=max(datetime[snow==100|snow=="Deep"],na.rm=TRUE)
  )|>
  ungroup()|>
  filter(datetime>=first_100&datetime<=last_100)

#Sort from first >50 to last >50
daymet_snow_gt50<-daymet_snow|>
  mutate(
    snow_chr=trimws(as.character(snow)),
    is_deep=snow_chr=="Deep",
    is_100=snow_chr=="100",
    is_gt50=snow_chr==">50",
    snow_season_5050=is_deep|is_100|is_gt50
  )|>
  group_by(site)|>
  mutate(  
    first_50=min(datetime[snow_season_5050],na.rm=TRUE),
    last_50=max(datetime[snow_season_5050],na.rm=TRUE)
  )|>
  ungroup()|>
  filter(datetime>=first_50&datetime<=last_50)


#----Freeze thaw event ID----

#ID FTF and create days since FTF column
#Snow cover: 100
daymet_ftf_100<-daymet_snow_100|>
  mutate(
    winter_year=if_else(month(date) >= 10 ,year(date),year(date)-1),
    winter=month(date)%in%c(10,11,12,1,2,3,4),
    freeze=min_temp_C<0,
    thaw=max_temp_C>0,
    ftf_event=as.integer(
      winter&
        freeze&lag(thaw,default=FALSE)
  ))|>
  group_by(site,winter_year)|>
  mutate(
    idx=row_number(),
    last_event_idx=cummax(if_else(ftf_event==1,idx,0)),
    days_since_ftf=if_else(last_event_idx==0,NA_integer_, idx-last_event_idx)
  )|>
  ungroup()

#Snow cover: >50
daymet_ftf_gt50<-daymet_snow_gt50|>
  mutate(
    winter_year=if_else(month(date) >= 10 ,year(date),year(date)-1),
    winter=month(date)%in%c(10,11,12,1,2,3,4),
    freeze=min_temp_C<0,
    thaw=max_temp_C>0,
    ftf_event=as.integer(
      winter&
        freeze&lag(thaw,default=FALSE)
    ))|>
  group_by(site,winter_year)|>
  mutate(
    idx=row_number(),
    last_event_idx=cummax(if_else(ftf_event==1,idx,0)),
    days_since_ftf=if_else(last_event_idx==0,NA_integer_, idx-last_event_idx)
  )|>
  ungroup()

#Write csvs
daymet_ftf_gt50|>write.csv("data/processed/daymet_gt50.csv")


#----Coyote detection sorting----

#Add date column
detect<-detect|>
  mutate(
    datetime=as.POSIXct(datetime,
                        format="%Y-%m-%dT%H:%M:%SZ",
                        tz="UTC"),
    date=as.Date(datetime))

#Remove non-coyote observations and make it daily
detect_coy<-detect|>
  filter(species=="coyote")|>
  group_by(site,date)|>
  summarise(presence_absence=1,.groups="drop")

#Combine coyote and FTF for >50 snow
ftf_gt50_coy<-daymet_ftf_gt50|>
  left_join(detect_coy,by=c("site","date"))

ftf_gt50_coy<-ftf_gt50_coy|>
select(site,date,month,snow,days_since_ftf,presence_absence,max_temp_C,min_temp_C,precip_mm)

ftf_gt50_coy<-ftf_gt50_coy|>
  mutate(presence_absence=if_else(is.na(presence_absence),0L,1L))

#Combinecoyote and FTF for 100 snow
ftf_100_coy<-daymet_ftf_100|>
  left_join(detect_coy,by=c("site","date"))

ftf_100_coy<-ftf_100_coy|>
  select(site,date,month,snow,days_since_ftf,presence_absence,max_temp_C,min_temp_C,precip_mm)

ftf_100_coy<-ftf_100_coy|>
  mutate(presence_absence=if_else(is.na(presence_absence),0L,1L))
