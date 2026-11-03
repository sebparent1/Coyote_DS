library(tidyverse)
library(ggplot2)
library(skimr)
library(dplyr)
library(patchwork)
rm(list=ls())
list.files()

#----Read data----

detect<-read.csv("data/raw/OSM_independent_detections_2021-2024.csv")
site_covs<-read.csv("data/raw/OSM_site_covariates_2021-2024.csv")
daymet<-read.csv("data/processed/daymet_gt50.csv")
cam_days<-read.csv("data/raw/OSM_operating_days_2021-2024.csv")


#----Site covariate exploration----

buffer<-1000
covs_buffer<-site_covs|>
  filter(buffer_dist==buffer)


#----Dotplots----

#Cleveland dotplot for cfi
#Highest p for cfi is ~0.5, good density until ~0.3
ggplot(covs_buffer,
       aes(x=cfi, y=reorder(site,buffer)))+
  geom_segment(aes(x=0,xend=cfi,y=reorder(site,cfi),
  yend=reorder(site,cfi)),
  colour="grey70")+
  geom_point(aes(x=cfi,y=reorder(site,cfi)),
    size=2)+
  labs(title=paste("Cleveland Dotplot", buffer,"m buffer"),
x="Covariate value",
y=NULL)+
  theme_minimal()+
  theme(
    axis.text.y=element_blank()
  )

#Cleveland dotplot for cfi with harvest
#Highest p for cfi with harvest is ~0.5, slightly better spread than cfi
ggplot(covs_buffer,
       aes(x=cfi_with_harvest, y=reorder(site,buffer)))+
  geom_segment(aes(x=0,xend=cfi_with_harvest,y=reorder(site,cfi_with_harvest),
                   yend=reorder(site,cfi_with_harvest)),
               colour="grey70")+
  geom_point(aes(x=cfi_with_harvest,y=reorder(site,cfi_with_harvest)),
             size=2)+
  labs(title=paste("Cleveland Dotplot", buffer,"m buffer"),
       x="Covariate value",
       y=NULL)+
  theme_minimal()+
  theme(
    axis.text.y=element_blank()
  )

#Cleveland dotplot for coniferous landcover
#Good density until ~0.8
ggplot(covs_buffer,
       aes(x=lc_coniferous, y=reorder(site,buffer)))+
  geom_segment(aes(x=0,xend=lc_coniferous,y=reorder(site,lc_coniferous),
                   yend=reorder(site,lc_coniferous)),
               colour="grey70")+
  geom_point(aes(x=lc_coniferous,y=reorder(site,lc_coniferous)),
             size=2)+
  labs(title=paste("Cleveland Dotplot", buffer,"m buffer"),
       x="Covariate value",
       y=NULL)+
  theme_minimal()+
  theme(
    axis.text.y=element_blank()
  )

#Cleveland dotplot for shrub landcover
#Highest p for shrub is ~0.7, very little data spread though. Clustered around 0.05
ggplot(covs_buffer,
       aes(x=lc_shrubs, y=reorder(site,buffer)))+
  geom_segment(aes(x=0,xend=lc_shrubs,y=reorder(site,lc_shrubs),
                   yend=reorder(site,lc_shrubs)),
               colour="grey70")+
  geom_point(aes(x=lc_shrubs,y=reorder(site,lc_shrubs)),
             size=2)+
  labs(title=paste("Cleveland Dotplot", buffer,"m buffer"),
       x="Covariate value",
       y=NULL)+
  theme_minimal()+
  theme(
    axis.text.y=element_blank()
  )

#Cleveland dotplot for herb landcover
#Highest p for herb is ~0.25, good density until ~0.1. Good spread
ggplot(covs_buffer,
       aes(x=lc_herbs, y=reorder(site,buffer)))+
  geom_segment(aes(x=0,xend=lc_herbs,y=reorder(site,lc_herbs),
                   yend=reorder(site,lc_herbs)),
               colour="grey70")+
  geom_point(aes(x=lc_herbs,y=reorder(site,lc_herbs)),
             size=2)+
  labs(title=paste("Cleveland Dotplot", buffer,"m buffer"),
       x="Covariate value",
       y=NULL)+
  theme_minimal()+
  theme(
    axis.text.y=element_blank()
  )


#----Histograms----

#Histogram of cfi, right-skewed
cfi<-ggplot(covs_buffer,
       aes(x=cfi))+
  geom_histogram(bins=50)+
  theme_minimal()

#Histogram of cfi with harvest, right-skewed
cfi_harvest<-ggplot(covs_buffer,
       aes(x=cfi_with_harvest))+
  geom_histogram(bins=50)+
  theme_minimal()

#Histogram of harvest 0-15yrs, right-skewed
harvest_0_15<-ggplot(covs_buffer,
       aes(x=harvest_0_15))+
  geom_histogram(bins=50)+
  theme_minimal()

#Histogram of total seismic lines, right-skewed
seismic<-ggplot(covs_buffer,
       aes(x=seismic_total))+
  geom_histogram(bins=50)+
  theme_minimal()

#Histogram of vegetation edges, right-skewed
veg_edge<-ggplot(covs_buffer,
       aes(x=veg_edges))+
  geom_histogram(bins=50)+
  theme_minimal()

#Histogram of coniferous landcover, normal
coni_lc<-ggplot(covs_buffer,
       aes(x=lc_coniferous))+
  geom_histogram(bins=50)+
  theme_minimal()

#Histogram of shrub landcover, VERY right-skewed
shrub_lc<-ggplot(covs_buffer,
       aes(x=lc_shrubs))+
  geom_histogram(bins=50)+
  theme_minimal()

#Histogram of herb landcover, right-skewed
herb_lc<-ggplot(covs_buffer,
       aes(x=lc_herbs))+
  geom_histogram(bins=50)+
  theme_minimal()

#Histogram of intact forest, 
intact_forest<-ggplot(covs_buffer,
                aes(x=intact_forest))+
  geom_histogram(bins=50)+
  theme_minimal()

#Plot all together
(cfi|cfi_harvest|intact_forest|seismic)/(veg_edge|coni_lc|shrub_lc|herb_lc)



#----Climate data exploration----

#Bar chart of days since FTF
days_since<-ggplot(ftf_gt50_coy,
       aes(x=factor(ftf_length)))+
  geom_bar()+
  coord_cartesian(xlim=c(0,7),ylim=c(0,1000))+
  theme_minimal()


#Histogram of min temp, normal
min_temp<-ggplot(ftf_gt50_coy,
       aes(x=min_temp_C))+
  geom_histogram(bins=30)+
theme_minimal()

#Histogram of max temp, normal
max_temp<-ggplot(ftf_gt50_coy,
       aes(x=max_temp_C))+
  geom_histogram(bins=30)+
  theme_minimal()

#Histogram of precip
precip<-ggplot(ftf_gt50_coy,
       aes(x=precip_mm))+
  geom_histogram(bins=30)+
  theme_minimal()

#Histogram of precip
thickness<-ggplot(ftf_gt50_coy,
               aes(x=crust_thickness))+
  geom_histogram(bins=20)+
  theme_minimal()

#Plot all together
(days_since|precip|thickness)/(min_temp|max_temp)


#----Coyote detection exploration----

#Basic coyote stats
sum(ftf_gt50_coy$presence_absence==1,na.rn=TRUE) #224 for >50, 194 for 100 independent winter detections

n_distinct(ftf_gt50_coy$site) #225 sites for >50, 224 sites for 100 (231 total)

detect_site_percent<-ftf_gt50_coy|>
  group_by(site)|>
  summarise(
    total_days=n(),
    detect_days=sum(presence_absence==1,na.rm=TRUE),
    pct_detect=detect_days/total_days*100
  )

mean(detect_site_percent$pct_detect)#0.74% for >50, 0.72% for 100

#Histogram of coyote detections through the winter
ftf_gt50_coy<-ftf_gt50_coy|>
  mutate(month=factor(month))

ggplot(ftf_gt50_coy|>
         filter(presence_absence==1),
       aes(x=month))+
  geom_histogram(stat="count")+
  theme_minimal()

#Histogram of coyote detections after FTF for >50
ggplot(ftf_gt50_coy|>
         filter(presence_absence==1),
       aes(x=days_since_ftf))+
  geom_histogram(bins=50)+
  coord_cartesian(xlim=c(0,10))
  theme_minimal()

#Histogram of coyote detections after FTF for 100
ggplot(ftf_100_coy|>
         filter(presence_absence==1),
       aes(x=days_since_ftf))+
  geom_histogram(bins=50)+
  theme_minimal()

#Histogram of percent of camera days with a coyote detection for >50 snow
ggplot(detect_site_percent,
       aes(x=pct_detect))+
  geom_histogram(bins=30)+
  theme_minimal()

#Histogram of coyote detections and min temp
ggplot(ftf_gt50_coy|>
         filter(presence_absence==1),
       aes(x=min_temp_C))+
  geom_histogram(bins=50)+
  theme_minimal()

#Histogram of coyote detections and max temp
ggplot(ftf_gt50_coy|>
         filter(presence_absence==1),
       aes(x=max_temp_C))+
  geom_histogram(bins=50)+
  theme_minimal()

#Bar chart for presence/absence
ggplot(ftf_gt50_coy, aes(x=factor(presence_absence)))+
  geom_bar()+
  theme_minimal()

#Dotplot for coyote detections and precipitation
ggplot(ftf_gt50_coy|>
         filter(presence_absence==1),
       aes(x=precip_mm, y=reorder(site)))+
  geom_segment(aes(x=0,xend=precip_mm,y=reorder(site,precip_mm),
                   yend=reorder(site,precip_mm)),
               colour="grey70")+
  geom_point(aes(x=precip_mm,y=reorder(site,precip_mm)),
             size=2)+
  labs(title=paste("Cleveland Dotplot","m buffer"),
       x="Covariate value",
       y=NULL)+
  theme_minimal()+
  theme(
    axis.text.y=element_blank()
  )


#----Testing for collinearity----

#Spearman's correlation
covs_interest<-covs_buffer|>
  select(cfi,cfi_with_harvest,harvest_0_15,seismic_total,veg_edges,lc_coniferous,lc_mixedwood,lc_shrubs,lc_herbs,lc_broadleaf,roads,intact_forest)

cor(covs_interest,method="spearman")

climate_covs<-ftf_gt50_coy|>
  select(crust_thickness,snow_water_equiv_kgM2,days_since_ftf)

cor(climate_covs,method="spearman")

 #VIF (variance inflation factors)???

