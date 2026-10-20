library(tidyverse)
library(ggplot2)
library(skimr)
rm(list=ls())
list.files()

#Read data----
detections<-read.csv("data/processed/OSM_independent_detections_2021-2024.csv")
covs<-read.csv("data/processed/OSM_DAYMET_annual_site_covariates.csv")

names(covs)
