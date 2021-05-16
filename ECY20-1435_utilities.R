#Manuscript ID: ECY20-1435
#Contents: Utilities -- load libraries, import data files, establish figure theme

#libraries
library(reshape2)
library(ggplot2)
library(openxlsx)
library(vegan)
library(dplyr)
library(lme4)
library(car)
library(lsmeans)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(ggpubr)
library(tidyverse)
library(BiodiversityR)
library(fitdistrplus)
library(data.table)
library(coxme)
library(ggeffects)
library(scales)


#import data
data.climate<- openxlsx::read.xlsx("C:/Users/croyj/Dropbox/ARCA/Manuscripts/Arthropod Time Series/Finalized for Publication/Raw Data/ECY20-1435_climate.xlsx",sheet = "raw",startRow = 1,colNames = TRUE)

data.climate2<-openxlsx::read.xlsx("C:/Users/croyj/Dropbox/ARCA/Manuscripts/Arthropod Time Series/Finalized for Publication/Raw Data/ECY20-1435_climate2.xlsx",sheet = "PRISM_raw",startRow = 1,colNames = TRUE)

data.climate3<-openxlsx::read.xlsx("C:/Users/croyj/Dropbox/ARCA/Manuscripts/Arthropod Time Series/Finalized for Publication/Raw Data/ECY20-1435_climate3.xlsx",sheet = "summary",startRow = 1,colNames = TRUE)

data.sites<-openxlsx::read.xlsx("C:/Users/croyj/Dropbox/ARCA/Manuscripts/Arthropod Time Series/Finalized for Publication/Raw Data/ECY20-1435_climate2.xlsx",sheet = "Sites",startRow = 1,colNames = TRUE)

data.soil<- openxlsx::read.xlsx("C:/Users/croyj/Dropbox/ARCA/Manuscripts/Arthropod Time Series/Finalized for Publication/Raw Data/ECY20-1435_S4_soil horizon data.xlsx",sheet = "full",startRow = 1,colNames = TRUE)

data.mukey<- openxlsx::read.xlsx("C:/Users/croyj/Dropbox/ARCA/Manuscripts/Arthropod Time Series/Finalized for Publication/Raw Data/ECY20-1435_S4_soil survey areas.xlsx",sheet = "mukey",startRow = 1,colNames = TRUE)

data.biomass<-openxlsx::read.xlsx("C:/Users/croyj/Dropbox/ARCA/Manuscripts/Arthropod Time Series/Finalized for Publication/Raw Data/ECY20-1435_biomass.xlsx",sheet = "raw",startRow = 1,colNames = TRUE)

data.survival<-openxlsx::read.xlsx("C:/Users/croyj/Dropbox/ARCA/Manuscripts/Arthropod Time Series/Finalized for Publication/Raw Data/ECY20-1435_survival.xlsx",sheet = "raw",startRow = 1,colNames = TRUE)

data.survival2<-openxlsx::read.xlsx("C:/Users/croyj/Dropbox/ARCA/Manuscripts/Arthropod Time Series/Finalized for Publication/Raw Data/ECY20-1435_survival2.xlsx",sheet = "raw",startRow = 1,colNames = TRUE)

data.bugs<-openxlsx::read.xlsx("C:/Users/croyj/Dropbox/ARCA/Manuscripts/Arthropod Time Series/Finalized for Publication/Raw Data/ECY20-1435_arthropods.xlsx",sheet = "raw",startRow = 1,colNames = TRUE)

#theme for figures
theme_manuscript <- theme(axis.title.x = element_text(size = 15,family = "sans"),axis.title.y = element_text(size = 15,family = "sans"),axis.text.x = element_text(size = 13,family = "sans"),panel.grid.major = element_blank(),axis.text.y = element_text(size = 13,family = "sans"), panel.grid.minor = element_blank(),        panel.background = element_rect(colour = "black", size=1.25, fill=NA), axis.line = element_line(colour = "black",size = 1),legend.position = c(.05,.875),legend.title = element_text(size=10))
