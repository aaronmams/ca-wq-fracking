rm(list=ls())
############################################################
############################################################
############################################################
############################################################
############################################################
#This script uses the data build from raw_data_build.R
# the get production, injection, and groundwater quality 
# data.  The main purpose of the script is to prepare these
# data to be fed into an ArcGIS program that will fortify
# these data with some important spatial characteristics such as


# -- groundwater basin and subbasin in which each well resides
# -- number of fracking injection wells within 1 and 5km of 
#       groundwater quality monitoring wells


############################################################
############################################################
############################################################
############################################################
############################################################

require(lubridate)
require(ggplot2)
require(lubridate)
require(data.table)

#Make sure to load dplyr last because plyr is loaded by some other packages and 
# will cause problems if plyr is loaded after dplyr
require(dplyr)
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
#The statewide monthly oil and gas production and monthly injection from 1977 to 2014 is available
# from the California Department of Oil, Gas, and Geothermal Resources here:
#   http://www.conservation.ca.gov/dog/prod_injection_db/Pages/Index.aspx

# data for each year is contained in a MS Access Database (there is a separate MS Access Database for
#  every year from 1977-2014).  We took the annual data from the 
# database and created a .csv file for each year.  These files have the following convention:
# i) injection data for 1977 is contained in a file called i1977.csv 
# ii) production data for 1977 is contained in a file called p1977.csv
# iii) well information data for 1977 is contained a filed called w1977.csv 

# Injection wells are identified by a unique PWT__ID number
# Production wells are also identified by a unique PWT__ID number
# Well information maps an API number to a PWT ID

# The well information does not change for many wells.  However, because of well construction
# each year may contain a different number of wells.  To deal with this, and to make sure,
# we are retaining information for all the wells, we read in the well information from
# all of the .csv files from 1977-2014 and then remove duplicate elements.

# The production and injection data contained in the annual .csv files lists production/injection
# for each well in each month of the year


#Spatial information on the wells (latitude/longitude coordinates) and infomration on whether the
# well has been hydraulically fractured comes from an Excel file provided by DOGGR on the GIS site:

# http://www.conservation.ca.gov/dog/maps/Pages/GISMapping2.aspx

###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################

path.to.data <- "R:/frackingmodels/FrackingDataAll"

OG.Well.Data <-read.csv(paste(path.to.data,"/AllWells_Web_8.27.14.csv",sep=""),sep=",", header=TRUE)
well.data <- readRDS('R:/frackingmodels/FrackingDataAll/well_data.RDA')
inj.df <- readRDS('R:/frackingmodels/FrackingDataAll/inj_df.RDA')
prod.df <- readRDS('R:/frackingmodels/FrackingDataAll/prod_df.RDA')
gw.wq.df <- readRDS('R:/frackingmodels/FrackingDataAll/gw_raw.RDA')
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################


#Here we want to merge 3 data frames:
# 1.  inj.df - this has the injection data for each well but only identifies wells by PWTID
# 2.  well.data - this has the basic info on each injection well, including the API number
# 3.  inj.location - we read this file in below.  It contains the lat/long for each well but
#                     only has API which is why we need to merge the inj.df dataframe with
#                     the well.data dataframe.


#-------------------------------------------------------------------------------------------------
#We start by getting the master list of all wells with lat/long coordinates
#Read in "All Wells" dataset

##Remove Wells with Lat Long  equal to zero##
OG.Well.Data <- subset(OG.Well.Data, OG.Well.Data$Latitude != 0)

OG.Well.Data <- subset(OG.Well.Data, OG.Well.Data$Longitude != 0)

##Remove Duplicate API###

##Remove wells with duplicate API and location###
OG.Well.Data <- OG.Well.Data[!(duplicated(OG.Well.Data[c("APINumber","Latitude","Longitude")])),]
#------------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------------
# NEXT WE READ IN THE WELL INFO DATA.  THIS IS WHAT ALLOWS US TO MATCH A PWT_ID TO AN API NUMBER

#There is one well with an API that cannot be converted to numeric because it includes a "-".  
# We remove the dash to make this a numeric entry
well.data$APINumber <- gsub("[[:punct:]]", "", as.character(well.data$APINumber))

#convert API #'s to numeric which will drop the leading 0's and solve the problem of having the same well
# appear in the data with an API of '100001' and '00100001'.
well.data$APINumber <- as.numeric(as.character(well.data$APINumber))

#remove duplicate API-PWTID combo.  later we will remove duplicate PWTIDs but we don't know which
# APIs match to the location data so we merge with that data first then remove duplicate PWTIDs
well.data <- well.data[!(duplicated(well.data[c("APINumber","PWT__ID")])),]
#-----------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------
#merge well.data with location data
well.data <- merge(well.data,OG.Well.Data,by="APINumber",all.x=T)

#get rid of duplicate APIs in the well.data dataset
#NOTE THAT SOME APIs WILL APPEAR MORE THAN ONCE...IT SEEMS THAT SOME WELLS HAVE TWO DIFFERENT 
# WELLTYPECODES AND THAT A SEPARATE PWT_ID IS ASSIGNED TO EACH WELL AND TYPE
# we want to keep the APIs with different PWTIDs but remove duplicate PWTIDS - this applies
# to about 8 wells that appear to be entered incorrectly.  Example: API # 23720052 appears to be
# the same well as API # 03720052...they have the same location, same data and same PWTID.  I will
# treat wells like this as duplicates 

# since the injection data are organized by PWTID this should not affect the merger with the 
# injection data
well.data <- well.data[!(duplicated(well.data[c("PWT__ID")])),]
#------------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# Now set up the injection wells data and production wells data for GIS analysis by
# getting lat/long data merged with injection and production, then save .csv files 
# for Alice.


#merge the locations with the injection data
inj.df <- merge(inj.df,well.data,by="PWT__ID")


#important:an API number can correspond to multiple PWT__ID numbers.  This is because the same well will have different
# PWT_IDs if it is used for more than one purpose (multiple well types).  The lat/long is unique to an API so for the GIS
# analysis we just want unique API numbers
inj.locs <- data.frame(PWT__ID=inj.df$PWT__ID,APINumber=inj.df$APINumber,Latitude=inj.df$Latitude,Longitude=inj.df$Longitude)
inj.locs <- inj.locs[which(duplicated(inj.locs$APINumber)==F),]

  
#merge the locations with the production data
#prod.df <- merge(prod.df,well.data,by="PWT__ID")


#merge production well info only for unique wells because the data are too big...
#
prod.locs <- data.frame(PWT__ID=unique(prod.df$PWT__ID))
prod.locs <- merge(prod.locs,well.data,by="PWT__ID")
prod.locs <- data.frame(PWT__ID=prod.locs$PWT__ID,APINumber=prod.locs$APINumber,Latitude=prod.locs$Latitude,Longitude=prod.locs$Longitude)
prod.locs <- prod.locs[which(duplicated(prod.locs$APINumber)==F),]

#write the injection wells and production wells to different .csv files for GIS analysis
write.csv(inj.locs, file=paste(path.to.data,"/doggr_injection_locations.csv",sep=""))
write.csv(prod.locs, file=paste(path.to.data,"/doggr_production_locations.csv",sep=""))
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
#Next step is to export the groundwater monitoring well locations to a .csv for GIS analysis.
# These wells will be matched to injection wells in two different ways:

# 1. based on groundwater subbasin - this portion of the analysis implicitly assumes that
#    all injection wells in the same groundwater subbasin as a monitoring well impact 
#    water quality observations of that well
# 
# 2. Based on distance buffers - for this analysis each groundwater quality monitoring well
#     will be matched to injection wells based on specified distance buffers (1 km, 5 km, and 10 km)
#     to start.  

#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------


#According to correspondence from the state water board the following qualifier codes are contained in the 
# groundwater data:

# DU    data unavailable
# =     equal to
# >     greater than
# >=    greater than or equal to
# <     less than
# <=    less than or equal to
# NA    result not available
# ND    not detected
# NR    not reported
# PA    present/absent
# SU    surrogate
# TI    tentatively identified compound



gw.wq.df$YEAR <- year(as.Date(gw.wq.df$DATE,format='%m/%d/%Y'))
gw.wq.df$MONTH <- month(as.Date(gw.wq.df$DATE,format='%m/%d/%y'))
gw.wq.df$DAY <- day(as.Date(gw.wq.df$DATE,format='%m/%d/%y'))
#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------


# NEXT WE FILTER THE GROUNDWATER QUALITY DATA

# #Limit years to 2004 - 2012 and limit to the following chemicals:--------------------------------------------------
# 
# # -- 1,2,4-Trimethylbenzene
# # -- Aluminum
# # -- Barium
# # -- Benzene
# # -- Chloride
# # -- Chromium
# # -- Chromium Hexavalent (Cr6)
# # -- Magnesium
# # -- Sulfate
# # -- Toulene
# # -- Xylenes
# 

gw.wq.df <- gw.wq.df[gw.wq.df$YEAR>2003,]
gw.wq.df <- gw.wq.df[gw.wq.df$CHEMICAL %in% c('TMB124','AL','BA','BZ','CL','CR','CR6', 'MG','SO4','BZME','XYLENES'),]
#---------------------------------------------------------------------------------------------------------------------


# Add some long-hand chemical identifiers
chem.ab <- data.frame(CHEMICAL=c('TMB124','AL','BA','BZ','CL','CR','CR6', 'MG','SO4','BZME','XYLENES'),
                      CHEMICAL.NAME=c('1,2,4-Trimethylbenzene','Aluminum','Barium','Benzene','Chloride','Chromium',
                                      'Chromium, Hexavalent (Cr6)','Magnesium','Sulfate','Toluene','Xylenes'))

# 
# #merge shorthand chem names with data set
gw.wq.df <- merge(gw.wq.df,chem.ab,by=c("CHEMICAL"),all.x=TRUE)
#

# #Drop duplicate wells
gw.wq.df <- gw.wq.df[order(gw.wq.df$WELL.NAME,gw.wq.df$DATE,gw.wq.df$APPROXIMATE.LATITUDE,gw.wq.df$CHEMICAL),]
# 
# #Remove duplicate wells from gw quality dataset #
gw.wq.df <- gw.wq.df[!duplicated(gw.wq.df),]

# Assign a unique identifier to each well...there are many instances where the WELL.NAME field corresponds to
# more than one well (wells in different groundwater basins and counties with the same WELL.NAME).  Here we create
# a unique well identifier for use in the GIS matching scripts.  For this we use the concatenated value of 
# WELL.NAME_latitude_longitude

#Edit: the differing treatment of decimal places seems to be causing some problems with this well id field.  Let's round
# lat/long to 3 decimal places, convert to text, then construct the well id field
gw.wq.df$lat.new <- as.character(round(gw.wq.df$APPROXIMATE.LATITUDE,digits=3))
gw.wq.df$long.new <- as.character(round(gw.wq.df$APPROXIMATE.LONGITUDE,digits=3))

gw.wq.df$WELL.ID.old <- paste(gw.wq.df$WELL.NAME,"_",gw.wq.df$APPROXIMATE.LATITUDE,"_",gw.wq.df$APPROXIMATE.LONGITUDE,sep="")
gw.wq.df$WELL.ID <- paste(gw.wq.df$WELL.NAME,"_",gw.wq.df$lat.new,"_",gw.wq.df$long.new,sep="")
#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------


# Now we write the well information to a .csv file which will be used in a GIS routine to match each monitoring well
# to a groundwater basin
monitoring.wells <- data.frame(WELL.ID=gw.wq.df$WELL.ID, LAT=gw.wq.df$APPROXIMATE.LATITUDE, LONG=gw.wq.df$APPROXIMATE.LONGITUDE)
monitoring.wells <- monitoring.wells[which(duplicated(monitoring.wells)==F),]

write.csv(monitoring.wells,file=paste(path.to.data,"/monitoring_well_locs.csv",sep=""))

saveRDS(gw.wq.df,file=paste(path.to.data,'/gw_wq_df.RDA',sep=""))
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################

