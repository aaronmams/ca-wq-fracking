rm(list=ls())


#-------------------------------------------------------------------------------------------
# YOU CAN BUILD THE DATA FROM RAW BY SOURCING THE FOLLOWING SCRIPT...BUT I DON'T RECOMMEND IT
#source('R/raw_data_build.R')
#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------
source('R/spatial_prep.R')
#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------
# set the path to data files stored outside the project directory
path.to.data <- 'R:/frackingmodels/FrackingDataAll'
#-------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------
#merge the groundwater quality data with the .txt file created in the ArcGIS analysis that
# matches each groundwater quality monitoring well to a groundwater basin
wq.gis.data <- read.table(paste(path.to.data,"/","wq.wells.basin.join.txt",sep=""),
                          sep=",",comment.char="",header=TRUE)

gw.wq.df <- merge(gw.wq.df,wq.gis.data,by.x=c("WELL.NAME","APPROXIMATE.LATITUDE","APPROXIMATE.LONGITUDE"),
                  by.y=c("well_name","latitude","longitude"),all.x=T)
#----------------------------------------------------------------------------------------


########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
# For the injection wells we want to calculate the running injection totals but we
# only need to do that for injection wells that were matched to at least one
# groundwater quality monitoring well

#read-in the distance band file
db1 <- read.csv(paste(path.to.data,"/","assignedInjWells1KM.csv",sep=""))
db5 <- read.csv(paste(path.to.data,"/","assignedInjWells5KM_new.txt",sep=""))

injwells.matched <- unique(c(unique(db1$PWT__ID),unique(db5$PWT__ID)))

inj.df <- inj.df[which(inj.df$PWT__ID %in% injwells.matched),]

#merge the groundwater basins with the injection data
wd.gis.data <- read.table(paste(path.to.data,"/","inj.wells.basin.join.txt",sep=""),sep=",",comment.char="",header=TRUE)

inj.df <- merge(inj.df,wd.gis.data,by=c("PWT__ID","Latitude","Longitude"),all.x=T)

#Remove observation that are missing a production date
inj.df <- inj.df[!is.na(inj.df$InjectionDate),]

#if there is not water injection set to 0
inj.df$Steam.WaterInjected.BBL.[is.na(inj.df$Steam.WaterInjected.BBL.)] <- 0

#### Water Kind Add water kind text based on water kind code#####

inj.df$WaterKind1 <- NA
inj.df$WaterKind1[inj.df$WaterKind1 == 0] <- "Not Applicable"
inj.df$WaterKind1[inj.df$WaterKind1 == 1] <- "Saline"
inj.df$WaterKind1[inj.df$WaterKind1 == 2] <- "Fresh"
inj.df$WaterKind1[inj.df$WaterKind1 == 3] <- "Chemical Mixture"
inj.df$WaterKind1[inj.df$WaterKind1 == 4] <- "Other"
inj.df$WaterKind1[inj.df$WaterKind1 == 6] <- "Unknown"
inj.df$WaterKind1[inj.df$WaterKind1 == 5] <- "Unknown"
inj.df$WaterKind1[is.na(inj.df$WaterKind1)] <- "Unknown"
inj.df$WaterKind1[inj.df$WaterKind1 == 9] <- "Unknown"


#The cumulative lagged production is computationally intense but can be done at 
# low cost for each production well as there are only 1,385 production wells
# in the groundwater basins that we are interested in.

#eventually, we will need to tag each injection well with a water quality monitoring well
# which will result in more calculations...but for now we just need to aggregate
# production by the subbasin


#Create a new placeholder data frame with each injection well in each month from 2000-01-01 to 
# 2014-07-01 which is the range of our data

inj.df$InjectionDate <- as.Date(inj.df$InjectionDate,format="%Y-%m-%d")
new.dates <- data.frame(InjectionDate=seq(as.Date("2000-01-01"),as.Date("2014-07-01"),by="month"))


#first create a data frame with all PWT__ID values and all year-month combinations
inj.new <- data.frame(rbindlist(lapply(unique(inj.df$PWT__ID),function(x){return(data.frame(InjectionDate=seq(as.Date("2000-01-01"),as.Date("2014-07-01"),by="month"),
                                                                                            PWT__ID=x))})))


total.inj <- tbl_df(inj.df[,c('PWT__ID','InjectionDate','Steam.WaterInjected.BBL.')])
total.inj <- arrange(total.inj,InjectionDate)

total.inj <- left_join(inj.new,total.inj,by=c("InjectionDate","PWT__ID")) %>%
  dplyr::mutate(inj=replace(Steam.WaterInjected.BBL.,is.na(Steam.WaterInjected.BBL.),0)) %>%
  group_by(PWT__ID) %>%
  mutate(total3mo = rollapplyr(inj,3,sum,partial=TRUE)) %>%
  mutate(total6mo = rollapplyr(inj,6,sum,partial=TRUE)) %>%
  mutate(total12mo = rollapplyr(inj,12,sum,partial=TRUE)) %>%
  mutate(total24mo = rollapplyr(inj,24,sum,partial=TRUE)) 

########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################

#The next step is a little funky because the frequency of the groundwater quality data does
# not match the frequency of the injection data.

#--the groundwater quality data can have multiple observations for the same well and same chemical
#     in the same day

#-- the injection well data are monthly

#So what I'm going to do here is:

#1. create a list of unique WELL.ID/month/year combinations from the groundwater quality data
#2. match injection wells to these water quality wells based on the 1Km distance band
#3. match the monthly injections by PWT_IDs to this data.
#4. repeat for the 5Km distance band


#============================================================================
#NOTE: AT THE MOMENT I THINK THE 5KM BANDS ARE MESSED UP.  I'M GOING TO PROCEED
# WITH THE MESSED UP ONES AS A PLACE HOLDER BUT WE WILL WANT TO EVENTUALLY MAKE
# SURE THAT ALL THE MONITORING WELLS THAT SHOW UP IN THE 1KM BAND FILE ALSO
# SHOW UP IN THE 5KM BAND FILE

#create a WELL.ID to be consistent with the WELL.ID in the water quality data
db5$lat.new <- as.character(round(db5$Lat_1,digits=3))
db5$long.new <- as.character(round(db5$Long_1,digits=3))

db5$WELL.ID <- paste(db5$WELL_NAME,"_",db5$lat.new,"_",db5$long.new,sep="")
db5 <- db5[,c('PWT__ID','Lat','Long','WELL_NAME','WELL.ID','BUFF_DIST')]
#=============================================================================

#=============================================================================
#for the 1km buffer we have to fix the WELL.ID field.  since we do not have 
# the well name field in the 1km buffer output file we are going to match between
# the original WELL.ID and the new WELL.ID

new.well.ids <- data.frame(WELL.ID=gw.wq.df$WELL.ID,WELL.ID.old=gw.wq.df$WELL.ID.old)
new.well.ids <- new.well.ids[!duplicated(new.well.ids),]
db1 <- merge(db1,new.well.ids,by.x="WELL_ID",by.y="WELL.ID.old")

names(db1)[which(names(db1)=="Latitude")] <- "Lat"
names(db1)[which(names(db1)=="Longitude")] <- "Long"
db1 <- db1[,c('PWT__ID','Lat','Long','WELL.ID','BUFF_DIST')]
#==========================================================================


total.inj$YEAR <- year(as.Date(total.inj$InjectionDate,format='%Y/%m/%d'))
total.inj$MONTH <- month(as.Date(total.inj$InjectionDate,format='%Y/%m/%d'))

#get a single row for each WELL.ID for each month and year that the WELL.ID is in the data
#IMPORTANT: WE WANT TO DO A LEFT JOIN HERE BECAUSE WE WANT TO KEEP THE MONITORING WELLS THAT WERE
#   NOT MATCHED TO INJECTION WELLS.  THESE WELLS WILL HAVE INJECTION QUANTITIES SHOW UP AS 
#   0.
well.month <- tbl_df(gw.wq.df[,c('WELL.NAME','WELL.ID','MONTH','YEAR')]) %>% group_by(WELL.ID,MONTH,YEAR) %>%
  filter(row_number()==1) %>%
  left_join(db5,by="WELL.ID") %>% #merge in the injection wells based on the 5km band
  left_join(total.inj,by=c("PWT__ID","MONTH","YEAR")) %>% #merge in the injection quantities for the injection wells
  summarise(total3mo_5km=sum(total3mo),
            total6mo_5km=sum(total6mo),
            total12mo_5km=sum(total12mo),
            total24mo_5km=sum(total24mo),
            totalinj_5km=sum(inj))


#do the same thing for the 1km distance band then we will merge the two
well.month1km <- tbl_df(gw.wq.df[,c('WELL.NAME','WELL.ID','MONTH','YEAR')]) %>% group_by(WELL.ID,MONTH,YEAR) %>%
  filter(row_number()==1) %>%
  left_join(db1,by="WELL.ID") %>% #merge in the injection wells based on the 5km band
  left_join(total.inj,by=c("PWT__ID","MONTH","YEAR")) %>% #merge in the injection quantities for the injection wells
  summarise(total3mo_1km=sum(total3mo),
            total6mo_1km=sum(total6mo),
            total12mo_1km=sum(total12mo),
            total24mo_1km=sum(total24mo),
            totalinj_1km=sum(inj))


#we are going to merge these two data frames with a dplyr full_join because:
#  1.  there are some monitoring wells in the 1km buffer file that are not in the 5km buffer file
#  2.  there are some monitoring wells in the 5km buffer file that are not in the 1 km buffer file
# we want to keep all of the monitoring wells.  

well.month <- well.month %>% full_join(well.month1km,by=c("WELL.ID","MONTH","YEAR"))

#because we did the full join here we want to convert the NA's to 0s....this is because, for water quality
# monitoring wells that were not linked to injection wells, we want to the injection totals to be 0.  Bascially,
# we want to 0s in the data so we have the 'control' basins that don't have any wastewater injection
well.month <- well.month %>% mutate(total3mo_5km=ifelse(is.na(total3mo_5km),0,total3mo_5km),
                                    total6mo_5km=ifelse(is.na(total6mo_5km),0,total6mo_5km),
                                    total12mo_5km=ifelse(is.na(total12mo_5km),0,total12mo_5km),
                                    total24mo_5km=ifelse(is.na(total24mo_5km),0,total24mo_5km),
                                    totalinj_5km=ifelse(is.na(totalinj_5km),0,totalinj_5km),
                                    total3mo_1km=ifelse(is.na(total3mo_1km),0,total3mo_1km),
                                    total6mo_1km=ifelse(is.na(total6mo_1km),0,total6mo_1km),
                                    total12mo_1km=ifelse(is.na(total12mo_1km),0,total12mo_1km),
                                    total24mo_1km=ifelse(is.na(total24mo_1km),0,total24mo_1km),
                                    totalinj_1km=ifelse(is.na(totalinj_1km),0,totalinj_1km))




########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################


#Next, we introduce the MCLs and DLRs

#since we are working with a rather small number of chemicals, I'm going to harcode these in
# The standards can be found here:
# https://www.cdph.ca.gov/certlic/drinkingwater/Documents/MCLreview/MCLs-DLRs-PHGs.xls


#MCLs
gw.wq.df$MCL <- NA
gw.wq.df$MCL.units <- NA


gw.wq.df$MCL[which(gw.wq.df$CHEMICAL.NAME=="Aluminum")] <- 1 * 1000 #convert to Ug/L
gw.wq.df$MCL.units[which(gw.wq.df$CHEMICAL.NAME=="Aluminum")] <- "UG/L"


gw.wq.df$MCL[which(gw.wq.df$CHEMICAL.NAME=="Barium")] <- 1 
gw.wq.df$MCL.units[which(gw.wq.df$CHEMICAL.NAME=="Barium")] <- "MG/L"


gw.wq.df$MCL[which(gw.wq.df$CHEMICAL.NAME=="Benzene")] <- 0.001 * 1000 #convert to UG/L
gw.wq.df$MCL.units[which(gw.wq.df$CHEMICAL.NAME=="Benzene")] <- "UG/L"


#not sure about Chloride.  We have a DLR and MCL for "Carbon Tetrachloride", "Methylene chloride", and "Vinyl Chloride"
#   but not for "Chloride
gw.wq.df$MCL[which(gw.wq.df$CHEMICAL.NAME=="Chloride")] <- NA

gw.wq.df$MCL[which(gw.wq.df$CHEMICAL.NAME=="Chromium")] <- 0.05 * 1000 #convert to UG/L
gw.wq.df$MCL.units[which(gw.wq.df$CHEMICAL.NAME=="Chromium")] <- "UG/L"

gw.wq.df$MCL[which(gw.wq.df$CHEMICAL.NAME=="Chromium, Hexavalent (Cr6)")] <- 0.01 * 1000 #convert to UG/L
gw.wq.df$MCL.units[which(gw.wq.df$CHEMICAL.NAME=="Chromium, Hexavalent (Cr6)")] <- "UG/L"


gw.wq.df$MCL[which(gw.wq.df$CHEMICAL.NAME=="Magnesium")] <- NA
gw.wq.df$MCL[which(gw.wq.df$CHEMICAL.NAME=="Sulfate")] <- NA

gw.wq.df$MCL[which(gw.wq.df$CHEMICAL.NAME=="Toluene")] <- 0.15 * 1000 #convert to UG/L
gw.wq.df$MCL.units[which(gw.wq.df$CHEMICAL.NAME=="Toluene")] <- "UG/L"

gw.wq.df$MCL[which(gw.wq.df$CHEMICAL.NAME=="Xylenes")] <- 1.75 * 1000 #convert to UG/L
gw.wq.df$MCL.units[which(gw.wq.df$CHEMICAL.NAME=="Xylenes")] <- "UG/L"


gw.wq.df$MCL[which(gw.wq.df$CHEMICAL.NAME=="1,2,4-Trimethylbenzene")] <- NA



#DLRs
gw.wq.df$DLR <- NA
gw.wq.df$DLR.units <- NA

gw.wq.df$DLR[which(gw.wq.df$CHEMICAL.NAME=="Aluminum")] <- 0.05 * 1000 #convert to UG/L
gw.wq.df$DLR.units[which(gw.wq.df$CHEMICAL.NAME=="Aluminum")] <- "UG/L"

gw.wq.df$DLR[which(gw.wq.df$CHEMICAL.NAME=="Barium")] <- 0.1 
gw.wq.df$DLR.units[which(gw.wq.df$CHEMICAL.NAME=="Barium")] <- "MG/L"

gw.wq.df$DLR[which(gw.wq.df$CHEMICAL.NAME=="Benzene")] <- 0.0005 * 1000
gw.wq.df$DLR.units[which(gw.wq.df$CHEMICAL.NAME=="Benzene")] <- "UG/L"

#not sure about Chloride.  We have a DLR and MCL for "Carbon Tetrachloride", "Methylene chloride", and "Vinyl Chloride"
#   but not for "Chloride
gw.wq.df$DLR[which(gw.wq.df$CHEMICAL.NAME=="Chloride")] <- NA

gw.wq.df$DLR[which(gw.wq.df$CHEMICAL.NAME=="Chromium")] <- 0.01 * 1000 #converto to UG/L
gw.wq.df$DLR.units[which(gw.wq.df$CHEMICAL.NAME=="Chromium")] <- "UG/L"

gw.wq.df$DLR[which(gw.wq.df$CHEMICAL.NAME=="Chromium, Hexavalent (Cr6)")] <- 0.001 * 1000 #convert to UG/L
gw.wq.df$DLR.units[which(gw.wq.df$CHEMICAL.NAME=="Chromium, Hexavalent (Cr6)")] <- "UG/L"

gw.wq.df$DLR[which(gw.wq.df$CHEMICAL.NAME=="Magnesium")] <- NA
gw.wq.df$DLR[which(gw.wq.df$CHEMICAL.NAME=="Sulfate")] <- NA

gw.wq.df$DLR[which(gw.wq.df$CHEMICAL.NAME=="Toluene")] <- 0.0005 * 1000 #convert to UG/L
gw.wq.df$DLR.units[which(gw.wq.df$CHEMICAL.NAME=="Toluene")] <- "UG/L"

gw.wq.df$DLR[which(gw.wq.df$CHEMICAL.NAME=="Xylenes")] <- 0.0005 * 1000 #convert to UG/L
gw.wq.df$DLR.units[which(gw.wq.df$CHEMICAL.NAME=="Xylenes")] <- "UG/L"


gw.wq.df$DLR[which(gw.wq.df$CHEMICAL.NAME=="1,2,4-Trimethylbenzene")] <- NA


#Need to make sure the units are consistent between the chemical result and the DLR/MCL
units <- data.frame(chem=gw.wq.df$CHEMICAL.NAME,unit=gw.wq.df$UNITS)
units <- units[which(duplicated(units)==F),]



gw.wq.df <- gw.wq.df %>%
  mutate(DLR_ex=ifelse((QUALIFIER %in% c(".",""," ","=",">")&RESULT>=DLR),1,
                       ifelse((QUALIFIER%in%c(".",""," ","=","ND","NR","<")&RESULT<=DLR),0,NA)),
         MCL_ex=ifelse((QUALIFIER %in% c(".",""," ","=")&RESULT>=MCL),1,
                       ifelse((QUALIFIER%in%c(".",""," ","=","ND","NR")&RESULT<=MCL),0,NA)))



#find out if there are any cases where there are multiple observations for the same well/same day/same chemical
# but one of the observations exceeds the MCL and the other does not
mult.day <- gw.wq.df %>% group_by(WELL.ID,DATE,CHEMICAL.NAME) %>% filter(!is.na(MCL_ex)) %>% summarise(ex=mean(MCL_ex,na.rm=T))
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################


########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################


#Next, we aggregate the water quality data set so that the cumulative sums are summed by
# well, chemical, and date.

# At this point what we have in the data are water quality observations for monitoring wells at particular
# dates.  These well/chemical/date combinations are repeated in the data frame because each monitoring
# well is matched to potentially many injection wells.  For each monitoring well/chemical/date we want to 
# sum up the cumulative sums of the injection wells paired with that monitoring well/date.  I am including in the 
# group_by statement the Qualifier code and units in order to retain these in the final data set.

# we will also create the data frame "spatial.info."  This contains info that does not vary over time and
# is specific to each monitoring well...including: county, groundwater basin, groundwater subbasin, and lat/long,
# and size of the groundwater basin 

gw.wq.df <- tbl_df(gw.wq.df)

spatial.info <- gw.wq.df %>%
  select(WELL.NAME,WELL.ID,APPROXIMATE.LATITUDE,APPROXIMATE.LONGITUDE,COUNTY,Basin_Name,Subbasin_N, sqkm, acres) %>%
  distinct(WELL.NAME,WELL.ID,APPROXIMATE.LATITUDE,APPROXIMATE.LONGITUDE,COUNTY,Basin_Name,Subbasin_N, sqkm, acres)  

#aggregate by month and year...because of the way the date work with the qualifier codes it does not make sense to try and
# aggregate actual contaminant values.  We can only really observer whether a particular observation was above or below the
# threshold.  Also, since the injection data are monthly, we don't need daily observations but we will keep daily observations
# here and then we have the option of filtering them out before estimation
wq <- gw.wq.df %>% group_by(WELL.ID,CHEMICAL.NAME,DATE) %>% summarise(MCL_ex=max(MCL_ex),DLR_ex=max(DLR_ex))

#now add the spatial information back in
wq <- wq %>% inner_join(spatial.info,by="WELL.ID")

#add the injection data
wq <- wq %>% mutate(MONTH=month(as.Date(DATE,format="%m/%d/%Y")),YEAR=year(as.Date(DATE,format="%m/%d/%Y"))) %>%  #recreate the month and year fields so we can merge with the injection data
  left_join(well.month,by=c("WELL.ID","MONTH","YEAR"))


saveRDS(gw.wq.df,file="data/gw_monitoring_raw.RDA")
saveRDS(wq,file="data/logit_data.RDA")


########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################


#as a final step I'm going to add the TRI data into the data frame

#the TRI data have 107 fields and I don't want to deal with that many so I'm going to 
# import annual files one at a time and select the columns I want
path <- paste("R:/frackingmodels/")
col.names <- c('YEAR','TRI_FACILITY_ID','CITY','COUNTY','LATITUDE','LONGITUDE','PRIMARY_SIC','PRIMARY_NAICS','CHEMICAL',
               'CLASSIFICATION','METAL','METAL_CATEGORY','UNIT_OF_MEASURE','X5.1_FUGITIVE_AIR','X5.2_STACK_AIR','X5.3_WATER',
               'X5.4.1_UNDERGROUND_CLASS_I','X5.4.2_UNDERGROUND_CLASS_II','X5.5.1A_RCRA_C_LANDFILLS','X5.5.1B_OTHER_LANDFILLS',
               'X5.5.2_LAND_TREATMENT','X5.5.3_SURFACE_IMPOUNDMENT','X5.5.3A_RCRA_C_SURFACE_IMP','X5.5.3B_Other_SURFACE_IMP',
               'X5.5.4_OTHER_DISPOSAL','ON.SITE_RELEASE_TOTAL','X6.2_M71','X6.2_M81','X6.2_M82','TOTAL_RELEASES','X8.1_RELEASES','PARENT_COMPANY_NAME')

tri.data <- list()
for(i in 2004:2013){
  filename <- paste("TRI","_",i,"_","CA",".csv",sep="")
  file <- paste(path,filename,sep="")
  tmp <- read.csv(file)
  tmp <- tmp[,which(names(tmp)%in%col.names)]
  tri.data[[i]] <- tmp
}

tri.data <- data.frame(rbindlist(tri.data))

#match TRI facilities to water quality monitoring wells
#first filter out a few counties that are not in our study area
tri.data <- tri.data[which(!tri.data$COUNTY %in% c("HUMBOLDT","AMADOR","LASSEN","EL DORADO","BUTTE","NAPA","TEHAMA",
                                                   "SUTTER","CALAVARAS","LAKE","MENDOCINO","DEL NORTE","SISKIYOU","SHASTA",
                                                   "PLACER")),]

#establish the distance function

d.km <- function(x,y){
  #function to calculate straight line distance between two sets of lat/long
  # x = c(lat1,long1)
  # y = c(lat2,long2)
  p1 <- (x*pi)/180
  p2 <- (y*pi)/180
  a <- sin((p2[1]-p1[1])*0.5)*sin((p2[1]-p1[1])*0.5) + sin((p2[2]-p1[2])*0.5)*sin((p2[2]-p1[2])*0.5)*cos(p1[1])*cos(p2[1])
  c <- 2*atan2(sqrt(a),sqrt(1-a))
  return(c*6371)
}

#set up a function to calculate distance between any particular TRI facility and every
# groundwater monitoring well in the data
tri.locs <- data.frame(TRI_FACILITY_ID=tri.data$TRI_FACILITY_ID,
                       LATITUDE=tri.data$LATITUDE,
                       LONGITUDE=tri.data$LONGITUDE)
tri.locs <- tri.locs[which(!duplicated(tri.locs)),]

well.locs <- data.frame(WELL.ID=wq$WELL.ID,LATITUDE=wq$APPROXIMATE.LATITUDE,LONGITUDE=wq$APPROXIMATE.LONGITUDE)
well.locs <- well.locs[which(!duplicated(well.locs)),]

tri.match <- function(tri.id){
  x <- c(tri.locs$LATITUDE[which(tri.locs$TRI_FACILITY_ID==tri.id)],
         tri.locs$LONGITUDE[which(tri.locs$TRI_FACILITY_ID==tri.id)])
  
  #create an approximate 8 km bounding box 
  north <- x[1]+0.1
  south <- x[1]-0.1
  east <- x[2] + 0.1
  west <- x[2] - 0.1
  
  matches <- well.locs[which(well.locs$LATITUDE<north & well.locs$LATITUDE>south & 
                               well.locs$LONGITUDE<east & well.locs$LONGITUDE>west),
                       c('WELL.ID','LATITUDE','LONGITUDE')]
  if(nrow(matches)>0){
    return(data.frame(cbind(matches,tri.id)))      
  }else{
    return(data.frame(WELL.ID=NA,LATITUDE=NA,LONGITUDE=NA,tri.id))
  }
}

#get a dataframe with all the TRI facilities and each groundwater monitoring well that is within 
# the approximate 8km bounding box
t <- Sys.time()
matched.data <- data.frame(rbindlist(lapply(1:nrow(tri.locs),function(i){return(tri.match(tri.locs$TRI_FACILITY_ID[i]))})))
Sys.time() - t

#now we have a matched data set we can propose a dplyr solution to calculating approximate linear distance

#first merge in the lat/long for the tri data
names(tri.locs) <- c("tri.id","tri.lat","tri.long")
matched.data <- merge(matched.data,tri.locs,by="tri.id")

matched.data <- tbl_df(matched.data) %>% mutate(lat.rad=(LATITUDE*pi)/180,long.rad=(LONGITUDE*pi)/180,
                                                tri.lat.rad=(tri.lat*pi)/180,tri.long.rad=(tri.long*pi)/180) %>%
  mutate(a=sin((tri.lat.rad-lat.rad)*0.5)*sin((tri.lat.rad-lat.rad)*0.5)+
           sin((tri.long.rad-long.rad)*0.5)*sin((tri.long.rad-long.rad)*0.5)*cos(lat.rad)*cos(tri.lat.rad),
         c=2*atan2(sqrt(a),sqrt(1-a)),
         Dkm=6371*c
  )


#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
