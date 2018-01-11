rm(list=ls())

library(data.table)

##############################################################################################################
# This script performs the most basic data build for our fracking analysis.

# we have raw data on

# 1. production wells
# 2. injection wells
# 3. water quality monitoring stations

#In the case of production and injection wells the data are contained in mulitple .csv and .txt files 
# (a different file for each year of data).  This script simply reads all those yearly files and binds them
# together into a single data frame.

#In the case of water quality monitoring wells, there is a different file for each county.  This script binds
# all the counties together in a single data frame.

##############################################################################################################

#--------------------------------------------------------------------------------------------------------------
#there are too many individual data files and they are too big to shove them all in the GitHub repository.  We
# are saving the raw data files in a directory outside of our GitHub project and using this path.to.data variable
# to point to the primary data files

path.to.data <- 'R:/frackingmodels/FrackingDataAll'
#----------------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------------------
#Build the Master List of All Wells with spatial info
OG_Well_Data <-read.csv(paste(path.to.data,"/AllWells_Web_8.27.14.csv",sep=""),sep=",", header=TRUE)
#---------------------------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------------------
# Next build the well info data.  This is what allows us to match a PWT_ID to an API number

well.data <- list()

for(i in 1:15){
  yr <- 1999 + i
  file <- paste("w",yr,".csv",sep="")
  
  file_path <- paste(path.to.data,"/",file,sep="")
  well.data[[i]] <- read.csv(file_path,colClasses=c(rep("integer",3),rep("factor",2),"integer",rep("factor",11),
                                                    "integer","factor","integer",rep("factor",4),"integer"))
}

well.data <- data.frame(rbindlist(well.data))
saveRDS(well.data,paste(path.to.data,"/",'well_data.RDA',sep=""))
#---------------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------
# Build the injection well data

inj.df <- list()
t <- Sys.time()
for(i in 1:15){
  yr <- 1999 + i
  file <- paste("i",yr,".csv",sep="")
  
  file_path <- paste(path.to.data,"/",file,sep="")
  inj.df[[i]] <- read.csv(file_path)
}
Sys.time() - t



#fix 2012 data because water kind is a factor variable...there is only 1 observation where "WaterKind" is an * so
# I just remove that one and coerce the column to integer
tmp <- inj.df[[13]]
tmp <- tmp[which(is.na(as.numeric(as.character(tmp$WaterKind)))==F),]
tmp$WaterKind <- as.numeric(tmp$WaterKind)
inj.df[[13]] <- tmp
inj.df <- data.frame(rbindlist(inj.df)) 


###Add Year Column###
inj.df$InjectionYear <- year(as.Date(inj.df$InjectionDate,format='%Y'))

###Add Month Column###
inj.df$InjectionMonth <- month(as.Date(inj.df$InjectionDate, format='%Y-%m-%d'))

saveRDS(inj.df,paste(path.to.data,"/",'inj_df.RDA',sep=""))
#--------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------
#Build the Production Well data set

prod.df <- list()

t <- Sys.time()
for(i in 1:16){
  yr <- 1998 + i
  file <- paste("p",yr,".csv",sep="")
  
  file_path <- paste(path.to.data,"/",file,sep="")
  prod.df[[i]] <- read.csv(file_path)
}
Sys.time() - t

prod.df <- data.frame(rbindlist(prod.df)) 


###Add Year Column###
prod.df$YEAR <- year(as.Date(prod.df$ProductionDate,format='%Y'))

###Add Month Column###
prod.df$MONTH <- month(as.Date(prod.df$ProductionDate, format='%Y-%m-%d'))

#Production data are huge....keep only observations after 2000 just to keeps things moving
# at a decent speed here
prod.df <- prod.df[prod.df$YEAR>1999,]
saveRDS(prod.df,file=paste(path.to.data,"/","prod_df.RDA",sep=""))
#------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------
#Build the basic Groundwater Quality Data
gw.wq.df <- rbind(read.table(file=paste(path.to.data,"/","gama_all_losangeles.txt",sep=""),sep="\t",comment.char="",header=TRUE),
                  read.table(file=paste(path.to.data,"/","gama_all_monterey.txt",sep=""),sep="\t",header=TRUE),
                  read.table(file=paste(path.to.data,"/","gama_all_orange.txt",sep=""),sep="\t",header=TRUE),
                  read.table(file=paste(path.to.data,"/","gama_all_sandiego.txt",sep=""),sep="\t",comment.char="",header=TRUE),
                  read.table(file=paste(path.to.data,"/","gama_all_sanluisobispo.txt",sep=""),sep="\t",header=TRUE),
                  read.table(file=paste(path.to.data,"/","gama_all_santabarbara.txt",sep=""),sep="\t",header=TRUE),
                  read.table(file=paste(path.to.data,"/","gama_all_ventura.txt",sep=""),sep="\t",comment.char="",header=TRUE),
                  read.table(file=paste(path.to.data,"/","gama_all_kern.txt",sep=""),sep="\t",header=TRUE),
                  read.table(file=paste(path.to.data,"/","gama_all_kings.txt",sep=""),sep="\t",header=TRUE),
                  read.table(file=paste(path.to.data,"/","gama_all_tulare.txt",sep=""),sep="\t",header=TRUE)
                  
                  
)

saveRDS(gw.wq.df,file=paste(path.to.data,"/",'gw_raw.RDA',sep=""))
#---------------------------------------------------------------------------------------------------

