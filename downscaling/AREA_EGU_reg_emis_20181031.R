#Processing EGU emissions reported by NEMS in the big, non-standard file taht has NOx, SO2 by region and fuel
#Need GFs total as input to modify GF_county_no_elec 
#We use this to process Industrial and EGUs
#

rm(list=ls())##clear working space

###
# SET THE WORKING DIRECTORY:
###
setwd("/Users/tylermatukonis/Documents/Research/Gentner_Group/Air_Quality_Co_Benefits/Area-Downscaling") ##Set your working directory here!
library(stringr)
library(data.table)

####
##CHOOSE SCENARIO:
####

scenario_name<-c("refnocpp")
#scenario_name<-c("highNG")
#scenario_name<-c("highEV")
#scenario_name<-c("portmod")
#scenario_name<-c("port")
#scenario_name<-c("highEE")

####
##Read in data files
## MAKE SURE DATA FILE NAMES CORRECT:
####
##Need to correct the title in input data for NEMS, col name FUEL to Fuel
NEMS<- read.csv(paste0("/Users/tylermatukonis/Documents/Research/Gentner_Group/Air_Quality_Co_Benefits/Area-Downscaling/input/output_ele_emis_annual_refnocpp_20181016.csv"), stringsAsFactors = FALSE) ## read in NEMS results refnocpp_20181016, highEV_20191227,highNG_20181016,port_20200527,highEE_20191219
SCC_map<-read.csv(paste0("/Users/tylermatukonis/Documents/Research/Gentner_Group/Air_Quality_Co_Benefits/Area-Downscaling/input/SCC_EGU_map_20181030.csv"), stringsAsFactors = FALSE) ## read in NEMS results
NEMS_to_county<-read.csv(file="/Users/tylermatukonis/Documents/Research/Gentner_Group/Air_Quality_Co_Benefits/Area-Downscaling/input/NEMS region to county mapping summary.csv", stringsAsFactors = FALSE)


####

# PROCESSING STEPS
####

#Step 1.0: aggregate yearly/regional emissions to get total electric power emissions (for upper-umbrella SCC growth factor) 
# Note: don't include renewables (which is really just biomass(wood) because that skews the growth factors high  
NEMS_by_emis<-NEMS[which(NEMS$Fuel!="renewables"),]##Need to correct the title in input data, col name FUEL to Fuel
NEMS_by_emis<-aggregate(list(NEMS_by_emis$value),
                        by=list(NEMS_by_emis$Year, NEMS_by_emis$RegionNum, NEMS_by_emis$Geogr, NEMS_by_emis$SubDat, NEMS_by_emis$Unit), stringsAsFactors = FALSE, sum)

names(NEMS_by_emis)<-c("Year", "RegionNum","Geogr","SubDat","Unit","Value") #Give columns better names

NEMS_by_emis$Fuel<-"electric power" #assign the "fuel type" to be electric power, so that this will be assigned to the top level SCC

NEMS_by_emis<-NEMS_by_emis[,c(1:4, 7, 5:6)] #rearrange column order


#STEP 1.1: aggregate yearly/regional emissions by fuel type
# It's ok to ignore the "descr" column, because 
#  -- coal has subcategories, but they are all being counted as coal
#  -- NG lists firm and non-firm, but they should both be counted just as NG
#  -- petroleum should all just be counted as Oil (the high sulfur has lower emissions than low sulfur residual, so I just decided for now to count all petroleum as oil)
#  -- the only subcategory of renewables that has emissions is biomass(wood)

NEMS_by_fuel<-aggregate(list(NEMS$value),
                         by=list(NEMS$Year, NEMS$RegionNum, NEMS$Geogr, NEMS$SubDat, NEMS$Fuel, NEMS$Unit), stringsAsFactors = FALSE, sum)
names(NEMS_by_fuel)<-c("Year", "RegionNum","Geogr","SubDat","Fuel","Unit","Value") #Give columns better names
NEMS_by_fuel$Fuel<-paste0("electric power-",NEMS_by_fuel$Fuel)

NEMS_for_GF<-rbind(NEMS_by_fuel,NEMS_by_emis)

NEMS_for_GF<-cbind(NEMS_for_GF,Desc="electric power")

NEMS_for_GF<-NEMS_for_GF[,c(1:5, 8, 6:7)]

#write.csv(NEMS_for_GF, file = paste0("E:/DoEHE/PhD/OneDrive - Johns Hopkins University/SEARCH/Area Downscaling/output/area/TEST_NEMS_for_GF_",scenario_name,"_",Sys.Date(),".csv"), quote=FALSE, row.names=F) 


#STEP 2.0: "transpose" emissions data so that yearly emissions are in columns (not separate rows)

#Create placeholder dataframe to hold final data - this is how I gEt the row-bind to work
NEMS_for_GF_final<-data.frame(matrix(ncol=15, nrow=0)) # Create placeholder date frame

names(NEMS_for_GF_final)<-c("RegionNum","Geogr","SubDat","Fuel","Desc","Unit",
                             "2011","2012","2013","2014","2015",
                             "emis_2020",
                             "emis_2030",
                             "emis_2040",
                             "emis_2050")

NEMS_looping<-unique(NEMS_for_GF[,2:7]) #Select all the unique regions/fuel/emission type (SubDat) to loop through

for(i in  1:nrow(NEMS_looping)){
#  i<-1
  NEMS_emis_row<-NEMS_looping[i,]  
  
  for (i_year in c( "2011","2012","2013","2014","2015",
                    "2020",
                    "2030",
                    "2040",
                    "2050")){
#    i_year <- 2021
    
    num_rows<-which(NEMS_for_GF$Year == i_year & 
                      NEMS_for_GF$RegionNum == NEMS_emis_row$RegionNum & 
                      NEMS_for_GF$SubDat == NEMS_emis_row$SubDat &
                      NEMS_for_GF$Fuel == NEMS_emis_row$Fuel &
                      NEMS_for_GF$Desc == NEMS_emis_row$Desc)
    if(length(num_rows) == 0){
      NEMS_emis_row<-cbind(NEMS_emis_row, "XX" = 0)
    }
    else{
      NEMS_emis_row<-cbind(NEMS_emis_row, "XX" = NEMS_for_GF[num_rows, c(8)])
    }
    
      
} #end for looping through years
  
  names(NEMS_emis_row)<- c("RegionNum","Geogr","SubDat","Fuel","Desc","Unit",
                           "2011","2012","2013","2014","2015",
                           "emis_2020",
                           "emis_2030",
                           "emis_2040",
                           "emis_2050")
  
  NEMS_for_GF_final<-rbind(NEMS_for_GF_final,NEMS_emis_row)
} #end for looping through rows


#write.csv(NEMS_for_GF_final, file = paste0("E:/OneDrive - Johns Hopkins/SEARCH/Area Downscaling/output/area/TEST_NEMS_for_GF_final_",scenario_name,"_",Sys.Date(),".csv"), quote=FALSE, row.names=F) 


#STEP 3.0: Calculate Growth Factors
#All 2011 as base
NEMS_for_GF_final<-cbind(NEMS_for_GF_final,emis_base = NEMS_for_GF_final[,7])#2011 as base now!

#All 2011-2015 avg as base
#NEMS_for_GF_final<-cbind(NEMS_for_GF_final,emis_base = rowMeans(NEMS_for_GF_final[,7:11]))

NEMS_for_GF_final <- cbind(NEMS_for_GF_final, GF_2020=(NEMS_for_GF_final$emis_2020/NEMS_for_GF_final$emis_base),
                   GF_2030=(NEMS_for_GF_final$emis_2030/NEMS_for_GF_final$emis_base),
                   GF_2040=(NEMS_for_GF_final$emis_2040/NEMS_for_GF_final$emis_base),
                   GF_2050=(NEMS_for_GF_final$emis_2050/NEMS_for_GF_final$emis_base))

NEMS_for_GF_final[which(NEMS_for_GF_final$emis_base == 0),c("GF_2020","GF_2030","GF_2040","GF_2050")]<- 1 # Replace the wacky (divide by zero) growth factor with a 1 -- no change in emission from base case

#write.csv(NEMS_for_GF_final, file = paste0("E:/OneDrive - Johns Hopkins/SEARCH/Area Downscaling/input/area/GFs_for_ArcGIS/EGU_GF_by_fuel_",scenario_name,"_",Sys.Date(),".csv"), quote=FALSE, row.names=F) 

#STEP 4.0: Allocate emissions to SCCs

EGU_GF_by_SCC_final<-data.frame(matrix(ncol=21, nrow=0))
names(EGU_GF_by_SCC_final)<-c("RegionNum","Geogr","SubDat","Fuel","Desc","Unit",
                              "2011","2012","2013","2014","2015",
                              "emis_2020",
                              "emis_2030",
                              "emis_2040",
                              "emis_2050", "emis_base",
                              "GF_2020", "GF_2030", "GF_2040","GF_2050","SCC")
for(scc_i in 1:nrow(SCC_map)){ ## loop through every row in the Emission Factor table
#  scc_i<-6
  Emis_by_SCC <- NEMS_for_GF_final[which(NEMS_for_GF_final$Fuel == SCC_map[scc_i,"NEMS_sector"]),]
  
  Emis_by_SCC <- cbind(Emis_by_SCC,SCC=SCC_map[scc_i,"SCC"])
  
  EGU_GF_by_SCC_final <-rbind(EGU_GF_by_SCC_final, Emis_by_SCC)
}#end of loop through SCCs

EGU_GF_by_SCC_final<- na.omit(EGU_GF_by_SCC_final)

#write.csv(EGU_GF_by_SCC_final, file = paste0("E:/DoEHE/PhD/OneDrive - Johns Hopkins University/SEARCH/Area Downscaling/output/area/EGU_GF_by_SCC_final_",scenario_name,"_",Sys.Date(),".csv"), quote=FALSE, row.names=F) 

EGU_GF_by_SCC<-EGU_GF_by_SCC_final[,c(5,4,3,1,21,17:20)]
names(EGU_GF_by_SCC)<-c("Sector","NEMS_sector","emis","region","SCC",
                    "GF_2020","GF_2030","GF_2040","GF_2050") #Give the dataframe the right column names

#Change emission names to match what SMOKE expects
EGU_GF_by_SCC[which(EGU_GF_by_SCC$emis  == "nitrogen oxide"),"emis"]<-"NOX"
EGU_GF_by_SCC[which(EGU_GF_by_SCC$emis  == "sulfur dioxide"),"emis"]<-"SO2"


#STEP 5.0: Distribute regional growth factors to counties
GF_county<-data.frame(matrix(ncol=10))
names(GF_county)<-c("list_of_counties","Sector","NEMS_sector","emis","region","SCC",
                    "GF_2020","GF_2030","GF_2040","GF_2050") #Give the dataframe the right column names

for(GF_i in 1:nrow(EGU_GF_by_SCC)){ 
  #  GF_i <-3
  
 list_of_counties<-NEMS_to_county[which(NEMS_to_county$EMMReg == EGU_GF_by_SCC[GF_i,"region"]),"County"]
 
 list_of_counties<-cbind(list_of_counties,EGU_GF_by_SCC[GF_i,c("Sector","NEMS_sector", "emis","region","SCC",
                                                            "GF_2020","GF_2030","GF_2040","GF_2050")])
 GF_county<-rbind(GF_county,list_of_counties)
  
}# End looping through GF file

GF_county<- na.omit(GF_county)

#write.csv(GF_county, file = paste0("E:/DoEHE/PhD/OneDrive - Johns Hopkins University/SEARCH/Area Downscaling/output/area/EGU_GF_county_",scenario_name,"_",Sys.Date(),".csv"), quote=FALSE, row.names=F) 
GF_county_no_elec <- read.csv(paste0("/Users/tylermatukonis/Documents/Research/Gentner_Group/Air_Quality_Co_Benefits/Area-Downscaling/output/area/GF_county_refnocpp_2026-08-17.csv"), stringsAsFactors = FALSE)#Need to correct the time
GF_county_all_secotr<- rbind(GF_county_no_elec,GF_county)
#write.csv(GF_county_all_secotr, file = paste0("E:/DoEHE/PhD/OneDrive - Johns Hopkins University/SEARCH/Area Downscaling/output/area/GF_for_SMOKE_",scenario_name,"_",Sys.Date(),".csv"), quote=FALSE, row.names=F) 
#Use this for Air pollutant:
#write.csv(GF_county_all_secotr, file = paste0("E:/OneDrive - Johns Hopkins/SEARCH/Area Downscaling/output/area/GF_Ind_EGU_ptC_for_SMOKE_",scenario_name,"_",Sys.Date(),".csv"), quote=FALSE, row.names=F) 
#Use this for GHG:
write.csv(GF_county_no_elec, file = paste0("/Users/tylermatukonis/Documents/Research/Gentner_Group/Air_Quality_Co_Benefits/Area-Downscaling/output/area/GF_Ind_EGU_ptC_for_SMOKE_",scenario_name,"_",Sys.Date(),".csv"), quote=FALSE, row.names=F) 

