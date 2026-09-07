###
# This code calculates emission growth factors by SCC, county and emission type for NEMS model scenario results to be processed in SMOKE
# The output of this script GF_county_scenario_name_time will be futher processed by EGU_reg_emiss_20181031DC.R 12232019
###

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

Emis_file<- read.csv(paste0("/Users/tylermatukonis/Documents/Research/Gentner_Group/Air_Quality_Co_Benefits/Area-Downscaling/output/area/EMIS_refnocpp_2026-08-17.csv"), stringsAsFactors = FALSE) ## read in NEMS results from AREA_downscaling_20181102, 
##be careful about time! _2020-08-21 is for air pollutant, _2023-02-14 for GHG
SCC_map<-read.csv("/Users/tylermatukonis/Documents/Research/Gentner_Group/Air_Quality_Co_Benefits/Area-Downscaling/input/SCC_map_20181009.csv", stringsAsFactors = FALSE)
NEMS_to_county<-read.csv(file="/Users/tylermatukonis/Documents/Research/Gentner_Group/Air_Quality_Co_Benefits/Area-Downscaling/input/NEMS region to county mapping summary.csv", stringsAsFactors = FALSE)


####
# PROCESSING STEPS
####

#Step 1.0: add (aggregate) emission by region, emission type, and Sector categories: Residential, Commercial, Industrial, Electric 
Emis_by_sector <- aggregate(list(Emis_file$emis_ave_2010.2015,
                                 Emis_file$emis_2020, 
                                 Emis_file$emis_2030, 
                                 Emis_file$emis_2040, 
                                 Emis_file$emis_2050), 
                            by = list(Emis_file$Sector, 
                                      Emis_file$NEMS_sector, 
                                      Emis_file$emis, 
                                      Emis_file$RegionNum), 
                            stringsAsFactors = FALSE, sum)

names(Emis_by_sector) <- c("Sector","NEMS_sector", "emis","region","emis_base", "emis_2020", "emis_2030","emis_2040","emis_2050") #give columns better names

#Step 1.1: add (aggregate) emission by region, emission type, and Sector categories: Residential, Commercial, Industrial, Electric 
#For 20230625 CO2 Ch4, for Yang, aggregrated by Sector only for sector GFs
#Other 
Emis_by_sector <- aggregate(list(Emis_file$emis_ave_2010.2015,
                                 Emis_file$emis_2020, 
                                 Emis_file$emis_2030, 
                                 Emis_file$emis_2040, 
                                 Emis_file$emis_2050), 
                            by = list(Emis_file$Sector, 
                                      Emis_file$emis, 
                                      Emis_file$RegionNum,
                                      Emis_file$Geogr), 
                            stringsAsFactors = FALSE, sum)

names(Emis_by_sector) <- c("Sector", "emis","region_cd","region","emis_base", "emis_2020", "emis_2030","emis_2040","emis_2050") #give columns better names

GF_by_sector <- cbind(Emis_by_sector, GF_2020=(Emis_by_sector$emis_2020/Emis_by_sector$emis_base),
                   GF_2030=(Emis_by_sector$emis_2030/Emis_by_sector$emis_base),
                   GF_2040=(Emis_by_sector$emis_2040/Emis_by_sector$emis_base),
                   GF_2050=(Emis_by_sector$emis_2050/Emis_by_sector$emis_base))
write.csv(GF_by_sector, file = paste0("/Users/tylermatukonis/Documents/Research/Gentner_Group/Air_Quality_Co_Benefits/Area-Downscaling/output/area/Areasource_GF_",scenario_name,"_",Sys.Date(),".csv"), quote=FALSE, row.names=F) 

#Step 2.0: allocate the emissions from sources to the SCCs

#create dataframe that will hold final data
Emis_by_SCC_final<-data.frame(matrix(ncol=9, nrow=0))
names(Emis_by_SCC_final)<-c("Sector","emis","region","X2011", "X2020", "X2030","X2040","X2050","SCC") #Give the dataframe the right column names


for(scc_i in 1:nrow(SCC_map)){ ## loop through all SCCs in teh SCC mapping file to allocate the source emissions to SCCs
  #scc_i<-2
  Emis_by_SCC <- Emis_by_sector[which(Emis_by_sector$NEMS_sector == SCC_map[scc_i,"NEMS_sector"]),]
  if(nrow(Emis_by_SCC)==0) next
  Emis_by_SCC <- cbind(Emis_by_SCC,SCC=SCC_map[scc_i,"SCC"])
  
  Emis_by_SCC_final <-rbind(Emis_by_SCC_final, Emis_by_SCC)
}#end of loop through SCCs

Emis_by_SCC_final<- na.omit(Emis_by_SCC_final)

#write.csv(Emis_by_SCC_final, file = paste0("output/area/Emis_by_SCC_final_",scenario_name,"_",Sys.Date(),".csv"), quote=FALSE, row.names=F) 

#Step 3.0: Calculate the growth factors for the SCC/region/emission type

GF_by_SCC <- cbind(Emis_by_SCC_final, GF_2020=(Emis_by_SCC_final$emis_2020/Emis_by_SCC_final$emis_base),
                      GF_2030=(Emis_by_SCC_final$emis_2030/Emis_by_SCC_final$emis_base),
                      GF_2040=(Emis_by_SCC_final$emis_2040/Emis_by_SCC_final$emis_base),
                      GF_2050=(Emis_by_SCC_final$emis_2050/Emis_by_SCC_final$emis_base))

GF_by_SCC[which(GF_by_SCC$emis_base == 0),c("GF_2020","GF_2030","GF_2040","GF_2050")]<- 1 # Replace the wacky (divide by zero) growth factor with a 1 -- no change in emission from base case

#write.csv(GF_by_SCC, file = paste0("E:/DoEHE/PhD/OneDrive - Johns Hopkins University/SEARCH/Area Downscaling/output/area/GF_by_SCC_",scenario_name,"_",Sys.Date(),".csv"), quote=FALSE, row.names=F) 

#Step 4.0: Allocate the GFs to all counties in each region. 
# Note: The region numbers refer to different kinds of regions depending on the sector

# create the dataframe that will hold the final results
GF_county<-data.frame(matrix(ncol=10))
names(GF_county)<-c("list_of_counties","Sector","NEMS_sector","emis","region","SCC",
                          "GF_2020","GF_2030","GF_2040","GF_2050") #Give the dataframe the right column names

for(GF_i in 1:nrow(GF_by_SCC)){ #Loop through each row of growth factors. 
#  GF_i <-3
  
#In the following IF statements, I select all the counties from the right region (depending on source Sector) 
    if(GF_by_SCC[GF_i,"Sector"] %in% c("electric power")){
      list_of_counties<-NEMS_to_county[which(NEMS_to_county$EMMReg == GF_by_SCC[GF_i,"region"]),"County"]
    }
    
    if(GF_by_SCC[GF_i,"Sector"] %in% c("residential","commercial")){
      list_of_counties<-NEMS_to_county[which(NEMS_to_county$CenDiv == GF_by_SCC[GF_i,"region"]),"County"]
    }
    
    if (GF_by_SCC[GF_i, "Sector"] %in% "industrial") {
      list_of_counties<-NEMS_to_county[which(NEMS_to_county$CenReg == GF_by_SCC[GF_i,"region"]),"County"]
    }
  
  #copy the GF data into each new row for each county        
    list_of_counties<-cbind(list_of_counties,GF_by_SCC[GF_i,c("Sector","NEMS_sector", "emis","region","SCC",
                                                               "GF_2020","GF_2030","GF_2040","GF_2050")])
  #append the new county/GFs to the final list of results
    GF_county<-rbind(GF_county,list_of_counties)
    
}# End looping through GF file

GF_county<- na.omit(GF_county) #Get rid of any empty rows that might have been added

#Write output file
write.csv(GF_county, file = paste0("/Users/tylermatukonis/Documents/Research/Gentner_Group/Air_Quality_Co_Benefits/Area-Downscaling/output/area/GF_county_",scenario_name,"_",Sys.Date(),".csv"), quote=FALSE, row.names=F) 

