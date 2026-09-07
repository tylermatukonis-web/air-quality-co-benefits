#######################
# For the October downscaling effort here's what I'm doing:
# 1.	AREA_downscaling_20181009
#   a.	Calculate emissions for 2011, 2012, 2013, 2014, 2015, 2020, 2030, 2040, 2050 based on fuel use in different sectors
#
# 2.	Outside of R (skip this step Nov 2018)
#   a.	Get rid of fuel use data columns
#   b.	Calculate the average 2010-2015 emissions as column "emis_ave_2010-2015" and get rid of X2010 - X2015
#   c.	Add column called NEMS_sector - copy data from "Sector" there, then copy all electricity rows and make Sector be "electric power - [fuel]" aligned with formatting of SCC mapping file.
#   d.	Save as EMIS_[scenario_name]_2018-10-09.csv - this is loaded back into R for more processing
#
# 3.	AREA_downscaling_PART2_20181009
#   a.	Group by sector/NEMS_sector/emis/Region
#   b.	assign to SCCs
#   c.	Calculate growth factors
#   d.	Replace anything that was "divide by zero" with a 1 
#
#DATA INPUTS
# Emission factors (EFs) are imported into the data frame: EFvals
#  ----->I spent a good amount of time QC'ing and adjusting the EFs, but this is something that could always use more tuning
#
# NEMS fuel usage is obtained from Yale-NEMS in a couple of output files - the standard output file, and some specially requested files that have more granularity 
# NEMS reports energy consumption for industrial sources in trillion (10^12) BTUS, but every other source is in quads (10^15 BTU).  
# NEMS data is imported into data frame: NEMS
#
# Electric generation is a special case
#  Emissions are calculated using emission factors for EGUs using coal, NG and oil
#   AND
#  NEMS provides annual, national NOx and SO2 in ***MMstons*** for the entire electricity sector. 
#  I collected both values separately. The amounts don't really match, and neither do the change factors - but to be honest, 
#      the calculated emission GF look slightly less wacky
#   ----> THIS IS SOMETHING TO REVISIT
#
#  Calculated emissions are all in ktonnes 
#  NEMS-reported NOx and SO2 for EGUS are in MMstons)
#  All emission factors are in the correct units to produce ktonnes from this code!!!
#
#
# Specific instructions and more detail about the whole process is in the file "NEMS downscaling impelmentation insrtuctions.docx"
#
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

NEMS<- read.csv(paste0("/Users/tylermatukonis/Documents/Research/Gentner_Group/Air_Quality_Co_Benefits/Area-Downscaling/input/refnocpp_TOTAL.csv"), stringsAsFactors = FALSE) ## read in NEMS results


NEMS$X2010<- as.numeric(as.character(NEMS$X2010))
NEMS$X2020<- as.numeric(as.character(NEMS$X2020))
NEMS$X2030<- as.numeric(as.character(NEMS$X2030))
NEMS$X2040<- as.numeric(as.character(NEMS$X2040))
NEMS$X2050<- as.numeric(as.character(NEMS$X2050))

EFvals<- read.csv("/Users/tylermatukonis/Documents/Research/Gentner_Group/Air_Quality_Co_Benefits/Area-Downscaling/input/EFvals_20181002.csv", stringsAsFactors = FALSE) ## read in Emission Factor values to use for each row of NEMS result !add CO2 CH4 02142023

#EFvals<- EFvals[which(EFvals$Sector_shortname != "Residential" & EFvals$Sector_shortname != "Commercial"),]##Only use for ind EGU and pt Commercial
#Set parameters
#Emis_list<-c("NOX","PM10.PRI","PM25.PRI","SO2","VOC","CO","BC","OC") #revised emission list as of July 2018 (to remove unnecessary PM species (-FIL, -CON) - see email exchange with Chinmay 7/19ish)
#Emis_list<-c("CO2","CH4")#2023 GHG

Emis_list<-c("NOX","PM10.PRI","PM25.PRI","SO2","VOC","CO","BC","OC", "CO2","CH4")

####
# PROCESSING STEPS
####

#Step 1.0: Calculate emissions for fuel consumption reported by NEMS for each sector and emission type

#Create data frame needed for the following loop and rbind
NEMS_rows_final<-data.frame(matrix(ncol=33, nrow = 0))

for(i in 1:nrow(EFvals)){ ## loop through every row in the Emission Factor table
  for(emis in Emis_list){ ##Loop through ech emission type for each row of Emission Factor
#Sometimes setting these things is useful for debugging:
    #i=29
    #emis="CO"


    TableNum <- EFvals[i,"TableNum"]
    RowNum <- EFvals[i,"RowNum"] 
    
    # If this is SO2 or NOx for an electric EGU emission factor, then skip it. 
    # Those emissions are handled separately in EGU_reg_emis_20181031.R
    if(emis %in% c("NOX","SO2") & TableNum == 62 & RowNum %in% c("131","132","133")){
      
    } else{
    NEMS_rows<-NEMS[which(NEMS$TableNumber == TableNum & NEMS$RowNum == RowNum & NEMS$RegionNum!=0),
                    c("TableNumber","RowNum","RegionNum","GLabel","Gunits","SubDat","Sector","SubSec","Source",
                      "SubSrc","Geogr","X2010", "X2011", "X2012", "X2013","X2014","X2015","X2020", "X2030","X2040","X2050")] #Grab NEMS output data for the particular ROW/Table combination for this EF
    NEMS_rows[is.na(NEMS_rows)]<- 0# fill na with 0
    NEMS_rows<-cbind(NEMS_rows,emis) #append a new column with the emission type to the NEMS data

    # Create the appropropriate column header names for this emission type
    year_bin_2010_2015<-paste0(emis,".2010.2015")
    year_bin_2015_2020<-paste0(emis,".2015.2020")
    year_bin_2020_2055<-paste0(emis,".2020.2055")
    
    #Multiply the fuel in each year by the correct year-specific emission factor, and append the calculated emission number to a new column in the table  
    NEMS_rows<-cbind(NEMS_rows, emis_2010=(NEMS_rows$X2010*EFvals[i,year_bin_2010_2015]))
    NEMS_rows<-cbind(NEMS_rows, emis_2011=(NEMS_rows$X2011*EFvals[i,year_bin_2010_2015]))
    NEMS_rows<-cbind(NEMS_rows, emis_2012=(NEMS_rows$X2012*EFvals[i,year_bin_2010_2015]))
    NEMS_rows<-cbind(NEMS_rows, emis_2013=(NEMS_rows$X2013*EFvals[i,year_bin_2010_2015]))
    NEMS_rows<-cbind(NEMS_rows, emis_2014=(NEMS_rows$X2014*EFvals[i,year_bin_2010_2015]))
    NEMS_rows<-cbind(NEMS_rows, emis_2015=(NEMS_rows$X2015*EFvals[i,year_bin_2010_2015]))
    NEMS_rows<-cbind(NEMS_rows, emis_2020=(NEMS_rows$X2020*EFvals[i,year_bin_2015_2020]))
    NEMS_rows<-cbind(NEMS_rows, emis_2030=(NEMS_rows$X2030*EFvals[i,year_bin_2020_2055]))
    NEMS_rows<-cbind(NEMS_rows, emis_2040=(NEMS_rows$X2040*EFvals[i,year_bin_2020_2055]))
    NEMS_rows<-cbind(NEMS_rows, emis_2050=(NEMS_rows$X2050*EFvals[i,year_bin_2020_2055]))
    NEMS_rows<-cbind(NEMS_rows, emis_units=("ktonnes"))
    
    NEMS_rows_final<-rbind(NEMS_rows_final,NEMS_rows)}  #attach this new data to the bottom of the data file that will contain all the data
    #close ELSE checking for whether this is EGU SO2 or NOx (both of which are skipped because they're handled in other R code)

  } #close emis for loop
   
} # close NEMS rows for loop

NEMS_rows_final<- na.omit(NEMS_rows_final) #Get rid of any empty rows that might have been added
#write.csv(NEMS_rows_final, file = paste0("E:/OneDrive - Johns Hopkins/SEARCH/Area Downscaling//output/area/EMIS_src_fuel_NEMSreg_",scenario_name,"_",Sys.Date(),".csv"), quote=FALSE, row.names=F) 

NEMS_rows_emmis<- subset(NEMS_rows_final,select = -c(X2010:X2050))
NEMS_rows_emmis$"emis_ave_2010-2015"<- rowMeans(NEMS_rows_emmis[c('emis_2010','emis_2011','emis_2012','emis_2013','emis_2014','emis_2015')])#, na.rm=TRUE
NEMS_rows_emmis<- subset(NEMS_rows_emmis,select = -c(emis_2010:emis_2015))
NEMS_rows_emmis$NEMS_sector<- NEMS_rows_emmis$Sector
NEMS_rows_elec_nemsec<- data.frame(matrix(ncol=19, nrow = 0))
colnames(NEMS_rows_elec_nemsec)<- colnames(NEMS_rows_emmis)
#add NEMS_Sector
count = 0
for (i in 1:nrow(NEMS_rows_emmis)){
  
  #i<-5126
  
  if (NEMS_rows_emmis$Sector[i] == "electric power"){
    count = count+1
    Test_row<- NEMS_rows_emmis[i,]
    NEMS_rows_elec_nemsec<-rbind(NEMS_rows_elec_nemsec,Test_row)
    #NEMS_rows_elec_nemsec[i,] <- NEMS_rows_emmis[i,]
    NEMS_rows_elec_nemsec$NEMS_sector[count]<- paste(NEMS_rows_elec_nemsec$Sector[count],NEMS_rows_elec_nemsec$SubSrc[count],sep = "-")
  }
}

NEMS_rows_elec_nemsec<- na.omit(NEMS_rows_elec_nemsec)
NEMS_rows_emmis<- rbind(NEMS_rows_emmis,NEMS_rows_elec_nemsec,stringsAsFactors = FALSE)
NEMS_rows_emmis<- NEMS_rows_emmis[,c(1:7,19,8:12,18,13:17)]
write.csv(NEMS_rows_emmis, file = paste0("/Users/tylermatukonis/Documents/Research/Gentner_Group/Air_Quality_Co_Benefits/Area-Downscaling/output/area/EMIS_",scenario_name,"_",Sys.Date(),".csv"), quote=FALSE, row.names=F) 


# ### I am commenting out the following code to get SO2 and NOx from the basic NEMS output file. 
# ### Instead of this use the code EGU_reg_emis_20181031.R to get EGU emissions and calculate growth factors from the bigger more detailed NEMS emissions reporting
# # Now get electric sector emissions that NEMS reports (WHICH ARE IN TABLE 62, ROWS 153 AND 154)
# 
# TableNum <- 62
# RowNum <- 153 ## Electric sector SO2 emissions in MMstons (calculated emissions in ktonnes)
# 
# 
# #Emis_output<-NEMS[which(NEMS$TableNumber == TableNum & NEMS$RowNum == RowNum ),c(3:5,7,11,19,28,38,48,58)]
# 
# Emis_output<-NEMS[which(NEMS$TableNumber == TableNum & NEMS$RowNum == RowNum ), c("TableNumber","RowNum","RegionNum","GLabel","Gunits","SubDat","Sector","SubSec","Source",
#                                                                                   "SubSrc","Geogr","X2010","X2011","X2012","X2013","X2014","X2015","X2020", "X2030","X2040","X2050")]
# 
# ## These columns correspond to:"TableNumber","RowNum","RegionNum","GLabel","SubDat", and the desired years 2011-2040
# 
# RowNum <- 154 ## Electric sector NOX emissions in MMstons (calculated emissions in ktonnes)
# 
# Emis_output<-rbind(Emis_output,NEMS[which(NEMS$TableNumber == TableNum & NEMS$RowNum == RowNum),  c("TableNumber","RowNum","RegionNum","GLabel","Gunits","SubDat","Sector","SubSec","Source",
#                                                                                                     "SubSrc","Geogr","X2010","X2011","X2012","X2013","X2014","X2015","X2020", "X2030","X2040","X2050")])
# 
# 
# write.csv(Emis_output, file = paste0("output/area/EGU_emis_",scenario_name,"_",Sys.Date(),".csv"), quote=FALSE, row.names=F) 
