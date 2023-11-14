#MGLP Project

#Version History:
  #Originally written by Jacob Angus 30June2022 (FullMNScript.R)
  #Modified by Heidi Rantala ~Aug 2023 to examine some questionable DO values and filter rule effects (FullMNScript_hmr_2ppm.R)
  #Modified by Mike Verhoeven Oct 2023 to separate data agg and prep from VHOD and TDO3 generation


# TO-do list:
#   1. Work through script deleting extra & deprecated code
#   2. Rename the input data so that a perosn can logically track packages of related files. 
#   3. 
 

#  Script for Minnesota that can be run as a job
#  It can also be run in individual sections
#  It has five major sections: Data Cleaning and Joining, Filtering,
#  Interpolation, Calculating VHOD, Calculating TDO3

library("tidyverse")
library("lubridate")
library("zoo")
library("rLakeAnalyzer")
library("broom")
library("readxl")
library("plotly")
library("skimr")
library("data.table")

###### Data Cleaning, Preparation, and Joining ==================================
#  The goal of this section of the script is to filter lakes for
#  geometric ratio, combine data sets with metadata, and join Minnesota
#  data sets together. It is split into two sections: WQP and MNPCA
#  Run time is about 5 minutes

#set wd to the data folder to avoid needing filepaths
  # setwd("data&scripts/data/input")

# Water Quality Portal Data -----------------------------------------------

### Load files
# WQP 
  # MN Observations from Water Quality Portal 
  # allMN.do <- read_csv("MNDataRetrievalCombined.csv") #All MN Observations of Temp and DO from 1940 to 2020 from WQP
    # problems(allMN.do)
    # problems(allMN.do)[,"col"] %>% unique()
    # names(allMN.do)[c(36,58, 17, 18, 19, 42, 38)]
  # ^^ This read into r has issues-- seems to be squashing SUS/ suspect data codes into NA values. Will use fread:
  allMN.do = fread("MNDataRetrievalCombined.csv") #All MN Observations of Temp and DO from 1940 to 2020 from WQP
    # allMN.do[ , .N , .(MeasureQualifierCode, ResultStatusIdentifier)  ]


# This read into r also has issues-- seems to be squashing some variable about watershed area... 
    # MN.allsites = read_csv("MNDataRetrievalMetadata.csv") #Site data for all the observations above
    # MN.allsites %>%
    #   filter(row_number() %in% pull(problems(MN.allsites)[,c("row")])) %>% 
    #   {probs <<- .}
  # Again doing this with fread
  MN.allsites <-  fread("MNDataRetrievalMetadata.csv") #Site data for all the observations above
  # MN.allsites[ ,.N , DrainageAreaMeasure.MeasureValue]
  # probably wasn't a big deal w/ the issues. 
  
# No issues in these two files  
  LakeDepthArea = read_csv("lake_depth.csv") #Dataset containing the lake depth and area for all lakes in the US (source? also, doubtful...there are only 17675 entries...)
  Link = read_csv("lake_link.csv") #Dataset that can link the Lagos dataset to the STORET dataset

#  Convert Monitoring Location Identifier into DOW 
#  DOW is the official way to identify lakes in Minnesota; whereas,
#  Monitoring Location Identifiers are specific sites on a lake.
  allMN.do[ OrganizationIdentifier == "MNPCA" , .N , MonitoringLocationIdentifier ]
  
# FOR MNPCA Records, parse the DOW
allMN.do[ OrganizationIdentifier == "MNPCA" , DOW := gsub("-", "", word(MonitoringLocationIdentifier, start = 2L, end = 4L, sep = "-")) , ]
  
    # ^ this line replaces the commented chunk below
    #   all.MN.do = allMN.do %>%
    #   filter(OrganizationIdentifier == "MNPCA")%>%
    #   separate(MonitoringLocationIdentifier, into = c("MNPCA", "County", "LakeID", "ID", "Basin"), sep = "-", remove = F)%>%
    #   unite(col = DOW, c(County, LakeID, ID), sep = "") %>% 
    #     anti_join()
    # 
    # 
    # 
    # mutate(DOW, if_else(ORG = MPCA, operation ,  ))
    # 
    # 
    # all.MN.DOW = allMN.do %>%
    #   filter(OrganizationIdentifier != "MNPCA")%>% # grab the non MPCA records
    #   mutate(DOW = MonitoringLocationIdentifier)%>% #shove monitoring locID into DOW -- but why!?!?!
    #   full_join(all.MN.do.DOW)

#Summary of WQP Data  #2217745 obs
# allMN.do %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n())  
# allMN.do %>%
#   group_by(DOW)%>%
#   summarise(n = n()) 
# allMN.do %>%
#   mutate(Year = year(ymd(ActivityStartDate)))%>%
#   group_by(Year)%>%
#   summarise(n = n())
# allMN.do %>%
#   mutate(Year = year(ymd(ActivityStartDate)))%>%
#   group_by(DOW, Year)%>%
#   summarise(n = n())
# allMN.do %>%
#   mutate(Year = year(ymd(ActivityStartDate)),
#          DOY = yday(ymd(ActivityStartDate)))%>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) 
# 
allMN.do[ , .N , .(MonitoringLocationIdentifier,ActivityStartDate) ][N>1, .N]
allMN.do[ , .N , is.na(DOW)]

# So this file still has about 13% missing DOWs 




# MNPCA Data --------------------------------------------------------------

#  MNPCA
MN_sum = read_csv("aug_profiles.csv") #Summary of profiles from MNPCA
MN_df = read_csv("1945_2020_All_MNDNR_MPCA_Temp_DO_Profiles.csv") #Lake Profile data
MN.station = fread("MNstation.csv")#Metadata for all MNPCA sites

# get DOW into the station data

MN.station[ OrganizationIdentifier == "MNPCA" , DOW := gsub("-", "", word(MonitoringLocationIdentifier, start = 2L, end = 4L, sep = "-")) , ]# Here we also need to make DOW from Monitoring Location Identifier
  
MN.station[ , .N , .(DOW, LatitudeMeasure, LongitudeMeasure) ][N>1 , .N] # how many cases where a DOW has > 1 lat & lon?
MN.station[!is.na(DOW) , length(unique(LatitudeMeasure)) , .(DOW) ][ ,summary(V1) ,] # this suggests there are 134 unique locs in one of the dows in these data
# Not using these now, but might need to...
#   select(c(DOW, LatitudeMeasure, LongitudeMeasure))%>%
#   distinct(DOW, .keep_all = T)



#  Here we are creating Monitoring Location Identifiers for the MNPCA and MNDNR
#  dataset. They have "PROFID" and DOW which can be combined to make a 
#  Monitoring Location Identifier. This will later help when removing duplicate
#  profiles and joining the data together.
MN_df <-  MN_df %>% 
  separate(DOW_DATE_AGENCY_PROFID, into = c("DOW", "DATE", "AGENCY", "PROFID"), sep = "_", remove = F)%>%
  mutate(Sample.Site = as.integer(PROFID)) %>% #
  mutate(County = substr(DOW, start = 1, stop = 2),
         Township = substr(DOW, start = 3, stop = 6),
         LakeID = substr(DOW, start = 7, stop = 8), #YIKES
         MNAgency = paste("MN", AGENCY, sep = ""),
         MonitoringLocationIdentifier = paste(MNAgency, County, Township, LakeID, Sample.Site, sep = "-"))


#' not sure how it gets used in later steps, but that DOW breakdown ^^ is INCORRECT


#' check on the monitoring loc id gen
MN_df[ , "MonitoringLocationIdentifier"  , ] 
MN_df[ , c("Sample.Site", "PROFID") , ]

allMN.do[OrganizationIdentifier=="MNPCA" , .(MonitoringLocationIdentifier)]

as.integer(MN_df$PROFID)
any(word(MN_df$PROFID, 2, sep = "\\." )!="0")#are there any non-zero sig digits after the decimal? Yes...
setDT(MN_df)



MN_df[word(PROFID, 2, sep = fixed(".")) != "0" , PROFID ,]
MN_df[word(PROFID, 2, sep = fixed(".")) != "0" , .N ,]
# about 1/8 of these records ave decimals in those profile IDents. This means that the PROFID variable is going to be dup over many measurements in some cases. 

#can we recover something similar in the WQP data? Like a multipart profile identifier?
allMN.do[ , ActivityIdentifier , ]

#' So it's obvious to me that there are multipart obs in these data (multi row at one location and date), and here we
#' have ignored those as we developed the variables that we will use to cross
#' these two datasets in search of duplicated variables. This means that we won't really check for duplicated measures, but instead duplicated locations/sampling runs (profiles)...


# Remove Dups Between WQP & MPCA ------------------------------------

### Remove Duplicate Profiles across data sets
#  The MNPCA reports most of their data to the WQP; therefore, we likely have 
#  duplicated data. If we screen for profiles (Monitoring Location ID + Date),
#  then we can see which ones are overlapping and remove them from one data set


WQP.Profiles = allMN.do %>% # This identifies all unique profiles in the WQP data
  mutate(Year = year(ymd(ActivityStartDate)),
         DOY = yday(ymd(ActivityStartDate)),
         Profile = paste(MonitoringLocationIdentifier,Year, DOY, sep = "/"))%>%
  distinct(Profile)

MN.Duplicate.Profiles = MN_df%>% # This identifies all unique profiles in the MNPCA data
  mutate(Year = year(mdy(DATE)),
         DOY = yday(mdy(DATE)),
         Profile = paste(MonitoringLocationIdentifier,Year, DOY, sep = "/"))%>%
  distinct(Profile)%>%
  inner_join(WQP.Profiles) # Joins the two together with inner_join, so we only select for the ones that both data sets have


MN_df = MN_df %>% #Finally remove the duplicate ones from the MNPCA data set 
  mutate(Year = year(mdy(DATE)),
         DOY = yday(mdy(DATE)),
         Profile = paste(MonitoringLocationIdentifier,Year, DOY, sep = "/"))%>%
  anti_join(MN.Duplicate.Profiles, by = "Profile") #Use anti_join to remove anything that overlaps

# # # Summarize the MNPCA Data #434023 obs
# MN_df %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #8,470 Sample Sites
# MN_df %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #4,358 Lakes
# MN_df %>%
#   mutate(Year = year(mdy(DATE)))%>%
#   group_by(Year)%>%
#   summarise(n = n()) #80 Years
# MN_df %>%
#   mutate(Year = year(mdy(DATE)))%>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #22,981 Lake-Years
# MN_df %>%
#   mutate(Year = year(mdy(DATE)),
#          DOY = yday(mdy(DATE)))%>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #40,120 Profiles

# # #Check for overlap -- here overlap is okay

# #DOW overlap
# WQP.DOW = allMN.do %>%
#   distinct(DOW)
# MN.DOW = MN_df %>%
#   distinct(DOW)%>%
#   inner_join(WQP.DOW) #2269 Overlap of Lakes
# 
# #Years overlap
# WQP.Year = allMN.do %>%
#   mutate(Year = year(ymd(ActivityStartDate)))%>%
#   distinct(Year)
# MN.Year = MN_df %>%
#   mutate(Year = year(mdy(DATE)))%>%
#   distinct(Year)%>%
#   inner_join(WQP.Year) #74 Overlap of Years
# 
# #Lk Year overlap
# WQP.LkYear = allMN.do %>%
#   mutate(Year = year(ymd(ActivityStartDate)),
#          Lk_Yr = paste(DOW, Year, sep = "/"))%>%
#   distinct(Lk_Yr)
# MN.LkYear = MN_df %>%
#   mutate(Year = year(mdy(DATE)),
#          Lk_Yr = paste(DOW, Year, sep = "/"))%>%
#   distinct(Lk_Yr)%>%
#   inner_join(WQP.LkYear)#4,098 Overlap of Lake Year Combos
# 
# #MLI Overlap
# WQP.MLI = allMN.do %>%
#   distinct(MonitoringLocationIdentifier)
# MN.MLI = MN_df %>%
#   distinct(MonitoringLocationIdentifier)%>%
#   inner_join(WQP.MLI) #301 Overlap of MonitoringLocationIdentifier
# 
# #profile overlap
# WQP.Profile.Check = allMN.do %>%
#   mutate(Year = year(ymd(ActivityStartDate)),
#          DOY = yday(ymd(ActivityStartDate)),
#          Profile = paste(MonitoringLocationIdentifier, Year, DOY, sep = "/"))%>%
#   distinct(Profile)
# MN.Profile.Check = MN_df %>%
#   mutate(Year = year(mdy(DATE)),
#          DOY = yday(mdy(DATE)),
#          Profile = paste(MonitoringLocationIdentifier,Year, DOY, sep = "/"))%>%
#   distinct(Profile)%>%
#   inner_join(WQP.Profile.Check)#0 Overlap of Profiles

#Clean
rm(WQP.Profiles, MN.Duplicate.Profiles, WQP.Profile.Check,MN.Profile.Check,
   MN.LkYear, MN.Year, MN.DOW, MN.MLI, WQP.MLI, WQP.DOW, WQP.Year, WQP.LkYear)
gc()



# Add Geom Ratio to Lake Area Data------------------------------------------------------

LakeDepthArea = LakeDepthArea %>% 
  # filter(lake_states == "MN")%>% #We are only looking at MN lakes right now, but we can add the GR to the whole dataset
  mutate(lake_area_m2 = lake_waterarea_ha * 10000)%>% #Fang and Stefan 2009 uses the As^0.25:Z with As in square meters
  mutate(GR = (lake_area_m2^0.25)/lake_maxdepth_m) #(Gorham and Boyce, 1989) is one of the first paper to use Geometric Ratio
  # filter(GR < 4) #simultaneously with a GR greater than 4 do not stratify. Here we leave GR in the data so that all filtering can happen simultaneously





# Join WQP datasets (FLAGGED: bad join) -----------------------------------------------------------
# Originally, this join resulted in duplicates for various reasons, including duplicating monitoring location identifiers because of multiple nhdplusv2_comid, lagosne_lagoslakeid per monitoringlocation identifier. Now we have specified thata a multiple match should nab only the first match (but note that that is not a very specific way to execute a join).


#WQP
#Connect "lake areas to the metadata from WQP:
#This join is lookin' crazy. 

# Link is a table of WQP (storet) ids and lagoslake ids, Lake Depth area looks like a LAGOS product? And MN.allsites is WQP metdata, 
setDT(Link)
Link[ , .N , "lagoslakeid"][N>1]
setDT(LakeDepthArea)
LakeDepthArea[ , .N , "lagoslakeid" ]


Link %>% #this guys shows LAGOS codes for WQP stations
  select(c("lagoslakeid","wqp_monitoringlocationidentifier","lake_nhdid")) %>% # get rid of extra columns in the data to keep it simpler
  full_join( . , LakeDepthArea, relationship = "many-to-one", suffix = c(".link", ".lakedeptharea")) %>% #lagoslakeid also contains all the basins for lakes This join BLOWS UP. Def many to many
  rename(MonitoringLocationIdentifier = wqp_monitoringlocationidentifier) %>% #Making the metadata files share columns
  left_join(MN.allsites,.,  relationship = "one-to-many", suffix = c(".linkXlakedep", ".mnallsites")) %>% 
  left_join(allMN.do, . ,   multiple = "first", suffix = c(".linkXlakedepXmnallsites", ".allmndo")) %>% #specifying multiple here prevent duplication of data at the cost of taking the first case in the all.sites set which is totally ambiguous
  { WQPdata_joined <<- .}
#for this join, we could do better by figuring out which columns will be important to us in the future and choosing more wisely how to match records (i.e., prep datasets to preserve info prior to the joins and avoid using first matches)


#' 
#' link2<-MN.WQP%>%select(c("lagoslakeid","wqp_monitoringlocationidentifier","lake_nhdid", 
#'                          "MonitoringLocationIdentifier", "DOW"))%>%
#'   unique()
#' #write.csv(link2, "link2.csv")
#' 
#' #' No idea why this command is in here. Grabs a few bit of lakeinfo out of the WQP data:
#' # 
#' # df = MN.WQP %>%
#' #   filter(DOW == "03035900" |
#' #            DOW == "03038300" |
#' #            DOW == "10004800" |
#' #            DOW == "11047200" |
#' #            DOW == "18021100" |
#' #            DOW == "19600600" |
#' #            DOW == "27009800" |
#' #            DOW == "29013000" |
#' #            DOW == "62005400" |
#' #            DOW == "77015002" |
#' #            DOW == "80003800" |
#' #            DOW == "85001102")%>%
#' #   select(DOW,lake_waterarea_ha, lake_maxdepth_m, GR )%>%
#' #   distinct(DOW, .keep_all = T)



# *Clean up measurements & units of T and DO ------------------------------------
#note one case of mm. did some digging, looks like a misentry of meters
WQPdata_joined %>% 
  group_by(ActivityDepthHeightMeasure.MeasureUnitCode) %>% 
  summarize(n = n())

WQPdata_joined %>% 
  group_by(ActivityIdentifier == "MNPCA-69-0498-00-201.1509220948.130F" ) %>% 
  summarize(n = n())

WQPdata_joined %>% 
  filter(ActivityDepthHeightMeasure.MeasureUnitCode == "mm")


# USE case when
#Prepare for joining of the Minnesota Data
WQPdata_joined %>% 
  mutate(Depth_meters = case_when(ActivityDepthHeightMeasure.MeasureUnitCode == "ft" | ActivityDepthHeightMeasure.MeasureUnitCode == "feet" ~ round(ActivityDepthHeightMeasure.MeasureValue * 0.3038 , digits = 2),
                           ActivityDepthHeightMeasure.MeasureUnitCode == "m" | ActivityDepthHeightMeasure.MeasureUnitCode == "meters" |
                             ActivityDepthHeightMeasure.MeasureUnitCode == "mm" ~ round(ActivityDepthHeightMeasure.MeasureValue, digits = 2) # this case includes a unit "mm" that was deemed a misentry of "m"
  )) %>% 
  {WQPdata_joined <<- . }

#Separate Temperature and DO
setDT(WQPdata_joined)
WQPdata_joined[ , .N , .(CharacteristicName, ResultMeasure.MeasureUnitCode) ]

WQPdata_joined %>% 
  mutate(ResultMeasureValue = case_when(
    CharacteristicName == "Temperature, water" & ResultMeasure.MeasureUnitCode == "deg F" ~ (ResultMeasureValue - 32) * 5/9 , 
    CharacteristicName == "Temperature, water" & ResultMeasure.MeasureUnitCode == "deg C" ~ ResultMeasureValue,
    CharacteristicName == "Dissolved oxygen (DO)" ~ ResultMeasureValue,
    .default = ResultMeasureValue
  ))%>% 
  mutate(ResultMeasure.MeasureUnitCode = case_when(
    CharacteristicName == "Temperature, water" & ResultMeasure.MeasureUnitCode == "deg F" ~ "deg C", 
    CharacteristicName == "Dissolved oxygen (DO)" ~ "mg/l",
    .default = ResultMeasure.MeasureUnitCode
  )) %>% 
  mutate(Latitude = LatitudeMeasure                                    ,
         Longitude = LongitudeMeasure                                   ,
         Max_Depth = lake_maxdepth_m                                   ,
         Date = ActivityStartDate                                      ,
         DOY = lubridate::yday(ActivityStartDate),
         Year = lubridate::year(ActivityStartDate))%>% 
  {WQPdata_joined <<- . }

#move up the cols we need:
glimpse(WQPdata_joined)

WQPdata_joined[ , .N , .(is.na(lagoslakeid), is.na(DOW))]

cols = as.character(expression(lagoslakeid, lake_namegnis, DOW, 
                               OrganizationIdentifier, OrganizationFormalName, 
                               Year, DOY, Date,
                               Max_Depth, GR,
                               MonitoringLocationIdentifier,Latitude, Longitude, Depth_meters,
                               ResultMeasureValue, ResultMeasure.MeasureUnitCode))

setcolorder(WQPdata_joined, cols)

                        
WQPdata_joined[ (ResultMeasure.MeasureUnitCode == "mg/l" & CharacteristicName == "Dissolved oxygen saturation") , .N, lagoslakeid]
WQPdata_joined[ (ResultMeasure.MeasureUnitCode == "mg/l" & CharacteristicName == "Dissolved oxygen saturation") , summary(ResultMeasureValue), lagoslakeid ] # these are indeed %sat vals
WQPdata_joined[ (ResultMeasure.MeasureUnitCode == "mg/l" & CharacteristicName == "Dissolved oxygen saturation") , ResultMeasure.MeasureUnitCode := "%" ]

# funky temp records
WQPdata_joined[ (!ResultMeasure.MeasureUnitCode == "deg C" |is.na(ResultMeasure.MeasureUnitCode)) & CharacteristicName == "Temperature, water" , .N, lagoslakeid]
WQPdata_joined[ (!ResultMeasure.MeasureUnitCode == "deg C" |is.na(ResultMeasure.MeasureUnitCode)) & CharacteristicName == "Temperature, water" ,  summary(ResultMeasureValue), lagoslakeid]
WQPdata_joined[ (ResultMeasure.MeasureUnitCode == "mg/l" ) & CharacteristicName == "Temperature, water" , ResultMeasure.MeasureUnitCode := "deg C" ]

#check our work:
WQPdata_joined[ , .N , .(CharacteristicName, ResultMeasure.MeasureUnitCode) ]



# *delete duplicates and consolidate data ---------------------------------



#here we can see that this dataset contains many duplicated records, especially when squeezed to the parameters of interest to us (see also line 292)
sum(duplicated(WQPdata_joined))
WQPdata_joined[ , .N , . (lagoslakeid, lake_namegnis, DOW, Latitude, Longitude,
                        OrganizationIdentifier, OrganizationFormalName, 
                        Year, DOY, Date,
                        MonitoringLocationIdentifier, Max_Depth, GR,
                        Depth_meters, 
                        CharacteristicName, ResultMeasureValue, ResultMeasure.MeasureUnitCode
                    )][ , .N , N]


WQPdata_joined_thinned <- WQPdata_joined[ (ResultMeasure.MeasureUnitCode == "deg C" & CharacteristicName == "Temperature, water") |
                                           (ResultMeasure.MeasureUnitCode == "mg/l" & CharacteristicName == "Dissolved oxygen (DO)")
                                          & !is.na(Depth_meters), 
                                          . (lagoslakeid, lake_namegnis, DOW, Latitude, Longitude,
                                             OrganizationIdentifier, OrganizationFormalName, 
                                             Year, DOY, Date,
                                             MonitoringLocationIdentifier, Max_Depth, GR,
                                             Depth_meters, 
                                             CharacteristicName,ResultMeasureValue, ResultMeasure.MeasureUnitCode)][!duplicated(
                                               WQPdata_joined[  (ResultMeasure.MeasureUnitCode == "deg C" & CharacteristicName == "Temperature, water") |
                                                                  (ResultMeasure.MeasureUnitCode == "mg/l" & CharacteristicName == "Dissolved oxygen (DO)")
                                                                & !is.na(Depth_meters) , 
                                                               . (lagoslakeid, lake_namegnis, DOW, Latitude, Longitude,
                                                                  OrganizationIdentifier, OrganizationFormalName, 
                                                                  Year, DOY, Date,
                                                                  MonitoringLocationIdentifier, Max_Depth, GR,
                                                                  Depth_meters, 
                                                                  CharacteristicName, ResultMeasureValue, ResultMeasure.MeasureUnitCode)]) , , ]

#check our work:
WQPdata_joined_thinned[ , .N , .(CharacteristicName, ResultMeasure.MeasureUnitCode) ]

#cast this wide
WQPdata_joined_thinned_wide  <- dcast( WQPdata_joined_thinned , 
            lagoslakeid + lake_namegnis + DOW + Latitude + Longitude +
              OrganizationIdentifier + OrganizationFormalName +
              Year + DOY + Date +
              MonitoringLocationIdentifier + Max_Depth + GR +
              Depth_meters ~ CharacteristicName,
            value.var = list("ResultMeasureValue","ResultMeasure.MeasureUnitCode"), 
            fun.aggregate = list(mean, last),
            fill = NA)

setnames(WQPdata_joined_thinned_wide, old = c("ResultMeasureValue_mean_Dissolved oxygen (DO)","ResultMeasureValue_mean_Temperature, water"), 
         new = c("do_mgperl", "watertemp_celcius"))
WQPdata_joined_thinned_wide[ , ':=' ("ResultMeasure.MeasureUnitCode_last_Dissolved oxygen (DO)" = NULL, "ResultMeasure.MeasureUnitCode_last_Temperature, water" = NULL), ] 


WQPdata_joined_thinned_wide[ , .N , is.na(GR) ]
WQPdata_joined_thinned_wide[ , .N , .(hastemps = !is.na(watertemp_celcius), hasDO = !is.na(do_mgperl))]
WQPdata_joined_thinned_wide [ , .N , .(lagoslakeid, DOW) ] 
WQPdata_joined_thinned_wide [!is.na(GR) & !is.na(watertemp_celcius) & !is.na(do_mgperl) & !is.na(Depth_meters)  , .N , .(lagoslakeid, DOW, MonitoringLocationIdentifier, Year, DOY) ] 

#keep only complete records eventually



# 
# #Summary of Data #588732 obs
# MN.WQP.join %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #2,438 MLI
# MN.WQP.join %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #1,220 Lakes
# MN.WQP.join %>%
#   group_by(Year)%>%
#   summarise(n = n()) #71 Years
# MN.WQP.join %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #7,300 Lake-Years
# MN.WQP.join %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #53,616 Profiles
# 

#Clean up
rm(WQPdata_joined, WQPdata_joined_thinned)
gc()

# Add Geom Ratio to MPCA data ------------------------------------------------


#  MNPCA
MN_sum %>%
  mutate(DOW = as.factor(DOW),###converts ID to factor, not character
         Z_m = Max_Depth_Ft*0.3048,
         area_km = Lake_Area_Acres*0.00404686,
         GR = (area_km^0.25)/Z_m)%>%###look at geometry ratio of lakes to filter out unstratified systems
  ###stefan et al 1996 GR=(A^0.25)/Zmax #(Gorham and Boyce, 1989) is one of the first paper to use Geometric Ratio
  # filter(GR < 4) %>% ###filter out lakes with GR>4 
  pivot_longer("1945":"2019",names_to = "Yr", #pivot longer
               values_to = "n_profile")%>%
  mutate(Year = as.numeric(Yr),
         DOW_yr = as.character(paste(DOW,"_",Year))) %>% 
  
#  Make a data frame of max depths for metadata [WTF is this?]
MN.sum.depths = MN.sum.long%>%
  distinct(DOW, .keep_all = T)%>%
  mutate(DOW2 = DOW,
         Max_Depth = Z_m)%>%
  select(c("DOW2", "Max_Depth"))
# Note: this command uses bad DOW var extensively -------------------------

###  pull lakes from profile dataset
MN_df2 = MN_df %>%
  mutate(DOW2 = as.factor(substring(DOW_DATE_AGENCY_PROFID,1,6)),###make DOW with leading 0s
         Year = year(mdy(DATE)),
         DOW_yr = as.character(paste(DOW2,"_",Year))) %>%
  # filter(DOW_yr %in% MN.sum.long$DOW_yr) %>% #Removes DOW that had a GR greater than 4
  inner_join(MN.station, by = "DOW") %>% #Adding a metadata file with Lat and Lon
  inner_join(MN.sum.depths, by = "DOW2")%>% #Adding max depth metadata
  mutate(Date = mdy(DATE),
         Depth = round(DEPTH_M),
         Temperature = TEMP_C,
         DO = DO_PPM,
         Latitude = LatitudeMeasure,
         Longitude = LongitudeMeasure,
         DOY = yday(Date)) %>%
  select(MonitoringLocationIdentifier, Year, Date, DOY,
         Depth, DO, Latitude, Longitude, Max_Depth, Temperature, DOW)

# #  Summary of Data


# Note: summary misaligned with code -------------------------------------------------------------------
# ON Mike's Machine, these summaries are not in line with the commented values:


MN_df2 %>% #359734 obs
  group_by(MonitoringLocationIdentifier)%>%
  summarise(n = n()) #7,256 Sites
MN_df2 %>%
  group_by(DOW)%>%
  summarise(n = n()) #2,203 Lakes
MN_df2 %>%
  group_by(Year)%>%
  summarise(n = n()) #70 Years
MN_df2 %>%
  group_by(DOW, Year)%>%
  summarise(n = n()) #20,780 Lake-Years
MN_df2 %>%
  group_by(MonitoringLocationIdentifier, Year, Date, DOY)%>%
  summarise(n = n()) #77,536 Profiles


#Clean
rm(MN_DF, MN_sum, MN.station, MN.sum.depths, MN.sum.long)
gc()



# merge WQP and MNPCA profile data now cleaned ----------------------------



####Join the Complete Observations Together

MN.data.forjoin = MN.WQP.join %>% 
  full_join(MN_df2)%>%
  group_by(MonitoringLocationIdentifier, Date, Depth, DOW)%>% #Checking for any multiple observations in the data and removing them
  summarise(DO = median(DO),
            Temperature = median(Temperature),
            Latitude = mean(Latitude),
            Longitude = mean(Longitude),
            Max_Depth = round(median(Max_Depth)))%>%
  mutate(Year = year(Date),
         DOY = yday(Date))%>%
  ungroup()

# HMR Green Lake add ------------------------------------------------------


#  Heidi has some additional data for Green Lake in Kandiyohi County
Green.Lake.fromH = read_csv("DOTEMP_Green.csv")%>%
  mutate(Agency = ifelse(Source == "DNR", "MNDNR", "MNPCA"),
         MonitoringLocationIdentifier = paste(Agency, "34-0079-00", Station, sep = "-"),
         DOW = "34007900",
         Latitude = 45.2521,
         Longitude = -94.9044,
         Date = mdy(Date),
         DOY = yday(Date),
         Max_Depth = 34,
         Depth = floor(`Depth (M)`),
         Temperature = `Temp (C)`,
         DO = `DO (ppm)`)%>%
  select(MonitoringLocationIdentifier ,
         Date                         ,
         Depth                        ,
         DOW                          ,
         DO                           ,
         Temperature                  ,
         Latitude                     ,
         Longitude                    ,
         Max_Depth                    ,
         Year                         ,
         DOY                          )


GreenLake = MN.data.forjoin %>%
  filter(DOW == "34007900")
#Remove duplicates
GreenLake.fromH.Profiles = Green.Lake.fromH %>%
  mutate(Profiles = paste(MonitoringLocationIdentifier, Year, DOY, sep = "/"))%>%
  distinct(Profiles)

GreenLake.DuplicateProfiles = GreenLake %>%
  ungroup()%>%
  mutate(Profiles = paste(MonitoringLocationIdentifier, Year, DOY, sep = "/"))%>%
  distinct(Profiles)%>%
  inner_join(GreenLake.fromH.Profiles)

GreenLake.forjoin = Green.Lake.fromH %>%
  mutate(Profiles = paste(MonitoringLocationIdentifier, Year, DOY, sep = "/"))%>%
  anti_join(GreenLake.DuplicateProfiles)%>%
  select(!Profiles)%>%
  group_by(MonitoringLocationIdentifier,Date,Depth,DOW,Latitude,
           Longitude,Max_Depth,Year,DOY)%>%
  summarise(DO = median(DO),
            Temperature = median(Temperature))%>%
  ungroup()
#  Join with the rest of the data
MN.ALL.Data = MN.data.forjoin %>%
  bind_rows(GreenLake.forjoin)


# #Summary #848842 obs

# Note: -------------------------------------------------------------------

#these data summaries are again matching the munging run on Mike's computer

# MN.ALL.Data %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #7,480 Sample Locations
# MN.ALL.Data %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #2,444 Lakes
# MN.ALL.Data %>%
#   group_by(Year)%>%
#   summarise(n = n()) #76 Years
# MN.ALL.Data %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #21,496 Lake-Years
# MN.ALL.Data %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #84,915 Profiles


#write_csv(MN.ALL.Data, "MN.ALL.DATA.csv") 


#Cleaning
rm(MN.WQP.join, MN_df2, Green.Lake.fromH, GreenLake, GreenLake.DuplicateProfiles,
   GreenLake.forjoin, GreenLake.fromH.Profiles, MN.data.forjoin)
gc()


###### Filtering the Data in Preparation for Interpolation =================

#  The goal of this section is to filter the data by several factors:
#  Lakes must have a profile at the start of stratification (121 <= DOY <= 166)
#  Lakes must have a profile at the end of stratification (196 <= DOY <= 258)
#  Profiles must be taken from glacial lakes
#  Profiles will be interpolated at 1m depths 
#  Data points where DO < 2 mg/L will be discarded due to non-linearity (there are some citations for this, insert them here. ) 
#  Run time is about 3.5 minutes

MN.ALL.Data = read_csv("MN.ALL.DATA.csv") 






# filter: temps< 40 C -----------------------------------------------------



#Filtering
filter1.mn <- MN.ALL.Data %>% #Remove impossible values ~ These likely came from input error
  mutate(Location_Year = as.character(paste(MonitoringLocationIdentifier,Year, sep = "/")), #add a loc-yr var
         Profile = paste(MonitoringLocationIdentifier, Year, DOY, sep = "/")) %>% #add a loc-yr-day variable
  filter(Temperature < 40) #Jane et al 2021

#  Summary of filter #843450 obs

# Note: -------------------------------------------------------------------

#summaries not jiving with mikes run on his machine. 

filter1.mn %>%
  group_by(MonitoringLocationIdentifier)%>%
  summarise(n = n()) #7,217 Sample Sites
filter1.mn %>%
  group_by(DOW)%>%
  summarise(n = n()) # 2,440 Lakes
filter1.mn %>%
  group_by(Year)%>%
  summarise(n = n()) #76 Years
filter1.mn %>%
  group_by(DOW, Year)%>%
  summarise(n = n()) #20,676 Lake-Years
filter1.mn %>%
  group_by(MonitoringLocationIdentifier, Year, DOY)%>%
  summarise(n = n()) #83,011 Profiles

filter2.mn = filter1.mn %>%            #Profiles must have a minimum depth less than 3 meters; 
  group_by(Profile)%>%                 #otherwise the profiles shallowest read could be in 
  summarise(min.depth = min(Depth))%>% #the metalimnion and capture none of the epilimnion
  filter(min.depth < 3)                #Jane et al 2021

filter3.mn = filter1.mn %>%
  semi_join(filter2.mn, by = "Profile") 


# Note: -------------------------------------------------------------------

#these summaries are not jiving. (first one off by 5)

# # Summary #842671 obs


filter3.mn %>%
  group_by(MonitoringLocationIdentifier)%>%
  summarise(n = n()) #7,196 Sample Sites
filter3.mn %>%
  group_by(DOW)%>%
  summarise(n = n()) #2,440 Lakes
filter3.mn %>%
  group_by(Year)%>%
  summarise(n = n()) #74 Years
filter3.mn %>%
  group_by(DOW, Year)%>%
  summarise(n = n()) #20,639 Lake-Years
filter3.mn %>%
  group_by(MonitoringLocationIdentifier, Year, DOY)%>%
  summarise(n = n()) #82,684 Profiles


# Note: -------------------------------------------------------------------

#  Why would we require the last profile be captured late summer (July 15 - Sep 15)?


#  We want to make sure that lakes were sampled in both the spring post stratification
#  and later in the summer. This ensures that we can calculate VHOD
filter4.mn = filter3.mn %>%
  filter(DOY >= 121 &
           DOY <= 258)%>%
  group_by(Year, MonitoringLocationIdentifier)%>% 
  summarise(Early=min(DOY),Late=max(DOY), N=n_distinct(DOY))%>% 
  filter(N>1)%>%
  filter(Early >= 121 & #The early sample must be taken between DOY 121 & 166 (May 1 - June 15)
           Early <= 166 &
           Late >= 196 & #and this adds that the last sample was taken between DOY 196 and 258 (July 15 - Sep 15)
           Late <= 258)%>%
  mutate(Location_Year = as.character(paste(MonitoringLocationIdentifier,Year, sep = "/"))) #11,289 obs of 6 variables (Lost 16917 Lake-Year Combos)

#  Finally, we put it all together
filter5.mn = filter3.mn %>%
  semi_join(filter4.mn, by = "Location_Year") %>% 
  filter(DOY >= 121 &
           DOY <= 258) #Removes profiles where it may have been sampled while ice is on or during turnover


# Note:  ------------------------------------------------------------------

# these numbers are not jiving with my current

# Summary #495659 obs
filter5.mn %>%
  group_by(MonitoringLocationIdentifier)%>%
  summarise(n = n()) #2,304 Site
filter5.mn %>%
  group_by(DOW)%>%
  summarise(n = n()) #1,332 DOW
filter5.mn %>%
  group_by(Year)%>%
  summarise(n = n()) #49 Years
filter5.mn %>%
  group_by(DOW, Year)%>%
  summarise(n = n()) #5,744 Lake-Years
filter5.mn %>%
  group_by(MonitoringLocationIdentifier, Year, DOY)%>%
  summarise(n = n()) #46,255 Profiles

# Remove Lake Superior Sites, Mines, and Pits ~ We only want Glacial Lakes
filter6.mn = filter5.mn %>%
  filter(MonitoringLocationIdentifier != "EPA_GLNPO-SU19") %>% #Lake Superior Site
  filter(MonitoringLocationIdentifier != "EPA_GLNPO-SU18") %>% #Lake Superior Site
  filter(MonitoringLocationIdentifier != "EPA_GLNPO-SU104550")%>% #Lake Superior Site
  filter(MonitoringLocationIdentifier != "21GPBCH-LS_PT_1") %>% #Lake Superior Site
  filter(MonitoringLocationIdentifier != "21GPBCH-LS_PT_9") %>% #Lake Superior Site
  filter(MonitoringLocationIdentifier != "GPC5_WQX-LS_PT_1") %>% #Lake Superior Site
  filter(MonitoringLocationIdentifier != "GPC5_WQX-LS_PT_9") %>% #Lake Superior Site
  filter(MonitoringLocationIdentifier != "GPC5_WQX-LS_PT_8") %>% #Lake Superior Site
  filter(MonitoringLocationIdentifier != "MNPCA-69-1297-00-202") %>% #Sherman Pit
  filter(MonitoringLocationIdentifier != "MNPCA-69-0429-00-201") %>% #Sabin Pit
  filter(MonitoringLocationIdentifier != "21GPBCH-LS_PT_8") %>%      #Lake Superior Site
  filter(MonitoringLocationIdentifier != "MNPCA-18-0440-01-100") %>% #Mahnomen Mine
  filter(MonitoringLocationIdentifier != "MNPCA-69-1297-00-201") %>% #Sherman Pit
  filter(MonitoringLocationIdentifier != "MNPCA-18-0439-00-100") %>% #Pennington Mine
  filter(MonitoringLocationIdentifier != "MNPCA-31-1325-04-201") %>% #Canisteo Mine Pit
  filter(MonitoringLocationIdentifier != "MNPCA-69-1298-00-203") %>% #South Twin City Pit
  filter(MonitoringLocationIdentifier != "MNPCA-18-0093-01-202") %>% #Rabbit Lake
  filter(MonitoringLocationIdentifier != "MNPCA-18-0041-01-201") %>% #Crooked Lake - Silver Bay
  filter(MonitoringLocationIdentifier != "MNPCA-69-0429-00-100") %>% #Embarrass Mine
  filter(MonitoringLocationIdentifier != "MNPCA-69-0428-00-201") %>% #St. James Pit
  filter(DOW != "18044001")%>% #Mahnomen Mine
  filter(DOW != "69129700")%>% #Sherman Pit
  filter(DOW != "18043900")%>% #Pennington Mine
  filter(DOW != "31132504")%>% #Canisteo Mine Pit
  filter(DOW != "69129800")%>% #South Twin City Pit
  filter(DOW != "18009301")%>% #Rabbit Lake
  filter(DOW != "18004101")%>% #Crooked Lake - Silver Bay
  filter(DOW != "69042900")%>% #Embarrass Mine
  filter(DOW != "69042800")%>% #St. James Pit
  filter(DOW != "25000100") #Lake Pepin
# # Summary #492712 obs
# filter6.mn %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #2,293 Sample Site
# filter6.mn %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #1,327 Lakes
# filter6.mn %>%
#   group_by(Year)%>%
#   summarise(n = n()) #49 Years
# filter6.mn %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #5,733 Lake-Years
# filter6.mn %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #45,918 Profiles

filter7.1mn = filter6.mn %>%                #Profiles must have at least 3 reads in them
  group_by(Profile)%>%
  summarise(n_depths = n_distinct(Depth))%>%
  filter(n_depths >= 3) #Jane et al 2021

filter7.mn = filter6.mn %>%
  semi_join(filter7.1mn, by = "Profile")

# #  Summary #485927 obs
# filter7.mn %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #2,119 Lakes
# filter7.mn %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #1,263 Lakes
# filter7.mn %>%
#   group_by(Year)%>%
#   summarise(n = n()) #48 Years
# filter7.mn %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #5,159 Lake-Years
# filter7.mn %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #40,245 Profiles

# Calculate the top and bottom of the metalimnion and remove profiles where it did not work
filter8.mn = filter7.mn %>%
  group_by(Profile)%>%
  mutate(Meta = list(meta.depths(wtr = Temperature, depths = Depth, slope = 0.1, seasonal = F)))%>%
  separate(Meta, into = c("top.meta", "top.hypo"), sep = ",")%>%
  mutate(top.meta = gsub("[^0-9.-]", "", top.meta),
         top.hypo = as.numeric(gsub("[^0-9.-]", "", top.hypo)),
         Depth = as.numeric(Depth))%>%
  drop_na(top.hypo)%>%  #Jane et al 2021
  filter(top.hypo != "0") 

# #Summary #463578 obs
# filter8.mn %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #2,072 Sample Sites
# filter8.mn %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #1,238 Lakes
# filter8.mn %>%
#   group_by(Year)%>%
#   summarise(n = n()) #48 Years
# filter8.mn %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #5,064 Lake-Years
# filter8.mn %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #36,722 Profiles

#Make a ratio across all lakes of the top of the hypolimnion to the max depth
filter9.1mn = filter8.mn %>%
  group_by(Location_Year)%>%
  filter(DOY == max(DOY))%>%
  distinct(DOY, .keep_all = T)%>%
  mutate(ratio = top.hypo/Max_Depth)%>%
  filter(ratio < 1)    #Remove profiles where the ratio is greater than one (Also would mean that the meta depths function didn't work)
#Extract the median of these ratios
ratio = skim(filter9.1mn$ratio)%>%
  pull(numeric.p50)

filter9.mn = filter8.mn %>%
  semi_join(filter9.1mn, by = "Location_Year")


# #Summary #444057 obs
# filter9.mn %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #1,986 Sample Sites
# filter9.mn %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #1,177 Lakes
# filter9.mn %>%
#   group_by(Year)%>%
#   summarise(n = n()) #48 Years
# filter9.mn %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #4,703 Lake-Years
# filter9.mn %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #34,448 Profiles

#See that each lake has a profile depth in the median hypolimnion
filter10.mn = filter9.mn %>%
  group_by(Profile)%>%
  mutate(Max_Profile = max(Depth))%>%
  filter(Max_Profile > Max_Depth * (ratio)) 

# #Summary #390099 obs
# filter10.mn %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #1,708 Sample Sites
# filter10.mn %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #1,105 Lakes
# filter10.mn %>%
#   group_by(Year)%>%
#   summarise(n = n()) #48 Years
# filter10.mn %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #4,320 Lake-Years
# filter10.mn %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #28,820 Profiles


# note: how can this filter be run again AND o something? -----------------



# Check start and end date again after losing profiles
filter11.1mn = filter10.mn %>%
  filter(DOY >= 121 &
           DOY <= 258)%>%
  group_by(Year, MonitoringLocationIdentifier)%>% 
  summarise(Early=min(DOY),Late=max(DOY), N=n_distinct(DOY))%>% 
  filter(N>1)%>%
  filter(Early >= 121 & 
           Early <= 166 &
           Late >= 196 & 
           Late <= 258)%>%
  mutate(Location_Year = as.character(paste(MonitoringLocationIdentifier,Year, sep = "/")))
filter11.mn = filter10.mn %>%
  semi_join(filter11.1mn, by = "Location_Year")%>%
  mutate(ID = paste(MonitoringLocationIdentifier, Year, DOY, Max_Depth, sep = "/"))


# #Summary #376380 obs
# filter11.mn %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #1,459 Sample Sites
# filter11.mn %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #980 Lakes
# filter11.mn %>%
#   group_by(Year)%>%
#   summarise(n = n()) #48 Years
# filter11.mn %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #3,894 Lake-Years
# filter11.mn %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #27,582 Profiles

#  Filter for most sampled site at each lake
filter12.1mn = filter11.mn %>%
  group_by(DOW, MonitoringLocationIdentifier)%>%
  summarise(n = n())%>%
  group_by(DOW)%>%
  mutate(MostSampled = max(n))%>%
  ungroup()%>%
  filter(n == MostSampled)%>%
  distinct(DOW, .keep_all = T)
MN.Filtered.Obs = filter11.mn %>%
  semi_join(filter12.1mn)

# #  Summary #298027 obs
# MN.Filtered.Obs %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #980 Sample Sites
# MN.Filtered.Obs %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #980 Lakes
# MN.Filtered.Obs %>%
#   group_by(Year)%>%
#   summarise(n = n()) #48 Years
# MN.Filtered.Obs %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #3,591 Lake-Years
# MN.Filtered.Obs %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #22,060 Profiles




####  Vis check
# 
# MN.presample = filter11.mn %>%
#   mutate(SampleID = paste(MonitoringLocationIdentifier, Year, DOY, sep = "/"))%>%
#   distinct(SampleID)
# Random.sample = tibble(SampleID = sample(MN.presample$SampleID, size = 100))
# 
# filter11.mn%>%
#   mutate(SampleID = paste(MonitoringLocationIdentifier, Year, DOY, sep = "/"))%>%
#   semi_join(Random.sample)%>%
#   mutate(Depth = as.numeric(Depth),
#          Temperature = as.numeric(Temperature),
#          top.hypo = as.numeric(top.hypo),
#          top.meta = as.numeric(top.meta))%>%
#   ggplot(aes(x = Depth, y = Temperature, group = SampleID))+
#   geom_point()+
#   geom_line()+
#   geom_vline(aes(group = SampleID, xintercept = top.hypo), color = "blue")+
#   scale_x_reverse()+
#   coord_flip()+
#   labs(title = "Uses Ratio = 0.571")+
#   facet_wrap(~SampleID)

#### Filter check
# filtereffect = read_xlsx("filtereffects.xlsx")
# fe.obsperbegin=filtereffect %>%
#   ggplot(aes(x = FilterNo, y = ObsPerFromBegin, color = Filter.Description, id = No.ofObs))+
#   geom_point()+
#   theme(legend.position = "none")+
#   ylab("Percent of Observations From Beginning")
# ggplotly(fe.obsperbegin)
# 
# fe.ObsPerFromPrevious = filtereffect %>%
#   ggplot(aes(x = FilterNo, y = ObsPerFromPrevious, color = Filter.Description, id = No.ofObs))+
#   geom_point()+
#   theme(legend.position = "none")+
#   ylab("Percent of Observations From Previous")
# ggplotly(fe.ObsPerFromPrevious)
# 
# fe.No.ofObs = filtereffect %>%
#   ggplot(aes(x = FilterNo, y = No.ofObs, id = Filter.Description))+
#   geom_point()+
#   geom_line()
# ggplotly(fe.No.ofObs)
# 
# fe.lakesperbegin=filtereffect %>%
#   ggplot(aes(x = FilterNo, y = LakesPerFromBegin, color = Filter.Description, id = Lakes))+
#   geom_point()+
#   theme(legend.position = "none")+
#   ylab("Percent of Lakes From Beginning")
# ggplotly(fe.lakesperbegin)
# 
# fe.profilesperbegin=filtereffect %>%
#   ggplot(aes(x = FilterNo, y = ProfilePerFromBegin, color = Filter.Description, id = Profiles))+
#   geom_point()+
#   theme(legend.position = "none")+
#   ylab("Percent of Profiles From Beginning")
# ggplotly(fe.profilesperbegin)

#Clean
rm(filter1.mn, filter2.mn, filter3.mn,filter4.mn,filter5.mn, filter6.mn,
   filter7.1mn, filter7.mn, filter8.mn, filter9.1mn, filter9.mn,
   filter10.mn, filter11.1mn, filter11.mn, filter12.1mn, ratio)
gc()



###### Interpolate the Data Frame ===============================================
#  In this section linear interpolation is preformed on DO and Temperature 
#  observations. Then, the interpolation are rejoined to the metadata before
#  being sent off to calculate AHOD and VHOD
#  Run time of about 2 minutes


### The next step is to interpolate data at 1 m increments
Depth.df = tibble(Depth = 0:90) #This creates a data frame to make sure we have data for the bottom of larger lakes
#  First DO
#  Pivot Wide
Wide.MN.DO.Obs = MN.Filtered.Obs %>% #There are more rows than profiles...
  ungroup()%>%
  mutate(ID = paste(MonitoringLocationIdentifier, Year, DOY, Max_Depth, sep = "/"))%>%
  select(ID, Depth, DO) %>%
  pivot_wider(names_from = ID, values_from = DO, values_fn = mean)%>%
  full_join(Depth.df)

#  Interpolate
Wide.Inter.MN.DO = na.approx(Wide.MN.DO.Obs, x = Wide.MN.DO.Obs$Depth)%>% #interpolates data
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()

#  Pivot Long
Long.Inter.MN.DO = Wide.Inter.MN.DO %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "DO", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  mutate(Max_Depth = as.numeric(Max_Depth))%>%
  filter(Depth < Max_Depth) # Makes sure that filled in values are less than the max depth of the lake


### Next is Temperature
#  Pivot Wide
Wide.MN.Temp.Obs = MN.Filtered.Obs %>%
  ungroup()%>%
  mutate(ID = paste(MonitoringLocationIdentifier, Year, DOY, Max_Depth, sep = "/"))%>%
  select(ID, Depth, Temperature) %>%
  pivot_wider(names_from = ID, values_from = Temperature, values_fn = mean)%>%
  full_join(Depth.df)

#  Interpolate
Wide.Inter.MN.Temp = na.approx(Wide.MN.Temp.Obs, x = Wide.MN.Temp.Obs$Depth)%>% #interpolates data - 75 was chosen to make sure every lake was fully interpolated
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()

#  Pivot Long
Long.Inter.MN.Temp = Wide.Inter.MN.Temp %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "Temperature", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  mutate(Max_Depth = as.numeric(Max_Depth))%>%
  filter(Depth < Max_Depth) # Makes sure that filled in values are less than the max depth of the lake

### Join our results together
MN.joined.inter = inner_join(Long.Inter.MN.Temp, Long.Inter.MN.DO)

#  We need to get the Lat and Lon
MN.Depth.Specific = MN.Filtered.Obs %>%
  ungroup()%>%
  distinct(MonitoringLocationIdentifier, .keep_all = T)%>%
  select(c(MonitoringLocationIdentifier, Latitude, Longitude, DOW))%>%
  inner_join(MN.joined.inter)%>%
  mutate(DOY = as.numeric(DOY))%>%
  group_by(MonitoringLocationIdentifier, Year, DOY)%>%
  distinct(Depth, .keep_all = T)



# thermofilter? ------------------------------------------------------------


###  Next we will thermofilter the data set

MN.DS.Thermo = MN.Depth.Specific %>%
  group_by(MonitoringLocationIdentifier, Year, DOY)%>%
  mutate(Meta = list(meta.depths(wtr = Temperature, depths = Depth, slope = 0.1, seasonal = F)))%>% #Following the methods of Jane et al, 2021
  separate(Meta, into = c("top.meta", "top.hypo"), sep = ",")%>%
  mutate(top.meta = gsub("[^0-9.-]", "", top.meta),
         top.hypo = as.numeric(gsub("[^0-9.-]", "", top.hypo)),
         Depth = as.numeric(Depth))



#write file for UMD students to use
#MN.DS.Thermo<-ungroup(MN.DS.Thermo)
#write.csv(MN.DS.Thermo, "MNprofiles.csv")


# MN.DS.Thermo<-read.csv("MNprofiles.csv")
#see if how many lakes we have trend data for
#MN.DS.Thermo$Year<-as.integer(MN.DS.Thermo$Year)
#MN.DS.Thermo$DOW<-as.factor(MN.DS.Thermo$DOW)
#df1<-MN.DS.Thermo%>%filter(DOY>196 & DOY<248)%>%
 # group_by(Year, DOW)%>%
 # summarise(min=min(DOY),max=max(DOY))%>%
 # group_by(DOW)%>%
##  summarise(min=min(Year),max=max(Year),range=max-min)%>%
 # filter(range>9)
#  VIS Check of the porfiles remaining
# MN.presample = MN.DS.Thermo %>%
#   mutate(SampleID = paste(MonitoringLocationIdentifier, Year, DOY, sep = "/"))%>%
#   distinct(SampleID)
# Random.sample = tibble(SampleID = sample(MN.presample$SampleID, size = 100))
#   
# MN.DS.Thermo%>%
#   mutate(SampleID = paste(MonitoringLocationIdentifier, Year, DOY, sep = "/"))%>%
#   semi_join(Random.sample)%>%
#   mutate(Depth = as.numeric(Depth),
#          Temperature = as.numeric(Temperature),
#          top.hypo = as.numeric(top.hypo),
#          top.meta = as.numeric(top.meta))%>%
#   ggplot(aes(x = Depth, y = Temperature, group = SampleID))+
#   geom_point()+
#   geom_line()+
#   geom_vline(aes(group = SampleID, xintercept = top.hypo), color = "blue")+
#   scale_x_reverse()+
#   coord_flip()+
#   facet_wrap(~SampleID)



#  Data Summary #343391
# MN.DS.Thermo %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n=n()) #980 Sample Site
# MN.DS.Thermo %>%
#   group_by(DOW)%>%
#   summarise(n=n()) #980 Lakes
# MN.DS.Thermo %>%
#   group_by(Year)%>%
#   summarise(n=n()) #48 Years
# MN.DS.Thermo %>%
#   group_by(DOW, Year)%>%
#   summarise(n=n()) #3,591 Lake-Years
# MN.DS.Thermo %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n=n()) #22,058 Profiles

#Clean up 
rm(MN.Filtered.Obs,MN.joined.inter,Long.Inter.MN.Temp,Wide.Inter.MN.Temp,
   Wide.MN.Temp.Obs,Long.Inter.MN.DO,Wide.Inter.MN.DO,Wide.MN.DO.Obs, Depth.df)
gc()


# filter: hypolimnion data only -------------------------------------------

### Select for just the hypolimnion of lakes

# #Using a lake-year specific ratio
MN.DS.Hypo.prefilter.1 = MN.DS.Thermo %>%
  group_by(DOW,Year)%>%
  mutate(ratio = top.hypo/Max_Depth)%>%
  ungroup()

#Extract the median of these ratios and filter using them
MN.DS.Hypo.prefilter = MN.DS.Hypo.prefilter.1%>%
  group_by(DOW, Year)%>%
  summarise(ratio = median(ratio))%>%
  ungroup()%>%
  inner_join(MN.DS.Thermo)%>%
  mutate(hypo = ratio * Max_Depth)%>%
  filter(Depth > hypo)%>%
  mutate(Location_Year_Depth = as.character(paste(MonitoringLocationIdentifier,Year,Depth, sep = "/")))



# warning: if the date filter is still cutting stuff, something is --------

# Re-do the date and DO filter just to make sure
MN.DS.Hypo.filter = MN.DS.Hypo.prefilter %>%
  filter(DO >= 2)%>%
  filter(DOY >= 121 &
           DOY <= 258)%>%
  group_by(Year, MonitoringLocationIdentifier, Depth)%>% 
  summarise(Early=min(DOY),Late=max(DOY), N=n_distinct(DOY))%>% 
  filter(N>1)%>%
  filter(Early >= 121 & 
           Early <= 166 &
           Late >= 196 & #and this adds that the last sample was taken between DOY 196 and 258
           Late <= 258)%>%
  mutate(Location_Year_Depth = as.character(paste(MonitoringLocationIdentifier,Year,Depth, sep = "/"))) #11,289 obs of 6 variables (Lost 16917 Lake-Year Combos)

####explore the 2 ppm filter


# Note: HR Add ------------------------------------------------------------



MN.DS.Hypo.filter2 = MN.DS.Hypo.prefilter %>%
  filter(DO >= 0)%>%
  filter(DOY >= 121 &
           DOY <= 258)%>%
  group_by(Year, MonitoringLocationIdentifier, Depth)%>% 
  summarise(Early=min(DOY),Late=max(DOY), N=n_distinct(DOY))%>% 
  filter(N>1)%>%
  filter(Early >= 121 & 
           Early <= 166 &
           Late >= 196 & #and this adds that the last sample was taken between DOY 196 and 258
           Late <= 258)%>%
  mutate(Location_Year_Depth = as.character(paste(MonitoringLocationIdentifier,Year,Depth, sep = "/"))) #11,289 obs of 6 variables (Lost 16917 Lake-Year Combos)


#  Finally, we put it all together
MN.DS.Hypo = MN.DS.Hypo.prefilter %>%
  filter(DO >= 2)%>%
  semi_join(MN.DS.Hypo.filter, by = "Location_Year_Depth") %>% 
  filter(DOY >= 121 &
           DOY <= 258)
###explore 2 ppm filter
MN.DS.Hypo2 = MN.DS.Hypo.prefilter %>%
  filter(DO >= 0)%>%
  semi_join(MN.DS.Hypo.filter, by = "Location_Year_Depth") %>% 
  filter(DOY >= 121 &
           DOY <= 258)

# #  Data Summary # 33035 obs
# MN.DS.Hypo %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n=n()) #424 Sites
# MN.DS.Hypo %>%
#   group_by(DOW)%>%
#   summarise(n=n()) #424 Lakes
# MN.DS.Hypo %>%
#   group_by(Year)%>%
#   summarise(n=n()) #47 Years
# MN.DS.Hypo %>%
#   group_by(DOW, Year)%>%
#   summarise(n=n()) #868 Lake-Years
# MN.DS.Hypo %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n=n()) #3,771 Profiles


#Clean
rm(MN.Depth.Specific, Random.sample, MN.presample, MN.DS.Hypo.filter, MN.DS.Hypo.prefilter)
gc()

###### Calculating AHOD and VHOD ===============================================

#  Here we will first calculate AHOD, 
#  Then using hyspography data we will calculate VHOD
#  Equations follow Livingstone and Imboden 1996
#  Run time of about 2 minutes (Haven't calculated with bathymetry data)


####  Some of the lakes have strange turnovers, so we will first filter them out
### Calculating DS Oxygen Depletion
### Crafting a Linear Model
#  Reworking an ID column
MN.AHOD = MN.DS.Hypo %>%
  filter(DO >= 2)%>%
  select(!ID)%>%
  mutate(ID = paste(Year, Depth, DOW, sep = "/"))

#code to understand the role of the 2 ppm filter
MN.AHOD2<-MN.DS.Hypo2 %>%
  select(!ID)%>%
  mutate(ID = paste(Year, Depth, DOW, sep = "/"))
###look at slopes in individual lakes
#  Create a Linear Model
MN.DO.LM = with(MN.AHOD,
                by(MN.AHOD, INDICES = ID, #The indices allows for it to broken up by lake, year, and depth
                   function(x) lm(DO~DOY, data = x)))
MN.DO.LM2 = with(MN.AHOD2,
                by(MN.AHOD2, INDICES = ID, #The indices allows for it to broken up by lake, year, and depth
                   function(x) lm(DO~DOY, data = x)))
#  Extract Coefficients
MN.do.coef = sapply(MN.DO.LM, coef)
MN.do.coef2 = sapply(MN.DO.LM2, coef)
#  Transpose and tidy it up
MN.do.tod = cbind(as.character(colnames(MN.do.coef)),data.frame(t(MN.do.coef),row.names=NULL))
colnames(MN.do.tod) = c("ID","Intercept","TOD") #What does TOD stand for? Total oxygen depletion? I think this value is equivalient to AHOD
MN.do.tod2 = cbind(as.character(colnames(MN.do.coef2)),data.frame(t(MN.do.coef2),row.names=NULL))
colnames(MN.do.tod2) = c("ID","Intercept","TOD") #What does TOD stand for? Total oxygen depletion? I think this value is equivalient to AHOD

#LOOK AT all the slopes in a lake for a given year
lakes<-MN.AHOD%>%select(MonitoringLocationIdentifier,Year)%>%distinct()%>%
  slice_head(n=100)
MN.AHODx<-MN.AHOD%>%
  filter(MonitoringLocationIdentifier%in%lakes$MonitoringLocationIdentifier&Year%in%lakes$Year)
MN.AHODx$lake_yr<-paste(MN.AHODx$DOW,MN.AHODx$Year,sep = "-")
MN.AHODx$Depth<-as.factor(MN.AHODx$Depth)
ggplot(MN.AHODx, aes(DOY, DO, color=Depth))+
         geom_point(aes(color=Depth),show.legend = FALSE)+
  geom_line(show.legend = FALSE)+
         theme_classic(base_size = 12)+
         facet_wrap(.~lake_yr,ncol=10)


MN.AHODy<-MN.AHOD2%>%
  filter(MonitoringLocationIdentifier%in%lakes$MonitoringLocationIdentifier&Year%in%lakes$Year)
MN.AHODy$lake_yr<-paste(MN.AHODy$DOW,MN.AHODy$Year,sep = "-")
MN.AHODy$Depth<-as.factor(MN.AHODy$Depth)
ggplot(MN.AHODy, aes(DOY, DO, color=Depth))+
  geom_point(aes(color=Depth),show.legend = FALSE)+
  geom_line(show.legend = FALSE)+
  theme_classic(base_size = 12)+
  facet_wrap(.~lake_yr,ncol=10)

###let's look specifically at profiles that have values<2 ppm in them
lowDO<-MN.AHOD2%>%filter(DO%in%0:2)%>%select(Location_Year_Depth)%>%distinct()%>%
  slice_head(n=100)
lowDO2<-MN.AHOD2%>%
  filter(Location_Year_Depth%in%lowDO$Location_Year_Depth)%>%
  mutate(lake_yr=paste(DOW,Year, sep="_"), Depth=as.factor(Depth))
ggplot(lowDO2, aes(DOY, DO, color=Depth))+
  geom_point(aes(color=Depth),show.legend = FALSE)+
  geom_line(show.legend = FALSE)+
  theme_classic(base_size = 12)+
  geom_hline(yintercept=0, color="lightgray",linetype="dashed")+
  geom_hline(yintercept=2, color="lightgray",linetype="dashed")+
  facet_wrap(.~lake_yr,ncol=10)  

#Rantala note: TOD=total oxygen depletion. It is equal to sediment and aquatic OD
MN.do.tod = MN.do.tod %>%
  separate(ID, into = c("Year", "Depth", "DOW"), sep = "/")%>%
  drop_na(TOD)%>% #This nearly halves our data because it cannot calculate a regression line
  mutate(Depth = as.double(Depth))
MN.do.tod2 = MN.do.tod2 %>%
  separate(ID, into = c("Year", "Depth", "DOW"), sep = "/")%>%
  drop_na(TOD)%>% #This nearly halves our data because it cannot calculate a regression line
  mutate(Depth = as.double(Depth))
#summarize slopes within a lake-y
MN.do.todx<-MN.do.tod%>%mutate(DOW_yr=paste(DOW,Year, sep="_"))%>%
  group_by(DOW_yr)%>%
  summarise(aMax=max(TOD),aMin=min(TOD),aRange=(max(TOD)-min(TOD)))
hist(MN.do.todx$aRange)
hist(MN.do.todx$aMax)
ggplot(MN.DS.Hypo, aes(DO))+
  geom_histogram(color="steelblue",fill="white")+
  theme_classic(base_size = 12)
MN.do.tod2x<-MN.do.tod2%>%mutate(DOW_yr=paste(DOW,Year, sep="_"))%>%
  group_by(DOW_yr)%>%
  summarise(aMax=max(TOD),aMin=min(TOD),aRange=(max(TOD)-min(TOD)))
ggplot(MN.DS.Hypo2, aes(DO))+
  geom_histogram(color="steelblue",fill="white")+
  theme_classic(base_size = 12)
#Data Summary #6418 obs
# MN.do.tod%>%
#   group_by(DOW)%>%
#   summarise(n = n()) #424 Lakes
# MN.do.tod%>%
#   group_by(Year)%>%
#   summarise(n = n()) #47 Years
# MN.do.tod%>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #868 Lake-Year


### Add Bathymetry/Hypsography Data
#  load bathymetry data
MN.Bath = read_rds("MN_bathy.rds")%>%
  mutate(Depth = depths,
         area = areas)
# more.Bath = read.dbf("lake_bathymetric_contours.dbf")
#write.csv(MN.Bath,"MN.Bath.csv")
#Filter the Bath
MN.Bath.DOW = MN.Bath %>%
  mutate(Depth = floor(Depth))%>%
  group_by(Depth, DOW)%>%
  summarise(area = mean(area))%>%
  ungroup(Depth)

MN.Bath.DOW = MN.Bath.DOW[order(MN.Bath.DOW$DOW),]

#  Calculate Alpha value 
#Rantala note: alpha is the layer-specific scaling variable related to the amount of contact the layer has wiht the sediment
MN.Bath.DOW$DiffDepth<-0 #fill with dummy values so diff does not bomb
MN.Bath.DOW$DiffDepth[1:nrow(MN.Bath.DOW)-1]<-diff(MN.Bath.DOW$Depth)
MN.Bath.DOW$DiffArea <- 0
MN.Bath.DOW$DiffArea[1:nrow(MN.Bath.DOW)-1]<-diff(MN.Bath.DOW$area)
MN.Bath.DOW = MN.Bath.DOW %>%
  mutate(alpha = -1*(DiffArea/DiffDepth)/area)%>%
  filter(DiffDepth > 0)

#  Calculate Depth Specific Volume
MN.Bath.DOW = MN.Bath.DOW %>%
  group_by(DOW)%>%
  mutate(Previousarea = data.table :: shift(area, type = c("lead")),
         Vol = (DiffDepth/3)*(area + Previousarea+(area*Previousarea)^0.5),
         Previousarea = coalesce(Previousarea, 0),
         Vol = coalesce(Vol, 0))
  

#Join the Bathymetry with the TOD
OD <- na.omit(inner_join(MN.Bath.DOW,MN.do.tod))

Bath.Need = MN.do.tod %>%
  distinct(DOW)%>%
  anti_join(MN.Bath.DOW)%>%
  distinct(DOW)
#write.csv(Bath.Need, "MN.BathymetryNeed.csv")

# #  Summary # 5239 obs
# OD%>%
#   group_by(DOW)%>%
#   summarise(n = n()) #310 Lakes
# OD%>%
#   group_by(Year)%>%
#   summarise(n = n()) #47 Years
# OD%>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #680 Lake-Year


#  Add interpolated temperatures to the depth-specific data
MN.DS.Thermo.ag = MN.DS.Thermo %>%
  group_by(DOW, Year, Depth)%>%
  summarise(Temperature = mean(Temperature))
DS.MN<-inner_join(OD,MN.DS.Thermo.ag)
DS.MN$DOY_0<-(0-DS.MN$Intercept)/DS.MN$TOD #day of year when DO hits 0
DS.MN$DOY_3<-(3-DS.MN$Intercept)/DS.MN$TOD #day of year when DO hits 3


#  Calculate entire hypolimnion volume-weighted values 
VW_MN = DS.MN %>%
  group_by(DOW, Year)%>%
  summarise(VW_TOD = sum(TOD*Vol)/sum(Vol),
            VW_Temp = sum(Temperature*Vol)/sum(Vol),
            VW_Intercept = sum(Intercept*Vol)/sum(Vol))%>%
  mutate(VW_DOY_0 = (0-VW_Intercept)/VW_TOD,
         VW_DOY_3 = (3-VW_Intercept)/VW_TOD)


#  Add Lat and Long back into the data set
VW_MN.Meta = MN.DS.Thermo %>%
  ungroup()%>%
  distinct(DOW, .keep_all = T)%>%
  select(DOW, Latitude, Longitude)%>%
  inner_join(VW_MN)


#### Inspection for positive values
#  Many of the lake years with positive values appear to have turned over
#  earlier than normal for the year. We'll filter the last sample date out and
#  calculate VHOD with what is left
Positive.Vals = VW_MN.Meta %>%
  filter(VW_TOD > 0)

PosTOD=MN.AHOD %>%
  semi_join(Positive.Vals)%>%
  group_by(DOW, Year)%>%
  mutate(Profile = paste(DOW, Year, DOY))

MN.AHOD.Neg = MN.AHOD%>%
  group_by(DOW, Year)%>%
  mutate(MaxDOY = max(DOY),
         Profile = paste(DOW, Year, DOY))%>%
  ungroup()%>%
  anti_join(PosTOD)

MN.AHOD.Pos = MN.AHOD%>%
  group_by(DOW, Year)%>%
  mutate(MaxDOY = max(DOY),
         Profile = paste(DOW, Year, DOY))%>%
  semi_join(PosTOD)%>%
  filter(MaxDOY != DOY)%>%
  ungroup

MN.AHOD.Pos.Filter = MN.AHOD.Pos %>%
  group_by(DOW, Year)%>%
  summarise(n = n_distinct(DOY))%>%
  ungroup()%>%
  filter(n > 1)

MN.AHOD.Filtered = MN.AHOD.Pos %>%
  semi_join(MN.AHOD.Pos.Filter)%>%
  full_join(MN.AHOD.Neg)

#Clean
rm(MN.DO.LM, MN.do.tod, MN.AHOD, MN.DS.Thermo.ag, MN.DS.Hypo.prefilter.1, 
   MN.DS.Hypo, MN.do.coef, MN.Bath, MN.AHOD.Pos.Filter,
   MN.AHOD.Pos, MN.AHOD.Neg, Positive.Vals,
   PosTOD, VW_MN, VW_MN.Meta, OD, DS.MN, Bath.Need)
gc()


### Now we have removed the last day of season where turnover happened early
### Let's redo for the actual VHOD
#  Create a Linear Model
MN.DO.LM = with(MN.AHOD.Filtered,
                by(MN.AHOD.Filtered, INDICES = ID, #The indices allows for it to broken up by lake, year, and depth
                   function(x) lm(DO~DOY, data = x)))
#  Extract Coefficients
MN.do.coef = sapply(MN.DO.LM, coef)
#  Transpose and tidy it up
MN.do.tod = cbind(as.character(colnames(MN.do.coef)),data.frame(t(MN.do.coef),row.names=NULL))
colnames(MN.do.tod) = c("ID","Intercept","TOD") 
MN.do.tod = MN.do.tod %>%
  separate(ID, into = c("Year", "Depth", "DOW"), sep = "/")%>%
  drop_na(TOD)%>% #This nearly halves our data because it cannot calculate a regression line
  mutate(Depth = as.double(Depth))

#Data Summary #6365 obs
# MN.do.tod%>%
#   group_by(DOW)%>%
#   summarise(n = n()) #420 Lakes
# MN.do.tod%>%
#   group_by(Year)%>%
#   summarise(n = n()) #46 Years
# MN.do.tod%>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #856 Lake-Year


#Join the Bathymetry with the TOD
OD <- na.omit(inner_join(MN.Bath.DOW,MN.do.tod))

Bath.Need = MN.do.tod %>%
  distinct(DOW)%>%
  anti_join(MN.Bath.DOW)%>%
  distinct(DOW)
#write.csv(Bath.Need, "MN.BathymetryNeed.csv")



# Note:  ------------------------------------------------------------------

#summaries haven't matched for quite awhile now. 


# #  Summary  #5190
# OD%>%
#   group_by(DOW)%>%
#   summarise(n = n()) #306 Lakes
# OD%>%
#   group_by(Year)%>%
#   summarise(n = n()) #46 Years
# OD%>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #668 Lake-Year

OD.LkYr = OD %>%
  mutate(LkYr = paste(DOW, Year, sep = "_"))

ODlm = with(OD.LkYr,
            by(OD.LkYr, INDICES = LkYr, #The indices allows for it to broken up by lake and year
               function(x) lm(TOD~alpha, data = x)))      
MN.OD.coef = sapply(ODlm, coef)
MN.TOD = cbind(as.character(colnames(MN.OD.coef)),data.frame(t(MN.OD.coef),row.names=NULL))
colnames(MN.TOD) = c("Lake_Year","WOD","SOD")#these are water and sediment oxygen demand. we didn't parcel these out in results

#  Add interpolated temperatures to the depth-specific data
MN.DS.Thermo.ag = MN.DS.Thermo %>%
  group_by(DOW, Year, Depth)%>%
  summarise(Temperature = mean(Temperature))
DS.MN<-inner_join(OD,MN.DS.Thermo.ag)
DS.MN$DOY_0<-(0-DS.MN$Intercept)/DS.MN$TOD #day of year when DO hits 0
DS.MN$DOY_3<-(3-DS.MN$Intercept)/DS.MN$TOD #day of year when DO hits 3


#  Calculate entire hypolimnion volume-weighted values 
VW_MN = DS.MN %>%
  group_by(DOW, Year)%>%
  summarise(VW_TOD = sum(TOD*Vol)/sum(Vol),
            VW_Temp = sum(Temperature*Vol)/sum(Vol),
            VW_Intercept = sum(Intercept*Vol)/sum(Vol))%>%
  mutate(VW_DOY_0 = (0-VW_Intercept)/VW_TOD,
         VW_DOY_3 = (3-VW_Intercept)/VW_TOD)


#  Add Lat and Long back into the data set
VW_MN.Meta.unfiltered = MN.DS.Thermo %>%
  ungroup()%>%
  distinct(DOW, .keep_all = T)%>%
  select(DOW, Latitude, Longitude)%>%
  inner_join(VW_MN)
  
Pos.obs = VW_MN.Meta.unfiltered %>%
  filter(VW_TOD > 0)%>%
  group_by(DOW)%>%
  summarise(n = n())

VW_MN.Meta = VW_MN.Meta.unfiltered %>%
  filter(VW_TOD <= 0)
  


# Note:  ------------------------------------------------------------------

# now matching summaries:


#Data Summary #605 obs
VW_MN.Meta %>%
  group_by(DOW, Latitude, Longitude)%>%
  summarise(n = n()) #270 Lakes
VW_MN.Meta %>%
  group_by(Year)%>%
  summarise(n = n()) #45 Years
VW_MN.Meta %>%
  group_by(DOW, Year)%>%
  summarise(n = n()) #605 DOW Year

#write.csv(VW_MN.Meta,"VW_MN.Meta.csv")
VW_MN.Meta<-read.csv("VW_MN.Meta.csv")
#Attempt to filter for long term trends
# Big.Data = VW_MN.Meta %>%
#   group_by(DOW)%>%
#   summarise(n_years = n())%>%
#   filter(n_years >=10)
# 
# Full = VW_MN.Meta %>%
#   semi_join(Big.Data)

###  Graphical Representation
require('maps')
library("plotly")
#  Read in the polygon data for Minnesota
MN.states = map_data("state", region = "Minnesota")


#  Make the base plot
MN.TOD.plot = ggplot()+
  geom_polygon(data = MN.states, aes(x = long, y = lat), 
              fill = "white", color = "black", size = 1, alpha = 0.4)+
  geom_point(data = VW_MN.Meta, aes(x = Longitude, 
                                      y = Latitude,frame=Year,
                            color = VW_TOD
                                      #ids = DOW
                            ),
             size = 2)+
  scale_color_gradientn(colors=c("#F21A00","#EBCC2A","#3B9AB2"))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()

MN.TOD.animation = ggplotly(MN.TOD.plot) %>% 
  animation_opts(mode = "afterall", frame = 250, transition = 100, redraw = F) %>%
  animation_slider(currentvalue = list(prefix = "YEAR ", font = list(color="red")))
MN.TOD.animation


ggplot(VW_MN.Meta, aes(x=VW_TOD))+
         geom_histogram(aes(fill=as.factor(Year)),bins=10)

MN.TOD.plot2 = ggplot()+
  geom_polygon(data = MN.states, aes(x = long, y = lat), 
               fill = "white", color = "black", size = 1, alpha = 0.4)+
  geom_point(data = VW_MN.Meta, aes(x = Longitude, 
                                    y = Latitude),
  size = 2, color="steelblue")+
   xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()
#  Clean
rm(MN.TOD.animation, MN.states, MN.Filtered.Obs, MN.TOD.plot, MN.Bath,
   MN.Bath.DOW, MN.do.coef, MN.do.tod, MN.DS.Thermo.ag, MN.DS.Thermo, 
   MN.DS.Hypo, MN.DO.LM, OD, ODlm, VW_MN, Bath.Need, DS.MN, MN.AHOD, SOD, WOD)

####### Calculating TDO3 =======================================================

#  This section is calculating TDO3 which is the minimum temperature where
#  the DO is 3 mg/L. 3mg/L is considered to be lethal to cold water fish
#  Runtime is less than  minutes



# load data
MN.ALL.Data = read_csv("MN.ALL.DATA.csv")

#Filter the dataset
filter1.mn <- MN.ALL.Data %>% #Remove Impossible Values
  mutate(Location_Year = as.character(paste(MonitoringLocationIdentifier,Year, sep = "/")),
         Profile = paste(MonitoringLocationIdentifier, Year, DOY, sep = "/")) %>%
  filter(Temperature < 40) #Jane et al 2021

#  See how much the filtering took away
# filter1.mn %>%  #840,558 obs
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #7,212 Sample Sites
# filter1.mn %>%
#   group_by(DOW)%>%
#   summarise(n = n()) # 2,440 Lakes
# filter1.mn %>%
#   group_by(Year)%>%
#   summarise(n = n()) #76 Years
# filter1.mn %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #20,675 Lake-Years
# filter1.mn %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #82,837 Profiles

filter2.mn = filter1.mn %>%            #Profiles must have a minimum depth less than 3 meters; 
  group_by(Profile)%>%                 #otherwise the profiles shallowest read could be in 
  summarise(min.depth = min(Depth))%>% #the metalimnion and capture none of the epilimnion
  filter(min.depth < 3) #Jane et al 2021

filter3.mn = filter1.mn %>%
  semi_join(filter2.mn, by = "Profile") 

# # Summary  #839,779 obs
# filter3.mn %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #7,191 Sample Sites
# filter3.mn %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #2,440 Lakes
# filter3.mn %>%
#   group_by(Year)%>%
#   summarise(n = n()) #74 Years
# filter3.mn %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #20,638 Lake-Years
# filter3.mn %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #82,510 Profiles

filter4.mn = filter3.mn %>%
  filter(DOY >= 121 &
           DOY <= 258)%>%
  group_by(Year, MonitoringLocationIdentifier)%>% 
  summarise(Late=max(DOY))%>%
  filter(Late >= 196 & #this adds that the last sample was taken between DOY 196 and 258
           Late <= 258)%>%
  mutate(Location_Year = as.character(paste(MonitoringLocationIdentifier,Year, sep = "/"))) 

#  Put it all together
filter5.mn = filter3.mn %>%
  semi_join(filter4.mn, by = "Location_Year") %>% 
  filter(DOY >= 121 &
           DOY <= 258) #Removes profiles where it may have been sampled while ice is on or during turnover

# Note:  ------------------------------------------------------------------

#summaries are matching again.


# Summary  #634,602 obs
# filter5.mn %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #5,790 Site
# filter5.mn %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #2,273 DOW
# filter5.mn %>%
#   group_by(Year)%>%
#   summarise(n = n()) #74 Years
# filter5.mn %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #14,882 Lake-Years
# filter5.mn %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #60,987 Profiles

# Remove Lake Superior Sites, Mines, and Pits ~ We only want Glacial Lakes
filter6.mn = filter5.mn %>%
  filter(MonitoringLocationIdentifier != "EPA_GLNPO-SU19") %>% #Lake Superior Site
  filter(MonitoringLocationIdentifier != "EPA_GLNPO-SU18") %>% #Lake Superior Site
  filter(MonitoringLocationIdentifier != "EPA_GLNPO-SU104550")%>% #Lake Superior Site
  filter(MonitoringLocationIdentifier != "21GPBCH-LS_PT_1") %>% #Lake Superior Site
  filter(MonitoringLocationIdentifier != "21GPBCH-LS_PT_9") %>% #Lake Superior Site
  filter(MonitoringLocationIdentifier != "GPC5_WQX-LS_PT_1") %>% #Lake Superior Site
  filter(MonitoringLocationIdentifier != "GPC5_WQX-LS_PT_9") %>% #Lake Superior Site
  filter(MonitoringLocationIdentifier != "GPC5_WQX-LS_PT_8") %>% #Lake Superior Site
  filter(MonitoringLocationIdentifier != "MNPCA-69-1297-00-202") %>% #Sherman Pit
  filter(MonitoringLocationIdentifier != "MNPCA-69-0429-00-201") %>% #Sabin Pit
  filter(MonitoringLocationIdentifier != "21GPBCH-LS_PT_8") %>%      #Lake Superior Site
  filter(MonitoringLocationIdentifier != "MNPCA-18-0440-01-100") %>% #Mahnomen Mine
  filter(MonitoringLocationIdentifier != "MNPCA-69-1297-00-201") %>% #Sherman Pit
  filter(MonitoringLocationIdentifier != "MNPCA-18-0439-00-100") %>% #Pennington Mine
  filter(MonitoringLocationIdentifier != "MNPCA-31-1325-04-201") %>% #Canisteo Mine Pit
  filter(MonitoringLocationIdentifier != "MNPCA-69-1298-00-203") %>% #South Twin City Pit
  filter(MonitoringLocationIdentifier != "MNPCA-18-0093-01-202") %>% #Rabbit Lake
  filter(MonitoringLocationIdentifier != "MNPCA-18-0041-01-201") %>% #Crooked Lake - Silver Bay
  filter(MonitoringLocationIdentifier != "MNPCA-69-0429-00-100") %>% #Embarrass Mine
  filter(MonitoringLocationIdentifier != "MNPCA-69-0428-00-201") %>% #St. James Pit
  filter(DOW != "18044001")%>%
  filter(DOW != "69129700")%>%
  filter(DOW != "18043900")%>%
  filter(DOW != "31132504")%>%
  filter(DOW != "69129800")%>%
  filter(DOW != "18009301")%>%
  filter(DOW != "18004101")%>%
  filter(DOW != "69042900")%>%
  filter(DOW != "69042800")

# Summary  #633,617 obs
# filter6.mn %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #5,774 Sample Site
# filter6.mn %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #2,266 Lakes
# filter6.mn %>%
#   group_by(Year)%>%
#   summarise(n = n()) #74 Years
# filter6.mn %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #14,863 Lake-Years
# filter6.mn %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #60,941 Profiles

filter7.1mn = filter6.mn %>%            #Profiles must have at least 3 reads in them
  group_by(Profile)%>%
  summarise(n_depths = n_distinct(Depth))%>%
  filter(n_depths >= 3) #Jane et al 2021

filter7.mn = filter6.mn %>%
  semi_join(filter7.1mn, by = "Profile")

# #  Summary #623,234 obs
# filter7.mn %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #4,657 Sites
# filter7.mn %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #2,178 Lakes
# filter7.mn %>%
#   group_by(Year)%>%
#   summarise(n = n()) #71 Years
# filter7.mn %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #12,598 Lake-Years
# filter7.mn %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #52,260 Profiles

filter8.mn = filter7.mn %>%
  group_by(Profile)%>%
  mutate(Meta = list(meta.depths(wtr = Temperature, depths = Depth, slope = 0.1, seasonal = F)))%>%
  separate(Meta, into = c("top.meta", "top.hypo"), sep = ",")%>%
  mutate(top.meta = gsub("[^0-9.-]", "", top.meta),
         top.hypo = as.numeric(gsub("[^0-9.-]", "", top.hypo)),
         Depth = as.numeric(Depth))%>%
  drop_na(top.hypo)%>%  #Jane et al 2021
  filter(top.hypo != "0") 

# #Summary #592173
# filter8.mn %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #4,471 Sample Sites
# filter8.mn %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #2,104 Lakes
# filter8.mn %>%
#   group_by(Year)%>%
#   summarise(n = n()) #71 Years
# filter8.mn %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #11,625 Lake-Years
# filter8.mn %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #47,231 Profiles

filter9.1mn = filter8.mn %>%
  filter(DOY >= 121 &
           DOY <= 258)%>%
  group_by(Year, MonitoringLocationIdentifier)%>% 
  summarise(Late=max(DOY))%>%
  filter(Late >= 196 & #and this adds that the last sample was taken between DOY 196 and 258
           Late <= 258)%>%
  mutate(Location_Year = as.character(paste(MonitoringLocationIdentifier,Year, sep = "/")))
filter9.mn = filter8.mn %>%
  semi_join(filter9.1mn, by = "Location_Year")%>%
  mutate(ID = paste(MonitoringLocationIdentifier, Year, DOY, Max_Depth, sep = "/"))


# #Summary #589458 obs
# filter9.mn %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #4,375 Sample Sites
# filter9.mn %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #2,075 Lakes
# filter9.mn %>%
#   group_by(Year)%>%
#   summarise(n = n()) #71Years
# filter9.mn %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #11,745 Lake-Years
# filter9.mn %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #46,793 Profiles

#  Filter for most sampled site at each lake
filter10.1mn = filter9.mn %>%
  group_by(DOW, MonitoringLocationIdentifier)%>%
  summarise(n = n())%>%
  group_by(DOW)%>%
  mutate(MostSampled = max(n))%>%
  ungroup()%>%
  filter(n == MostSampled)%>%
  distinct(DOW, .keep_all = T)
MN.Filtered.Obs = filter9.mn %>%
  semi_join(filter10.1mn)

# #  Summary  #400884 obs
# MN.Filtered.Obs %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #2,075 Sample Sites
# MN.Filtered.Obs %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #2,075 Lakes
# MN.Filtered.Obs %>%
#   group_by(Year)%>%
#   summarise(n = n()) #69 Years
# MN.Filtered.Obs %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #8,574 Lake-Years
# MN.Filtered.Obs %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #31,835 Profiles

#  Select the latest profile
EndofYear = MN.Filtered.Obs %>%
  group_by(DOW, Year)%>%
  mutate(Max = max(DOY))%>%
  filter(DOY == Max)%>%
  ungroup()


#  Summary  #101750 obs
# EndofYear %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #2,075 Sample Sites
# EndofYear %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #2,075 Lakes
# EndofYear %>%
#   group_by(Year)%>%
#   summarise(n = n()) #69 Years
# EndofYear %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #8,574 Lake-Years
# EndofYear %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #8,574 Profiles


#  Clean
rm(filter9.1mn, filter9.mn, filter8.mn, filter7.mn, filter7.1mn, filter6.mn,
   filter5.mn, filter4.mn, filter3.mn, filter2.mn, filter1.mn, filter10.1mn)
gc()



### The next step is to interpolate data at 1 m increments
Depth.df = tibble(Depth = 0:90) #This creates a data frame to make sure we have data for the bottom of larger lakes

#  First DO
#  Pivot Wide
Wide.MN.DO.Obs = EndofYear %>%
  mutate(ID = paste(MonitoringLocationIdentifier, Year, DOY, Max_Depth, sep = "/"))%>%
  select(ID, Depth, DO) %>%
  pivot_wider(names_from = ID, values_from = DO, values_fn = mean)%>%
  full_join(Depth.df)

#  Interpolate
Wide.Inter.MN.DO = na.approx(Wide.MN.DO.Obs, x = Wide.MN.DO.Obs$Depth)%>% #interpolates data
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()

#  Pivot Long
Long.Inter.MN.DO = Wide.Inter.MN.DO %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "DO", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  filter(Depth <= Max_Depth)

#Clean
rm(Wide.MN.DO.Obs, Wide.Inter.MN.DO)

### Next is Temperature
#  Pivot Wide
Wide.MN.Temp.Obs = EndofYear %>%
  mutate(ID = paste(MonitoringLocationIdentifier, Year, DOY, Max_Depth, sep = "/"))%>%
  select(ID, Depth, Temperature) %>%
  pivot_wider(names_from = ID, values_from = Temperature, values_fn = mean)%>%
  full_join(Depth.df)

#  Interpolate
Wide.Inter.MN.Temp = na.approx(Wide.MN.Temp.Obs, x = Wide.MN.Temp.Obs$Depth)%>% #interpolates data - 75 was chosen to make sure every lake was fully interpolated
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()

#  Pivot Long
Long.Inter.MN.Temp = Wide.Inter.MN.Temp %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "Temperature", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  filter(Depth <= Max_Depth)

### Join our results together
MN.joined.inter = inner_join(Long.Inter.MN.Temp, Long.Inter.MN.DO)

#  We need to get the Lat and Lon
MN.Depth.Specific = EndofYear %>%
  distinct(MonitoringLocationIdentifier, .keep_all = T)%>%
  select(c(MonitoringLocationIdentifier, Latitude, Longitude, DOW))%>%
  inner_join(MN.joined.inter)

# Note:  ------------------------------------------------------------------

# summaries not jiving with expected, again. 

#Data summary #228102 obs
# MN.Depth.Specific %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #2,074 Sample Sites
# MN.Depth.Specific %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #2,074 Lakes
# MN.Depth.Specific %>%
#   group_by(Year)%>%
#   summarise(n = n()) #68 Years
# MN.Depth.Specific %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #8,556 Lake-Years

#  Clean
rm(Long.Inter.MN.DO, Long.Inter.MN.Temp, Wide.Inter.MN.Temp, Wide.MN.Temp.Obs,
   MN.joined.inter)

###  Next we calculate TDO3
MN.TDO3 = MN.Depth.Specific %>%
  filter(DO >= 3)%>%
  group_by(MonitoringLocationIdentifier, Year)%>%
  summarise(TDO3 = min(Temperature))

#  Add in Lat and Lon
MN.TDO3.Meta = EndofYear %>%
  distinct(MonitoringLocationIdentifier, .keep_all = T)%>%
  select(c(MonitoringLocationIdentifier, Latitude, Longitude, DOW))%>%
  inner_join(MN.TDO3)


# Note: -------------------------------------------------------------------

# This summary jives again...

#  Summary  #8527 obs
# MN.TDO3.Meta %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #2,071 Sample Sites
# MN.TDO3.Meta %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #2,071 Lakes
# MN.TDO3.Meta %>%
#   group_by(Year)%>%
#   summarise(n = n()) #68 Years
# MN.TDO3.Meta %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #8,527 Lake-Years

#write_csv(MN.TDO3.Meta, "MN_TDO3.csv")

# Clean
rm(MN.TDO3, EndofYear)

### Graphical Representation of Data

#  Load Libraries
library("plotly")
require('maps')

#  Read in the polygon data for Minnesota
MN.states = map_data("state", region = "Minnesota")

# Take out outliers 
MN.TDO3.Meta = MN.TDO3.Meta %>%
  filter(TDO3 < 30)
#  Make the base plot
MN.TDO3.plot = ggplot()+
  geom_polygon(data = MN.states, aes(x = long, y = lat), 
               fill = "white", color = "black", size = 1, alpha = 0.4)+
  geom_point(data = MN.TDO3.Meta, aes(x = Longitude, 
                                      y = Latitude,
                                      color = TDO3, 
                                      frame = Year,
                                      ids = MonitoringLocationIdentifier),
             size = 3)+
  scale_color_gradientn(colors = c("seagreen", "green", "red"))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()

MN.TDO3.Animation = ggplotly(MN.TDO3.plot) %>% 
  animation_opts(mode = "afterall", frame = 750, transition = 500, redraw = F) %>%
  animation_slider(
    currentvalue = list(prefix = "YEAR ", font = list(color="red")))
MN.TDO3.Animation

# Lake Ida Analysis
# Ida.ds = MN.Depth.Specific %>%
#   filter(MonitoringLocationIdentifier == "MNPCA-21-0123-00-100"|
#            MonitoringLocationIdentifier == "MNPCA-21-0123-00-101"|
#            MonitoringLocationIdentifier == "MNPCA-21-0123-00-102")%>%
#   filter(Depth < 32)%>%
#   filter(Year > 2011)
# 
# Ida.ds %>%
#   ggplot(aes(x = Depth, y = DO, color = Year))+
#   geom_point()+
#   geom_line()+
#   scale_x_reverse()+
#   coord_flip()
