
# Header ------------------------------------------------------------------


#MGLP Project

#Version History:
  #Originally written by Jacob Angus 30June2022 (FullMNScript.R)
  #Modified by Heidi Rantala ~Aug 2023 to examine some questionable DO values and filter rule effects (FullMNScript_hmr_2ppm.R)
  #Modified by Mike Verhoeven Oct 2023 to separate data aggregation and prep fromfiltering, VHOD and TDO3 generation

#  Script for Minnesota that can be run as a job
#  It can also be run in individual sections
#  It has five major sections: Data Cleaning and Joining, Filtering,
#  Interpolation, Calculating VHOD, Calculating TDO3


# to-do list --------------------------------------------------------------


# to-do:
#   1. Work through script deleting extra & deprecated code
#   2. Rename the input data so that a person can logically track packages of related files. 
#   3. Remove duplicated profiles (currently just tagged in MNPCA data because of the issue outlined in the remove dups section)
#   4. For ambiguous joins, set aside probs and move forward with good stuff
#   5. Resume checking old work at line 505
# - Remove GR filter from Section 1
# - nix any code that runs out huge blocks of results
# - is this code setup to allow a re-pull from WQP?
 

# load packages -----------------------------------------------------------


library("tidyverse")
library("lubridate")
library("zoo")
library("rLakeAnalyzer")
library("broom")
library("readxl")
library("plotly")
library("skimr")
library("data.table")
library("mwlaxeref")

# 
# The goals of this section of the script: 
# 1. combine data sets with metadata, and 
# 2. Join Minnesota data sets together. 

# It is split into two sections: WQP and MNPCA


#set wd to the data folder to avoid needing filepaths
# setwd("data&scripts/data/input")

# Load Data - WQP  -----------------------------------------------

### Load files

# WQP 
# MN Observations from Water Quality Portal 
WQP_observations = fread("MNDataRetrievalCombined.csv")
#All MN Observations of Temp and DO from 1940 to 2020 from WQP

WQP_observations[ , .N  ]
WQP_observations[ , .N , .(CharacteristicName)  ] # long format-- each measurement on its own line
glimpse(WQP_observations)

# MN metadata from WQP
WQP_metadata <-  fread("MNDataRetrievalMetadata.csv") #Site data for all the observations above
WQP_metadata[ ,.N , OrganizationFormalName]
glimpse(WQP_metadata)


# No issues in these two file reads  
LakeDepthArea <-  fread("lake_depth.csv") #Dataset containing the lake depth and area for all lakes in the US (source? also, doubtful...there are only 17675 entries...)
Link <-  fread("lake_link.csv") #Dataset that can link the Lagos dataset to the STORET dataset

#  Convert Monitoring Location Identifier into DOW 
#  DOW is the official way to identify lakes in Minnesota; whereas,
#  Monitoring Location Identifiers are specific sites on a lake.
WQP_observations[ OrganizationIdentifier == "MNPCA" , .N , MonitoringLocationIdentifier ]
  
# FOR MNPCA Records, parse the DOW (Why?)
WQP_observations[ OrganizationIdentifier == "MNPCA" , DOW := gsub("-", "", word(MonitoringLocationIdentifier, start = 2L, end = 4L, sep = "-")) , ]
#checkwork
WQP_observations[, unique(DOW) , ]
WQP_observations[ ,as.numeric(unique(DOW)) ,]

#are any MLI in the obs missing metadata?
any(is.na(match(WQP_observations[ , unique(MonitoringLocationIdentifier) ,], WQP_metadata[ ,MonitoringLocationIdentifier ,])))

#what if I just join up the metadata?
WQP_observations[WQP_metadata, on = .(MonitoringLocationIdentifier = MonitoringLocationIdentifier) , ][ , .N , CharacteristicName]
WQP_observations[ , .N , CharacteristicName]
#some small amount of duplication is clearly happening in the temps

WQP_observations[WQP_metadata, on = .(MonitoringLocationIdentifier = MonitoringLocationIdentifier) , ][ , .N , ActivityIdentifier]


#duplicated metadata records (each has 2 vals for Horiz accuracy):
WQP_metadata[MonitoringLocationIdentifier %in% c("USGS-481633096031801","USGS-482353096001901")] # show a reocrd with 2 different HOriz. accuracy measures
  WQP_metadata <- WQP_metadata[!(MonitoringLocationIdentifier %in% c("USGS-481633096031801","USGS-482353096001901") & HorizontalAccuracyMeasure.MeasureValue == .1)] #drop one of the duplicated metadata records
  
# check for multijoins
left_join(WQP_observations,WQP_metadata, by = ("MonitoringLocationIdentifier"), relationship = "many-to-one")
# DONT execute left join
# WQP_observations <- left_join(WQP_observations,WQP_metadata, by = ("MonitoringLocationIdentifier"), relationship = "many-to-one")
# jsut grab the location name
WQP_observations[WQP_metadata, on = .(MonitoringLocationIdentifier = MonitoringLocationIdentifier) , MonitoringLocationName := MonitoringLocationName ]


# can we get DOWs or other lake names from another place? Lake names and loc notes are buried in the MLName field (usually)
WQP_observations[is.na(DOW), .N , .(MonitoringLocationName, MonitoringLocationIdentifier) ] # 1364 locations with no DOW, but that do have a monitoring loc name

# Load Data - MNPCA  --------------------------------------------------------------

#  MNPCA
PCA_profiles <-  fread("1945_2020_All_MNDNR_MPCA_Temp_DO_Profiles.csv") #Lake Profile data


#  Here we are creating Monitoring Location Identifiers for the MNPCA and MNDNR
#  dataset. They have "PROFID" and DOW which can be combined to make a 
#  Monitoring Location Identifier. This will later help when removing duplicate
#  profiles and joining the data together.
names(PCA_profiles)
PCA_profiles <-  PCA_profiles %>% 
  separate(DOW_DATE_AGENCY_PROFID, into = c("DOW", "DATE", "AGENCY", "PROFID"), sep = "_", remove = F)%>%
  mutate(Sample.Site = as.integer(PROFID)) %>% # This is a bit of a concerning coercion of integers from numerics
  mutate(County = substr(DOW, start = 1, stop = 2),
         LakeID = substr(DOW, start = 3, stop = 6),
         SubbasinID = substr(DOW, start = 7, stop = 8), #YIKES: Here I renamed some things that may not ever cause a problem unless they get misapplied later (See line 132 in the old work)
         MNAgency = paste("MN", AGENCY, sep = ""),
         MonitoringLocationIdentifier = paste(MNAgency, County, LakeID, SubbasinID, Sample.Site, sep = "-")) %>% 
  setDT() #set as data.table to allow that syntax:

#' check on the monitoring loc id gen
PCA_profiles[ , .N , MonitoringLocationIdentifier] 
PCA_profiles[ , c("Sample.Site", "PROFID") , ]

WQP_observations[OrganizationIdentifier=="MNPCA" , .(MonitoringLocationIdentifier)]

as.integer(PCA_profiles$PROFID)
any(word(PCA_profiles$PROFID, 2, sep = "\\." )!="0")#are there any non-zero sig digits after the decimal? Yes...


PCA_profiles[word(PROFID, 2, sep = fixed(".")) != "0" , PROFID ,] #tons of decimals in here
PCA_profiles[word(PROFID, 2, sep = fixed(".")) != "0" , .N ,] #122k to be excact
# about 1/8 of these records have decimals in those profile IDents. This means that the PROFID variable is going to be dup over many measurements in some cases. 

#can we recover something similar in the WQP data? Like a multipart profile identifier? Keep in mind that these were not built, but instead were shipped with the data
WQP_observations[OrganizationIdentifier=="MNPCA" , MonitoringLocationIdentifier , ]

#add metadata to the PCA datasets

PCA_profiles[, .N , AGENCY]


# Comment -----------------------------------------------------------------


#' So it's obvious to me that there are multipart obs in these data (multi row at one location and date), and here we have ignored those as we developed the variables that we will use to cross these two datasets in search of duplicated variables. This means that we won't really check for duplicated measures, but instead duplicated locations/sampling runs (profiles)... This may have NO effect at all, but would result in dumping of PCA data (that's the one that gets cut) where a combo of loc-lake-date matches but has different measurements. This problem warrants a closer look, but moving on for now. Below you'll see that I don't remove those dups, just tag them as such. ONe note here is that the WQP data are a huge mess in the join/merge to lake attributes. So if a person could remove them from the WQP data instead of PCA you might preserve some data. 


# Tag Dups Between WQP & MPCA ------------------------------------

### Tag Duplicate Profiles across data sets
#  The MNPCA reports most of their data to the WQP; therefore, we likely have 
#  duplicated data. If we screen for profiles (Monitoring Location ID + Date),
#  then we can see which ones are overlapping and remove them from one data set


WQP_unique_profileIDs = WQP_observations %>% # This identifies all unique profiles in the WQP data
  mutate(Year = year(ymd(ActivityStartDate)),
         DOY = yday(ymd(ActivityStartDate)),
         Profile = paste(MonitoringLocationIdentifier,Year, DOY, sep = "/"))%>%
  distinct(Profile)

PCA_WQP.Duplicate.Profiles = PCA_profiles%>% # This identifies all unique profiles in the MNPCA data
  mutate(Year = year(mdy(DATE)),
         DOY = yday(mdy(DATE)),
         Profile = paste(MonitoringLocationIdentifier,Year, DOY, sep = "/"))%>%
  distinct(Profile)%>%
  inner_join(WQP_unique_profileIDs) # Joins the two together with inner_join, so we only select for the ones that both data sets have

# Mark the duplicated records
PCA_profiles %>%
  mutate(Year = year(mdy(DATE)),
         DOY = yday(mdy(DATE)),
         Profile = paste(MonitoringLocationIdentifier,Year, DOY, sep = "/")) %>% 
  mutate(duplicate_prof_WQP = Profile %in% PCA_WQP.Duplicate.Profiles$Profile) %>%  #mark the duplicate ones from the MNPCA data set 
  { PCA_profiles <<- .}  #Don't actually remove those records, just add a tag showing duplication in the two datasets

PCA_profiles[ , .N , duplicate_prof_WQP ]


WQP_observations %>% 
  mutate(Year = year(ymd(ActivityStartDate)),
         DOY = yday(ymd(ActivityStartDate)),
         Profile = paste(MonitoringLocationIdentifier,Year, DOY, sep = "/"))%>% 
  mutate(duplicate_prof_PCA = Profile %in% PCA_WQP.Duplicate.Profiles$Profile) %>%  #mark the duplicate ones from the MNPCA data set 
  { WQP_observations <<- .}
  
WQP_observations[ , .N , duplicate_prof_PCA ]


# # Summarize the MNPCA Data [All of these are matching with the new work]
PCA_profiles %>%
  filter(duplicate_prof_WQP == FALSE ) %>% #434023 obs not tagged duplicated
  group_by(MonitoringLocationIdentifier)%>%
  summarise(n = n()) #8,470 Sample Sites
PCA_profiles %>%
  filter(duplicate_prof_WQP == FALSE ) %>% 
  group_by(DOW)%>%
  summarise(n = n()) #4,358 Lakes
PCA_profiles %>%
  filter(duplicate_prof_WQP == FALSE ) %>% 
  mutate(Year = year(mdy(DATE)))%>%
  group_by(Year)%>%
  summarise(n = n()) #80 Years
PCA_profiles %>%
  filter(duplicate_prof_WQP == FALSE ) %>% 
  mutate(Year = year(mdy(DATE)))%>%
  group_by(DOW, Year)%>%
  summarise(n = n()) #22,981 Lake-Years
PCA_profiles %>%
  filter(duplicate_prof_WQP == FALSE ) %>% 
  mutate(Year = year(mdy(DATE)),
         DOY = yday(mdy(DATE)))%>%
  group_by(MonitoringLocationIdentifier, Year, DOY)%>%
  summarise(n = n()) #40,120 Profiles



# These data are not formatted the same. WQP data are "longer". This makes it very hard to know if there's data in one that are not in the other. 
a <- PCA_profiles[Profile=="MNPCA-01-0001-00-206/2016/180"]
b <- WQP_observations[Profile=="MNPCA-01-0001-00-206/2016/180"]




# # # #Check for overlap -- here overlap is okay
# 
# #DOW overlap
# WQP_observations[]
# 
# #Years overlap
# WQP.Year = WQP_observations %>%
#   mutate(Year = year(ymd(ActivityStartDate)))%>%
#   distinct(Year)
# PCA.WQP.Year = PCA_profiles %>%
#   filter(duplicate_prof_WQP == FALSE ) %>% 
#   mutate(Year = year(mdy(DATE)))%>%
#   distinct(Year)%>%
#   inner_join(WQP.Year) #74 Overlap of Years
# 
# #Lk Year overlap
# WQP.LkYear = WQP_observations %>%
#   mutate(Year = year(ymd(ActivityStartDate)),
#          Lk_Yr = paste(DOW, Year, sep = "/"))%>%
#   distinct(Lk_Yr)
# PCA.WQP.LkYear = PCA_profiles %>%
#   filter(duplicate_prof_WQP == FALSE ) %>% 
#   mutate(Year = year(mdy(DATE)),
#          Lk_Yr = paste(DOW, Year, sep = "/"))%>%
#   distinct(Lk_Yr)%>%
#   inner_join(WQP.LkYear)#4,098 Overlap of Lake Year Combos
# 
# #MLI Overlap
# WQP.MLI = WQP_observations %>%
#   distinct(MonitoringLocationIdentifier)
# PCA.WQP.MLI = PCA_profiles %>%
#   filter(duplicate_prof_WQP == FALSE ) %>% 
#   distinct(MonitoringLocationIdentifier)%>%
#   inner_join(WQP.MLI) #301 Overlap of MonitoringLocationIdentifier
# 
# #profile overlap
# WQP.Profile.Check = WQP_observations %>%
#   mutate(Year = year(ymd(ActivityStartDate)),
#          DOY = yday(ymd(ActivityStartDate)),
#          Profile = paste(MonitoringLocationIdentifier, Year, DOY, sep = "/"))%>%
#   distinct(Profile)
# PCA.WQP.Profile.Check = PCA_profiles %>%
#   filter(duplicate_prof_WQP == FALSE ) %>% 
#   mutate(Year = year(mdy(DATE)),
#          DOY = yday(mdy(DATE)),
#          Profile = paste(MonitoringLocationIdentifier,Year, DOY, sep = "/"))%>%
#   distinct(Profile)%>%
#   inner_join(WQP.Profile.Check)#0 Overlap of Profiles
# #Clean
# rm(PCA.WQP.Profile.Check, PCA_WQP.Duplicate.Profiles, PCA.WQP.DOWshared, PCA.WQP.LkYear, PCA.WQP.MLI, PCA.WQP.Year, WQP.MLI, WQP.DOW, WQP.Year, WQP.LkYear, WQP.Profile.Check, WQP_unique_profileIDs)
# gc()


# 

# cleaning WQP data -------------------------------------------------------



names(PCA_profiles)
names(WQP_observations)

WQP_observations[ , .N, ActivityIdentifier]

WQP_observations[ , .N , .(ResultSampleFractionText, ResultMeasure.MeasureUnitCode) ]

#drop a ton of WQP fields
WQP_observations[ , "WQP_obs_id" := .I , ]#add a row ident for easy recouping info if needed

keepcols <- c("DOW", "MonitoringLocationIdentifier", "MonitoringLocationName",
              "Year", "DOY", "Profile", "duplicate_prof_PCA",
              "ActivityStartDate", "OrganizationIdentifier", 
              "ActivityIdentifier", "ActivityDepthHeightMeasure.MeasureValue", "ActivityDepthHeightMeasure.MeasureUnitCode",
              "CharacteristicName", "ResultMeasureValue", "ResultMeasure.MeasureUnitCode", "MeasureQualifierCode", "WQP_obs_id"
              )
any(duplicated(WQP_observations[ , .SD , .SDcols = keepcols ]  ))# we've not lost anythign req'd for a key set across all cols

WQP_observations <- WQP_observations[ , .SD , .SDcols = keepcols ]


#add a measurment count variable to indicate how many rows collapsed into one mean val in the subsequent operation:
WQP_observations[ , Result_count := .N , .(DOW, MonitoringLocationIdentifier, MonitoringLocationName,
                    Year, DOY, Profile, duplicate_prof_PCA,
                    ActivityStartDate, OrganizationIdentifier, 
                    ActivityDepthHeightMeasure.MeasureValue, ActivityDepthHeightMeasure.MeasureUnitCode, CharacteristicName) ]

#cast this wide
WQPdata_wide  <- dcast(WQP_observations,
                                      DOW + MonitoringLocationIdentifier + MonitoringLocationName +
                                      Year + DOY + Profile + duplicate_prof_PCA +
                                      ActivityStartDate + OrganizationIdentifier + 
                                      ActivityDepthHeightMeasure.MeasureValue + ActivityDepthHeightMeasure.MeasureUnitCode ~ CharacteristicName,
                                       value.var = list("ResultMeasureValue", "ResultMeasure.MeasureUnitCode", "MeasureQualifierCode", "Result_count"), 
                                       fun.aggregate = list( mean, first, first, first),
                                       fill = NA)

#no dups in here?
any(duplicated(WQPdata_wide))

#consolidate units
# DEPTH
WQPdata_wide[ , .N , ActivityDepthHeightMeasure.MeasureUnitCode ]
  WQPdata_wide[is.na(ActivityDepthHeightMeasure.MeasureUnitCode), .N , ActivityDepthHeightMeasure.MeasureValue ] # where no units have we got any values? Nope
  
  #coerce depths 
  WQPdata_wide[ActivityDepthHeightMeasure.MeasureUnitCode %in% c("m", "mm", "meters"), DEPTH_M := ActivityDepthHeightMeasure.MeasureValue]
  WQPdata_wide[ActivityDepthHeightMeasure.MeasureUnitCode %in% c("ft", "feet"), DEPTH_M := ActivityDepthHeightMeasure.MeasureValue*0.3048]
  
  #drop old cols
  WQPdata_wide[ , ':=' (ActivityDepthHeightMeasure.MeasureUnitCode = NULL,
                        ActivityDepthHeightMeasure.MeasureValue = NULL) , ]
  
  WQPdata_wide[ , summary(DEPTH_M) , ]

# Temps
WQPdata_wide[ , .N , `ResultMeasureValue_mean_Temperature, water` ]  
WQPdata_wide[ , .N , `ResultMeasure.MeasureUnitCode_first_Temperature, water` ]  
  #check weird ones
  WQPdata_wide[`ResultMeasure.MeasureUnitCode_first_Temperature, water` %in% c("mg/l")] #oughtta be C
  WQPdata_wide[ is.na(`ResultMeasure.MeasureUnitCode_first_Temperature, water`), .N , `ResultMeasureValue_mean_Temperature, water` ] #no data in these temps 
  
  #coerce temps
  WQPdata_wide[`ResultMeasure.MeasureUnitCode_first_Temperature, water` %in% c("mg/l", "deg C"), TEMP_C := `ResultMeasureValue_mean_Temperature, water`]
  WQPdata_wide[`ResultMeasure.MeasureUnitCode_first_Temperature, water` %in% c("deg F"), TEMP_C := (`ResultMeasureValue_mean_Temperature, water`-32)/1.8]

  #drop old cols
  WQPdata_wide[ , ':=' (`ResultMeasure.MeasureUnitCode_first_Temperature, water` = NULL,
                        `ResultMeasureValue_mean_Temperature, water` = NULL) , ]
  
# DO
  WQPdata_wide[ , .N , `ResultMeasureValue_mean_Dissolved oxygen (DO)` ]  
  WQPdata_wide[ , .N , `ResultMeasure.MeasureUnitCode_first_Dissolved oxygen (DO)` ]  
  #check weird ones
  WQPdata_wide[is.na(`ResultMeasure.MeasureUnitCode_first_Dissolved oxygen (DO)`), .N , `ResultMeasureValue_mean_Dissolved oxygen (DO)` ] #no DO data in here.
  
  #coerce DO into one column
  WQPdata_wide[`ResultMeasure.MeasureUnitCode_first_Dissolved oxygen (DO)` %in% c("mg/l", "ppm"), DO_PPM := `ResultMeasureValue_mean_Dissolved oxygen (DO)`]

  #drop old cols
  WQPdata_wide[ , ':=' (`ResultMeasure.MeasureUnitCode_first_Dissolved oxygen (DO)` = NULL,
                        `ResultMeasureValue_mean_Dissolved oxygen (DO)` = NULL) , ]
# DO%
  WQPdata_wide[ , .N , `ResultMeasureValue_mean_Dissolved oxygen saturation` ]  
  WQPdata_wide[ , .N , `ResultMeasure.MeasureUnitCode_first_Dissolved oxygen saturation` ]  
  #check weird ones
  WQPdata_wide[`ResultMeasure.MeasureUnitCode_first_Dissolved oxygen saturation` %in% c("mg/l"), .N , is.na(DO_PPM)   ] #forget these. wonky units on redundant data
  
  #coerce DO into one column
  WQPdata_wide[`ResultMeasure.MeasureUnitCode_first_Dissolved oxygen saturation` %in% c("%"), DO_perc_sat := `ResultMeasureValue_mean_Dissolved oxygen saturation`]
  
  #drop old cols
  WQPdata_wide[ , ':=' (`ResultMeasure.MeasureUnitCode_first_Dissolved oxygen saturation` = NULL,
                        `ResultMeasureValue_mean_Dissolved oxygen saturation` = NULL) , ]
  
  WQPdata_wide[ , .N , .(is.na(DO_perc_sat), is.na(DO_PPM)) ]

#tidy up names  
  names(WQPdata_wide)

  oldnames = c("Result_count_first_Dissolved oxygen (DO)", "Result_count_first_Dissolved oxygen saturation", "Result_count_first_Temperature, water", "MeasureQualifierCode_first_Temperature, water", "MeasureQualifierCode_first_Dissolved oxygen saturation", "MeasureQualifierCode_first_Dissolved oxygen (DO)")
  newnames = c("measurecount_DOppm", "measurecount_DOpercsat", "measurecount_temp", "TEMP_FLAG", "DOperc_FLAG", "DO_FLAG")
  
  setnames(WQPdata_wide, old = oldnames, new = newnames)

  names(PCA_profiles)
  

# needing a ws cleanout ---------------------------------------------------
rm(a,b,PCA_WQP.Duplicate.Profiles, WQP_unique_profileIDs)

# combine MN datasets -----------------------------------------------------------------
# So at this point we have these two sets of OBS tidied up. My sense is that they oughtta be merged right here


  keepers = c("DOW", "MonitoringLocationIdentifier", "Year", "DOY", "Profile", "duplicate_prof_WQP", "DATE", "AGENCY", "DEPTH_DIF_FLAG", "DO_FLAG", "TEMP_FLAG", "DEPTH_M", "TEMP_C", "DO_PPM" )
  
PCA_profiles <- PCA_profiles[ , .SD, .SDcols = keepers ]  

  sum(duplicated(PCA_profiles))
  PCA_profiles <- PCA_profiles[!duplicated(PCA_profiles)]

# Choose between dup'd profiles and drop one set
 
  PCA_profiles[ , ndep := length(unique(DEPTH_M)), Profile ]
  
  WQPdata_wide[ , ndep := length(unique(DEPTH_M)), Profile ]
  
a <- WQPdata_wide[duplicate_prof_PCA==T, .("WQPobs" = first(ndep)), Profile ]
b <- PCA_profiles[duplicate_prof_WQP==T, .("PCAobs" = first(ndep)), Profile]

#any reason to keep any WQP observations? YES. some have more depths sampled than the entry in the PCA db
full_join(a,b) %>% 
  mutate("keepWQP" = WQPobs>PCAobs) %>% 
  group_by(keepWQP) %>% 
  count()

full_join(a,b) %>% 
  mutate("keepWQP" = WQPobs>PCAobs) %>% 
  { joinchooser <<- . }

WQPdata_wide <- WQPdata_wide[duplicate_prof_PCA==F |
               (duplicate_prof_PCA == T &
                Profile %in% joinchooser[keepWQP==T,Profile,])
                ]


PCA_profiles <- PCA_profiles[duplicate_prof_WQP==F |
                               (duplicate_prof_WQP == T &
                                  Profile %in% joinchooser[keepWQP==F,Profile,])
]

#check work
any(PCA_profiles[ , unique(Profile) , ]%in%WQPdata_wide[ , unique(Profile) , ])


#drop old useless columns

PCA_profiles[ , duplicate_prof_WQP := NULL , ]
WQPdata_wide[ , duplicate_prof_PCA:= NULL, ]
rm(joinchooser,a,b)




names(PCA_profiles)
names(WQPdata_wide)

#borrow MonLocIdent names from WQP:
MLI_names <- WQP_observations[ , .N , .(MonitoringLocationIdentifier, MonitoringLocationName) ]
PCA_profiles[ , unique(MonitoringLocationIdentifier) , ]

PCA_profiles[MLI_names, on = .(MonitoringLocationIdentifier), MonitoringLocationName := MonitoringLocationName ]
rm(MLI_names)


PCA_profiles[ , .N , MonitoringLocationName]

PCA_profiles[ , .N , DATE ]

PCA_profiles[ ,DATE_clean := as.IDate(DATE, format = "%m/%d/%Y")]
  PCA_profiles[ , .N, DATE_clean ]
  PCA_profiles[ , hist(year(DATE_clean)) , ]
  PCA_profiles[ , DATE := NULL ]
  
WQPdata_wide[ , hist(year(ActivityStartDate)) , ]
setnames(WQPdata_wide, old = "ActivityStartDate", new = "DATE_clean")

WQPdata_wide[, .N, OrganizationIdentifier]
WQPdata_wide[WQP_metadata[, .N , .(OrganizationIdentifier,OrganizationFormalName) ], on = .(OrganizationIdentifier) , AGENCY := OrganizationFormalName ]
  WQPdata_wide[ , OrganizationIdentifier := NULL ] 

PCA_profiles[ ,.N ,  AGENCY]

##column type matching
PCA_profiles[ , .N , DO_FLAG ]
  PCA_profiles[ , DO_FLAG := as.character(DO_FLAG) , ]
WQPdata_wide[ , .N , DO_FLAG ]

PCA_profiles[ , .N , TEMP_FLAG ]
PCA_profiles[ , TEMP_FLAG := as.character(TEMP_FLAG) , ]
WQPdata_wide[ , .N , TEMP_FLAG ]


glimpse(PCA_profiles)
glimpse(WQPdata_wide)


MN_profiles <- full_join(WQPdata_wide, PCA_profiles)


MN_profiles[ , hist(year(DATE_clean)) ,]


#  Heidi has some additional data for Green Lake in Kandiyohi County
Green.Lake.fromH = read_csv("DOTEMP_Green.csv")%>%
  mutate(Agency = ifelse(Source == "DNR", "MNDNR", "MNPCA"),
         MonitoringLocationIdentifier = paste(Agency, "34-0079-00", Station, sep = "-"),
         DOW = "34007900",
         Latitude = 45.2521,
         Longitude = -94.9044,
         DATE_clean = as.IDate(mdy(Date)),
         DOY = yday(DATE_clean),
         Max_Depth = 34,
         DEPTH_M = floor(`Depth (M)`),
         TEMP_C = `Temp (C)`,
         DO_PPM = `DO (ppm)`)%>%
  select(MonitoringLocationIdentifier ,
         DATE_clean                         ,
         DEPTH_M                        ,
         DOW                          ,
         DO_PPM                           ,
         TEMP_C                  ,
         Latitude                     ,
         Longitude                    ,
         Max_Depth                    ,
         Year                         ,
         DOY                          )


GreenLake = MN_profiles %>%
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
  group_by(MonitoringLocationIdentifier,DATE_clean,DEPTH_M,DOW,Latitude,
           Longitude,Max_Depth,Year,DOY)%>%
  summarise(DO_PPM = median(DO_PPM),
            TEMP_C = median(TEMP_C))%>%
  ungroup()

#  Join with the rest of the data
MN_profiles = MN_profiles %>%
  bind_rows(GreenLake.forjoin)


rm(Green.Lake.fromH, GreenLake, GreenLake.DuplicateProfiles, GreenLake.forjoin, GreenLake.fromH.Profiles, WQP_observations, WQPdata_wide, PCA_profiles)


# Prep Lake Area Data------------------------------------------------------

LakeDepthArea = LakeDepthArea %>% 
  # filter(lake_states == "MN")%>% #We are only looking at MN lakes right now, but we can add the GR to the whole dataset
  mutate(lake_area_m2 = lake_waterarea_ha * 10000)%>% #Fang and Stefan 2009 uses the As^0.25:Z with As in square meters
  mutate(GR = (lake_area_m2^0.25)/lake_maxdepth_m) #(Gorham and Boyce, 1989) is one of the first paper to use Geometric Ratio

setDT(LakeDepthArea)
LakeDepthArea[ , .N , "lagoslakeid" ][N>1] #because we will be using the lagos IDs for the lake depth/gr etc. we need to make sure we preserve that in our MLI to lake ID join.


# Prep Link Table ------------------------------------------------------------

# Link is a table of WQP (storet) ids and lagoslake ids, Lake Depth area looks like a LAGOS product?
setDT(Link) #format to data.table for easy syntax

# grab out only MN and border states data, review how many records exist for each MonitoringLoc Ident. These multi-lagos Ids for single Monitoringlocidents are the root of the issue in this join. Also nix out any Link records without a monitoring loc ident  
Link[ , .N , lake_centroidstate][order(lake_centroidstate)]

Link <- Link[ !is.na(wqp_monitoringlocationidentifier) & lake_centroidstate %in% c("MN", "WI", "IA", "ND", "SD", "MI", "OH")] #include all border states in case of border water records

Link[ , .N , "lagoslakeid"][N>10]#how many records for each lagosID in the link? SHow those with >10.
Link[ , length(unique(nhdplusv2_comid)) , .(lagoslakeid, lake_nhdid) ][V1>1]
Link[ , .N , .(NHDidNA=is.na(lake_nhdid),NHDV2comidNA=is.na(nhdplusv2_comid))] # we have more info in the nhdID field than the NHDv2 comid field.

#collapse link to work towards a 1 ID per MonLocIdent
Link[ , length(unique(lake_nhdid)) , wqp_monitoringlocationidentifier ][V1>1] # only 15 cases where multiple NHDs on a monitoringlocation ident
mults<- Link[ , length(unique(lake_nhdid)) , wqp_monitoringlocationidentifier ][V1>1][ ,wqp_monitoringlocationidentifier ]

a <- Link[wqp_monitoringlocationidentifier %in% mults][order(lake_reachcode)]

a[ , .(lake_namegnis, wqp_monitoringlocationname) ,] #view the pairings

a[ , match_inidcator := c(T,F, # Little Maiden Lake Access
                          F,T, # Little Newton Lake -- Access..
                          T,F, # Lake Oakland
                          F,T, #jefferson Lk 2
                          F,T, # Schultz Lake
                          T,F, # Central Silica pond
                          T,F, # Ohio pwr dam 2
                          T,F, # Muzzys lk2
                          T,F, # bradford lk 2
                          T,F, # blanchester lk 1
                          F,T, # Round Lk
                          F,T, # Little crawlingstone
                          T,   # west plum
                          F,T, # Big Carr
                          F,   # west plum
                          T,F) # lwr neemahbin) 
   , ]


#now remove those badmatches from LInk
Link <- Link[!a[match_inidcator==F], on = .NATURAL ]
rm(a,mults)

# no more multiple nhd_ids for a given wqp_MLI
Link[ , length(unique(lake_nhdid)) , wqp_monitoringlocationidentifier ][V1>1]

Link[ , length(unique(lagoslakeid)) , .(wqp_monitoringlocationidentifier,lake_nhdid) ][V1>1]

lake_idents <- Link[ , .("n_records_in_lake_link" = .N) , .(wqp_monitoringlocationidentifier,lagoslakeid, lake_nhdid, lake_namelagos)]


# Join to lake ident keys  -----------------------------------------------------------

# Originally, this join resulted in duplicates for various reasons, including duplicating monitoring location identifiers because of multiple nhdplusv2_comid, lagosne_lagoslakeid per monitoringlocation identifier (the previously included select function did not operate like distinct(), but instead was simply dropping other cols I think). I worked on this chunk for a long time, an now I think that the joins of Link to LakeDepthArea and that to WQP_metadata are clean (but see note on join to WQP_metdata).  Now we have specified that a multiple match should nab only the first match (but note that that is not a very specific way to execute a join).


#WQP
#Connect "lake areas to the metadata from WQP:

MN_profiles[lake_idents, on = .(MonitoringLocationIdentifier = wqp_monitoringlocationidentifier), ':=' ("link_nhd_id" = lake_nhdid, "link_lagosid" = lagoslakeid)   ]

#summarize completeness
MN_profiles[ , .N , is.na(link_nhd_id) ] #438k obs without an NHD ident

#use mwlaxeref crosswalk to backfill nhdhr IDs from the USGS data team's crosswalks
mwlaxeref::mn_to_nhdhr(MN_profiles, "DOW") %>% 
  {MN_profiles <<- .}

MN_profiles[ , .N , .(nhdhr.id, link_nhd_id)][ !is.na(nhdhr.id) & 
               !is.na(link_nhd_id) , .("match" = link_nhd_id == nhdhr.id) , ][ ,.N , match] # most of the nhds match (recall that the nhd ids from lake_link are of an unknown source)

#these remaining ones could be chased down further, including via lat/long point to polygon spatial join. 
MN_profiles[is.na(link_nhd_id)==T & is.na(nhdhr.id), .(AGENCY, MonitoringLocationIdentifier, MonitoringLocationName) , ]

MN_profiles[ , .N , .(Latitude, Longitude) ]
#recover lat/lon where it baked into the WQP metadata
MN_profiles[WQP_metadata, on = .(MonitoringLocationIdentifier), `:=` ("Latitude" = LatitudeMeasure, "Longitude" = LongitudeMeasure)  ]

#green lake lat/lon from Heidi (we jeust squashed it, so manually re-adding here.)
MN_profiles[DOW == "34007900", `:=` (Latitude = 45.2521,
                                     Longitude = -94.9044 ) ,  ]


MN_profiles[is.na(link_nhd_id)==T & is.na(nhdhr.id) & is.na(Latitude), .N, .(MonitoringLocationIdentifier, MonitoringLocationName, DOW) , ] #254 stations with no spatial info



# Join to lake size data --------------------------------------------------

## join to lake size info
MN_profiles[LakeDepthArea, on = .(link_lagosid = lagoslakeid), ':=' ("lake_GR"= GR,
                                                                          "lake_area_m2" = lake_area_m2,
                                                                          "lake_waterarea_ha" = lake_waterarea_ha,
                                                                          "lake_maxdepth_m" = lake_maxdepth_m
                                                                          )  ]






# Summaize and Basic QC ---------------------------------------------------


#summarize that product
glimpse(MN_profiles)

MN_profiles[ , .N , Profile ]
  MN_profiles[is.na(Profile), .N, DOW]
  MN_profiles[ , .N , .(is.na(MonitoringLocationIdentifier), is.na(Year), is.na(DOY)) ]
  MN_profiles[ , Profile := paste(MonitoringLocationIdentifier,Year, DOY, sep = "/") , ]#fix up Profile names
  MN_profiles[ , .N , Profile ][N<50 , hist(N, breaks = 1:50) ,]

#more than 50 obs within a profile?? Seems okay in review of those data   
MN_profiles[ , .N , Profile ][N>50, Profile]
a <- MN_profiles[Profile%in%MN_profiles[ , .N , Profile ][N>50, Profile]]  
  head(a)
  rm(a)

#missingness in spatial data or location keys
MN_profiles[ , .N ,.("noDOW" = is.na(DOW), "noLAGOSnhdid" = is.na(link_nhd_id), "no_usgs_nhdid" = is.na(nhdhr.id), "nolat" = is.na(Latitude), "nolong" = is.na(Longitude))]   

MN_profiles[ ,Max_Depth:=NULL , ]

# Tidy up the column names
oldnames = c("DOW", "nhdhr.id", "MonitoringLocationIdentifier", "MonitoringLocationName", 
             "Year", "DOY", "Profile", "DATE_clean", "DO_FLAG", "DOperc_FLAG", 
             "TEMP_FLAG", "measurecount_DOppm", "measurecount_DOpercsat", 
             "measurecount_temp", "DEPTH_M", "TEMP_C", "DO_PPM", "DO_perc_sat", 
             "ndep", "AGENCY", "DEPTH_DIF_FLAG", "Latitude", "Longitude", 
             "link_nhd_id", "link_lagosid", "lake_GR", "lake_area_m2", 
             "lake_waterarea_ha", "lake_maxdepth_m")
newnames = c("dow", "usgs_nhdhr.id", "monitoringlocationidentifier", "monitoringlocationname", 
               "year", "doy", "profile_ident", "date_clean", "flag_do", "falg_doperc", 
               "flag_temp", "measurecount_doppm", "measurecount_dopercsat", 
               "measurecount_temp", "depth_m", "temp_c", "do_ppm", "do_perc_sat", 
               "n_dep_inprofile", "agency", "flag_dep_diff", "latitude", "longitude", 
               "link_nhd_id", "link_lagosid", "lake_geomratio", "lake_area_m2", 
               "lake_watershdarea_ha", "lake_maxdepth_m")


setnames(MN_profiles,
         old = oldnames,
         new = newnames
         )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

