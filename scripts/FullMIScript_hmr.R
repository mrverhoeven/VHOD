#MGLP Project
#Jacob Angus

#  Script for Michigan that can be run as a job
#  It can also be run in individual sections

library("tidyverse")
# library("lubridate")
library("zoo")
library("compiler")
library("rLakeAnalyzer")
library("readxl")
library("skimr")

setwd("C:/Users/herantal/OneDrive - State of Minnesota - MN365/Documents/Coldwater_MGLP2020/Jacob")

##### Data Cleaning, Preparation, and Joining ==================================
#  The goal of this section of the script is to filter lakes for
#  geometric ratio, combine data sets with metadata, and join Minnesota
#  data sets together.  
#  Run time 

#  Load files
#  WQP
allMI.do = read.csv("C:/Users/herantal/OneDrive - State of Minnesota - MN365/Documents/Coldwater_MGLP2020/Jacob/Michigan.Data/Profiles/MIDataRetrievalCombined.csv",
                    fileEncoding = "latin1") #All MI Observations of Temp and DO from 1940 to 2020
MI.allsites = read_csv("./Michigan.Data/Metadata/MIDataRetrievalMetadata.csv") #Site data for all the observations above
LakeDepthArea = read_csv("./Michigan.Data/Metadata/lake_depth.csv") #Dataset containing the lake depth and area for all lakes in the US
Link = read_csv("./Michigan.Data/Metadata/lake_link.csv") #Dataset that can link the Lagos dataset to the STORET dataset



#  DNR Data
MI.DNR = read_csv("./Michigan.Data/Profiles/MichiganProfiles.csv")
names(MI.DNR) = c("Lake.Name","County", "Date.Sampled",
               "CONCATENATE", "Latitude", "Longitude", "STORETID",
               "Township", "Section", "Site.ID", "Watershed", "Surface.Area",
               "Datum", "GPS.Source", "Collecting.Organization","Nothing", "Time.Sampled",
               "Weather.Conditions", "Sampling.Depth..feet.", "Meter.Type",
               "Meter.ID", "Calibration.DO...air.saturation.", "Calibration.Temp...C.",
               "Lake.Altitude.Value", "Unusual.Conditions", "Comments", "Depth..feet.",
               "Temp...C.", "DO.Level..mg.L.","Tier")
MI.DNR = MI.DNR %>%
  mutate(MonitoringLocationIdentifier = paste("21MICH", STORETID, sep = "-"))%>%
  select(!c(Latitude, Longitude))



### Filter Metadata
#  WQP
#  To remove lakes that don't stratify
MI.LakeDepthArea = LakeDepthArea %>% 
  filter(lake_states == "MI")%>% #We are only looking at MI lakes right now
  mutate(lake_area_m2 = lake_waterarea_ha * 10000,
         Max_Depth = lake_maxdepth_m)%>% #Fang and Stefan 2009 uses the As^0.25:Z with As in square meters
  mutate(GR = (lake_area_m2^0.25)/Max_Depth)%>%
  filter(GR < 4) #Lakes with a GR greater than 4 do not stratify
#  Connect it all together
MI.WQP = Link %>%
  select(c("lagoslakeid","wqp_monitoringlocationidentifier")) %>% #get rid of extra columns in the data to keep it simpler
  inner_join(MI.LakeDepthArea, by = "lagoslakeid") %>% #lagoslakeid also contains all the basins for lakes
  mutate(MonitoringLocationIdentifier = wqp_monitoringlocationidentifier) %>% #Making the metadata files share columns
  inner_join(MI.allsites, by = "MonitoringLocationIdentifier") %>% #Joining by wqp monitoring location identifier
  inner_join(allMI.do, by = "MonitoringLocationIdentifier") #Join to Observation


#  Prepare for joining of the Michigan Data
#  Convert from feet to meters
MI.feet.WQP = MI.WQP %>% 
  filter(ActivityDepthHeightMeasure.MeasureUnitCode == "feet"|
           ActivityDepthHeightMeasure.MeasureUnitCode == "ft")%>%
  mutate(Depth = round(ActivityDepthHeightMeasure.MeasureValue * 0.3038))
#  Join them together
MI.meters.WQP = MI.WQP %>%
  filter(ActivityDepthHeightMeasure.MeasureUnitCode == "m" |
           ActivityDepthHeightMeasure.MeasureUnitCode == "meters") %>%
  mutate(Depth = round(ActivityDepthHeightMeasure.MeasureValue))%>%
  full_join(MI.feet.WQP)%>%
  select(!ActivityDepthHeightMeasure.MeasureUnitCode)

#  Convert Fahrenheit to Celsius
MI.fah.WQP = MI.meters.WQP %>% 
  filter(CharacteristicName == "Temperature, water")%>%
  filter(ResultMeasure.MeasureUnitCode == "deg F")%>%
  mutate(Temperature = (ResultMeasureValue - 32) * 5/9)
#  Join them together and get proper columns
MI.cel.WQP = MI.meters.WQP %>% 
  filter(CharacteristicName == "Temperature, water")%>%
  filter(ResultMeasure.MeasureUnitCode == "deg C")%>%
  mutate(Temperature = ResultMeasureValue)%>%
  full_join(MI.fah.WQP) %>%
  mutate(Latitude = lake_lat_decdeg                                    ,
         Longitude = lake_lon_decdeg                                   ,
         Max_Depth = round(lake_maxdepth_m)                            ,
         MonitoringLocationIdentifier                                  ,
         Date = ActivityStartDate                                      ,
         DOY = yday(Date),
         Year = year(Date))%>%
  select(c(MonitoringLocationIdentifier, Year, Date, DOY, 
           Depth, Temperature, Latitude, Longitude, Max_Depth))

#  Separate DO from Temperature, then pull them back together
MI.WQP.join = MI.meters.WQP %>%
  filter(ResultMeasure.MeasureUnitCode == "mg/l")%>%
  mutate(DO = ResultMeasureValue,
         Latitude = lake_lat_decdeg                                    ,
         Longitude = lake_lon_decdeg                                   ,
         Max_Depth = round(lake_maxdepth_m)                            ,
         MonitoringLocationIdentifier                                  ,
         Date = ActivityStartDate                                      ,
         DOY = yday(Date),
         Year = year(Date)) %>%
  select(c(MonitoringLocationIdentifier, Year, Date, DOY, 
           Depth, DO, Latitude, Longitude, Max_Depth)) %>%
  inner_join(MI.cel.WQP) %>%
  group_by(MonitoringLocationIdentifier, Year, Date, DOY, 
           Depth, Latitude, Longitude, Max_Depth)%>%
  summarise(DO = median(DO), #This will remove duplicates
            Temperature = median(Temperature))%>%
  ungroup()

#Clean
rm(MI.allsites, MI.cel.WQP, MI.fah.WQP, MI.feet.WQP,
   MI.meters.WQP, MI.WQP, allMI.do)
gc()

# Data Summary #7269 obs
# MI.WQP.join %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #42 sites
# MI.WQP.join %>%
#   group_by(Year)%>%
#   summarise(n = n()) #17 Years
# MI.WQP.join %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #136 Site-Years
# MI.WQP.join %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #649 Profiles




#DNR 
#  To remove lakes that don't stratify
MI.LakeDepthArea = LakeDepthArea %>% 
  filter(lake_states == "MI")%>% #We are only looking at MI lakes right now
  mutate(lake_area_m2 = lake_waterarea_ha * 10000,
         Max_Depth = lake_maxdepth_m)%>% #Fang and Stefan 2009 uses the As^0.25:Z with As in square meters
  mutate(GR = (lake_area_m2^0.25)/Max_Depth)%>%
  filter(GR < 4) #Lakes with a GR greater than 4 do not stratify
#  Connect it all together
MI.DNR.META = Link %>%
  select(c("lagoslakeid","wqp_monitoringlocationidentifier")) %>% #get rid of extra columns in the data to keep it simpler
  inner_join(MI.LakeDepthArea, by = "lagoslakeid") %>% #lagoslakeid also contains all the basins for lakes
  mutate(MonitoringLocationIdentifier = wqp_monitoringlocationidentifier,
         Latitude = as.double(lake_lat_decdeg),
         Longitude = as.double(lake_lon_decdeg)) %>%#Making the files share columns
  distinct(MonitoringLocationIdentifier, .keep_all = T)%>%
  select(c(MonitoringLocationIdentifier, Max_Depth, Latitude, Longitude))%>%
  inner_join(MI.DNR, by = "MonitoringLocationIdentifier") %>% #Join to observations
  mutate(Depth = round(Depth..feet. * 0.3038),
         Temperature = Temp...C.,
         DO = DO.Level..mg.L.,
         Date = ymd(Date.Sampled),
         Year = year(Date),
         DOY = yday(Date))%>%
  select(c(MonitoringLocationIdentifier, Date, Year, DOY, 
           Depth, Temperature, DO, Max_Depth, Latitude, Longitude))

#Clean
rm(Link, LakeDepthArea, MI.LakeDepthArea, MI.DNR)
gc()

####Join the Complete Observations Together
MI.WQP.join$Date<-as.Date(MI.WQP.join$Date, format="Y-m-d")
MI.ALL.Data = MI.WQP.join %>% 
  full_join(MI.DNR.META)%>%
  mutate(Max_Depth = round(Max_Depth))%>%
  group_by(MonitoringLocationIdentifier, Date)%>%
  distinct(Depth, .keep_all = T)%>%
  ungroup()
  

# Data Summary #60949 obs
# MI.ALL.Data %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #143 sites
# MI.ALL.Data %>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# MI.ALL.Data %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #677 Site-Years
# MI.ALL.Data %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #4,417 Profiles



#write_csv(MI.ALL.Data, "./Michigan.Data/MI.ALL.DATA.csv")


#Clean
rm(MI.DNR.META, MI.WQP.join)
gc()


###### Filtering the Data in Preparation for VHOD Calculations =================
#  The goal of this section is to filter the data by several factors:
#  Lakes must have a profile at the start of stratification (121 <= DOY <= 166)
#  Lakes must have a profile at the end of stratification (196 <= DOY <= 258)
#  Profiles must be taken from glacial lakes
#  Profiles will be interpolated at 1m depths 
#  Data points where DO < 2 mg/L will be discarded due to non-linearity 
#  Run time is about 2 minutes

#  Load Data
MI.ALL.Data = read_csv("./Michigan.Data/MI.ALL.DATA.csv")


#Filtering
filter1.MI <- MI.ALL.Data %>% #Remove impossible values ~ These likely came from input error
  mutate(Location_Year = as.character(paste(MonitoringLocationIdentifier,Year, sep = "/")),
         Profile = paste(MonitoringLocationIdentifier, Year, DOY, sep = "/")) %>%
  filter(Temperature < 40) #Jane et al 2021

#  Summary of filter #60934 obs
# filter1.MI %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #143 Lakes
# filter1.MI %>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# filter1.MI %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #677 Lake-Years
# filter1.MI %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #4,414 Profiles

filter2.1MI = filter1.MI %>%            #Profiles must have a minimum depth less than 3 meters; 
  group_by(Profile)%>%                 #otherwise the profiles shallowest read could be in 
  summarise(min.depth = min(Depth))%>% #the metalimnion and capture none of the epilimnion
  filter(min.depth < 3) #Jane et al 2021

filter2.MI = filter1.MI %>%
  semi_join(filter2.1MI, by = "Profile") 

# # Summary #60917 obs
# filter2.MI %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #143 Lakes
# filter2.MI %>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# filter2.MI %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #677 Lake-Years
# filter2.MI %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #4,410 Profiles


#  We want to make sure that lakes were sampled in both the spring post stratification
#  and later in the summer. This ensures that we can calculate VHOD
filter3.1.MI = filter2.MI %>%
  filter(DOY >= 121 &
           DOY <= 258)%>%
  group_by(Year, MonitoringLocationIdentifier)%>% 
  summarise(Early=min(DOY),Late=max(DOY), N=n_distinct(DOY))%>% 
  filter(N>1)%>%
  filter(Early >= 121 & #The early sample must be taken between DOY 121 & 166
           Early <= 166 &
           Late >= 196 & #and this adds that the last sample was taken between DOY 196 and 258
           Late <= 258)%>%
  mutate(Location_Year = as.character(paste(MonitoringLocationIdentifier,Year, sep = "/"))) #11,289 obs of 6 variables (Lost 16917 Lake-Year Combos)

#  Finally, we put it all together
filter3.MI = filter2.MI %>%
  semi_join(filter3.1.MI, by = "Location_Year") %>% 
  filter(DOY >= 121 &
           DOY <= 258) #Removes profiles where it may have been sampled while ice is on or during turnover

# Summary #50494 obs
# filter3.MI %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #108 Sites
# filter3.MI %>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# filter3.MI %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #545 Lake-Years
# filter3.MI %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #3,582 Profiles


filter4.1MI = filter3.MI %>%                #Profiles must have at least 3 reads in them
  group_by(Profile)%>%
  summarise(n_depths = n_distinct(Depth))%>%
  filter(n_depths >= 3) #Jane et al 2021

filter4.MI = filter3.MI %>%
  semi_join(filter4.1MI, by = "Profile")

# #  Summary #50474 obs
# filter4.MI %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #107 Lakes
# filter4.MI %>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# filter4.MI %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #542 Lake-Years
# filter4.MI %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #3,564 Profiles

# Calculate the top and bottom of the metalimnion and remove profiles where it did not work
filter5.MI = filter4.MI %>%
  group_by(Profile)%>%
  mutate(Meta = list(meta.depths(wtr = Temperature, depths = Depth, slope = 0.1, seasonal = F)))%>%
  separate(Meta, into = c("top.meta", "top.hypo"), sep = ",")%>%
  mutate(top.meta = gsub("[^0-9.-]", "", top.meta),
         top.hypo = as.numeric(gsub("[^0-9.-]", "", top.hypo)),
         Depth = as.numeric(Depth))%>%
  drop_na(top.hypo)%>%  #Jane et al 2021
  filter(top.hypo != "0") 

# #Summary #49388 obs
# filter5.MI %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #107 Lakes
# filter5.MI %>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# filter5.MI %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #542 Lake-Years
# filter5.MI %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #3,458 Profiles

#Make a ratio across all lakes of the top of the hypolimnion to the max depth
filter6.1MI = filter5.MI %>%
  group_by(Location_Year)%>%
  filter(DOY == max(DOY))%>%
  distinct(DOY, .keep_all = T)%>%
  mutate(ratio = top.hypo/Max_Depth)%>%
  filter(ratio < 1)    #Remove profiles where the ratio is greater than one (Also would mean that the meta depths function didn't work)
#Extract the median of these ratios
ratio = skim(filter6.1MI$ratio)%>%
  pull(numeric.p50)

filter6.MI = filter5.MI %>%
  semi_join(filter6.1MI, by = "Location_Year")

# #Summary #47740 obs
# filter6.MI %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #104 Lakes
# filter6.MI %>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# filter6.MI %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #468 Lake-Years
# filter6.MI %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #3,341 Profiles

#See that each lake has a profile depth in the median hypolimnion
filter7.MI = filter6.MI %>%
  group_by(Profile)%>%
  mutate(Max_Profile = max(Depth))%>%
  filter(Max_Profile > Max_Depth * (ratio)) 

# #Summary #46604 obs
# filter7.MI %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #100 Lakes
# filter7.MI %>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# filter7.MI %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #501 Lake-Years
# filter7.MI %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #3,176 Profiles


# Check start and end date again after losing profiles
filter8.1MI = filter7.MI %>%
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
MI.Filtered.Obs = filter7.MI %>%
  semi_join(filter8.1MI, by = "Location_Year")%>%
  mutate(ID = paste(MonitoringLocationIdentifier, Year, DOY, Max_Depth, sep = "/"))%>%
  ungroup()%>%
  mutate(Depth = floor(Depth))%>%
  group_by(ID, MonitoringLocationIdentifier,Year,DOY,Max_Depth,Latitude,
           Longitude, Date, Depth)%>%
  summarise(DO = median(DO),
            Temperature = median(Temperature))%>%
  ungroup()


# #Summary #46054 obs
# MI.Filtered.Obs %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #98 Lakes
# MI.Filtered.Obs %>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# MI.Filtered.Obs %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #483 Lake-Years
# MI.Filtered.Obs %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #3,133 Profiles
#write.csv(MI.Filtered.Obs, "MI.Filtered.Obs.csv")

#  Clean
rm(filter1.MI, filter2.MI, filter3.MI,filter4.MI,filter5.MI, filter6.MI,
   filter7.MI, filter2.1MI, filter3.1.MI, filter4.1MI,
   filter6.1MI, filter8.1MI, ratio)
gc()
###### Interpolate the Data Frame ===============================================
#  In this section linear interpolation is preformed on DO and Temperature 
#  observations. Then, the interpolation are rejoined to the metadata before
#  being sent off to calculate AHOD and VHOD
#  Run time of about 2 minutes
MI.Filtered.Obs<-read.csv("MI.Filtered.Obs.csv",header=TRUE)
### The next step is to interpolate data at 1 m increments
Depth.df = tibble(Depth = 0:35) #This creates a data frame to make sure we have data for the bottom of larger lakes
#  First DO
#  Pivot Wide
Wide.MI.DO.Obs = MI.Filtered.Obs %>%
  mutate(ID = paste(MonitoringLocationIdentifier, Year, DOY, Max_Depth, sep = "/"))%>%
  select(ID, Depth, DO) %>%
  pivot_wider(names_from = ID, values_from = DO, values_fn = mean)%>%
  full_join(Depth.df)

#  Interpolate
Wide.Inter.MI.DO = na.approx(Wide.MI.DO.Obs, x = Wide.MI.DO.Obs$Depth)%>% #interpolates data
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()

#  Pivot Long
Long.Inter.MI.DO = Wide.Inter.MI.DO %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "DO", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  filter(Depth <= Max_Depth)



### Next is Temperature
#  Pivot Wide
Wide.MI.Temp.Obs = MI.Filtered.Obs %>%
  mutate(ID = paste(MonitoringLocationIdentifier, Year, DOY, Max_Depth, sep = "/"))%>%
  select(ID, Depth, Temperature) %>%
  pivot_wider(names_from = ID, values_from = Temperature, values_fn = mean)%>%
  full_join(Depth.df)

#  Interpolate
Wide.Inter.MI.Temp = na.approx(Wide.MI.Temp.Obs, x = Wide.MI.Temp.Obs$Depth)%>% #interpolates data - 75 was chosen to make sure every lake was fully interpolated
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()

#  Pivot Long
Long.Inter.MI.Temp = Wide.Inter.MI.Temp %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "Temperature", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  filter(Depth <= Max_Depth)

### Join our results together
MI.joined.inter = inner_join(Long.Inter.MI.Temp, Long.Inter.MI.DO)

#  We need to get the Lat and Lon
MI.Depth.Specific = MI.Filtered.Obs %>%
  distinct(MonitoringLocationIdentifier, .keep_all = T)%>%
  select(c(MonitoringLocationIdentifier, Latitude, Longitude))%>%
  inner_join(MI.joined.inter)

#  Summary #54100 obs
# MI.Depth.Specific %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n=n()) #98 Lakes
# MI.Depth.Specific %>%
#   group_by(Year)%>%
#   summarise(n=n()) #22 Years
# MI.Depth.Specific %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n=n()) #483 Lake-Years
# MI.Depth.Specific %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n=n()) #3,133 Profiles


#Clean up 
rm(MI.joined.inter,Long.Inter.MI.Temp,Wide.Inter.MI.Temp,Depth.df,
   Wide.MI.Temp.Obs,Long.Inter.MI.DO,Wide.Inter.MI.DO,Wide.MI.DO.Obs)
gc()


###  Next we will thermofilter the data set

MI.DS.Thermo = MI.Depth.Specific %>%
  group_by(MonitoringLocationIdentifier, Year, DOY)%>%
  mutate(Meta = list(meta.depths(wtr = Temperature, depths = Depth, slope = 0.1, seasonal = F)))%>% #Following the methods of Jane et al, 2021
  separate(Meta, into = c("top.meta", "top.hypo"), sep = ",")%>%
  mutate(top.meta = gsub("[^0-9.-]", "", top.meta),
         top.hypo = as.numeric(gsub("[^0-9.-]", "", top.hypo)),
         Depth = as.numeric(Depth))

#  Summary # 54100 obs
# MI.DS.Thermo %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n=n()) #98 Lakes
# MI.DS.Thermo %>%
#   group_by(Year)%>%
#   summarise(n=n()) #22 Years
# MI.DS.Thermo %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n=n()) #483 Lake-Years
# MI.DS.Thermo %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n=n()) #3,133 Profiles

### Select for just the hypolimnion of lakes
MI.DS.Hypo.prefilter = MI.DS.Thermo %>%
  filter(Depth > top.hypo)%>%
  mutate(Location_Year = as.character(paste(MonitoringLocationIdentifier,Year, sep = "/")))
# Re-do the date filter just to make sure
MI.DS.Hypo.filter = MI.DS.Hypo.prefilter %>%
  filter(DOY >= 121 &
           DOY <= 258)%>%
  group_by(Year, MonitoringLocationIdentifier)%>% 
  summarise(Early=min(DOY),Late=max(DOY), N=n_distinct(DOY))%>% 
  filter(N>1)%>%
  filter(Early >= 121 & 
           Early <= 166 &
           Late >= 196 & #and this adds that the last sample was taken between DOY 196 and 258
           Late <= 258)%>%
  mutate(Location_Year = as.character(paste(MonitoringLocationIdentifier,Year, sep = "/"))) #11,289 obs of 6 variables (Lost 16917 Lake-Year Combos)

#  Finally, we put it all together
MI.DS.Hypo = MI.DS.Hypo.prefilter %>%
  semi_join(MI.DS.Hypo.filter, by = "Location_Year") %>% 
  filter(DOY >= 121 &
           DOY <= 258) %>%
  filter(DO > 2)%>%
  mutate(DOY = as.numeric(DOY))


# #  Data Summary #23471 obs
# MI.DS.Hypo %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n=n()) #93 Lakes
# MI.DS.Hypo %>%
#   group_by(Year)%>%
#   summarise(n=n()) #22 Years
# MI.DS.Hypo %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n=n()) #442 Lake-Years
# MI.DS.Hypo %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n=n()) #1,904 Profiles

#write.csv(MI.DS.Hypo,"MI.DS.Hypo.csv")
#Clean
rm(MI.Depth.Specific, Random.sample, MI.presample, MI.DS.Hypo.filter, MI.DS.Hypo.prefilter)
gc()

###### Calculating AHOD and VHOD ===============================================

#  Here we will first calculate AHOD, 
#  Then using hyspography data we will calculate VHOD
#  This code could be written cleaner, but I am not sure how to use some of the functions
#  Run time of about 2 minutes (Haven't calculated with bathymetry data)

### Calculating DS Oxygen Depletion
### Crafting a Linear Model

#MI.DS.Hypo<-read.csv("MI.DS.Hypo.csv", header = TRUE, sep=",")
#  Reworking an ID column
MI.AHOD = MI.DS.Hypo %>%
  filter(DO >= 2)%>% #DO does not behave linearly below 2mg/L
  select(!ID)%>%
  mutate(ID = paste(MonitoringLocationIdentifier, Year, Depth, sep = "/"))

#  Create a Linear Model
MI.DO.LM = with(MI.AHOD,
                by(MI.AHOD, INDICES = ID, #The indices allows for it to broken up by lake, year, and depth
                   function(x) lm(DO~DOY, data = x)))
#  Extract Coefficients
MI.do.coef = sapply(MI.DO.LM, coef)
#  Transpose and tidy it up
MI.do.tod = cbind(as.character(colnames(MI.do.coef)),data.frame(t(MI.do.coef),row.names=NULL))
colnames(MI.do.tod) = c("ID","Intercept","TOD") #What does TOD stand for? Total oxygen depletion? I think this value is equivalient to AHOD
MI.do.tod = MI.do.tod %>%
  separate(ID, into = c("MonitoringLocationIdentifier","Year", "Depth"), sep = "/")%>%
  drop_na(TOD)%>%
  mutate(Depth = as.double(Depth))%>%
  filter(MonitoringLocationIdentifier != "LRBOI_WQX-TPine")%>% #Tippy Pond
  filter(MonitoringLocationIdentifier != "LRBOI-TPine")%>% #Tippy Pond
  filter(MonitoringLocationIdentifier != "LRBOI-MANS")%>% #Manistee Lake
  filter(MonitoringLocationIdentifier != "LRBOI-TDAM")%>% #Tippy Pond
  filter(MonitoringLocationIdentifier != "LRBOI-TMAN")%>% #Tippy Pond
  filter(MonitoringLocationIdentifier != "LRBOI_WQX-TDam")%>% #Tippy Pond
  filter(MonitoringLocationIdentifier != "21MICH-720028")%>% #Tippy Pond
  filter(MonitoringLocationIdentifier != "LRBOI-TMAN")%>% #Tippy Pond
  filter(MonitoringLocationIdentifier != "LRBOI-TPINE")%>% #Tippy Pond
  filter(MonitoringLocationIdentifier != "LRBOI_WQX-TMan")  #Tippy Pond
#Summary #4525 obs###heidi got 3974 at this step
# MI.do.tod %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n=n())#84 Lakes
# MI.do.tod %>%
#   group_by(Year)%>%
#   summarise(n=n()) #22 Years
# MI.do.tod %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n=n()) #363 Lake-Years

### Add Bathymetry/Hypsography Data
#Load for metadata
MI.DNR = read_csv("./Michigan.Data/Profiles/MichiganProfiles.csv")
names(MI.DNR) = c("Lake.Name","County", "Date.Sampled",
                  "CONCATENATE", "Latitude", "Longitude", "STORETID",
                  "Township", "Section", "Site.ID", "Watershed", "Surface.Area",
                  "Datum", "GPS.Source", "Collecting.Organization","Nothing", "Time.Sampled",
                  "Weather.Conditions", "Sampling.Depth..feet.", "Meter.Type",
                  "Meter.ID", "Calibration.DO...air.saturation.", "Calibration.Temp...C.",
                  "Lake.Altitude.Value", "Unusual.Conditions", "Comments", "Depth..feet.",
                  "Temp...C.", "DO.Level..mg.L.","Tier")
MI.DNR = MI.DNR %>%
  mutate(MonitoringLocationIdentifier = paste("21MICH", STORETID, sep = "-"))%>%
  select(!c(Latitude, Longitude))

MI.DNR.distinct = MI.DNR%>%
  distinct(MonitoringLocationIdentifier, .keep_all = T)

#Bath.Need = MI.do.tod %>%
  #left_join(MI.DNR.distinct, by = "MonitoringLocationIdentifier")%>%
 # group_by(MonitoringLocationIdentifier)%>%
  #summarise(n = n(),
      #      Lake = unique(Lake.Name),
      #      Town = unique(Township))

#write_csv(Bath.Need, "./Michigan.Data/MI.BathmetryNeed.csv")

#  load bathymetry data
MI.Bath = read.csv("MI.Bath.csv")

#Filter the Bath
MI.Bath.MonitoringLocationIdentifier = MI.Bath %>%
  mutate(Depth = floor(depth_m))%>%
  group_by(Depth, MonitoringLocationIdentifier)%>%
  summarise(area = mean(area_m2))%>%
  ungroup(Depth)

MI.Bath.MonitoringLocationIdentifier = MI.Bath.MonitoringLocationIdentifier[order(MI.Bath.MonitoringLocationIdentifier$MonitoringLocationIdentifier),]

#  Calculate Alpha value 
#Rantala note: alpha is the layer-specific scaling variable related to the amount of contact the layer has wiht the sediment
MI.Bath.MonitoringLocationIdentifier$DiffDepth<-0 #fill with dummy values so diff does not bomb
MI.Bath.MonitoringLocationIdentifier$DiffDepth[1:nrow(MI.Bath.MonitoringLocationIdentifier)-1]<-diff(MI.Bath.MonitoringLocationIdentifier$Depth)
MI.Bath.MonitoringLocationIdentifier$DiffArea <- 0
MI.Bath.MonitoringLocationIdentifier$DiffArea[1:nrow(MI.Bath.MonitoringLocationIdentifier)-1]<-
  diff(MI.Bath.MonitoringLocationIdentifier$area)
MI.Bath.MonitoringLocationIdentifier = MI.Bath.MonitoringLocationIdentifier %>%
  mutate(alpha = -1*(DiffArea/DiffDepth)/area)%>%
  filter(DiffDepth > 0&alpha<=1&alpha>0)#put constraints on alpha

#  Calculate Depth Specific Volume
MI.Bath.MonitoringLocationIdentifier = MI.Bath.MonitoringLocationIdentifier %>%
  group_by(MonitoringLocationIdentifier)%>%
  mutate(Previousarea = data.table :: shift(area, type = c("lead")),
         Vol = (DiffDepth/3)*(area + Previousarea+(area*Previousarea)^0.5),
         Previousarea = coalesce(Previousarea, 0),
         Vol = coalesce(Vol, 0))


#Join the Bathymetry with the TOD
OD <- na.omit(inner_join(MI.Bath.MonitoringLocationIdentifier,MI.do.tod))

# #  Summary # 439 obs
# OD%>%
  # group_by(MonitoringLocationIdentifier)%>%
 #  summarise(n = n()) #35 Lakes
# OD%>%
  # group_by(Year)%>%
 # summarise(n = n()) #21 Years
# OD%>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #148 Lake-Year

#  Add interpolated temperatures to the depth-specific data
MI.DS.Thermo.ag = MI.DS.Thermo %>%
  group_by(MonitoringLocationIdentifier, Year, Depth)%>%
  summarise(Temperature = mean(Temperature))
DS.MI<-inner_join(OD,MI.DS.Thermo.ag)
DS.MI$DOY_0<-(0-DS.MI$Intercept)/DS.MI$TOD #day of year when DO hits 0
DS.MI$DOY_3<-(3-DS.MI$Intercept)/DS.MI$TOD #day of year when DO hits 3


#  Calculate entire hypoliMIion volume-weighted values 
VW_MI = DS.MI %>%
  group_by(MonitoringLocationIdentifier, Year)%>%
  summarise(VW_TOD = sum(TOD*Vol)/sum(Vol),
            VW_Temp = sum(Temperature*Vol)/sum(Vol),
            VW_Intercept = sum(Intercept*Vol)/sum(Vol))%>%
  mutate(VW_DOY_0 = (0-VW_Intercept)/VW_TOD,
         VW_DOY_3 = (3-VW_Intercept)/VW_TOD)


#  Add Lat and Long back into the data set
VW_MI.Meta = MI.DS.Thermo %>%
  ungroup()%>%
  distinct(MonitoringLocationIdentifier, .keep_all = T)%>%
  select(MonitoringLocationIdentifier, Latitude, Longitude)%>%
  inner_join(VW_MI)

#### Inspection for positive values
#  Many of the lake years with positive values appear to have turned over
#  earlier than normal for the year. We'll filter the last sample date out and
#  calculate VHOD with what is left
Positive.Vals = VW_MI.Meta %>%#9 observations
  filter(VW_TOD > 0)

PosTOD=MI.AHOD %>%
  semi_join(Positive.Vals)%>%
  group_by(MonitoringLocationIdentifier, Year)%>%
  mutate(Profile = paste(MonitoringLocationIdentifier, Year, DOY))

MI.AHOD.Neg = MI.AHOD%>%
  group_by(MonitoringLocationIdentifier, Year)%>%
  mutate(MaxDOY = max(DOY),
         Profile = paste(MonitoringLocationIdentifier, Year, DOY))%>%
  ungroup()%>%
  anti_join(PosTOD)

MI.AHOD.Pos = MI.AHOD%>%
  group_by(MonitoringLocationIdentifier, Year)%>%
  mutate(MaxDOY = max(DOY),
         Profile = paste(MonitoringLocationIdentifier, Year, DOY))%>%
  semi_join(PosTOD)%>%
  filter(MaxDOY != DOY)%>%
  ungroup

MI.AHOD.Pos.Filter = MI.AHOD.Pos %>%
  group_by(MonitoringLocationIdentifier, Year)%>%
  summarise(n = n_distinct(DOY))%>%
  ungroup()%>%
  filter(n > 1)

MI.AHOD.Filtered = MI.AHOD.Pos %>%
  semi_join(MI.AHOD.Pos.Filter)%>%
  full_join(MI.AHOD.Neg)

#Clean
rm(MI.DO.LM, MI.do.tod, MI.AHOD, MI.DS.Thermo.ag, MI.DS.Hypo.prefilter.1, 
   MI.DS.Hypo, MI.do.coef, MI.Bath, MI.AHOD.Pos.Filter,
   MI.AHOD.Pos, MI.AHOD.Neg, Positive.Vals,
   PosTOD, VW_MI, VW_MI.Meta, OD, DS.MI, Bath.Need)
gc()

#write.csv(MI.AHOD.Filtered, "MI.AHOD.Filtered.csv")
### Now we have removed the last day of season where turnover happened early
### Let's redo for the actual VHOD
#  Create a Linear Model
#MI.AHOD.Filtered<-read.csv("MI.AHOD.Filtered.csv",header = TRUE,sep=",")
MI.DO.LM = with(MI.AHOD.Filtered,
                by(MI.AHOD.Filtered, INDICES = ID, #The indices allows for it to broken up by lake, year, and depth
                   function(x) lm(DO~DOY, data = x)))
#  Extract Coefficients
MI.do.coef = sapply(MI.DO.LM, coef)
#  Transpose and tidy it up
MI.do.tod = cbind(as.character(colnames(MI.do.coef)),data.frame(t(MI.do.coef),row.names=NULL))
colnames(MI.do.tod) = c("ID","Intercept","TOD") 
MI.do.tod = MI.do.tod %>%
  separate(ID, into = c("MonitoringLocationIdentifier","Year","Depth"), sep = "/")%>%
  drop_na(TOD)%>% 
  mutate(Depth = as.double(Depth))

#Data Summary #4517 obs
# MI.do.tod%>%
#   group_by(MonitoringLocationIdentifier)%>%
 #  summarise(n = n()) #84 Lakes
# MI.do.tod%>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# MI.do.tod%>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#  summarise(n = n()) #363 Lake-Year

#Join the Bathymetry with the TOD
OD <- na.omit(inner_join(MI.Bath.MonitoringLocationIdentifier,MI.do.tod))

# #  Summary  #438 observations
# OD%>%
#   group_by(MonitoringLocationIdentifier)%>%
#  summarise(n = n()) #306 Lakes
# OD%>%
#   group_by(Year)%>%
#  summarise(n = n()) #21 Years
# OD%>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #135 Lake-Year

OD.LkYr = OD %>%
  mutate(LkYr = paste(MonitoringLocationIdentifier, Year, sep = "_"))

####seems to be more impossible values here-hmr need to figure that out
#### Inspection for positive values
#  Many of the lake years with positive values appear to have turned over
#  earlier than normal for the year. We'll filter the last sample date out and
#  calculate VHOD with what is left
Positive.Vals = OD.LkYr %>%#40 observations
  filter(TOD > 0)
Postive.Vals2<-Positive.Vals%>%
  group_by(LkYr)%>%
  summarise(n = n())

Negalpha=OD.LkYr %>%#0 observations
  filter(alpha < 0)

###back to Jacob's code
ODlm = with(OD.LkYr,
            by(OD.LkYr, INDICES = LkYr, #The indices allows for it to broken up by lake and year
               function(x) lm(TOD~alpha, data = x)))      
MI.OD.coef = sapply(ODlm, coef)
MI.TOD = cbind(as.character(colnames(MI.OD.coef)),data.frame(t(MI.OD.coef),row.names=NULL))
colnames(MI.TOD) = c("Lake_Year","WOD","SOD")#these are water and sediment oxygen demand. we didn't parcel these out in results
#148 estimates


#  Add interpolated temperatures to the depth-specific data
MI.DS.Thermo.ag = MI.DS.Thermo %>%
  group_by(MonitoringLocationIdentifier, Year, Depth)%>%
  summarise(Temperature = mean(Temperature))
DS.MI<-inner_join(OD,MI.DS.Thermo.ag)
DS.MI$DOY_0<-(0-DS.MI$Intercept)/DS.MI$TOD #day of year when DO hits 0
DS.MI$DOY_3<-(3-DS.MI$Intercept)/DS.MI$TOD #day of year when DO hits 3


#  Calculate entire hypoliMIion volume-weighted values 
VW_MI = DS.MI %>%
  group_by(MonitoringLocationIdentifier, Year)%>%
  summarise(VW_TOD = sum(TOD*Vol)/sum(Vol),
            VW_Temp = sum(Temperature*Vol)/sum(Vol),
            VW_Intercept = sum(Intercept*Vol)/sum(Vol))%>%
  mutate(VW_DOY_0 = (0-VW_Intercept)/VW_TOD,
         VW_DOY_3 = (3-VW_Intercept)/VW_TOD)


#  Add Lat and Long back into the data set
VW_MI.Meta.unfiltered = MI.DS.Thermo %>%
  ungroup()%>%
  distinct(MonitoringLocationIdentifier, .keep_all = T)%>%
  select(MonitoringLocationIdentifier, Latitude, Longitude)%>%
  inner_join(VW_MI)

Pos.obs = VW_MI.Meta.unfiltered %>%
  filter(VW_TOD > 0)%>%
  group_by(MonitoringLocationIdentifier)%>%
  summarise(n = n())

VW_MI.Meta = VW_MI.Meta.unfiltered %>%
  filter(VW_TOD <= 0)

#Data Summary #120 obs
VW_MI.Meta %>%
  group_by(MonitoringLocationIdentifier, Latitude, Longitude)%>%
  summarise(n = n()) #28 Lakes
VW_MI.Meta %>%
  group_by(Year)%>%
  summarise(n = n()) #21 Years
VW_MI.Meta %>%
  group_by(MonitoringLocationIdentifier, Year)%>%
  summarise(n = n()) #120 MonitoringLocationIdentifier Year

#write.csv(VW_MI.Meta,"VW_MI.Meta.csv")
VW_MI.Meta<-read.csv("VW_MI.Meta.csv")

####### Calculating TDO3 =======================================================

#  This section is calculating TDO3 which is the minimum temperature where
#  the DO is 3 mg/L. 3mg/L is considered to be lethal to cold water fish
#  Runtime is less than 1 minutes


#  Load Data
#  ZMEX Data
ZMEX02 = read_csv("./Michigan.Data/Profiles/2002ZMEX_2.csv")
ZMEX03 = read_csv("./Michigan.Data/Profiles/2003ZMEX_2.csv")
ZMEX.Meta = read_csv("./Michigan.Data/Metadata/ZMEX.Meta.csv")%>%
  mutate(Lake.County = paste(Lake, County, sep = "-"))

#  WQP and DNR Data
MI.ALL.Data = read_csv("./Michigan.Data/MI.ALL.DATA.csv")

#  Joining the data
ZMEX02$`Hypo Depth_m`<-as.numeric(ZMEX02$`Hypo Depth_m`)
ZMEX02$`DCM Depth_m`<-as.numeric(ZMEX02$`DCM Depth_m`)

ZMEX.Obs = full_join(ZMEX02, ZMEX03)%>%
  mutate(Lake.County = paste(Lake, County, sep = "-"),
         Temperature = as.numeric(Temp_C),
         Depth = round(as.numeric(Depth_m)),
         Date = dmy(Date),
         DO = as.numeric(DO_ppm),
         Year = year(Date),
         DOY = yday(Date))%>%
  select(c(Lake.County, Date, Depth, Temperature, DO, Year, DOY))%>%
  inner_join(ZMEX.Meta)%>%
  drop_na(Depth, DO, Temperature)%>%
  mutate(Max_Depth = round(Max_Depth.ft * 0.3038),
         Surface.area.m2 = 4046.8564224 * Surfacearea.acres,
         GR = (Surface.area.m2^0.25)/Max_Depth)%>%
  filter(GR<4)%>%
  select(!c(Lake.County, Lake, County, GR, Surface.area.m2, 
            Surfacearea.acres, Max_Depth.ft))%>%
  group_by(Date,Depth ,Year,DOY,Latitude,Longitude,MonitoringLocationIdentifier,Max_Depth)%>%
  summarise(DO = median(DO),
            Temperature = median(DO))%>%
  ungroup()

#  Join to Existing Data
MI.ALL.Data = MI.ALL.Data %>%
  full_join(ZMEX.Obs)

#Filter the dataset
filter1.MI <- MI.ALL.Data %>% #Remove Impossible Values
  mutate(Location_Year = as.character(paste(MonitoringLocationIdentifier,Year, sep = "/")),
         Profile = paste(MonitoringLocationIdentifier, Year, DOY, sep = "/")) %>%
  filter(Temperature < 40) #Jane et al 2021

#  See how much the filtering took away
# filter1.MI %>%  #61,981 obs
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #200 Sample Sites
# filter1.MI %>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# filter1.MI %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #738 Lake-Years
# filter1.MI %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #4,478 Profiles

filter2.1MI = filter1.MI %>%            #Profiles must have a minimum depth less than 3 meters; 
  group_by(Profile)%>%                 #otherwise the profiles shallowest read could be in 
  summarise(min.depth = min(Depth))%>% #the metalimnion and capture none of the epilimnion
  filter(min.depth < 3) #Jane et al 2021

filter2.MI = filter1.MI %>%
  semi_join(filter2.1MI, by = "Profile") 

# # Summary  #61964 obs
# filter2.MI %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #200 Sample Sites
# filter2.MI %>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# filter2.MI %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #738 Lake-Years
# filter2.MI %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #4,474 Profiles

filter3.1MI = filter2.MI %>%
  filter(DOY >= 121 &
           DOY <= 258)%>%
  group_by(Year, MonitoringLocationIdentifier)%>% 
  summarise(Late=max(DOY))%>%
  filter(Late >= 196 & #this adds that the last sample was taken between DOY 196 and 258
           Late <= 258)%>%
  mutate(Location_Year = as.character(paste(MonitoringLocationIdentifier,Year, sep = "/")))
#  Put it all together
filter3.MI = filter2.MI %>%
  semi_join(filter3.1MI, by = "Location_Year") %>% 
  filter(DOY >= 121 &
           DOY <= 258) #Removes profiles where it may have been sampled while ice is on or during turnover

# # Summary  #55251 obs
# filter3.MI %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #197 Sample Sites
# filter3.MI %>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# filter3.MI %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #709 Lake-Years
# filter3.MI %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #3,921 Profiles

filter4.1MI = filter3.MI %>%            #Profiles must have at least 3 reads in them
  group_by(Profile)%>%
  summarise(n_depths = n_distinct(Depth))%>%
  filter(n_depths >= 3) #Jane et al 2021

filter4.MI = filter3.MI %>%
  semi_join(filter4.1MI, by = "Profile")

# Summary  #55231 obs
# filter4.MI %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #196 Site
# filter4.MI %>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# filter4.MI %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #706 Lake-Years
# filter4.MI %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #3,903 Profiles

filter5.MI = filter4.MI %>%
  group_by(Profile)%>%
  mutate(Meta = list(meta.depths(wtr = Temperature, depths = Depth, slope = 0.1, seasonal = F)))%>%
  separate(Meta, into = c("top.meta", "top.hypo"), sep = ",")%>%
  mutate(top.meta = gsub("[^0-9.-]", "", top.meta),
         top.hypo = as.numeric(gsub("[^0-9.-]", "", top.hypo)),
         Depth = as.numeric(Depth))%>%
  drop_na(top.hypo)%>%  #Jane et al 2021
  filter(top.hypo != "0") 

# Summary  #54074 obs
# filter5.MI %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #196 Site
# filter5.MI %>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# filter5.MI %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #706 Lake-Years
# filter5.MI %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #3,784 Profiles

filter6.1MI = filter5.MI %>%
  filter(DOY >= 121 &
           DOY <= 258)%>%
  group_by(Year, MonitoringLocationIdentifier)%>% 
  summarise(Late=max(DOY))%>%
  filter(Late >= 196 & #and this adds that the last sample was taken between DOY 196 and 258
           Late <= 258)%>%
  mutate(Location_Year = as.character(paste(MonitoringLocationIdentifier,Year, sep = "/")))
MI.Filtered.Obs = filter5.MI %>%
  semi_join(filter6.1MI, by = "Location_Year")%>%
  mutate(ID = paste(MonitoringLocationIdentifier, Year, DOY, Max_Depth, sep = "/"))%>%
  filter(MonitoringLocationIdentifier != "")%>%
  filter(MonitoringLocationIdentifier != "LRBOI_WQX-TPine")%>% #Tippy Pond
  filter(MonitoringLocationIdentifier != "LRBOI-TPINE")%>% #Tippy Pond
  filter(MonitoringLocationIdentifier != "LRBOI-MANS")%>% #Manistee Lake
  filter(MonitoringLocationIdentifier != "LRBOI-TDAM")%>% #Tippy Pond
  filter(MonitoringLocationIdentifier != "LRBOI-TMAN")%>% #Tippy Pond
  filter(MonitoringLocationIdentifier != "LRBOI_WQX-TDam")%>% #Tippy Pond
  filter(MonitoringLocationIdentifier != "21MICH-720028")%>% #Tippy Pond
  filter(MonitoringLocationIdentifier != "LRBOI-MANN")%>% #Tippy Pond
  filter(MonitoringLocationIdentifier != "LRBOI-TPINE")%>% #Tippy Pond
  filter(MonitoringLocationIdentifier != "LRBOI_WQX-TMan")%>%  #Tippy Pond
  filter(MonitoringLocationIdentifier != "LK-0050")%>%  #Duplicate Site - Bankson Lake
  filter(MonitoringLocationIdentifier != "LK-0026")%>%  #Duplicate Site - Cass Lake
  filter(MonitoringLocationIdentifier != "LK-0051")%>%  #Duplicate Site - Cedar Lake
  filter(MonitoringLocationIdentifier != "LK-0065")%>%  #Duplicate Site - Chemung Lake
  filter(MonitoringLocationIdentifier != "LK-0047")%>%  #Duplicate Site - Corey Lake
  filter(MonitoringLocationIdentifier != "LK-0057")%>%  #Duplicate Site - Devils Lake
  filter(MonitoringLocationIdentifier != "LK-0016")%>%  #Duplicate Site - Diamond Lake
  filter(MonitoringLocationIdentifier != "LK-0001")%>%  #Duplicate Site - Gull Lake
  filter(MonitoringLocationIdentifier != "21MICH-540093")%>%  #Duplicate Site - horsehead Lake
  filter(MonitoringLocationIdentifier != "21MICH-620276") %>% #Duplicate Site - Kimball Lake
  filter(MonitoringLocationIdentifier != "LK-0049")%>%  #Duplicate Site - Klinger Lake
  filter(MonitoringLocationIdentifier != "LK-0059")%>%  #Duplicate Site - North Lake
  filter(MonitoringLocationIdentifier != "21MICH-810265")%>%  #Duplicate Site - Pleasant Lake - Central Basin
  filter(MonitoringLocationIdentifier != "21MICH-380263")%>%  #Duplicate Site - Vineyard Lake
  filter(MonitoringLocationIdentifier != "NARS_WQX-NLA06608-1450")%>%  #Duplicate Site - Blue Lake
  filter(MonitoringLocationIdentifier != "NARS_WQX-NLA06608-1014")%>%  #Duplicate Site - Chemung Lake
  filter(MonitoringLocationIdentifier != "LK-0046")  #Duplicate Site - Donnell Lake
  


# Summary  #49107 obs
# MI.Filtered.Obs %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #169 Sample Site
# MI.Filtered.Obs %>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# MI.Filtered.Obs %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #605 Lake-Years
# MI.Filtered.Obs %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #3,407 Profiles

#  Select the latest profile
EndofYear = MI.Filtered.Obs %>%
  group_by(MonitoringLocationIdentifier, Year)%>%
  mutate(Max = max(DOY))%>%
  filter(DOY == Max)%>%
  ungroup()

#  Summary  #8607 obs
# EndofYear %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #169 Sample Sites
# EndofYear %>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# EndofYear %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #605 Lake-Years

# MI.DNR = read_csv("MichiganProfiles.csv")
# names(MI.DNR) = c("Lake","County", "Date.Sampled",
#                   "CONCATENATE", "Latitude", "Longitude", "STORETID",
#                   "Township", "Section", "Site.ID", "Watershed", "Surface.Area",
#                   "Datum", "GPS.Source", "Collecting.Organization","Nothing", "Time.Sampled",
#                   "Weather.Conditions", "Sampling.Depth..feet.", "Meter.Type",
#                   "Meter.ID", "Calibration.DO...air.saturation.", "Calibration.Temp...C.",
#                   "Lake.Altitude.Value", "Unusual.Conditions", "Comments", "Depth..feet.",
#                   "Temp...C.", "DO.Level..mg.L.","Tier")
# MI.DNR = MI.DNR %>%
#   mutate(MonitoringLocationIdentifier = paste("21MICH", STORETID, sep = "-"))%>%
#   select(!c(Latitude, Longitude))
# 
# MI.DNR.distinct = MI.DNR%>%
#   distinct(MonitoringLocationIdentifier, .keep_all = T)%>%
#   full_join(ZMEX.Meta)
# 
# df2 = df %>% 
#   left_join(MI.DNR.distinct)%>%
#   select(MonitoringLocationIdentifier, n, Lake, County)%>%
#   arrange(Lake)
# 
# write_csv(df2, "MITDOinspection.csv")
# 
# df3 = read_xlsx("MITDOinspection.xlsx")%>%
#   arrange(Lake)
# 
# write_csv(df3, "MITDOinspection.csv")


#  Clean
rm(filter1.MI, filter2.MI, filter2.1MI, filter3.MI, filter3.1MI,
   filter4.MI, filter4.1MI, filter5.MI, filter6.MI, filter6.1MI,
   filter7.1MI, MI.Filtered.Obs, ZMEX.Meta, ZMEX.Obs, ZMEX02, ZMEX03, MI)
gc()



### The next step is to interpolate data at 1 m increments
Depth.df = tibble(Depth = 0:40)
#  First DO
#  Pivot Wide
Wide.MI.DO.Obs = EndofYear %>%
  mutate(ID = paste(MonitoringLocationIdentifier, Year, DOY, Max_Depth, sep = "/"))%>%
  select(ID, Depth, DO) %>%
  pivot_wider(names_from = ID, values_from = DO, values_fn = mean)%>%
  full_join(Depth.df)

#  Interpolate
Wide.Inter.MI.DO = na.approx(Wide.MI.DO.Obs, x = Wide.MI.DO.Obs$Depth)%>% #interpolates data
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()

#  Pivot Long
Long.Inter.MI.DO = Wide.Inter.MI.DO %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "DO", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  filter(Depth <= Max_Depth)

#Clean
rm(Wide.MI.DO.Obs, Wide.Inter.MI.DO)

### Next is Temperature
#  Pivot Wide
Wide.MI.Temp.Obs = EndofYear %>%
  mutate(ID = paste(MonitoringLocationIdentifier, Year, DOY, Max_Depth, sep = "/"))%>%
  select(ID, Depth, Temperature) %>%
  pivot_wider(names_from = ID, values_from = Temperature, values_fn = mean)%>%
  full_join(Depth.df)

#  Interpolate
Wide.Inter.MI.Temp = na.approx(Wide.MI.Temp.Obs, x = Wide.MI.Temp.Obs$Depth)%>% #interpolates data - 75 was chosen to make sure every lake was fully interpolated
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()

#  Pivot Long
Long.Inter.MI.Temp = Wide.Inter.MI.Temp %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "Temperature", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  filter(Depth <= Max_Depth)

### Join our results together
MI.joined.inter = inner_join(Long.Inter.MI.Temp, Long.Inter.MI.DO)

#  We need to get the Lat and Lon
MI.Depth.Specific = EndofYear %>%
  distinct(MonitoringLocationIdentifier, .keep_all = T)%>%
  select(c(MonitoringLocationIdentifier, Latitude, Longitude))%>%
  inner_join(MI.joined.inter)

###  Next we calculate TDO3
MI.TDO3 = MI.Depth.Specific %>%
  filter(DO >= 3)%>%
  group_by(MonitoringLocationIdentifier, Year)%>%
  summarise(TDO3 = min(Temperature))

#  Add in Lat and Lon
MI.TDO3.Meta = EndofYear %>%
  distinct(MonitoringLocationIdentifier, .keep_all = T)%>%
  select(c(MonitoringLocationIdentifier, Latitude, Longitude))%>%
  inner_join(MI.TDO3)

#Data summary #605 obs
# MI.TDO3.Meta %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #169 Lakes
# MI.TDO3.Meta %>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# MI.TDO3.Meta %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #605 Lake-Years

### Graphical Representation of Data

#  Load Libraries
library("plotly")
require('maps')

#  Read in the polygon data for Michigan
MI.states = map_data("state", region = "Michigan")

# Take out outliers 
MI.TDO3.Meta = MI.TDO3.Meta %>%
  filter(TDO3 < 30)
#  Make the base plot
MI.TDO3.plot = ggplot()+
  geom_polygon(data = MI.states, aes(x = long, y = lat), 
               fill = "white", color = "black", size = 1, alpha = 0.4)+
  geom_point(data = MI.TDO3.Meta, aes(x = Longitude, 
                                      y = Latitude,
                                      color = TDO3, 
                                      frame = Year,
                                      ids = MonitoringLocationIdentifier),
             size = 3)+
  scale_color_gradientn(colors = c("seagreen", "green", "red"))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()

MI.TDO3.Animation = ggplotly(MI.TDO3.plot) %>% 
  animation_opts(mode = "afterall", frame = 750, transition = 500, redraw = F) %>%
  animation_slider(
    currentvalue = list(prefix = "YEAR ", font = list(color="red")))
MI.TDO3.Animation