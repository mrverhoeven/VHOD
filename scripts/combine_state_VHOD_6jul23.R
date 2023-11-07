###combine VHOD estimates for all states
###H Rantala
###21June2023

#set wd
setwd("C:/Users/herantal/OneDrive - State of Minnesota - MN365/Documents/Coldwater_MGLP2020/Jacob")

#load packages
library(tidyverse)

#load VHOD estimates for each state
MN<-read.csv("VW_MN.Meta.csv", header=TRUE, sep=",")
MI<-read.csv("VW_MI.Meta.csv", header=TRUE, sep=",")
WI<-read.csv("./Wisconsin.Data/WI_VHOD.csv",header=TRUE, sep=",")

#and link file for nhd IDs
link<-read.csv("C:/Users/herantal/OneDrive - State of Minnesota - MN365/Documents/Coldwater_MGLP2020/data/mglp_data_rantala/lake_link.csv",
              header=TRUE)
mn_link<-readRDS("mndow_nhdhr_xwalk.rds")
wi_link<-readRDS("wbic_nhdhr_xwalk.rds")

#need to do a little cleaning up
MN$state<-c("MN")
MN$state_id<-c("DOW")
MI$state<-c("MI")
MI$state_id<-c("STORET")
WI$state<-c("WI")
WI$state_id<-c("wbic")

#MN clean up
mn_link<-mn_link%>%
 separate(MNDOW_ID, into = c("state_id","DOWc"),
                      sep = "_", remove = F)%>%
  separate(site_id, into = c("id2","nhdhr"),
           sep = "_", remove = F)%>%
  mutate(DOW=as.integer(DOWc))
  
MN2<-left_join(MN,mn_link, by="DOW")#final MN
MN2<-MN2%>%
  mutate(state_id_type=c("DOW"),state_id=DOWc)%>%
  select(nhdhr,site_id,state,state_id,state_id_type,Latitude:VW_DOY_3)

#MI clean up
df3<-link%>%filter(lake_centroidstate=='MI'&lagosus_legacysitelabel=="STORETID")%>%
  select(lagoslakeid,lake_nhdid,lagosne_legacysiteid)%>%
  mutate(STORETID=lagosne_legacysiteid)%>%
  unique()

MI<-MI%>%
  separate(MonitoringLocationIdentifier,into = c("Prefix", "STORETID"), sep = "-", remove = F)

MI.1<-left_join(MI,df3, by="STORETID")
MI.1<-MI.1%>%filter(lagoslakeid>0)

MI.2<-MI.1%>%filter(is.na(lagosne_legacysiteid))%>%
  select(X:state_id)%>%
  mutate(lat2=round(Latitude, digits=5), lon2=round(Longitude,digits=5))

df3.1<-link%>%filter(lake_centroidstate=='MI')%>%
  select(lagoslakeid,lake_nhdid,lagosne_legacysiteid,lake_lat_decdeg,lake_lon_decdeg)%>%
  unique()%>%
  mutate(lat2=round(lake_lat_decdeg, digits=5), lon2=round(lake_lon_decdeg,digits=5))

MI.3<-left_join(MI.2,df3.1, by=c("lat2","lon2"))
MI.3<-MI.3%>%select(X:state_id,lagoslakeid,lake_nhdid,lagosne_legacysiteid)%>%
  filter(!is.na(lagosne_legacysiteid))

MI.4<-bind_rows(MI.1,MI.3)#final MI
MI.4$state_id<-MI.4$MonitoringLocationIdentifier
MI.4$state_id_type<-c("STORET")

MI.4<-MI.4%>%
  mutate(nhdhr=as.character(lake_nhdid),site_id=paste("nhdhr_",nhdhr,sep = ""))%>%
  select(nhdhr,site_id,state,state_id,state_id_type,Latitude:VW_DOY_3)

#WI clean up
wi_link<-wi_link%>%
  separate(WBIC_ID, into = c("state_id","wbic"),
           sep = "_", remove = F)%>%
  separate(site_id, into = c("id2","nhdhr"),
           sep = "_", remove = F)%>%
  mutate(wbic=as.integer(wbic))

WI<-WI%>%
  mutate(wbic=wibic)

WI2<-left_join(WI,wi_link,by="wbic")
WI2<-WI2%>%
  mutate(state_id_type=c("wbic"),state_id=as.character(wbic),Latitude=as.integer(c("NA")),
         Longitude=as.integer(c("NA")))%>%
  select(nhdhr,site_id,state,state_id,state_id_type,Latitude,Longitude,Year:VW_DOY_3)

###combine VHOD estimates
VHOD<-bind_rows(MN2,WI2)
#VHOD$state_id<-as.character(VHOD$state_id)
VHOD<-bind_rows(VHOD, MI.4)
summary(VHOD)
write.csv(VHOD, "VHOD.csv")
