###combine VHOD estimates for all states
###H Rantala
###21June2023

#set wd
setwd("C:/Users/herantal/OneDrive - State of Minnesota - MN365/Documents/Coldwater_MGLP2020/Jacob")

#load packages
library(tidyverse)

#load VHOD estimates for each state
MN<-read.csv("MN_TDO3.csv", header=TRUE, sep=",")
MI<-read.csv("MI_TDO3.csv", header=TRUE, sep=",")
WI<-read.csv("WI_TDO3.csv",header=TRUE, sep=",")
LAGOS<-read.csv("C:/Users/herantal/OneDrive - State of Minnesota - MN365/Documents/Coldwater_MGLP2020/data/mglp_data_rantala/lakes_with_lagoslakeid.csv", header = TRUE)

#and link file for nhd IDs
link<-read.csv("C:/Users/herantal/OneDrive - State of Minnesota - MN365/Documents/Coldwater_MGLP2020/data/mglp_data_rantala/lake_link.csv",
               header=TRUE)
mn_link<-readRDS("mndow_nhdhr_xwalk.rds")
wi_link<-readRDS("wbic_nhdhr_xwalk.rds")

#need to do a little cleaning up
MN<-MN%>%select(MonitoringLocationIdentifier:Longitude,Year:TDO3)
WI<-WI%>%select(MonitoringLocationIdentifier:TDO3)

MN$state<-c("MN")
MI$state<-c("MI")
WI$state<-c("WI")

#TDO3<-bind_rows(MN,WI,MI)

#write.csv(TDO3,"TDO3.csv")

#TDO3.1<-left_join(TDO3, LAGOS, by=c("MonitoringLocationIdentifier"="Monitoring"))
#MN clean up

mn_link<-mn_link%>%
  separate(MNDOW_ID, into = c("state_id","DOW"),
           sep = "_", remove = F)%>%
  separate(site_id, into = c("id2","nhdhr"),
           sep = "_", remove = F)

MN1<-MN%>%
  filter(str_detect(MonitoringLocationIdentifier,'MNPCA')|str_detect(MonitoringLocationIdentifier,'MNDNR'))%>%
  separate(MonitoringLocationIdentifier, into = c("agency", "County", "LakeID", "ID", "Basin"), sep = "-", remove = F)%>%
  unite(col = DOW, c(County, LakeID, ID), sep = "")

MN1.1<-left_join(MN1,mn_link,by="DOW")#linked TDO3, df1
MN1.1<-MN1.1%>%select(MonitoringLocationIdentifier,Latitude,Longitude,Year,TDO3,state,nhdhr)

###link other sites using non-DOW ids
MN2<-MN%>%
  filter(!str_detect(MonitoringLocationIdentifier,'MNPCA')&!str_detect(MonitoringLocationIdentifier,'MNDNR'))
  

df1<-link%>%filter(lake_centroidstate=='MN')%>%
  filter(!str_detect(wqp_monitoringlocationidentifier,'MNPCA')&!str_detect(wqp_monitoringlocationidentifier,'MNDNR'))%>%
  mutate(Latitude=lake_lat_decdeg,Longitude=lake_lon_decdeg,nhdhr=as.character(nhdplusv2_comid))%>%
  select(Latitude,Longitude,nhdhr)

MN2.1<-left_join(MN2,df1,by=c("Latitude","Longitude"))

MN3<-bind_rows(MN1.1,MN2.1)
MN3<-MN3%>%filter(nhdhr!="NA")

#MI clean up
#match to nhd by storet
df3<-link%>%filter(lake_centroidstate=='MI'&lagosus_legacysitelabel=="STORETID")%>%
  select(lagoslakeid,lake_nhdid,lagosne_legacysiteid)%>%
  mutate(STORETID=lagosne_legacysiteid)%>%
  unique()

MI<-MI%>%
  separate(MonitoringLocationIdentifier,into = c("Prefix", "STORETID"), sep = "-", remove = F)

MI.1<-left_join(MI,df3, by="STORETID")
MI.1<-MI.1%>%filter(lake_nhdid>0)%>%
  mutate(nhdhr=as.character(lake_nhdid))%>%#matched by storetID, n=246
select(MonitoringLocationIdentifier,Latitude:state,nhdhr)
###match by lat long-n=282
MI.2<-MI.1%>%filter(is.na(lake_nhdid))%>%
  select(MonitoringLocationIdentifier,Latitude:state)%>%
  mutate(Latitude=round(Latitude, digits=5), Longitude=round(Longitude,digits=5))

df3.1<-link%>%filter(lake_centroidstate=='MI')%>%
  select(lagoslakeid,lake_nhdid,lagosne_legacysiteid,lake_lat_decdeg,lake_lon_decdeg)%>%
  unique()%>%
  mutate(Latitude=round(lake_lat_decdeg, digits=5), Longitude=round(lake_lon_decdeg,digits=5))

MI.3<-left_join(MI.2,df3.1, by=c("Latitude","Longitude"))#second batch of MI
MI.3<-MI.3%>%
  mutate(nhdhr=as.character(lake_nhdid))%>%
  select(1:6,nhdhr)
###match by monitoring ID 
MI.4<-MI.3%>%
  filter(is.na(lake_nhdid))%>%
  filter(str_detect(MonitoringLocationIdentifier,'21MICH'))%>%
separate(MonitoringLocationIdentifier, into = c("agency", "ID"),  sep = "-", remove = F)%>%
  select(MonitoringLocationIdentifier:TDO3)

df3.2<-link%>%filter(lake_centroidstate=='MI')%>%
  select(lagoslakeid,lake_nhdid,lagosne_legacysiteid,lake_lat_decdeg,lake_lon_decdeg)%>%
  unique()%>%
  mutate(ID=lagosne_legacysiteid)

MI.5<-left_join(MI.4,df3.2,by="ID")#no matches

df3.3<-LAGOS%>%
  select(Monitoring,Latitude,Longitude,lagoslakei)%>%
  mutate(MonitoringLocationIdentifier=Monitoring)

MI.6<-left_join(MI.4,df3.3,by="MonitoringLocationIdentifier")#no matches 

MI.7<-bind_rows(MI.1,MI.3)#final MI

#WI clean up WIDNR_WQX-985100

wi_link<-wi_link%>%
  separate(WBIC_ID, into = c("state_id","wbic"),
           sep = "_", remove = F)%>%
  separate(site_id, into = c("id2","nhdhr"),
           sep = "_", remove = F)%>%
  mutate(wbic=as.integer(wbic))

WI.1<-WI%>%
  filter(str_detect(MonitoringLocationIdentifier,'WIDNR_WQX'))%>%
  separate(MonitoringLocationIdentifier, into = c("agency", "ID"),  sep = "-", remove = F)%>%
  mutate(wbic=as.integer(ID))

WI.2<-left_join(WI.1,wi_link,by="wbic")#448 observations linked by xwalk from GJAH
WI.2.1<-WI.2%>%filter(nhdhr!="NA")%>%
  select(MonitoringLocationIdentifier,Latitude:state,nhdhr)
###some non-matches in above
WI.2.3<-WI.2%>%filter(is.na(nhdhr))%>%
  select(MonitoringLocationIdentifier:state)%>%
  mutate(Latitude=round(Latitude, digits=5), Longitude=round(Longitude,digits=5))
#lets try to link those by lat/long
df4<-link%>%filter(lake_centroidstate=='WI')%>%
  select(lagoslakeid,lake_nhdid,lagosne_legacysiteid,lake_lat_decdeg,lake_lon_decdeg)%>%
  unique()%>%
  mutate(Latitude=round(lake_lat_decdeg, digits=5), Longitude=round(lake_lon_decdeg,digits=5))

WI.2.4<-left_join(WI.2.3,df4,by=c("Latitude","Longitude"))
WI.2.5<-WI.2.4%>%
  mutate(state_id=MonitoringLocationIdentifier, state_id_type=c("Varies"),
         nhdhr=as.character(lake_nhdid),site_id=c("NA"),id2=c("nhdhr"))%>%
  select(MonitoringLocationIdentifier,Latitude:state,nhdhr)

#matches not from DNR Ids
WI.3<-WI%>%#52 obeservations 
  filter(!str_detect(MonitoringLocationIdentifier,'WIDNR_WQX'))%>%
  mutate(Latitude=round(Latitude, digits=5), Longitude=round(Longitude,digits=5))

#lets try to link those by lat/long using df4

WI.4<-left_join(WI.3,df4,by=c("Latitude","Longitude"))#all matched!
WI.4<-WI.4%>%
  mutate(state_id=MonitoringLocationIdentifier,state_id_type=c("Varies"),id2="nhdhr",
         nhdhr=as.character(lake_nhdid))%>%
    select(MonitoringLocationIdentifier:state,nhdhr)

  
WI.5<-bind_rows(WI.2.1,WI.4)
WI.5<-bind_rows(WI.5,WI.2.5)#500 observations
###need to put all the states together WI.5, MN3, MI.7
TDO3<-bind_rows(WI.5,MN3)
TDO3<-bind_rows(TDO3,MI.7)

TDO3<-TDO3%>%filter(nhdhr!="NA")

ggplot(data=TDO3, aes(Year, TDO3))+
  geom_point(aes(color=Latitude))

#write.csv(TDO3, "TDO3.csv")
