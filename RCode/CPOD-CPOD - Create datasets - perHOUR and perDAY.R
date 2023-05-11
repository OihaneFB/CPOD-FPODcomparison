library(tidyverse)
library(here)
Sys.setenv(tz="GMT")

#~~~~~~~~~~      ----
#NB data:      ----
#~~~~~~~~~~      ----

#1. CREATE PPM and BPM datasets - per HOUR:             ----
cpodbuzz=data.table::fread(here("CPOD-CPOD","ClickDetails_Buzzes","NB_Clickdetails_Buzzes.csv"),
                             header = T) %>%  as_tibble()

cpodbuzz=data.table::fread(here("CPOD-CPOD","ClickDetails_Buzzes","NB_Clickdetails_Priority1-2_Buzzes.csv"),
                           header = T) %>%  as_tibble()

cpodbuzz2 = cpodbuzz %>% 
  rename(
    location.id=site,
    cpod.depl=dep) %>% 
  mutate(
    datetime=as.character(as.POSIXct(Date,format="%Y-%m-%d %H:%M:%S",tz="GMT")),
    datetime=case_when(is.na(datetime)~ as.character(as.POSIXct(Date,format="%d/%m/%Y %H:%M",tz="GMT")),
                       TRUE~as.character(datetime)),
    datetime=as.POSIXct(datetime,format="%Y-%m-%d %H:%M:%S",tz="GMT"),
    datetimehour = lubridate::floor_date(datetime,"hour")+59*60, 
    bz=ifelse(clicktype=="buzz",1,0),
    datetimemin=as.character(format(datetime,"%Y-%m-%d %H:%M",tz="GMT")),
    date=as.Date(datetimehour,tz="GMT")) 

cpod903=filter(cpodbuzz2,cpod.depl==903)

cpoddatasumh = cpodbuzz2 %>%
  group_by(location.id,cpod.depl,date,datetimehour,datetimemin) %>% 
  summarise(
    pres.buzzes=ifelse(sum(bz)>0,1,0)) %>% 
  ungroup() %>% 
  group_by(location.id,cpod.depl,date,datetimehour) %>% 
  summarise(
    cpod.bpm=sum(pres.buzzes)) %>% 
  mutate(
    cpod.buzz.pres=ifelse(cpod.bpm>0,1,0)) %>% 
  ungroup()


cpoddep=cpoddatasumh %>% distinct(cpod.depl) %>% pull()

#Read access dpm summary table:
accessdpm=data.table::fread(here::here("Data","Access","Access_Summary data DPM per hour.txt"),
                     sep = "\t" ,header = T) %>%
  as_tibble() %>% 
  rename_all(list(~make.names(.))) %>% 
  rename_all(list(~tolower(.))) 

#Prepare access database to merge:
accessdpm2 = accessdpm %>% 
  filter(deployment.number %in% cpoddep) %>% 
  mutate(
    datetimehour=as.POSIXct(date,format="%d/%m/%Y %H:%M:%S",tz="GMT"),
    date=as.Date(datetimehour)) %>% 
  rename(
    cpod.priority=pod.priority,
    cpod.depl=deployment.number,
    cpod.total.minutes=total.minutes,
    cpod.PPM=porpoise..ve.min,
    cpod.por.pres=porp..  ) %>% 
  select(cpod.depl,location.id,date,cpod.priority,datetimehour,cpod.total.minutes,cpod.PPM,cpod.por.pres)
  

#Merge with buzz dataset:
cpodacc=left_join(accessdpm2,cpoddatasumh, by = c("cpod.depl", "location.id", "datetimehour","date"))
cpodacc[is.na(cpodacc)]=0

#Data days with paired CPODs:
paireddays=read.csv(here("CPOD-CPOD","PairedCPODs_DataDays.csv"),header = T) %>% 
  mutate(date=as.Date(date)) %>% 
  select(-pod.priority)

cpodbuzz201=filter(paireddays,location.id==201) 


#Filter database for those days:
paireddata=left_join(paireddays,cpodacc, by = c("date", "location.id", "cpod.depl")) %>% 
  filter(!is.na(datetimehour))

write.csv(paireddata,here::here("Data","CPOD-CPOD_HP-PPM-BPM_perHOUR.csv"),
          row.names = F)

write.csv(paireddata,here::here("Data","CPOD-CPOD_HP-PPM-BPM_Priority1-2_perHOUR.csv"),
          row.names = F)


#Make it wide:
priority1=paireddata %>% 
  filter(cpod.priority==1) %>% 
  rename(
    cpod1.depl=cpod.depl,
    cpod1.priority=cpod.priority,
    cpod1.total.minutes=cpod.total.minutes,
    cpod1.PPM=cpod.PPM,
    cpod1.por.pres=cpod.por.pres,
    cpod1.bpm=cpod.bpm,
    cpod1.buzz.pres=cpod.buzz.pres)

priority2=paireddata %>% 
  filter(cpod.priority==2)%>% 
  rename(
    cpod2.depl=cpod.depl,
    cpod2.priority=cpod.priority,
    cpod2.total.minutes=cpod.total.minutes,
    cpod2.PPM=cpod.PPM,
    cpod2.por.pres=cpod.por.pres,
    cpod2.bpm=cpod.bpm,
    cpod2.buzz.pres=cpod.buzz.pres)

paireddata.wide=full_join(priority1,priority2,
                     by = c("date", "location.id", "depl.pair", "year", "datetimehour")) 

daystodrop=paireddata.wide %>% 
  filter_all(any_vars(is.na(.))) %>% 
  distinct(location.id,date) %>% 
  mutate(todrop=1)

paireddata.wide2=paireddata.wide%>% 
  drop_na()

cpodbuzz201=filter(paireddata.wide2,location.id==201)   

#Save:
write.csv(paireddata.wide2,here::here("Data","CPOD-CPOD_HP-PPM-BPM_perHOUR_wide.csv"),
          row.names = F)

write.csv(paireddata.wide2,here::here("Data","CPOD-CPOD_HP-PPM-BPM_Priority1-2_perHOUR_wide.csv"),
          row.names = F)


#2. CREATE DPH and BPH datasets - perDAY:     ----
cpoddatasumday = paireddata %>%
  group_by(date,location.id,depl.pair,cpod.depl) %>%
  summarise(
    bph=sum(cpod.buzz.pres)) %>% 
  ungroup() 

#Access database DPH:
accessdph=read.csv(here::here("Data","Access","Access_Summary data DPH per day.txt"),
                    sep="\t",header = T) %>% 
  rename_all(list(~tolower(.))) %>% 
  rename(cpod.depl=deployment.number) %>% 
  filter(cpod.depl %in% cpoddep)  %>% 
  mutate(
    date=as.Date(date,format="%d/%m/%Y %H:%M:%S"))

#Filter days with paired pods:
paireddatah=left_join(paireddays,accessdph) %>% 
  drop_na()
paireddatah2=left_join(paireddatah,daystodrop) %>% 
  filter(is.na(todrop))

#Merge cpod data with access database:
cpodaccdph=left_join(paireddatah2,cpoddatasumday) %>% 
  rename(
    cpod.total.hours=total.hours,
    cpod.por.pres=porp.. ,
    cpod.DPH=porpoise..ve.hours,
    cpod.BPH=bph) %>% 
  select(date,depl.pair,cpod.depl,location.id,pod.priority,cpod.total.hours,cpod.por.pres,cpod.DPH,cpod.BPH)

write.csv(cpodaccdph,here::here("Data","CPOD-CPOD_HP-DPH-BPH_perDAY.csv"),row.names = F)

write.csv(cpodaccdph,here::here("Data","CPOD-CPOD_HP-DPH-BPH_Priority1-2_perDAY.csv"),row.names = F)


priority1h=cpodaccdph %>% 
  filter(pod.priority==1) %>% 
  rename(
    cpod1.depl=cpod.depl,
    cpod1.priority=pod.priority,
    cpod1.total.hours=cpod.total.hours,
    cpod1.por.pres=cpod.por.pres,
    cpod1.DPH=cpod.DPH,
    cpod1.BPH=cpod.BPH)

priority2h=cpodaccdph %>% 
  filter(pod.priority==2)%>% 
  rename(
    cpod2.depl=cpod.depl,
    cpod2.priority=pod.priority,
    cpod2.total.hours=cpod.total.hours,
    cpod2.por.pres=cpod.por.pres,
    cpod2.DPH=cpod.DPH,
    cpod2.BPH=cpod.BPH)

cpodaccdph.wide=full_join(priority1h,priority2h, by = c("date", "depl.pair", "location.id")) %>% 
  drop_na()


write.csv(cpodaccdph.wide,here::here("Data","CPOD-CPOD_HP-DPH-BPH_perDAY_wide.csv"),row.names = F)

write.csv(cpodaccdph.wide,here::here("Data","CPOD-CPOD_HP-DPH-BPH_Priority1-2_perDAY_wide.csv"),row.names = F)


#~~~~~~~~~~      ----
#Dol data:      ----
#~~~~~~~~~~      ----

#1. CREATE DPM and BPM datasets - per HOUR:             ----
cpodbuzz=data.table::fread(here("CPOD-CPOD","ClickDetails_Buzzes","Dol_Clickdetails_Buzzes.csv"),
                           header = T) %>%  as_tibble()

cpodbuzz=data.table::fread(here("CPOD-CPOD","ClickDetails_Buzzes","Dol_Clickdetails_Priority1-2_Buzzes.csv"),
                           header = T) %>%  as_tibble()

cpodbuzz2 = cpodbuzz %>% 
  rename(
    location.id=site,
    cpod.depl=dep) %>% 
  mutate(
    datetime=as.character(as.POSIXct(Date,format="%Y-%m-%d %H:%M:%S",tz="GMT")),
    datetime=case_when(is.na(datetime)~ as.character(as.POSIXct(Date,format="%d/%m/%Y %H:%M",tz="GMT")),
                       TRUE~as.character(datetime)),
    datetime=as.POSIXct(datetime,format="%Y-%m-%d %H:%M:%S",tz="GMT"),
    datetimehour = lubridate::floor_date(datetime,"hour")+59*60, 
    bz=ifelse(clicktype=="buzz",1,0),
    datetimemin=as.character(format(datetime,"%Y-%m-%d %H:%M",tz="GMT")),
    date=as.Date(datetimehour,tz="GMT")) 

cpod903=filter(cpodbuzz2,cpod.depl==903)

cpoddatasumh = cpodbuzz2 %>%
  group_by(location.id,cpod.depl,date,datetimehour,datetimemin) %>% 
  summarise(
    pres.buzzes=ifelse(sum(bz)>0,1,0)) %>% 
  ungroup() %>% 
  group_by(location.id,cpod.depl,date,datetimehour) %>% 
  summarise(
    cpod.bpm=sum(pres.buzzes)) %>% 
  mutate(
    cpod.buzz.pres=ifelse(cpod.bpm>0,1,0)) %>% 
  ungroup()


cpoddep=cpoddatasumh %>% distinct(cpod.depl) %>% pull()

#Read access dpm summary table:
accessdpm=data.table::fread(here::here("Data","Access","Access_Summary data DPM per hour.txt"),
                            sep = "\t" ,header = T) %>%
  as_tibble() %>% 
  rename_all(list(~make.names(.))) %>% 
  rename_all(list(~tolower(.))) 

#Prepare access database to merge:
accessdpm2 = accessdpm %>% 
  filter(deployment.number %in% cpoddep) %>% 
  mutate(
    datetimehour=as.POSIXct(date,format="%d/%m/%Y %H:%M:%S",tz="GMT"),
    date=as.Date(datetimehour)) %>% 
  rename(
    cpod.priority=pod.priority,
    cpod.depl=deployment.number,
    cpod.total.minutes=total.minutes,
    cpod.DPM=dolphin..ve.min,
    cpod.dol.pres=dol..  ) %>% 
  select(cpod.depl,location.id,date,cpod.priority,datetimehour,cpod.total.minutes,cpod.DPM,cpod.dol.pres)


#Merge with buzz dataset:
cpodacc=left_join(accessdpm2,cpoddatasumh, by = c("cpod.depl", "location.id", "datetimehour","date"))
cpodacc[is.na(cpodacc)]=0

#Data days with paired CPODs:
paireddays=read.csv(here("CPOD-CPOD","PairedCPODs_DataDays.csv"),header = T) %>% 
  mutate(date=as.Date(date)) %>% 
  select(-pod.priority)

cpodbuzz201=filter(paireddays,location.id==201) 


#Filter database for those days:
paireddata=left_join(paireddays,cpodacc, by = c("date", "location.id", "cpod.depl")) %>% 
  filter(!is.na(datetimehour))

write.csv(paireddata,here::here("Data","CPOD-CPOD_Dol-DPM-BPM_Priority1-2_perHOUR.csv"),
          row.names = F)

#Make it wide:
priority1=paireddata %>% 
  filter(cpod.priority==1) %>% 
  rename(
    cpod1.depl=cpod.depl,
    cpod1.priority=cpod.priority,
    cpod1.total.minutes=cpod.total.minutes,
    cpod1.DPM=cpod.DPM,
    cpod1.dol.pres=cpod.dol.pres,
    cpod1.bpm=cpod.bpm,
    cpod1.buzz.pres=cpod.buzz.pres)

priority2=paireddata %>% 
  filter(cpod.priority==2)%>% 
  rename(
    cpod2.depl=cpod.depl,
    cpod2.priority=cpod.priority,
    cpod2.total.minutes=cpod.total.minutes,
    cpod2.DPM=cpod.DPM,
    cpod2.dol.pres=cpod.dol.pres,
    cpod2.bpm=cpod.bpm,
    cpod2.buzz.pres=cpod.buzz.pres)

paireddata.wide=full_join(priority1,priority2,
                          by = c("date", "location.id", "depl.pair", "year", "datetimehour")) 

daystodrop=paireddata.wide %>% 
  filter_all(any_vars(is.na(.))) %>% 
  distinct(location.id,date) %>% 
  mutate(todrop=1)

paireddata.wide2=paireddata.wide%>% 
  drop_na()

cpodbuzz201=filter(paireddata.wide2,location.id==201)   

#Save:
write.csv(paireddata.wide2,here::here("Data","CPOD-CPOD_Dol-DPM-BPM_Priority1-2_perHOUR_wide.csv"),
          row.names = F)



#2. CREATE DPH and BPH datasets - perDAY:     ----
cpoddatasumday = paireddata %>%
  group_by(date,location.id,depl.pair,cpod.depl) %>%
  summarise(
    bph=sum(cpod.buzz.pres)) %>% 
  ungroup() 

#Access database DPH:
accessdph=read.csv(here::here("Data","Access","Access_Summary data DPH per day.txt"),
                   sep="\t",header = T) %>% 
  rename_all(list(~tolower(.))) %>% 
  rename(cpod.depl=deployment.number) %>% 
  filter(cpod.depl %in% cpoddep)  %>% 
  mutate(
    date=as.Date(date,format="%d/%m/%Y %H:%M:%S"))

#Filter days with paired pods:
paireddatah=left_join(paireddays,accessdph) %>% 
  drop_na()
paireddatah2=left_join(paireddatah,daystodrop) %>% 
  filter(is.na(todrop))

#Merge cpod data with access database:
cpodaccdph=left_join(paireddatah2,cpoddatasumday) %>% 
  rename(
    cpod.total.hours=total.hours,
    cpod.dol.pres=dol.. ,
    cpod.DPH=dolphin..ve.hours,
    cpod.BPH=bph) %>% 
  select(date,depl.pair,cpod.depl,location.id,pod.priority,cpod.total.hours,cpod.dol.pres,cpod.DPH,cpod.BPH)

write.csv(cpodaccdph,here::here("Data","CPOD-CPOD_Dol-DPH-BPH_Priority1-2_perDAY.csv"),row.names = F)


priority1h=cpodaccdph %>% 
  filter(pod.priority==1) %>% 
  rename(
    cpod1.depl=cpod.depl,
    cpod1.priority=pod.priority,
    cpod1.total.hours=cpod.total.hours,
    cpod1.dol.pres=cpod.dol.pres,
    cpod1.DPH=cpod.DPH,
    cpod1.BPH=cpod.BPH)

priority2h=cpodaccdph %>% 
  filter(pod.priority==2)%>% 
  rename(
    cpod2.depl=cpod.depl,
    cpod2.priority=pod.priority,
    cpod2.total.hours=cpod.total.hours,
    cpod2.dol.pres=cpod.dol.pres,
    cpod2.DPH=cpod.DPH,
    cpod2.BPH=cpod.BPH)

cpodaccdph.wide=full_join(priority1h,priority2h, by = c("date", "depl.pair", "location.id")) %>% 
  drop_na()


write.csv(cpodaccdph.wide,here::here("Data","CPOD-CPOD_Dol-DPH-BPH_Priority1-2_perDAY_wide.csv"),row.names = F)





