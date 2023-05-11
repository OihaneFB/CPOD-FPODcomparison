library(tidyverse)
library(here)
Sys.setenv(tz="GMT")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----
#HP                                  -----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----

#1. CREATE PPM and BPM datasets - per HOUR:             ----
cpodbuzz=data.table::fread(here("Data","ClickDetails_Buzzes","CPOD_NB_Clickdetails_Buzzes.csv"),
                             header = T) %>%  as_tibble()

test=filter(cpodbuzz,dep==1242)

cpoddatasumh = cpodbuzz %>% 
  rename(
    location.id=site,
    cpod.depl=dep) %>% 
  mutate(
    datetime=as.POSIXct(Date,format="%Y-%m-%d %H:%M:%S",tz="GMT"),
    datetimehour = lubridate::floor_date(datetime,"hour")+59*60, 
    bz=ifelse(clicktype=="buzz",1,0),
    datetimemin=as.character(format(datetime,"%Y-%m-%d %H:%M",tz="GMT")),
    date=as.Date(datetimehour,tz="GMT")) %>%
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
    cpod.depl=deployment.number,
    cpod.total.minutes=total.minutes,
    cpod.PPM=porpoise..ve.min,
    cpod.por.pres=porp..  ) %>% 
  select(cpod.depl,location.id,date,datetimehour,cpod.total.minutes,cpod.PPM,cpod.por.pres)
  
test=filter(accessdpm2,cpod.depl==1242)

#Merge with buzz dataset to add porpoise absence:
cpodacc=left_join(accessdpm2,cpoddatasumh, by = c("cpod.depl", "location.id", "datetimehour","date"))
cpodacc[is.na(cpodacc)]=0


##3b. Add FPOD data:
fpodbuzz=data.table::fread(here::here("Data","ClickDetails_Buzzes","FPOD_NB_Clickdetails_Buzzes.csv"),
                             header = T) %>%  as_tibble()

test=filter(fpodbuzz,dep==1285)

fpoddatasumh = fpodbuzz %>% 
  rename(
    location.id=site,
    fpod.depl=dep) %>% 
  mutate(
    datetime=as.POSIXct(Date,format="%Y-%m-%d %H:%M:%S",tz="GMT"),
    datetimehour = lubridate::floor_date(datetime,"hour")+59*60,
    fpod.por.pres=1,
    bz=ifelse(clicktype=="buzz",1,0),
    click=1,
    datetimemin=as.character(format(datetime,"%Y-%m-%d %H:%M",tz="GMT")),
    date=as.Date(datetimehour,tz="GMT")) %>%
  group_by(location.id,fpod.depl,date,datetimehour,datetimemin,fpod.por.pres) %>% 
  summarise(
    pres.clicks=ifelse(sum(click)>0,1,0),
    pres.buzzes=ifelse(sum(bz)>0,1,0)) %>% 
  ungroup() %>% 
  group_by(location.id,fpod.depl,date,datetimehour,fpod.por.pres) %>% 
  summarise(
    fpod.PPM=sum(pres.clicks),
    fpod.bpm=sum(pres.buzzes)) %>% 
  mutate(
    fpod.buzz.pres=ifelse(fpod.bpm>0,1,0)) %>% 
  ungroup()

fpoddep=fpoddatasumh %>% distinct(fpod.depl) %>% pull()

#Read access deployment table - FPOD dep start stop:
accessdep=read.csv(here::here("Data","Access","Deployments.csv"),header = T) %>%
  rename_all(list(~tolower(.))) 

accessdep2=accessdep%>% 
  filter(deployment_number %in% fpoddep) %>% 
  mutate(
    across(c(deployment_date,retrieval_date),~as.POSIXct(., format="%d/%m/%Y %H:%M", tz="GMT")),
    data_end_date=as.POSIXct(as.Date(data_end_date,format="%d/%m/%Y", tz="GMT")),
    start= deployment_date + 24*60*60,
    end= case_when(retrieval_date<data_end_date~ as.character(retrieval_date),
                   retrieval_date>data_end_date~ as.character(data_end_date),
                   TRUE~as.character(NA)),
    end=as.POSIXct(end,tz="GMT"),
    datetimehour=start)

test=filter(accessdep2,deployment_number==1285)

#FPOD deployments - per hour:
accessdep2h=accessdep2 %>%
  group_by(deployment_number) %>% 
  complete(datetimehour=seq(from=start,to=end,by="1 hour")) %>% 
  select(deployment_number,datetimehour) %>% 
  mutate(
    datetimehour=datetimehour+59*60 ) %>% 
  rename(fpod.depl=deployment_number) %>% 
  ungroup()
  
test=filter(accessdep2h,fpod.depl==1285)

#Get porpoise absence:
fpoddatasumh2=left_join(accessdep2h,fpoddatasumh, by = c("fpod.depl", "datetimehour"))

#Fill the gaps in the fpod dataset:
fpoddatasumh2=fpoddatasumh2 %>% 
group_by(fpod.depl) %>% 
  fill(location.id, .direction="updown") %>% 
  mutate(
    date=as.Date(datetimehour)) %>% 
  replace(is.na(.),0) %>% 
  ungroup()


#Join with CPOD dataset:
cpodfpodacc=inner_join(cpodacc,fpoddatasumh2, by = c( "location.id", "datetimehour","date")) 

test=filter(cpodfpodacc,fpod.depl==1285)


#Save:
write.csv(cpodfpodacc,here::here("Data","CPOD-FPOD_HP-PPM-BPM_perHOUR.csv"),
          row.names = F)



#2. CREATE DPH and BPH datasets - perDAY:     ----
cfpoddatasumday = cpodfpodacc %>%
  group_by(date,location.id,cpod.depl,fpod.depl) %>%
  summarise(
    cpod.DPH=sum(cpod.por.pres),
    cpod.BPH=sum(cpod.buzz.pres),
    fpod.DPH=sum(fpod.por.pres),
    fpod.BPH=sum(fpod.buzz.pres)) %>% 
  ungroup() %>% 
  mutate(
    cpod.por.pres=ifelse(cpod.DPH>0,1,0),
    fpod.por.pres=ifelse(fpod.DPH>0,1,0)  )

test=filter(cfpoddatasumday,fpod.depl==1285)

write.csv(cfpoddatasumday,here::here("Data","CPOD-FPOD_HP-DPH-BPH_perDAY.csv"),row.names = F)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----
#Dol                                  -----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----

#1. CREATE DPM and BPM datasets - per HOUR:             ----
cpodbuzz=data.table::fread(here("Data","ClickDetails_Buzzes","CPOD_Dol_Clickdetails_Buzzes.csv"),
                           header = T) %>%  as_tibble()

cpoddatasumh = cpodbuzz %>% 
  rename(
    location.id=site,
    cpod.depl=dep) %>% 
  mutate(
    datetime=as.POSIXct(Date,format="%Y-%m-%d %H:%M:%S",tz="GMT"),
    datetimehour = lubridate::floor_date(datetime,"hour")+59*60, 
    cpod.dol.pres=1,
    click=1,
    bz=ifelse(clicktype=="buzz",1,0),
    datetimemin=as.character(format(datetime,"%Y-%m-%d %H:%M",tz="GMT")),
    date=as.Date(datetimehour,tz="GMT")) %>%
  group_by(location.id,cpod.depl,date,datetimehour,datetimemin,cpod.dol.pres) %>% 
  summarise(
    pres.clicks=ifelse(sum(click)>0,1,0),
    pres.buzzes=ifelse(sum(bz)>0,1,0)) %>% 
  ungroup() %>% 
  group_by(location.id,cpod.depl,date,datetimehour,cpod.dol.pres) %>% 
  summarise(
    cpod.DPM=sum(pres.clicks),
    cpod.BPM=sum(pres.buzzes)) %>% 
  mutate(
    cpod.buzz.pres=ifelse(cpod.BPM>0,1,0)) %>% 
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
    access.dpm=dolphin..ve.min,
    cpod.depl=deployment.number,
    cpod.total.minutes=total.minutes,) %>% 
  select(cpod.depl,location.id,date,datetimehour,cpod.total.minutes)


#Merge with buzz dataset to get dolphin absence:
cpodacc=left_join(accessdpm2,cpoddatasumh, by = c("cpod.depl", "location.id", "datetimehour","date"))
cpodacc[is.na(cpodacc)]=0


##3b. Add FPOD data:
fpodbuzz=data.table::fread(here::here("Data","ClickDetails_Buzzes","FPOD_Dol_Clickdetails_Buzzes.csv"),
                           header = T) %>%  as_tibble()

fpoddatasumh = fpodbuzz %>% 
  rename(
    location.id=site,
    fpod.depl=dep) %>% 
  mutate(
    datetime=as.POSIXct(Date,format="%Y-%m-%d %H:%M:%S",tz="GMT"),
    datetimehour = lubridate::floor_date(datetime,"hour")+59*60,
    fpod.dol.pres=1,
    bz=ifelse(clicktype=="buzz",1,0),
    click=1,
    datetimemin=as.character(format(datetime,"%Y-%m-%d %H:%M",tz="GMT")),
    date=as.Date(datetimehour,tz="GMT")) %>%
  group_by(location.id,fpod.depl,date,datetimehour,datetimemin,fpod.dol.pres) %>% 
  summarise(
    pres.clicks=ifelse(sum(click)>0,1,0),
    pres.buzzes=ifelse(sum(bz)>0,1,0)) %>% 
  ungroup() %>% 
  group_by(location.id,fpod.depl,date,datetimehour,fpod.dol.pres) %>% 
  summarise(
    fpod.DPM=sum(pres.clicks),
    fpod.BPM=sum(pres.buzzes)) %>% 
  mutate(
    fpod.buzz.pres=ifelse(fpod.BPM>0,1,0)) %>% 
  ungroup()

fpoddep=fpoddatasumh %>% distinct(fpod.depl) %>% pull()

#Read access deployment table - FPOD dep start stop:
accessdep=read.csv(here::here("Data","Access","Deployments.csv"),header = T) %>%
  rename_all(list(~tolower(.))) 

accessdep2=accessdep%>% 
  filter(deployment_number %in% fpoddep) %>% 
  mutate(
    across(c(deployment_date,retrieval_date),~as.POSIXct(., format="%d/%m/%Y %H:%M", tz="GMT")),
    data_end_date=as.POSIXct(as.Date(data_end_date,format="%d/%m/%Y", tz="GMT")),
    start= deployment_date + 24*60*60,
    end= case_when(retrieval_date<data_end_date~ as.character(retrieval_date),
                   retrieval_date>data_end_date~ as.character(data_end_date),
                   TRUE~as.character(NA)),
    end=as.POSIXct(end,tz="GMT"),
    datetimehour=start)

#FPOD deployments - per hour:
accessdep2h=accessdep2 %>%
  group_by(deployment_number) %>% 
  complete(datetimehour=seq(from=start,to=end,by="1 hour")) %>% 
  select(deployment_number,datetimehour) %>% 
  mutate(
    datetimehour=datetimehour+59*60 ) %>% 
  rename(fpod.depl=deployment_number) %>% 
  ungroup()

#Get dolphin absence:
fpoddatasumh2=left_join(accessdep2h,fpoddatasumh, by = c("fpod.depl", "datetimehour"))

#Fill the gaps in the fpod dataset:
fpoddatasumh2=fpoddatasumh2 %>% 
  group_by(fpod.depl) %>% 
  fill(location.id, .direction="updown") %>% 
  mutate(
    date=as.Date(datetimehour)) %>% 
  replace(is.na(.),0) %>% 
  ungroup()


#Join with CPOD dataset:
cpodfpodacc=inner_join(cpodacc,fpoddatasumh2, by = c( "location.id", "datetimehour","date")) 


#Save:
write.csv(cpodfpodacc,here::here("Data","CPOD-FPOD_Dol-DPM-BPM_perHOUR.csv"),
          row.names = F)


#2. CREATE DPH and BPH datasets - perDAY:     ----
cfpoddatasumday = cpodfpodacc %>%
  group_by(date,location.id,cpod.depl,fpod.depl) %>%
  summarise(
    cpod.DPH=sum(cpod.dol.pres),
    cpod.BPH=sum(cpod.buzz.pres),
    fpod.DPH=sum(fpod.dol.pres),
    fpod.BPH=sum(fpod.buzz.pres)) %>% 
  ungroup() %>% 
  mutate(
    cpod.dol.pres=ifelse(cpod.DPH>0,1,0),
    fpod.dol.pres=ifelse(fpod.DPH>0,1,0)  )


write.csv(cfpoddatasumday,here::here("Data","CPOD-FPOD_Dol-DPH-BPH_perDAY.csv"),row.names = F)



