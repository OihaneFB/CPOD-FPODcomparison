library(tidyverse)


#Access database DPH:
accessdph=read.csv(here::here("Data","Access","Access_Summary data DPH per day.txt"),
                   sep="\t",header = T) %>% 
  rename_all(list(~tolower(.))) %>% 
  rename(cpod.depl=deployment.number) %>% 
  filter(pod.type=="cpod", year>2009,number.of.pods==2) %>% 
  mutate(
    date=as.Date(date,format="%d/%m/%Y %H:%M:%S"))


#Deployments and dates with 2 CPODs:

multipledepl = accessdph %>% 
  distinct(year,date,location.id,cpod.depl,pod.priority)

pairs = multipledepl %>% 
  group_by(date,location.id) %>% 
  summarise(
    depl.pair=toString(unique(cpod.depl))) %>% 
  filter(nchar(depl.pair)>4) %>% 
  group_by(date,location.id) %>% 
  mutate(
    depl1=as.numeric(strsplit(depl.pair, ",")[[1]][1]),
    depl2=as.numeric(strsplit(depl.pair, ",")[[1]][2])) %>% 
  ungroup()

#Datasets with pod.depl and dates with 2 CPODs:
paired.datedepl=pivot_longer(pairs,!c(date,location.id,depl.pair),names_to = "test",values_to = "cpod.depl") %>% select(-test)
paired.datedepl2=left_join(paired.datedepl,multipledepl)

write.csv(paired.datedepl2,here::here("CPOD-CPOD","PairedCPODs_DataDays.csv"),row.names = F)

#List of paired CPOD deployments:
dep.list=paired.datedepl %>% distinct(cpod.depl) %>% pull()

paireddep.year=paired.datedepl2 %>% distinct(location.id,cpod.depl,pod.priority,depl.pair)

write.csv(paireddep.year,here::here("CPOD-CPOD","PairedCPODs_List.csv"),row.names = F)




