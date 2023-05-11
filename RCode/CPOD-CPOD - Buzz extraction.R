library(mixtools)
library(tidyverse)
library(here)
Sys.setenv(tz="GMT")

#~~~~~~~~~~      ----
#NB data:      ----
#~~~~~~~~~~      ----

#1. Create new click details cropped files incl depl, site and device type                                  ----
originfiles=here("CPOD-CPOD","ClickDetails","NB")
finalfiles=here("CPOD-CPOD","ClickDetails_SiteDep","NB")

filelist <- as.data.frame(list.files (originfiles,pattern = "\\.csv$"))
names(filelist) <- c("files")
filelist$files <- as.character(filelist$files)

for(i in 1:length(filelist$files)) {
  try(                              #skips empty files
    {
      filename <- filelist$files[i]   #obtains the filename from the list of files        
      #reads in the text data file 
      dat <- read.csv(paste(originfiles,filename,sep="/"),header=T)
      
      dat$site= substr(filename,5,8)
      dat$dep=substr(filename,1,4)
      dat$POD="CPOD"
      
      filename1 <- strsplit(filename,".csv")
      filename2=paste0(filename1,"_sitedep.csv")
      
      write.csv(dat,paste(finalfiles,filename2, sep = "/"),row.names = F)
    }, silent = T
  )
}

#2. Paste all click details files together:    ----
#CPOD-CPOD comparison:
filelist2 <- list.files (here::here("CPOD-CPOD","ClickDetails_SiteDep","NB"))
cpod <- do.call("rbind", lapply(here::here("CPOD-CPOD","ClickDetails_SiteDep","NB",filelist2), read.csv, header = TRUE))
cpod2 = cpod %>% 
  select(Date,site,dep,POD,Minute,microsec,TrN)
data.table::fwrite(cpod2, here::here("CPOD-CPOD","ClickDetails_SiteDep","NB_Clickdetails.csv"), row.names=F)


#3. Create ICI column to model: -----
# cpod2= data.table::fread(here("Data","ClickDetails","NB","CPOD_NB_Clickdetails.csv"),header=T) %>%
#   as_tibble()

cpod3 = cpod2 %>%   
  group_by(dep,site,depl.pair,pod.priority) %>% 
  arrange(site,Minute,microsec) %>% 
  mutate(
    mincomplete = Minute + (microsec / (60 * 1000000)),
    ICI=mincomplete-lag(mincomplete),
    ICI=replace(ICI,row_number()==1,NA), 
    ICI=ifelse(ICI < 0,NA,ICI),         #negative ICIs correspond to overlapping trains and must be removed
    logICI= log(ICI)) %>%  #julian date: have to add one because it starts at 0
  ungroup() %>% 
  drop_na(logICI) #remove NA values (required to be able to run the mixmodel)


#4. Run Mixmodel - pairs priority1:    ----
mix3 <-normalmixEM(cpod3$logICI, k = 3, maxit=2000) 
summary(mix3) #Model not OK: none of the mu values is around -9 or -10 (~Buzzes group)
plot(mix3, 2)

mix4 <-normalmixEM(cpod3$logICI, k = 4, maxit=2000) 
summary(mix4) #Model OK: mu value of comp1 around -9 or -10 (~Buzzes group)
plot(mix4, 2)

# Summary of normalmixEM object:
#           comp 1    comp 2    comp 3     comp 4
# lambda  0.170849  0.358678  0.406322  0.0641508
# mu     -9.961941 -7.558783 -7.338033 -3.5500130
# sigma   0.568200  0.775111  0.199074  2.9772478
# loglik at estimate:  -8227590 
#Buzzes under component1

#5. Add click type to click details dataset:    ----
clicktype<-apply(mix4$posterior,1,which.max)

clicksbuzz<-cbind(cpod3,clicktype)  %>% 
  mutate(         
    clicktype=case_when(clicktype==1 ~ "buzz",
                        clicktype==2 ~ "regular",
                        clicktype==3 ~ "regular",
                        clicktype==4 ~ "long"))
#6. Save dataset:             ----
data.table::fwrite(clicksbuzz, here::here("CPOD-CPOD","ClickDetails_Buzzes","NB_Clickdetails_Buzzes.csv"), row.names=F)

#_____        -----
# RUN 2 different models - one for each pod priority group:   ----

#2b. Divide click details files into 2 groups: priority1 and priority2:      ----
cpod2= data.table::fread(here("CPOD-CPOD","ClickDetails_SiteDep","Dol_Clickdetails.csv"),header=T) %>%
  as_tibble()

pairlist=read.csv(here("CPOD-CPOD","PairedCPODs_List.csv"),header = T) %>% 
  rename(site=location.id,
         dep=cpod.depl) %>% 
  distinct()

cpods.pairs=left_join(cpod2,pairlist)


cpods.pairs1=cpods.pairs %>% filter(pod.priority==1)
cpods.pairs2=cpods.pairs %>% filter(pod.priority==2)

data.table::fwrite(cpods.pairs1, here::here("CPOD-CPOD","ClickDetails_SiteDep","Dol_Clickdetails_Priority1.csv"), row.names=F)
data.table::fwrite(cpods.pairs2, here::here("CPOD-CPOD","ClickDetails_SiteDep","Dol_Clickdetails_Priority2.csv"), row.names=F)

#PRIORITY1 files:         ----
##3b. Create ICI column to model: -----
# cpods.pairs1 = data.table::fread(here("CPOD-CPOD","ClickDetails_SiteDep","Dol_Clickdetails_Priority1.csv"),header=T) %>%
#   as_tibble()

cpod3 = cpods.pairs1 %>%   
  group_by(dep,site,depl.pair,pod.priority) %>% 
  arrange(site,Minute,microsec) %>% 
  mutate(
    mincomplete = Minute + (microsec / (60 * 1000000)),
    ICI=mincomplete-lag(mincomplete),
    ICI=replace(ICI,row_number()==1,NA), 
    ICI=ifelse(ICI < 0,NA,ICI),         #negative ICIs correspond to overlapping trains and must be removed
    logICI= log(ICI)) %>%  #julian date: have to add one because it starts at 0
  ungroup() %>% 
  drop_na(logICI) #remove NA values (required to be able to run the mixmodel)


##4b. Run Mixmodel - pairs priority1:    ----
mix3 <-normalmixEM(cpod3$logICI, k = 3, maxit=2000) 
summary(mix3) #Model not OK: none of the mu values is around -9 or -10 (~Buzzes group)
plot(mix3, 2)

# #Priority1: 
# summary of normalmixEM object:
#            comp 1    comp 2    comp 3
# lambda  0.334596  0.577744  0.0876599
# mu     -8.270040 -6.443211 -4.3903584
# sigma   1.318891  0.739827  3.5085881
# loglik at estimate:  -5752649 


mix4 <-normalmixEM(cpod3$logICI, k = 4, maxit=2000) 
summary(mix4) #Model OK: mu value of comp1 around -9 or -10 (~Buzzes group)
plot(mix4, 2)

# Summary of normalmixEM object:
#             comp 1    comp 2    comp 3     comp 4
# lambda  0.105935  0.122600  0.500952  0.270513
# mu     -4.946751 -9.474573 -7.158962 -5.924290
# sigma   3.576300  0.768192  0.768727  0.416094
# loglik at estimate:  -5695489 

#Buzzes under comp2

##5b. Add click type to click details dataset:    ----
clicktype<-apply(mix4$posterior,1,which.max)

clicksbuzz<-cbind(cpod3,clicktype)  %>% 
  mutate(         
    clicktype=case_when(clicktype==2 ~ "buzz",
                        clicktype==3 ~ "regular",
                        clicktype==1 ~ "long",
                        clicktype==4 ~ "long"))
##6b. Save dataset:             ----
data.table::fwrite(clicksbuzz, here::here("CPOD-CPOD","ClickDetails_Buzzes","Dol_Clickdetails_Priority1_Buzzes.csv"), row.names=F)

#PRIORITY2 files:         ----
##3c. Create ICI column to model: -----
# cpods.pairs2 = data.table::fread(here("CPOD-CPOD","ClickDetails_SiteDep","Dol_Clickdetails_Priority2.csv"),header=T) %>%
#   as_tibble()

cpod3 = cpods.pairs2 %>%   
  group_by(dep,site,depl.pair,pod.priority) %>% 
  arrange(site,Minute,microsec) %>% 
  mutate(
    mincomplete = Minute + (microsec / (60 * 1000000)),
    ICI=mincomplete-lag(mincomplete),
    ICI=replace(ICI,row_number()==1,NA), 
    ICI=ifelse(ICI < 0,NA,ICI),         #negative ICIs correspond to overlapping trains and must be removed
    logICI= log(ICI)) %>%  #julian date: have to add one because it starts at 0
  ungroup() %>% 
  drop_na(logICI) #remove NA values (required to be able to run the mixmodel)


##4c. Run Mixmodel - pairs priority2:    ----
mix3 <-normalmixEM(cpod3$logICI, k = 3, maxit=2000) 
summary(mix3) #Model not OK: none of the mu values is around -9 or -10 (~Buzzes group)
plot(mix3, 2)

# #Priority2: 
# summary of normalmixEM object:
#            comp 1    comp 2    comp 3
# lambda  0.337304  0.576732  0.085964
# mu     -8.294760 -6.442082 -4.266584
# sigma   1.319112  0.743936  3.480452
# loglik at estimate:  -5305241 

mix4 <-normalmixEM(cpod3$logICI, k = 4, maxit=2000) 
summary(mix4) #Model OK: none of the mu values is around -9 or -10 (~Buzzes group)
plot(mix4, 2)

# summary of normalmixEM object:
#            comp 1    comp 2    comp 3    comp 4
# lambda  0.0537664  0.34786  0.595955   0.0024183
# mu     -2.2173261 -8.31729 -6.437693 -12.7923539
# sigma   2.3273587  1.31319  0.755160   0.5372792
# loglik at estimate:  -5298300 

mix5 <-normalmixEM(cpod3$logICI, k = 5, maxit=2000) 
summary(mix5) #Model OK: none of the mu values is around -9 or -10 (~Buzzes group)
plot(mix5, 2)



#Buzzes under comp


##5c. Add click type to click details dataset:    ----
clicktype<-apply(mix4$posterior,1,which.max)

clicksbuzz<-cbind(cpod3,clicktype)  %>% 
  mutate(         
    clicktype=case_when(clicktype==1 ~ "buzz",
                        clicktype==2 ~ "regular",
                        clicktype==3 ~ "regular",
                        clicktype==4 ~ "long"))
##6c. Save dataset:             ----
data.table::fwrite(clicksbuzz, here::here("CPOD-CPOD","ClickDetails_Buzzes","Dol_Clickdetails_Priority2_Buzzes.csv"), row.names=F)



##7. Paste both priority1 and priority 2 together:
buzz.p1 = read.csv(here("CPOD-CPOD","ClickDetails_Buzzes","Dol_Clickdetails_Priority1_Buzzes.csv"),header=T) 
buzz.p2 = read.csv(here("CPOD-CPOD","ClickDetails_Buzzes","Dol_Clickdetails_Priority2_Buzzes.csv"),header=T) 

buzz.p1p2=rbind(buzz.p1,buzz.p2)
data.table::fwrite(buzz.p1p2, here::here("CPOD-CPOD","ClickDetails_Buzzes","Dol_Clickdetails_Priority1-2_Buzzes.csv"), row.names=F)



#~~~~~~~~~~      ----
#~~~~~~~~~~      ----
#Dol data:      ----
#~~~~~~~~~~      ----

#1. Create new click details cropped files incl depl, site and device type                                  ----
originfiles=here("CPOD-CPOD","ClickDetails","Dol")
finalfiles=here("CPOD-CPOD","ClickDetails_SiteDep","Dol")

filelist <- as.data.frame(list.files (originfiles,pattern = "\\.csv$"))
names(filelist) <- c("files")
filelist$files <- as.character(filelist$files)

for(i in 1:length(filelist$files)) {
  try(                              #skips empty files
    {
      filename <- filelist$files[i]   #obtains the filename from the list of files        
      #reads in the text data file 
      dat <- read.csv(paste(originfiles,filename,sep="/"),header=T)
      
      dat$site= substr(filename,5,8)
      dat$dep=substr(filename,1,4)
      dat$POD="CPOD"
      
      filename1 <- strsplit(filename,".csv")
      filename2=paste0(filename1,"_sitedep.csv")
      
      write.csv(dat,paste(finalfiles,filename2, sep = "/"),row.names = F)
    }, silent = T
  )
}

#2. Paste all click details files together:    ----
#CPOD-CPOD comparison:
filelist2 <- list.files (here::here("CPOD-CPOD","ClickDetails_SiteDep","Dol"))
cpod <- do.call("rbind", lapply(here::here("CPOD-CPOD","ClickDetails_SiteDep","Dol",filelist2), read.csv, header = TRUE))
cpod2 = cpod %>% 
  select(Date,site,dep,POD,Minute,microsec,TrN)
data.table::fwrite(cpod2, here::here("CPOD-CPOD","ClickDetails_SiteDep","Dol_Clickdetails.csv"), row.names=F)


#3. Create ICI column to model: -----

# cpod2= data.table::fread(here("CPOD-CPOD","ClickDetails_SiteDep","Dol_Clickdetails.csv),header=T) %>% 
#   as_tibble()

cpod3 = cpod2 %>% 
  group_by(dep,site) %>% 
  arrange(site,Minute,microsec) %>% 
  mutate(
    mincomplete = Minute + (microsec / (60 * 1000000)),
    ICI=mincomplete-lag(mincomplete),
    ICI=replace(ICI,row_number()==1,NA), 
    ICI=ifelse(ICI < 0,NA,ICI),         #negative ICIs correspond to overlapping trains and must be removed
    logICI= log(ICI)) %>%  #julian date: have to add one because it starts at 0
  ungroup() %>% 
  drop_na(logICI) #remove NA values (required to be able to run the mixmodel)


#4. Run Mixmodel:    ----
mix3 <-normalmixEM(cpod3$logICI, k = 3, maxit=2000) 
summary(mix3) #Model not OK: none of the mu values is around -9 or -10 (~Buzzes group)
plot(mix3, 2)

mix4 <-normalmixEM(cpod3$logICI, k = 4, maxit=2000) 
summary(mix4) #Model OK: mu value of comp1 around -9 or -10 (~Buzzes group)
plot(mix4, 2)

# Summary of normalmixEM object:
#           comp 1    comp 2    comp 3     comp 4


# Summary of normalmixEM object:
#           comp 1    comp 2    comp 3     comp 4
 

 
#Buzzes under component4

#5. Add click type to click details dataset:    ----
clicktype<-apply(mix4$posterior,1,which.max)

clicksbuzz<-cbind(cpod3,clicktype)  %>% 
  mutate(         
    clicktype=case_when(clicktype==1 ~ "regular",
                        clicktype==2 ~ "long",
                        clicktype==3 ~ "regular",
                        clicktype==4 ~ "buzz"))
#6. Save dataset:             ----
data.table::fwrite(clicksbuzz, here::here("CPOD-CPOD","ClickDetails_Buzzes","Dol_Clickdetails_Buzzes.csv"), row.names=F)


#_____        -----
# RUN 2 different models - one for each pod priority group:   ----

#2b. Divide click details files into 2 groups: priority1 and priority2:      ----
cpod2= data.table::fread(here("CPOD-CPOD","ClickDetails_SiteDep","Dol_Clickdetails.csv"),header=T) %>%
  as_tibble()

pairlist=read.csv(here("CPOD-CPOD","PairedCPODs_List.csv"),header = T) %>% 
  rename(site=location.id,
         dep=cpod.depl) %>% 
  distinct()

cpods.pairs=left_join(cpod2,pairlist)


cpods.pairs1=cpods.pairs %>% filter(pod.priority==1)
cpods.pairs2=cpods.pairs %>% filter(pod.priority==2)

data.table::fwrite(cpods.pairs1, here::here("CPOD-CPOD","ClickDetails_SiteDep","Dol_Clickdetails_Priority1.csv"), row.names=F)
data.table::fwrite(cpods.pairs2, here::here("CPOD-CPOD","ClickDetails_SiteDep","Dol_Clickdetails_Priority2.csv"), row.names=F)

#PRIORITY1 files:         ----
##3b. Create ICI column to model: -----
# cpods.pairs1 = data.table::fread(here("CPOD-CPOD","ClickDetails_SiteDep","Dol_Clickdetails_Priority1.csv"),header=T) %>%
#   as_tibble()

cpod3 = cpods.pairs1 %>%   
  group_by(dep,site,depl.pair,pod.priority) %>% 
  arrange(site,Minute,microsec) %>% 
  mutate(
    mincomplete = Minute + (microsec / (60 * 1000000)),
    ICI=mincomplete-lag(mincomplete),
    ICI=replace(ICI,row_number()==1,NA), 
    ICI=ifelse(ICI < 0,NA,ICI),         #negative ICIs correspond to overlapping trains and must be removed
    logICI= log(ICI)) %>%  #julian date: have to add one because it starts at 0
  ungroup() %>% 
  drop_na(logICI) #remove NA values (required to be able to run the mixmodel)

cpod3.908=filter(cpod3,dep==908)



##4b. Run Mixmodel - pairs priority1:    ----
mix3 <-normalmixEM(cpod3$logICI, k = 3, maxit=2000) 
summary(mix3) #Model not OK: none of the mu values is around -9 or -10 (~Buzzes group)
plot(mix3, 2)

# #Priority1: 
# summary of normalmixEM object:
#            comp 1    comp 2    comp 3
# lambda  0.357755  0.590772  0.0514726
# mu     -8.248598 -6.454472 -2.1155419
# sigma   1.426720  0.758505  2.2413965
# loglik at estimate:  -5733273  

mix4 <-normalmixEM(cpod3$logICI, k = 4, maxit=2000) 
summary(mix4) #Model OK: mu value of comp1 around -9 or -10 (~Buzzes group)
plot(mix4, 2)

# Summary of normalmixEM object:
#             comp 1    comp 2    comp 3     comp 4
# lambda  0.122421  0.500638  0.27106  0.105881
# mu     -9.471260 -7.159210 -5.92395 -4.945106
# sigma   0.771400  0.767853  0.41595  3.573495
# loglik at estimate:  -5677794 



#Buzzes under comp1

##5b. Add click type to click details dataset:    ----
clicktype<-apply(mix4$posterior,1,which.max)

clicksbuzz<-cbind(cpod3,clicktype)  %>% 
  mutate(         
    clicktype=case_when(clicktype==1 ~ "buzz",
                        clicktype==3 ~ "regular",
                        clicktype==4 ~ "regular",
                        clicktype==2 ~ "long"))
##6b. Save dataset:             ----
data.table::fwrite(clicksbuzz, here::here("CPOD-CPOD","ClickDetails_Buzzes","Dol_Clickdetails_Priority1_Buzzes.csv"), row.names=F)

#PRIORITY2 files:         ----
##3c. Create ICI column to model: -----
# cpods.pairs2 = data.table::fread(here("CPOD-CPOD","ClickDetails_SiteDep","Dol_Clickdetails_Priority2.csv"),header=T) %>%
#   as_tibble()

cpod3 = cpods.pairs2 %>%   
  group_by(dep,site,depl.pair,pod.priority) %>% 
  arrange(site,Minute,microsec) %>% 
  mutate(
    mincomplete = Minute + (microsec / (60 * 1000000)),
    ICI=mincomplete-lag(mincomplete),
    ICI=replace(ICI,row_number()==1,NA), 
    ICI=ifelse(ICI < 0,NA,ICI),         #negative ICIs correspond to overlapping trains and must be removed
    logICI= log(ICI)) %>%  #julian date: have to add one because it starts at 0
  ungroup() %>% 
  drop_na(logICI) #remove NA values (required to be able to run the mixmodel)


##4c. Run Mixmodel - pairs priority2:    ----
mix3 <-normalmixEM(cpod3$logICI, k = 3, maxit=2000) 
summary(mix3) #Model not OK: none of the mu values is around -9 or -10 (~Buzzes group)
plot(mix3, 2)

# #Priority2: 
# summary of normalmixEM object:
#            comp 1    comp 2    comp 3
# lambda  0.0860805  0.336932  0.576988
# mu     -4.2741534 -8.289373 -6.442603
# sigma   3.4817372  1.319317  0.744348
# loglik at estimate:  -5284833 


mix4 <-normalmixEM(cpod3$logICI, k = 4, maxit=2000) 
summary(mix4) #Model not OK: none of the mu values is around -9 or -10 (~Buzzes group)
plot(mix4, 2)

# summary of normalmixEM object:
#            comp 1    comp 2    comp 3    comp 4  
# lambda  0.122918  0.517726  0.254534  0.104822
# mu     -9.501398 -7.140890 -5.902181 -4.874505
# sigma   0.754914  0.785025  0.404796  3.572948
# loglik at estimate:  -5230803 



#Buzzes under comp1

##5c. Add click type to click details dataset:    ----
clicktype<-apply(mix4$posterior,1,which.max)

clicksbuzz<-cbind(cpod3,clicktype)  %>% 
  mutate(         
    clicktype=case_when(clicktype==1 ~ "buzz",
                        clicktype==3 ~ "regular",
                        clicktype==4 ~ "regular",
                        clicktype==2 ~ "long"))

##6c. Save dataset:             ----
data.table::fwrite(clicksbuzz, here::here("CPOD-CPOD","ClickDetails_Buzzes","Dol_Clickdetails_Priority2_Buzzes.csv"), row.names=F)


##7. Paste both priority1 and priority 2 together:
buzz.p1 = read.csv(here("CPOD-CPOD","ClickDetails_Buzzes","Dol_Clickdetails_Priority1_Buzzes.csv"),header=T) 
buzz.p2 = read.csv(here("CPOD-CPOD","ClickDetails_Buzzes","Dol_Clickdetails_Priority2_Buzzes.csv"),header=T) 

buzz.p1p2=rbind(buzz.p1,buzz.p2)
data.table::fwrite(buzz.p1p2, here::here("CPOD-CPOD","ClickDetails_Buzzes","Dol_Clickdetails_Priority1-2_Buzzes.csv"), row.names=F)




