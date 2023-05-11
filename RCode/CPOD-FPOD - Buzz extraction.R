library(mixtools)
library(tidyverse)
library(here)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~----
#HP        ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~----

#~~~~~~~~~~      ----
#CPOD data:      ----
#~~~~~~~~~~      ----

#1. Create new click details cropped files incl depl, site and device type                                  ----
originfiles=here("Data","ClickDetails","NB","CPOD")
finalfiles=here("Data","ClickDetails","NB","CPOD","ClickDetails_SiteDep")

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
filelist2 <- list.files (here::here("Data","ClickDetails","NB","CPOD","ClickDetails_SiteDep"))
cpod <- do.call("rbind", lapply(here::here("Data","ClickDetails","NB","CPOD","ClickDetails_SiteDep",filelist2), read.csv, header = TRUE))
cpod2 = cpod %>% 
  select(Date,site,dep,POD,Minute,microsec,TrN)
data.table::fwrite(cpod2, here::here("Data","ClickDetails","NB","CPOD_NB_Clickdetails.csv"), row.names=F)

#3. Create ICI column to model: -----

# cpod2= data.table::fread(here("Data","ClickDetails","NB","CPOD_NB_Clickdetails.csv"),header=T) %>%
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
summary(mix3) #Model not OK: not one comp is around -9 or -10 (~Buzzes group)
plot(mix3, 2)

mix4 <-normalmixEM(cpod3$logICI, k = 4, maxit=2000) 
summary(mix4) #Model OK: not one comp is around -9 or -10 (~Buzzes group)
plot(mix4, 2)

# summary of normalmixEM object:
#            comp 1    comp 2    comp 3    comp 4
# lambda  0.0672729  0.372209  0.371263  0.189255
# mu     -3.6463934 -7.405796 -7.276117 -9.654249
# sigma   3.0860487  0.691235  0.203189  0.679645
# loglik at estimate:  -5673746 


#5. Add click type to click details dataset:    ----
clicktype<-apply(mix4$posterior,1,which.max)

clicksbuzz<-cbind(cpod3,clicktype)  %>% 
  mutate(         
    clicktype=case_when(clicktype==1 ~ "long",
                        clicktype==2 ~ "regular",
                        clicktype==3 ~ "regular",
                        clicktype==4 ~ "buzz"))

#6. Save dataset:             ----
data.table::fwrite(clicksbuzz, here::here("Data","ClickDetails_Buzzes","CPOD_NB_Clickdetails_Buzzes.csv"), row.names=F)


#~~~~~~~~~~      ----
# FPOD data:     ----
#~~~~~~~~~~      ----

#1. Paste all fpod click details files together ----
filelist3 <- list.files (here::here("Data","ClickDetails","NB","FPOD"))
fpod <- do.call("rbind", lapply(here::here("Data","ClickDetails","NB","FPOD",filelist3), read.csv, header = TRUE))
fpod2 = fpod %>%
  rename(dep=depl,TrN=TrnIDn) %>% 
  mutate(POD="FPOD") %>% 
  select(Date,site,dep,POD,Minute,microsec,TrN)

data.table::fwrite(fpod2, here::here("Data","ClickDetails","NB","FPOD_NB_Clickdetails.csv"), row.names=F)


#2. Create ICI column to model -----

# fpod2= data.table::fread(here("Data","ClickDetails","NB","FPOD_NB_Clickdetails.csv"),header=T) %>%
#   as_tibble()

fpod3 = fpod2 %>% 
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


#3. Run Mixmodel:   ----
mix3 <-normalmixEM(fpod3$logICI, k = 3, maxit=2000) 
summary(mix3) #Model not OK: none of the mu values is around -9 or -10 (~Buzzes group)
plot(mix3, 2)

# summary of normalmixEM object:
#           comp 1    comp 2     comp 3
# lambda  0.299521  0.645444  0.0550347
# mu     -9.868441 -7.339847 -4.0941176
# sigma   0.658336  0.576337  3.0448801
# loglik at estimate:  -9618588 

#Buzzes under component1

#4. Add click type to click details dataset:    ----
clicktype<-apply(mix3$posterior,1,which.max)

clicksbuzz<-cbind(fpod3,clicktype)  %>% 
  mutate(         
    clicktype=case_when(clicktype==1 ~ "buzz",
                        clicktype==2 ~ "regular",
                        clicktype==3 ~ "long"))

#5. Save dataset:   ----
data.table::fwrite(clicksbuzz, here::here("Data","ClickDetails_Buzzes","FPOD_NB_Clickdetails_Buzzes.csv"), row.names=F)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~----

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~----
#Dol        ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~----

#~~~~~~~~~~      ----
#CPOD data:      ----
#~~~~~~~~~~      ----

#1. Create new click details cropped files incl depl, site and device type                                  ----
originfiles=here("Data","ClickDetails","Dol","CPOD")
finalfiles=here("Data","ClickDetails","Dol","CPOD","ClickDetails_SiteDep")

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
filelist2 <- list.files (here::here("Data","ClickDetails","Dol","CPOD","ClickDetails_SiteDep"))
cpod <- do.call("rbind", lapply(here::here("Data","ClickDetails","Dol","CPOD","ClickDetails_SiteDep",filelist2), read.csv, header = TRUE))
cpod2 = cpod %>% 
  select(Date,site,dep,POD,Minute,microsec,TrN)
data.table::fwrite(cpod2, here::here("Data","ClickDetails","Dol","CPOD_Dol_Clickdetails.csv"), row.names=F)

#3. Create ICI column to model: -----

cpod2= data.table::fread(here("Data","ClickDetails","Dol","CPOD_Dol_Clickdetails.csv"),header=T) %>%
  as_tibble()

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
summary(mix3) #Model OK: one comp is around -9 or -10 (~Buzzes group)
plot(mix3, 2)

# > summary(mix3) 
# summary of normalmixEM object:
#           comp 1    comp 2     comp 3
# lambda  0.217292  0.686936  0.0957719
# mu     -9.229492 -6.844730 -4.3494056
# sigma   0.852815  0.736161  3.2362683
# loglik at estimate:  -1552847 


#5. Add click type to click details dataset:    ----
clicktype<-apply(mix3$posterior,1,which.max)

clicksbuzz<-cbind(cpod3,clicktype)  %>% 
  mutate(         
    clicktype=case_when(clicktype==1 ~ "buzz",
                        clicktype==2 ~ "regular",
                        clicktype==3 ~ "long"))

#6. Save dataset:             ----
data.table::fwrite(clicksbuzz, here::here("Data","ClickDetails_Buzzes","CPOD_Dol_Clickdetails_Buzzes.csv"), row.names=F)


#~~~~~~~~~~      ----
# FPOD data:     ----
#~~~~~~~~~~      ----

#1. Paste all fpod click details files together ----
filelist3 <- list.files (here::here("Data","ClickDetails","Dol","FPOD"))
fpod <- do.call("rbind", lapply(here::here("Data","ClickDetails","Dol","FPOD",filelist3), read.csv, header = TRUE))
fpod2 = fpod %>%
  rename(dep=depl,TrN=TrnIDn) %>% 
  mutate(POD="FPOD") %>% 
  select(Date,site,dep,POD,Minute,microsec,TrN)

data.table::fwrite(fpod2, here::here("Data","ClickDetails","Dol","FPOD_Dol_Clickdetails.csv"), row.names=F)


#2. Create ICI column to model -----
fpod2= data.table::fread(here("Data","ClickDetails","Dol","FPOD_Dol_Clickdetails.csv"),header=T) %>%
  as_tibble()

fpod3 = fpod2 %>% 
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


#3. Run Mixmodel:   ----
#mix3 <-normalmixEM(fpod3$logICI, k = 3, maxit=2000, mean.const=c(-9,NA,NA))
mix3 <-normalmixEM(fpod3$logICI, k = 3, maxit=2000) 
summary(mix3) #Model OK: the mu value of one group is around -9 or -10 (~Buzzes group)
plot(mix3, 2)

# Summary of normalmixEM object:
#             comp 1    comp 2    comp 3
# lambda   0.2258397  0.358319  0.415841
# mu     -10.3052468 -6.847827 -7.559251
# sigma    0.0837304  0.610324  2.209938
# loglik at estimate:  -10150894  
 

#Buzzes under component1

#4. Add click type to click details dataset:    ----
clicktype<-apply(mix3$posterior,1,which.max)

fclicksbuzz<-cbind(fpod3,clicktype)  %>% 
  mutate(         
    clicktype=case_when(clicktype==1 ~ "buzz",
                        clicktype==3 ~ "regular",
                        clicktype==2 ~ "long"))

#5. Save dataset:   ----
data.table::fwrite(fclicksbuzz, here::here("Data","ClickDetails_Buzzes","FPOD_Dol_Clickdetails_Buzzes.csv"), row.names=F)

