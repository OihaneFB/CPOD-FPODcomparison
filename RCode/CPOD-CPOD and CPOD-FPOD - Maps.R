Sys.setenv(TZ = "GMT")

library(tidyverse)
library(sf)
library(ggspatial)                # needed for annotation scale
library(ggpubr) # for ggarrange function
library(cowplot)



#GRAL map features:              ----
land <- st_read(here::here("Data","GIS","Coastline_UTM30.shp"))
st_crs(land) # crs=32630


#HP----
#CPOD-FPOD HP deployments:             ----
cfpods.hp=read.csv(here::here("Data","CPOD-FPOD_HP-DPH-BPH_perDAY.csv"),header = T)

cpoddep=cfpods.hp %>% distinct(cpod.depl) %>% rename(depl=cpod.depl)
fpoddep=cfpods.hp %>% distinct(fpod.depl) %>% rename(depl=fpod.depl)
cfdep=rbind(cpoddep,fpoddep) %>% pull

#CPOD-CPOD HP deployments:             ----
ccpods.hp=read.csv(here::here("Data","CPOD-CPOD_HP-DPH-BPH_perDAY.csv"),header = T)
ccdep=ccpods.hp %>% distinct(cpod.depl) %>% pull()


#HP POD locations:
#HP CPOD-FPOD:
cfhp.dep <- read.table(here::here("Data","Access","Acc_Deployments.txt"),sep="\t",header=T) %>% 
  filter(Deployment.number %in% cfdep) %>% 
  select(Location.ID,Deployment.number,POD.Type,Latitude,Longitude,Deployment.time.GMT,Retrieval.time.GMT)

cfhp.pods_WGS <- st_as_sf(cfhp.dep, coords=c("Longitude", "Latitude"), crs=4326)
cfhp.pods_sf1 <- st_transform(cfhp.pods_WGS, crs=32630)
cfhp.pods_coord=st_coordinates(cfhp.pods_sf1)
cfhp.pods_sf=cbind(cfhp.pods_sf1,cfhp.pods_coord)
cfhp.pods_sf_unique=cfhp.pods_sf %>% distinct(Location.ID,X,Y)

#HP CPOD-CPOD:
cchp.dep <- read.table(here::here("Data","Access","Acc_Deployments.txt"),sep="\t",header=T) %>% 
  filter(Deployment.number %in% ccdep) %>% 
  select(Location.ID,Deployment.number,POD.Type,Latitude,Longitude,Deployment.time.GMT,Retrieval.time.GMT) %>% 
  group_by(Location.ID) %>% 
  filter(Latitude==max(Latitude)) %>% 
  ungroup()

cchp.pods_WGS <- st_as_sf(cchp.dep, coords=c("Longitude", "Latitude"), crs=4326)
cchp.pods_sf1 <- st_transform(cchp.pods_WGS, crs=32630)
cchp.pods_coord=st_coordinates(cchp.pods_sf1)
cchp.pods_sf=cbind(cchp.pods_sf1,cchp.pods_coord)
cchp.pods_sf_unique=cchp.pods_sf %>% distinct(Location.ID,X,Y) 


#HP: Maps ----

##1.HP CPOD-FPOD MAP: 
cfhp.deployments <- ggplot() +
  geom_sf(data=land, fill="lightgrey")+
  geom_sf(data=BOWL_UTM, col="#98afba", fill="white",size=0.8)+
  geom_sf(data=MEOW_UTM, col="#98afba", fill="white",size=0.8)+
  geom_sf(data=MWOW_UTM, col="#98afba", fill="white",size=0.8)+
  geom_sf(data=cfhp.pods_sf, colour="#00393c",size=2.5) +
  ggrepel::geom_text_repel(data=cfhp.pods_sf_unique,aes(x=X,y=Y,label=Location.ID),
                           box.padding = 0.6,max.overlaps = Inf) +
  
  annotation_scale(location = "br", style = "ticks", text_cex = 1, width_hint = 0.4) +
  annotation_north_arrow(location = "tl",
                         height=unit(1.5,"cm"),width=unit(0.5,"cm"),which_north = "true",
                         style=north_arrow_orienteering(text_size = 8)) +
  coord_sf(xlim = c(488000, 530000),  ylim = c(6430000, 6469000))+     # to show offshore depl
  #coord_sf(xlim = c(430000, 450000),  ylim = c(6380000, 6400000))+    # to show inshore depl
  theme_bw() +
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right")

cfhp.deployments 

ggsave(cfhp.deployments, file=here::here("Maps","CPOD-FPOD_Deployments_ins_HP.png"), 
       width = 10, height = 10, units = "cm")
ggsave(cfhp.deployments, file=here::here("Maps","CPOD-FPOD_Deployments_off_HP.png"), 
       width = 10, height = 10, units = "cm")

#HP CPOD-FPOD MAP: 
cchp.deployments <- ggplot() +
  geom_sf(data=land, fill="lightgrey")+
  geom_sf(data=BOWL_UTM, col="#98afba", fill="white",size=0.8)+
  geom_sf(data=MEOW_UTM, col="#98afba", fill="white",size=0.8)+
  geom_sf(data=MWOW_UTM, col="#98afba", fill="white",size=0.8)+
  geom_sf(data=cchp.pods_sf, colour="#00393c",size=2.5) +
  ggrepel::geom_text_repel(data=cchp.pods_sf_unique,aes(x=X,y=Y,label=Location.ID),
                           box.padding = 0.6,max.overlaps = Inf) +
  
  annotation_scale(location = "br", style = "ticks", text_cex = 1, width_hint = 0.4) +
  annotation_north_arrow(location = "tl",
                         height=unit(1.5,"cm"),width=unit(0.5,"cm"),which_north = "true",
                         style=north_arrow_orienteering(text_size = 8)) +
  #coord_sf(xlim = c(488000, 530000),  ylim = c(6430000, 6469000))+     # to show offshore depl
  coord_sf(xlim = c(430000, 450000),  ylim = c(6380000, 6400000))+     # to show inshore depl
  theme_bw() +
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right")

cchp.deployments 

ggsave(cchp.deployments, file=here::here("Maps","CPOD-CPOD_Deployments_ins_HP.png"), 
       width = 10, height = 10, units = "cm")
ggsave(cchp.deployments, file=here::here("Maps","CPOD-CPOD_Deployments_off_HP.png"), 
       width = 10, height = 10, units = "cm")


#DOL ----
#CPOD-FPOD Dol deployments:             ----
cfpods.bn=read.csv(here::here("Data","CPOD-FPOD_Dol-DPH-BPH_perDAY.csv"),header = T)
cpoddep=cfpods.bn %>% distinct(cpod.depl) %>% rename(depl=cpod.depl)
fpoddep=cfpods.bn %>% distinct(fpod.depl) %>% rename(depl=fpod.depl)
cfdep.bn=rbind(cpoddep,fpoddep) %>% pull

#CPOD-CPOD Dol deployments:             ----
ccpods.bn=read.csv(here::here("Data","CPOD-CPOD_Dol-DPH-BPH_perDAY.csv"),header = T)
ccdep.bn=ccpods.bn %>% filter(location.id<200) %>% distinct(cpod.depl)  %>% pull()



#Dol CPOD-FPOD:
cfbn.dep <- read.table(here::here("Data","Access","Acc_Deployments.txt"),sep="\t",header=T) %>% 
  filter(Deployment.number %in% cfdep.bn) %>% 
  select(Location.ID,Deployment.number,POD.Type,Latitude,Longitude,Deployment.time.GMT,Retrieval.time.GMT) %>% 
  group_by(Location.ID) %>% 
  filter(Latitude==max(Latitude)) %>% 
  ungroup()



cfbn.pods_WGS <- st_as_sf(cfbn.dep, coords=c("Longitude", "Latitude"), crs=4326)
cfbn.pods_sf1 <- st_transform(cfbn.pods_WGS, crs=32630)
cfbn.pods_coord=st_coordinates(cfbn.pods_sf1)
cfbn.pods_sf=cbind(cfbn.pods_sf1,cfbn.pods_coord)
cfbn.pods_sf_unique=cfbn.pods_sf %>% distinct(Location.ID,X,Y)


#Dol CPOD-CPOD:
ccbn.dep <- read.table(here::here("Data","Access","Acc_Deployments.txt"),sep="\t",header=T) %>% 
  filter(Deployment.number %in% ccdep.bn) %>% 
  select(Location.ID,Deployment.number,POD.Type,Latitude,Longitude,Deployment.time.GMT,Retrieval.time.GMT) %>% 
  group_by(Location.ID) %>% 
  filter(Latitude==max(Latitude)) %>% 
  ungroup()

ccbn.pods_WGS <- st_as_sf(ccbn.dep, coords=c("Longitude", "Latitude"), crs=4326)
ccbn.pods_sf1 <- st_transform(ccbn.pods_WGS, crs=32630)
ccbn.pods_coord=st_coordinates(ccbn.pods_sf1)
ccbn.pods_sf=cbind(ccbn.pods_sf1,ccbn.pods_coord)
ccbn.pods_sf_unique=ccbn.pods_sf %>% distinct(Location.ID,X,Y)




#Dol CPOD-FPOD MAP: 
cfbn.deployments <- ggplot() +
  geom_sf(data=land, fill="lightgrey")+
  geom_sf(data=BOWL_UTM, col="#98afba", fill="white",size=0.8)+
  geom_sf(data=MEOW_UTM, col="#98afba", fill="white",size=0.8)+
  geom_sf(data=MWOW_UTM, col="#98afba", fill="white",size=0.8)+
  geom_sf(data=cfbn.pods_sf, colour="#00393c",size=2.5) +
  ggrepel::geom_text_repel(data=cfbn.pods_sf_unique,aes(x=X,y=Y,label=Location.ID),
                           box.padding = 0.6,max.overlaps = Inf) +
  
  annotation_scale(location = "br", style = "ticks", text_cex = 1, width_hint = 0.3) +
  annotation_north_arrow(location = "tl",
                         height=unit(1.5,"cm"),width=unit(0.5,"cm"),which_north = "true",
                         style=north_arrow_orienteering(text_size = 8)) +
  #coord_sf(xlim = c(488000, 530000),  ylim = c(6430000, 6469000))+     # to show offshore depl
  coord_sf(xlim = c(430000, 450000),  ylim = c(6380000, 6400000))+    # to show inshore depl
  theme_bw() +
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right")

cfbn.deployments 

ggsave(cfbn.deployments, file=here::here("Maps","CPOD-FPOD_Deployments_Dol.png"), 
       width = 10, height = 10, units = "cm")


#Dol CPOD-CPOD MAP: 
ccbn.deployments <- ggplot() +
  geom_sf(data=land, fill="lightgrey")+
  geom_sf(data=BOWL_UTM, col="#98afba", fill="white",size=0.8)+
  geom_sf(data=MEOW_UTM, col="#98afba", fill="white",size=0.8)+
  geom_sf(data=MWOW_UTM, col="#98afba", fill="white",size=0.8)+
  geom_sf(data=ccbn.pods_sf, colour="#00393c",size=2.5) +
  ggrepel::geom_text_repel(data=ccbn.pods_sf_unique,aes(x=X,y=Y,label=Location.ID),
                           box.padding = 0.6,max.overlaps = Inf) +
  
  annotation_scale(location = "br", style = "ticks", text_cex = 1, width_hint = 0.3) +
  annotation_north_arrow(location = "tl",
                         height=unit(1.5,"cm"),width=unit(0.5,"cm"),which_north = "true",
                         style=north_arrow_orienteering(text_size = 8)) +
  #coord_sf(xlim = c(488000, 530000),  ylim = c(6430000, 6469000))+     # to show offshore depl
  coord_sf(xlim = c(430000, 450000),  ylim = c(6380000, 6400000))+     # to show inshore depl
  theme_bw() +
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right")

ccbn.deployments 

ggsave(ccbn.deployments, file=here::here("Maps","CPOD-CPOD_Deployments_Dol.png"), 
       width = 10, height = 10, units = "cm")


#________________________________________----
#REPORT MAP:                             ----

#Figure1: ----

fig1off <- ggplot() +
  geom_sf(data=land, fill="#B8BCC1")+
  geom_sf(data=cfhp.pods_sf, colour="black",fill="#EAB14B",size=1.5,shape=21) +
  geom_sf(data=cchp.pods_sf,colour="black" , fill="black",size=1.5,shape=21) +
  geom_text(data = cfhp.pods_sf, aes(X+500, Y+500, label = Location.ID), colour = "black") +
  geom_text(data = cchp.pods_sf_unique, 
            aes(X+c(600,600,600), Y+c(-500,0,500), label = Location.ID),
            colour = "black") +
  annotation_scale(location = "br", style = "ticks", text_cex = 1, width_hint = 0.4) +
  coord_sf(xlim = c(502000, 519000),  ylim = c(6449000, 6465000))+   # to show offshore depl
  theme_bw()+
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right")

fig1off 


fig1sut <- ggplot() +
  geom_sf(data=land, fill="#B8BCC1")+
  geom_sf(data=cchp.pods_sf,colour="black" , fill="black",size=1.5,shape=21) +
  geom_sf(data=cfhp.pods_sf, colour="black",fill="#EAB14B",size=1.5,shape=21) +
  annotation_scale(location = "br", style = "ticks", text_cex = 1, width_hint = 0.4) +
  geom_text(data = cchp.pods_sf, aes(X+100, Y+100, label = Location.ID), colour = "black") +
  coord_sf(xlim = c(439500, 442000),  ylim = c(6392850, 6395350))+ 
  theme_bw()+
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right")

fig1sut


fig1cha <- ggplot() +
  geom_sf(data=land, fill="#B8BCC1")+
  geom_sf(data=cchp.pods_sf,colour="black" , fill="black",size=1.5,shape=21) +
  geom_sf(data=cfhp.pods_sf, colour="black",fill="#EAB14B",size=1.5,shape=21) +
  annotation_scale(location = "br", style = "ticks", text_cex = 1, width_hint = 0.4) +
  geom_text(data = cchp.pods_sf, aes(X+100, Y+100, label = Location.ID), colour = "black") +
    geom_text(data = cfhp.pods_sf, aes(X+100, Y+100, label = Location.ID), colour = "black") +
  coord_sf(xlim = c(433200, 436500),  ylim = c(6380800, 6384100))+ 
  theme_bw()+
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right")

fig1cha


sut = data.frame(
  lat= c(6392850, 6395350),
  lon = c(439500, 442000)) 

sut_sf = st_polygon(
  list(
    cbind(
      sut$lon[c(1,2,2,1,1)], 
      sut$lat[c(1,1,2,2,1)]))) %>% 
  st_sfc(crs = 32630)

cha = data.frame(
  lat= c(6380800, 6384100),
  lon = c(433200, 436500)) 

cha_sf = st_polygon(
  list(
    cbind(
      cha$lon[c(1,2,2,1,1)], 
      cha$lat[c(1,1,2,2,1)]))) %>% 
  st_sfc(crs = 32630)


offshore = data.frame(
  lat=c(6449000, 6465000),
  lon=c(502000, 519000))

offshore_sf = st_polygon(
  list(
    cbind(
      offshore$lon[c(1,2,2,1,1)], 
      offshore$lat[c(1,1,2,2,1)]))) %>% 
  st_sfc(crs = 32630)


moraylandmap= ggplot()+
  geom_sf(data=land,fill="#B8BCC1")+
  geom_sf(data = sut_sf, fill = NA, color = "black", size = 2) +
  geom_sf(data = cha_sf, fill = NA, color = "black", size = 2) +
  geom_sf(data = offshore_sf, fill = NA, color = "black", size = 2) +
  geom_sf(data=cfhp.pods_sf, colour="black",fill="#EAB14B",size=0.8,shape=21) +
  geom_sf(data=cchp.pods_sf,colour="black" , fill="black",size=0.8,shape=21) +
  coord_sf(xlim = c(430000, 530000),  ylim = c(6380000, 6469000))+     # to show both offshore inshore depl
  annotation_scale(location = "br", style = "ticks", text_cex = 1, width_hint = 0.4) +
  annotation_north_arrow(location = "tl",
                         height=unit(1.5,"cm"),width=unit(0.5,"cm"),which_north = "true",
                         style=north_arrow_orienteering(text_size = 8)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")
moraylandmap


ggsave(fig1sut, file=here::here("Maps","Report_Fig1_Sutors.png"), 
       width = 10, height = 10, units = "cm")
ggsave(fig1cha, file=here::here("Maps","Report_Fig1_Chanonry.png"), 
       width = 10, height = 10, units = "cm")
ggsave(fig1off, file=here::here("Maps","Report_Fig1_Offshore.png"), 
       width = 10, height = 10, units = "cm")
ggsave(moraylandmap, file=here::here("Maps","Report_Fig1_InOff.png"), 
       width = 15, height =13, units = "cm")




