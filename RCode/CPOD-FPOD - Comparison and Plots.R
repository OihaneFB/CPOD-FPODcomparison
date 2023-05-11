library(tidyverse)
library(cowplot)
library(GGally) #ggpairs

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----
#HP                                     -----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----

#1. PerDAY datasets: DPH-BPH     ------
podday.cfh= read.csv(here::here("Data","CPOD-FPOD_HP-DPH-BPH_perDAY.csv"),header = T) %>% 
  mutate(
    across(c("location.id","cpod.depl","fpod.depl"),~as.factor(.)),
    pair=paste0("cpod-",cpod.depl,"_fpod-",fpod.depl)) %>% 
  drop_na()

podday.cfh.tablesum=podday.cfh %>% 
  group_by(location.id) %>% 
  summarise(
    cpod.depl=unique(cpod.depl),
    fpod.depl=unique(fpod.depl),
    tot.days=n_distinct(date))

write.csv(podday.cfh.tablesum,here::here("Data","SummaryData","CPOD-FPOD_Data summary_HP.csv"),row.names = F ) 


##1a. DPH scatter plot:          -----
dphplot1= podday.cfh %>% 
  ggplot()+
  geom_point(aes(x=cpod.DPH, y=fpod.DPH, fill=location.id),
             size=2, shape=21, colour="black",
             position=position_jitter(width = 0.3,height = 0.3)) +
  paletteer::scale_fill_paletteer_d("rcartocolor::Vivid") +
  theme_classic() +
  labs(x="CPOD",y="FPOD",
       title = "DPH - HP",
       fill="Location") +
  theme(
    plot.title = element_text(face = "bold",size=16),
    legend.position = "none")

dphplot1

dphplot2= podday.cfh %>% 
  ggplot()+
  geom_point(aes(x=cpod.DPH, y=fpod.DPH, fill=location.id),
             size=2, shape=21, colour="black",
             position=position_jitter(width = 0.3,height = 0.3)) +
  paletteer::scale_fill_paletteer_d("rcartocolor::Vivid") +
  facet_wrap(~location.id,ncol=4)+
  theme_classic() +
  labs(x="CPOD",y="FPOD",
       title = "DPH - HP",
       fill="Location") +
  theme(
    plot.title = element_blank(),
    legend.position = "none")

dphplot2

#Save combined plot:
dphplots= ggdraw() +
  draw_plot(dphplot1, x =0.3, y = 0.60, width = 0.45, height =0.4) +
  draw_plot(dphplot2, x = 0, y = 0, width = 1, height = 0.60) +
  draw_plot_label( c("A", "B"),c(0,0),c(1,0.6))

dphplots

ggsave(dphplots, file=here::here("Plots","CPOD-FPOD","CPOD-FPOD_DPH_HP.png"), 
       width = 20, height = 25, units = "cm")


##1b. BPH scatter plot:          -----
bphplot1= podday.cfh %>% 
  ggplot()+
  geom_point(aes(x=cpod.BPH, y=fpod.BPH, fill=location.id),
             size=2, shape=21, colour="black",
             position=position_jitter(width = 0.3,height = 0.3)) +
  paletteer::scale_fill_paletteer_d("rcartocolor::Vivid") +
  theme_classic() +
  labs(x="CPOD",y="FPOD",
       title = "BPH - HP",
       fill="Location") +
  theme(
    plot.title = element_text(face = "bold",size=16),
    legend.position = "none")

bphplot1

bphplot2= podday.cfh %>% 
  ggplot()+
  geom_point(aes(x=cpod.BPH, y=fpod.BPH, fill=location.id),
             size=2, shape=21, colour="black",
             position=position_jitter(width = 0.3,height = 0.3)) +
  paletteer::scale_fill_paletteer_d("rcartocolor::Vivid") +
  facet_wrap(~location.id,ncol=4)+
  theme_classic() +
  labs(x="CPOD",y="FPOD",
       title = "BPH - HP",
       fill="Location") +
  theme(
    plot.title = element_blank(),
    legend.position = "none")

bphplot2

#Save combined plot:
bphplots= ggdraw() +
  draw_plot(bphplot1, x =0.3, y = 0.60, width = 0.45, height =0.4) +
  draw_plot(bphplot2, x = 0, y = 0, width = 1, height = 0.60) +
  draw_plot_label( c("A", "B"),c(0,0),c(1,0.6))

bphplots

ggsave(bphplots, file=here::here("Plots","CPOD-FPOD","CPOD-FPOD_BPH_HP.png"), 
       width = 20, height = 25, units = "cm")


##1c. Correlation and pair plots:  -----
cor.test(podday.cfh$cpod.DPH,podday.cfh$fpod.DPH, method="spearman") #r=0.95
cor.test(podday.cfh$cpod.BPH,podday.cfh$fpod.BPH, method="spearman") #r=0.90

png(here::here("Plots","CPOD-FPOD","CPOD-FPOD_Spearman_DPH-BPH_HP.png"), 
    width = 600, height = 300)
par(mfrow = c(1, 2))
plot(podday.cf$cpod.DPH,podday.cf$fpod.DPH, 
     pch = 19, col = "#b06c92",
     xlab = "CPOD DPHd", ylab = "FPOD DPHd",
     ylim = c(0, 24),
     xlim = c(0, 24))
abline(lm(podday.cf$fpod.DPH ~ podday.cf$cpod.DPH), col = "black", lwd = 3)
text(paste("Correlation:", round(cor(podday.cf$cpod.DPH,podday.cf$fpod.DPH,method = "spearman"), 2)),
     x = 18, y = 2)

plot(podday.cf$cpod.BPH,podday.cf$fpod.BPH, 
     pch = 19, col = "#b06c92",
     xlab = "CPOD BPHd", ylab = "FPOD BPHd",
     ylim = c(0, 24),
     xlim = c(0, 24))
abline(lm(podday.cf$fpod.BPH ~ podday.cf$cpod.BPH), col = "black", lwd = 3)
text(paste("Correlation:", round(cor(podday.cf$cpod.BPH,podday.cf$fpod.BPH,method = "spearman"), 2)),
     x = 18, y = 2)
dev.off()



pairs.dph <- ggpairs(podday.cfh[, c("cpod.DPH","fpod.DPH")],
                    upper = list(continuous = wrap('cor', size = 8)),
                    lower = list(combo = wrap("facethist", bins = 30)),
                    diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
                    title = "DPH/day - HP")+
  theme_bw()+
  theme(text = element_text(size=14), 
        panel.grid.minor = element_blank(), 
        panel.grid.major =   element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
pairs.dph

ggsave(pairs.dph, file=here::here("Plots","CPOD-FPOD","CPOD-FPOD_DPH-Correlation_HP.png"), 
       width = 20, height = 20, units = "cm")

pairs.bph <- ggpairs(podday.cfh[, c("cpod.BPH","fpod.BPH")],
                     upper = list(continuous = wrap('cor', size = 8)),
                     lower = list(combo = wrap("facethist", bins = 30)),
                     diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
                     title = "BPH/day - HP")+
  theme_bw()+
  theme(text = element_text(size=14), 
        panel.grid.minor = element_blank(), 
        panel.grid.major =   element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
pairs.bph

ggsave(pairs.bph, file=here::here("Plots","CPOD-FPOD","CPOD-FPOD_BPH-Correlation_HP.png"), 
       width = 20, height = 20, units = "cm")


#2. PerHOUR datasets:      ------
podhour.cfh= read.csv(here::here("Data","CPOD-FPOD_HP-PPM-BPM_perHOUR.csv"),header = T) %>% 
  mutate(
    across(c("location.id","cpod.depl","fpod.depl"),~as.factor(.)),
    datetimehour=as.POSIXct(datetimehour,tz="GMT"),
    hour=format(datetimehour,"%H"),
    cpodfpod.dif=cpod.por.pres-fpod.por.pres,
    cpodfpod.dif2=case_when(cpodfpod.dif==0 ~ "cpod=fpod",
                            cpodfpod.dif==-1 ~ "fpod only",
                            cpodfpod.dif==1 ~ "cpod only"),
    cpodfpod.dif2=factor(cpodfpod.dif2,
                         levels = c("cpod only","fpod only","cpod=fpod"),
                         ordered = T),
    cpodfpod.difb=cpod.buzz.pres-fpod.buzz.pres,
    cpodfpod.difb2=case_when(cpodfpod.difb==0 ~ "cpod=fpod",
                            cpodfpod.difb==-1 ~ "fpod only",
                            cpodfpod.difb==1 ~ "cpod only"),
    cpodfpod.difb2=factor(cpodfpod.difb2,
                         levels = c("cpod only","fpod only","cpod=fpod"),
                         ordered = T))

##2a. Differences - percentages:  ----
hplot1= podhour.cfh %>% 
  ggplot()+
  geom_bar(aes(x=location.id, 
               y = after_stat(count/sum(count)),
               fill=cpodfpod.dif2),
           colour="black",
           position = "fill") +
  scale_y_continuous(labels=scales::percent) +
  paletteer::scale_fill_paletteer_d("rcartocolor::Vivid") +
  #facet_wrap(~location.id,ncol=5)+
  theme_classic() +
  labs(x="Location",y="Porpoise presence",
       title = "CPOD-FPOD: presence per hour - HP",
       fill="") +
  theme(
    axis.text.x = element_text(size=12),
    plot.title = element_text(face = "bold",size=16),
    legend.position = "none")

hplot1


hplot2= podhour.cfh %>% 
  ggplot()+
  geom_bar(aes(x=location.id, 
               y = after_stat(count/sum(count)),
               fill=cpodfpod.difb2),
           colour="black",
           position = "fill") +
  scale_y_continuous(labels=scales::percent) +
  paletteer::scale_fill_paletteer_d("rcartocolor::Vivid") +
  #facet_wrap(~location.id,ncol=5)+
  theme_classic() +
  labs(x="Location",y="Porpoise buzz presence",
       title = "CPOD-FPOD: buzz presence per hour - HP",
       fill="") +
  theme(
    axis.text.x = element_text(size=12),
    plot.title = element_text(face = "bold",size=16),
    legend.position = "bottom")

hplot2

#Save combined plot:
hplots= ggdraw() +
  draw_plot(hplot1, x =0, y = 0.5, width = 1, height =0.5) +
  draw_plot(hplot2, x = 0, y = 0, width = 1, height = 0.5) 

hplots

ggsave(hplots, file=here::here("Plots","CPOD-FPOD","CPOD-FPOD_PerHour_Percentages_HP.png"), 
       width = 20, height = 20, units = "cm")



##2b. Correlation and Pair plots: ----
cor.test(podhour.cfh$cpod.por.pres,podhour.cfh$fpod.por.pres, method="spearman") #r=0.78
cor.test(podhour.cfh$cpod.buzz.pres,podhour.cfh$fpod.buzz.pres, method="spearman") #r=0.69


pairs.pres <- ggpairs(podhour.cfh[, c("cpod.por.pres","fpod.por.pres")],
                     upper = list(continuous = wrap('cor', size = 8)),
                     lower = list(combo = wrap("facethist", bins = 30)),
                     diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
                     title = "Hourly presence - HP")+
  theme_bw()+
  theme(text = element_text(size=14), 
        panel.grid.minor = element_blank(), 
        panel.grid.major =   element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
pairs.pres

ggsave(pairs.pres, file=here::here("Plots","CPOD-FPOD","CPOD-FPOD_HourlyPresence-Correlation_HP.png"), 
       width = 20, height = 20, units = "cm")


pairs.buzzpres <- ggpairs(podhour.cfh[, c("cpod.buzz.pres","fpod.buzz.pres")],
                      upper = list(continuous = wrap('cor', size = 8)),
                      lower = list(combo = wrap("facethist", bins = 30)),
                      diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
                      title = "Hourly buzz presence - HP")+
  theme_bw()+
  theme(text = element_text(size=14), 
        panel.grid.minor = element_blank(), 
        panel.grid.major =   element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
pairs.buzzpres

ggsave(pairs.buzzpres, file=here::here("Plots","CPOD-FPOD","CPOD-FPOD_HourlyBuzzPresence-Correlation_HP.png"), 
       width = 20, height = 20, units = "cm")



##2c. GAM:  ----
podhour.cfh.long=podhour.cfh %>% 
  pivot_longer(cols = c(cpod.por.pres,fpod.por.pres),names_to = "device",values_to = "por.pres") %>% 
  mutate(
    hour=as.numeric(hour),
    device=as.factor(substr(device,1,4)),
    por.pres=as.factor(por.pres))

library(mgcv)
gam1=gam(por.pres ~ s(hour,k=7, bs="cc",by=device) + device, 
         data=podhour.cfh.long, family=binomial(link = "probit"), method = "REML")

### Plot gam - occurrence:   ----
hour = seq(0,23.5,by=0.5)
device=as.factor(c("cpod","fpod"))
predData=as.data.frame(expand.grid(hour,device)) %>% 
  setNames(c("hour","device"))

predgam1=jtools::make_predictions(gam1, new_data = predData, outcome.scale="response")
podhour.cfh.long2=podhour.cfh.long %>% 
  mutate(
    por.pres=as.numeric(as.character(por.pres)),
    por.pres=case_when(device=="cpod"  ~ por.pres,
                       device=="fpod" ~ por.pres-0.03  ) )

plot.gam1=ggplot() +
  geom_point(data=podhour.cfh.long2,aes(x=hour,y=por.pres,shape=device,colour=device),
             size=1,position = position_jitter(width=0.4,height = 0)) +
  geom_ribbon(data=predgam1,aes(x=hour,ymin=ymin,ymax=ymax,fill=device),
              alpha=0.2) +
  geom_line(data=predgam1,aes(x=hour,y=por.pres,colour=device,linetype=device),
            linewidth=1) +
  labs(title = "Hourly occurrence - HP",
       x="Time of day",
       y="p(occurrence)") +
  scale_linetype_manual(values=c("dashed","solid"))+
  scale_shape_manual(values=c(1,2))+
  scale_y_continuous(limits=c(-0.05,1.02),breaks=seq(0,1,by=0.2),expand = c(0,0))+
  scale_x_continuous(limits = c(0,23.5), breaks = seq(0,21,by=3),labels = paste0(seq(0,21,by=3),"h"),expand = c(0.01,0)) +
  scale_colour_manual(values = c("#9dadbc","#7F4362")) +
  scale_fill_manual(values = c("#9dadbc","#7F4362")) +
  theme_bw()+
  theme(axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=12),
        axis.title.x = element_blank(),
        plot.title = element_text(face = "bold",size=12, hjust = 0.5),
        strip.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none") 
plot.gam1

###  Plot gam - foraging         ----
podhour.cfh.longbuzz=podhour.cfh %>% 
  filter(!(cpod.por.pres==0 & fpod.por.pres==0) ) %>% 
  pivot_longer(cols = c(cpod.buzz.pres,fpod.buzz.pres),names_to = "device",values_to = "por.buzz.pres") %>% 
  mutate(
    hour=as.numeric(hour),
    device=as.factor(substr(device,1,4)),
    por.buzz.pres=as.factor(por.buzz.pres))


gam2=gam(por.buzz.pres ~ s(hour,k=7, bs="cc",by=device) + device, 
         data=podhour.cfh.longbuzz, family=binomial(link = "probit"), method = "REML")

gam.check(gam2)
plot(gam2)



#Plot model: 
hour = seq(0,23.5,by=0.5)
device=as.factor(c("cpod","fpod"))
predData=as.data.frame(expand.grid(hour,device)) %>% 
  setNames(c("hour","device"))
predgam2=jtools::make_predictions(gam2, new_data = predData, outcome.scale="response")

podhour.cfh.longbuzz2=podhour.cfh.longbuzz %>% 
  mutate(
    por.buzz.pres=as.numeric(as.character(por.buzz.pres)),
    por.buzz.pres=case_when(device=="cpod"  ~ por.buzz.pres,
                        device=="fpod" ~por.buzz.pres-0.03  ) )


plot.gam2=ggplot() +
  geom_ribbon(data=predgam2,aes(x=hour,ymin=ymin,ymax=ymax,fill=device),
              alpha=0.2) +
  geom_line(data=predgam2,aes(x=hour,y=por.buzz.pres,colour=device,linetype=device),
            linewidth=1) +
  geom_point(data=podhour.cfh.longbuzz2,aes(x=hour,y=por.buzz.pres,shape=device,colour=device),
             size=1,position = position_jitter(width=0.4,height = 0)) +
  labs(title = "Hourly Foraging - HP",
       x="Time of day",
       y="p(foraging)") +
  scale_linetype_manual(values=c("dashed","solid"))+
  scale_shape_manual(values=c(1,2))+
  scale_y_continuous(limits=c(-0.05,1.02),breaks=seq(0,1,by=0.2),expand = c(0,0))+
  scale_x_continuous(limits = c(0,23.5), breaks = seq(0,21,by=3),labels = paste0(seq(0,21,by=3),"h"),expand = c(0.01,0)) +
  scale_colour_manual(values = c("#9dadbc","#7F4362")) +
  scale_fill_manual(values = c("#9dadbc","#7F4362")) +
  theme_bw()+
  theme(axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=12),
        axis.title.x = element_blank(),
        plot.title = element_text(face = "bold",size=12, hjust = 0.5),
        strip.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="bottom") 
plot.gam2


gam_legend <- get_legend(plot.gam2 + theme(legend.position="bottom"))

gam.plots=ggpubr::ggarrange(plot.gam1,plot.gam2, ncol=2,  common.legend = TRUE, legend="right")

ggsave(gam.plots, file=here::here("Plots","CPOD-FPOD","CPOD-FPOD_PerHour_GAMS_HP.png"), 
       width = 23, height = 12, units = "cm")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----
#Dol                                  -----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----


#1. PerDAY datasets: DPH-BPH     ------
podday.cf= read.csv(here::here("Data","CPOD-FPOD_Dol-DPH-BPH_perDAY.csv"),header = T) %>% 
  mutate(
    across(c("location.id","cpod.depl","fpod.depl"),~as.factor(.)),
    pair=paste0("cpod-",cpod.depl,"_fpod-",fpod.depl)) %>% 
  drop_na()

podday.cf.tablesum=podday.cf %>% 
  group_by(location.id) %>% 
  summarise(
    cpod.depl=unique(cpod.depl),
    fpod.depl=unique(fpod.depl),
    tot.days=n_distinct(date))

write.csv(podday.cf.tablesum,here::here("Data","SummaryData","CPOD-FPOD_Data summary_Dol.csv"),row.names = F ) 


##1a. DPH scatter plot:          -----
dphplot1= podday.cf %>% 
  ggplot()+
  geom_point(aes(x=cpod.DPH, y=fpod.DPH, fill=location.id),
             size=2, shape=21, colour="black",
             position=position_jitter(width = 0.3,height = 0.3)) +
  paletteer::scale_fill_paletteer_d("rcartocolor::Vivid") +
  theme_classic() +
  labs(x="CPOD",y="FPOD",
       title = "DPH - Dol",
       fill="Location") +
  theme(
    plot.title = element_text(face = "bold",size=16))

dphplot1

dphplot2= podday.cf %>% 
  ggplot()+
  geom_point(aes(x=cpod.DPH, y=fpod.DPH, fill=location.id),
             size=2, shape=21, colour="black",
             position=position_jitter(width = 0.3,height = 0.3)) +
  paletteer::scale_fill_paletteer_d("rcartocolor::Vivid") +
  facet_wrap(~location.id,ncol=4)+
  theme_classic() +
  labs(x="CPOD",y="FPOD",
       title = "DPH - Dol",
       fill="Location") +
  theme(
    plot.title = element_blank(),
    legend.position = "none")

dphplot2

#Save combined plot:
dphplots= ggdraw() +
  draw_plot(dphplot1, x =0.25, y = 0.50, width = 0.5, height =0.5) +
  draw_plot(dphplot2, x = 0, y = 0, width = 1, height = 0.5) +
  draw_plot_label( c("A", "B"),c(0,0),c(1,0.5))

dphplots

ggsave(dphplots, file=here::here("Plots","CPOD-FPOD","CPOD-FPOD_DPH_Dol.png"), 
       width = 22, height = 20, units = "cm")


##1b. BPH scatter plot:          -----
bphplot1= podday.cf %>% 
  ggplot()+
  geom_point(aes(x=cpod.BPH, y=fpod.BPH, fill=location.id),
             size=2, shape=21, colour="black",
             position=position_jitter(width = 0.3,height = 0.3)) +
  paletteer::scale_fill_paletteer_d("rcartocolor::Vivid") +
  theme_classic() +
  labs(x="CPOD",y="FPOD",
       title = "BPH - Dol",
       fill="Location") +
  theme(
    plot.title = element_text(face = "bold",size=16))

bphplot1

bphplot2= podday.cf %>% 
  ggplot()+
  geom_point(aes(x=cpod.BPH, y=fpod.BPH, fill=location.id),
             size=2, shape=21, colour="black",
             position=position_jitter(width = 0.3,height = 0.3)) +
  paletteer::scale_fill_paletteer_d("rcartocolor::Vivid") +
  facet_wrap(~location.id,ncol=4)+
  theme_classic() +
  labs(x="CPOD",y="FPOD",
       title = "BPH - Dol",
       fill="Location") +
  theme(
    plot.title = element_blank(),
    legend.position = "none")

bphplot2

#Save combined plot:
bphplots= ggdraw() +
  draw_plot(bphplot1, x =0.25, y = 0.50, width = 0.5, height =0.5) +
  draw_plot(bphplot2, x = 0, y = 0, width = 1, height = 0.5) +
  draw_plot_label( c("A", "B"),c(0,0),c(1,0.5)) 

bphplots

ggsave(bphplots, file=here::here("Plots","CPOD-FPOD","CPOD-FPOD_BPH_Dol.png"), 
       width = 20, height = 25, units = "cm")


##1c. Correlation and Pair plots:  -----
cor.test(podday.cf$cpod.DPH,podday.cf$fpod.DPH, method="spearman") #r=0.87
cor.test(podday.cf$cpod.BPH,podday.cf$fpod.BPH, method="spearman") #r=0.84


png(here::here("Plots","CPOD-FPOD","CPOD-FPOD_Spearman_DPH-BPH_Dol.png"), 
     width = 600, height = 300)
par(mfrow = c(1, 2))
plot(podday.cf$cpod.DPH,podday.cf$fpod.DPH, 
     pch = 19, col = "lightblue",
     xlab = "CPOD DPHd", ylab = "FPOD DPHd",
     ylim = c(0, 24),
     xlim = c(0, 24))
abline(lm(podday.cf$fpod.DPH ~ podday.cf$cpod.DPH), col = "black", lwd = 3)
text(paste("Correlation:", round(cor(podday.cf$cpod.DPH,podday.cf$fpod.DPH,method = "spearman"), 2)),
     x = 18, y = 2)

plot(podday.cf$cpod.BPH,podday.cf$fpod.BPH, 
     pch = 19, col = "lightblue",
     xlab = "CPOD BPHd", ylab = "FPOD BPHd",
     ylim = c(0, 24),
     xlim = c(0, 24))
abline(lm(podday.cf$fpod.BPH ~ podday.cf$cpod.BPH), col = "black", lwd = 3)
text(paste("Correlation:", round(cor(podday.cf$cpod.BPH,podday.cf$fpod.BPH,method = "spearman"), 2)),
     x = 18, y = 2)
dev.off()


pairs.dph <- ggpairs(podday.cf[, c("cpod.DPH","fpod.DPH")],
                     upper = list(continuous = wrap('cor', size = 8)),
                     lower = list(combo = wrap("facethist", bins = 30)),
                     diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
                     title = "DPH/day - Dol")+
  theme_bw()+
  theme(text = element_text(size=14), 
        panel.grid.minor = element_blank(), 
        panel.grid.major =   element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
pairs.dph

ggsave(pairs.dph, file=here::here("Plots","CPOD-FPOD","CPOD-FPOD_DPH-Correlation_Dol.png"), 
       width = 20, height = 20, units = "cm")

pairs.bph <- ggpairs(podday.cf[, c("cpod.BPH","fpod.BPH")],
                     upper = list(continuous = wrap('cor', size = 8)),
                     lower = list(combo = wrap("facethist", bins = 30)),
                     diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
                     title = "BPH/day - Dol")+
  theme_bw()+
  theme(text = element_text(size=14), 
        panel.grid.minor = element_blank(), 
        panel.grid.major =   element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
pairs.bph

ggsave(pairs.bph, file=here::here("Plots","CPOD-FPOD","CPOD-FPOD_BPH-Correlation_Dol.png"), 
       width = 20, height = 20, units = "cm")


#2. PerHOUR datasets:      ------
podhour.cf= read.csv(here::here("Data","CPOD-FPOD_Dol-DPM-BPM_perHOUR.csv"),header = T) %>% 
  mutate(
    across(c("location.id","cpod.depl","fpod.depl"),~as.factor(.)),
    datetimehour=as.POSIXct(datetimehour,tz="GMT"),
    hour=format(datetimehour,"%H"),
    cpodfpod.dif=cpod.dol.pres-fpod.dol.pres,
    cpodfpod.dif2=case_when(cpodfpod.dif==0 ~ "cpod=fpod",
                            cpodfpod.dif==-1 ~ "fpod only",
                            cpodfpod.dif==1 ~ "cpod only"),
    cpodfpod.dif2=factor(cpodfpod.dif2,
                         levels = c("cpod only","fpod only","cpod=fpod"),
                         ordered = T),
    cpodfpod.difb=cpod.buzz.pres-fpod.buzz.pres,
    cpodfpod.difb2=case_when(cpodfpod.difb==0 ~ "cpod=fpod",
                             cpodfpod.difb==-1 ~ "fpod only",
                             cpodfpod.difb==1 ~ "cpod only"),
    cpodfpod.difb2=factor(cpodfpod.difb2,
                          levels = c("cpod only","fpod only","cpod=fpod"),
                          ordered = T))

##2a. Differences - percentages:  ----
hplot1= podhour.cf %>% 
  ggplot()+
  geom_bar(aes(x=location.id, 
               y = (..count..)/sum(..count..),
               fill=cpodfpod.dif2),
           colour="black",
           position = "fill") +
  scale_y_continuous(labels=scales::percent) +
  paletteer::scale_fill_paletteer_d("rcartocolor::Vivid") +
  #facet_wrap(~location.id,ncol=5)+
  theme_classic() +
  labs(x="Location",y="Porpoise presence",
       title = "CPOD-FPOD: presence per hour - Dol",
       fill="") +
  theme(
    axis.text.x = element_text(size=12),
    plot.title = element_text(face = "bold",size=16),
    legend.position = "none")

hplot1


hplot2= podhour.cf %>% 
  ggplot()+
  geom_bar(aes(x=location.id, 
               y = (..count..)/sum(..count..),
               fill=cpodfpod.difb2),
           colour="black",
           position = "fill") +
  scale_y_continuous(labels=scales::percent) +
  paletteer::scale_fill_paletteer_d("rcartocolor::Vivid") +
  #facet_wrap(~location.id,ncol=5)+
  theme_classic() +
  labs(x="Location",y="Porpoise buzz presence",
       title = "CPOD-FPOD: buzz presence per hour - Dol",
       fill="") +
  theme(
    axis.text.x = element_text(size=12),
    plot.title = element_text(face = "bold",size=16),
    legend.position = "bottom")

hplot2

#Save combined plot:
hplots= ggdraw() +
  draw_plot(hplot1, x =0, y = 0.5, width = 1, height =0.5) +
  draw_plot(hplot2, x = 0, y = 0, width = 1, height = 0.5) 

hplots

ggsave(hplots, file=here::here("Plots","CPOD-FPOD","CPOD-FPOD_PerHour_Percentages_Dol.png"), 
       width = 15, height = 15, units = "cm")



##2b. Correlation and Pair plots: ----
cor.test(podhour.cf$cpod.dol.pres,podhour.cf$fpod.dol.pres, method="spearman") #r=0.75
cor.test(podhour.cf$cpod.buzz.pres,podhour.cf$fpod.buzz.pres, method="spearman") #r=0.73


pairs.pres <- ggpairs(podhour.cf[, c("cpod.dol.pres","fpod.dol.pres")],
                      upper = list(continuous = wrap('cor', size = 8)),
                      lower = list(combo = wrap("facethist", bins = 30)),
                      diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
                      title = "Hourly presence - Dol")+
  theme_bw()+
  theme(text = element_text(size=14), 
        panel.grid.minor = element_blank(), 
        panel.grid.major =   element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
pairs.pres

ggsave(pairs.pres, file=here::here("Plots","CPOD-FPOD","CPOD-FPOD_HourlyPresence-Correlation_Dol.png"), 
       width = 20, height = 20, units = "cm")


pairs.buzzpres <- ggpairs(podhour.cf[, c("cpod.buzz.pres","fpod.buzz.pres")],
                          upper = list(continuous = wrap('cor', size = 8)),
                          lower = list(combo = wrap("facethist", bins = 30)),
                          diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
                          title = "Hourly buzz presence - Dol")+
  theme_bw()+
  theme(text = element_text(size=14), 
        panel.grid.minor = element_blank(), 
        panel.grid.major =   element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
pairs.buzzpres

ggsave(pairs.buzzpres, file=here::here("Plots","CPOD-FPOD","CPOD-FPOD_HourlyBuzzPresence-Correlation_Dol.png"), 
       width = 20, height = 20, units = "cm")



##2c. GAM:  ----
podhour.cf.long=podhour.cf %>% 
  pivot_longer(cols = c(cpod.dol.pres,fpod.dol.pres),names_to = "device",values_to = "dol.pres") %>% 
  mutate(
    hour=as.numeric(hour),
    device=as.factor(substr(device,1,4)),
    dol.pres=as.factor(dol.pres))

podhour.cf.long.s=podhour.cf.long %>% filter(location.id==1 | location.id==117 | location.id==204)

library(mgcv)
gam1=gam(dol.pres ~ s(hour,k=7, bs="cc",by=device) + device, 
         data=podhour.cf.long.s, family=binomial(link = "probit"), method = "REML")



### Plot gam - occurrence:   ----
hour = seq(0,23.5,by=0.5)
device=as.factor(c("cpod","fpod"))
predData=as.data.frame(expand.grid(hour,device)) %>% 
  setNames(c("hour","device"))

predgam1=jtools::make_predictions(gam1, new_data = predData, outcome.scale="response")
podhour.cf.long2=podhour.cf.long %>% 
  mutate(
    dol.pres=as.numeric(as.character(dol.pres)),
    dol.pres=case_when(device=="cpod"  ~ dol.pres,
                       device=="fpod" ~ dol.pres-0.03  ) )

plot.gam1=ggplot() +
  geom_point(data=podhour.cf.long2,aes(x=hour,y=dol.pres,shape=device,colour=device),
             size=1,position = position_jitter(width=0.4,height = 0)) +
  geom_ribbon(data=predgam1,aes(x=hour,ymin=ymin,ymax=ymax,fill=device),
              alpha=0.2) +
  geom_line(data=predgam1,aes(x=hour,y=dol.pres,colour=device,linetype=device),
            size=1) +
  labs(title = "Hourly occurrence - Dol",
       x="Time of day",
       y="p(occurrence)") +
  scale_linetype_manual(values=c("dashed","solid"))+
  scale_shape_manual(values=c(1,2))+
  scale_y_continuous(limits=c(-0.05,1.02),breaks=seq(0,1,by=0.2),expand = c(0,0))+
  scale_x_continuous(limits = c(0,23.5), breaks = seq(0,21,by=3),labels = paste0(seq(0,21,by=3),"h"),expand = c(0.01,0)) +
  scale_colour_manual(values = c("#9dadbc","#7F4362")) +
  scale_fill_manual(values = c("#9dadbc","#7F4362")) +
  theme_bw()+
  theme(axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=12),
        axis.title.x = element_blank(),
        plot.title = element_text(face = "bold",size=12, hjust = 0.5),
        strip.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none") 
plot.gam1

###  Plot gam - foraging         ----
podhour.cf.longbuzz=podhour.cf %>% 
  filter(!(cpod.dol.pres==0 & fpod.dol.pres==0) ) %>% 
  pivot_longer(cols = c(cpod.buzz.pres,fpod.buzz.pres),names_to = "device",values_to = "por.buzz.pres") %>% 
  mutate(
    hour=as.numeric(hour),
    device=as.factor(substr(device,1,4)),
    por.buzz.pres=as.factor(por.buzz.pres))

podhour.cf.longbuzz.s=podhour.cf.longbuzz %>% filter(location.id==1 | location.id==117 )


gam2=gam(por.buzz.pres ~ s(hour,k=7, bs="cc",by=device) + device, 
         data=podhour.cf.longbuzz, family=binomial(link = "probit"), method = "REML")

gam.check(gam2)
plot(gam2)



#Plot model: 
hour = seq(0,23.5,by=0.5)
device=as.factor(c("cpod","fpod"))
predData=as.data.frame(expand.grid(hour,device)) %>% 
  setNames(c("hour","device"))
predgam2=jtools::make_predictions(gam2, new_data = predData, outcome.scale="response")

podhour.cf.longbuzz2=podhour.cf.longbuzz %>% 
  mutate(
    por.buzz.pres=as.numeric(as.character(por.buzz.pres)),
    por.buzz.pres=case_when(device=="cpod"  ~ por.buzz.pres,
                            device=="fpod" ~por.buzz.pres-0.03  ) )


plot.gam2=ggplot() +
  geom_ribbon(data=predgam2,aes(x=hour,ymin=ymin,ymax=ymax,fill=device),
              alpha=0.2) +
  geom_line(data=predgam2,aes(x=hour,y=por.buzz.pres,colour=device,linetype=device),
            size=1) +
  geom_point(data=podhour.cf.longbuzz2,aes(x=hour,y=por.buzz.pres,shape=device,colour=device),
             size=1,position = position_jitter(width=0.4,height = 0)) +
  labs(title = "Hourly Foraging - Dol",
       x="Time of day",
       y="p(foraging)") +
  scale_linetype_manual(values=c("dashed","solid"))+
  scale_shape_manual(values=c(1,2))+
  scale_y_continuous(limits=c(-0.05,1.02),breaks=seq(0,1,by=0.2),expand = c(0,0))+
  scale_x_continuous(limits = c(0,23.5), breaks = seq(0,21,by=3),labels = paste0(seq(0,21,by=3),"h"),expand = c(0.01,0)) +
  scale_colour_manual(values = c("#9dadbc","#7F4362")) +
  scale_fill_manual(values = c("#9dadbc","#7F4362")) +
  theme_bw()+
  theme(axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=12),
        axis.title.x = element_blank(),
        plot.title = element_text(face = "bold",size=12, hjust = 0.5),
        strip.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="bottom") 
plot.gam2


gam_legend <- get_legend(plot.gam2 + theme(legend.position="bottom"))

gam.plots=ggpubr::ggarrange(plot.gam1,plot.gam2, ncol=2,  common.legend = TRUE, legend="right")

ggsave(gam.plots, file=here::here("Plots","CPOD-FPOD","CPOD-FPOD_PerHour_GAMS_Dol.png"), 
       width = 23, height = 12, units = "cm")



