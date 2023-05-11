library(tidyverse)
library(cowplot)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----
#HP                                         -----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----
# Based on 2 different mix models to extract buzzes (one each priority)     


#1. PerDAY datasets: DPH-BPH     ------
cpodpairs= read.csv(here::here("Data","CPOD-CPOD_HP-DPH-BPH_Priority1-2_perDAY.csv"),header = T) 

poddayh.tablesum=cpodpairs %>% 
  group_by(location.id,depl.pair) %>% 
  summarise(
    cpod.depl1=unique(cpod.depl[pod.priority==1]),
    cpod.depl2=unique(cpod.depl[pod.priority==2]),
    tot.days=n_distinct(date))

write.csv(poddayh.tablesum,here::here("Data","SummaryData","CPOD_CPOD_Data summary_HP.csv"),row.names = F ) 


##1a. DPH scatter plot:          -----
poddayh= read.csv(here::here("Data","CPOD-CPOD_HP-DPH-BPH_Priority1-2_perDAY_wide.csv"),header = T) %>% 
  mutate(location.id=as.factor(location.id))

dphplot1= poddayh %>% 
  ggplot()+
  geom_point(aes(x=cpod1.DPH, y=cpod2.DPH, fill=location.id),
             size=2, shape=21, colour="black",
             position=position_jitter(width = 0.3,height = 0.3)) +
  paletteer::scale_fill_paletteer_d("rcartocolor::Vivid") +
  theme_classic() +
  labs(x="CPOD",y="CPOD2",
       title = "DPH - HP",
       fill="Location") +
  theme(
    plot.title = element_text(face = "bold",size=16))

dphplot1

dphplot2= poddayh %>% 
  ggplot()+
  geom_point(aes(x=cpod1.DPH, y=cpod2.DPH, fill=location.id),
             size=2, shape=21, colour="black",
             position=position_jitter(width = 0.3,height = 0.3)) +
  paletteer::scale_fill_paletteer_d("rcartocolor::Vivid") +
  facet_wrap(~location.id,ncol=4)+
  theme_classic() +
  labs(x="CPOD",y="CPOD2",
       title = "DPH - HP",
       fill="Location") +
  theme(
    plot.title = element_blank(),
    legend.position = "none")

dphplot2

#Save combined plot:
dphplots= ggdraw() +
  draw_plot(dphplot1, x =0.25, y = 0.6, width = 0.55, height =0.4) +
  draw_plot(dphplot2, x = 0, y = 0, width = 1, height = 0.60) +
  draw_plot_label( c("A", "B"),c(0,0),c(1,0.6)) 

dphplots

ggsave(dphplots, file=here::here("Plots","CPOD-CPOD","CPOD_CPOD_DPH_HP.png"), 
       width = 20, height = 25, units = "cm")


##1b. BPH scatter plot:          -----
bphplot1= poddayh %>% 
  ggplot()+
  geom_point(aes(x=cpod1.BPH, y=cpod2.BPH, fill=location.id),
             size=2, shape=21, colour="black",
             position=position_jitter(width = 0.3,height = 0.3)) +
  paletteer::scale_fill_paletteer_d("rcartocolor::Vivid") +
  theme_classic() +
  labs(x="CPOD",y="CPOD2",
       title = "BPH - HP",
       fill="Location") +
  theme(
    plot.title = element_text(face = "bold",size=16))

bphplot1

bphplot2= poddayh %>% 
  ggplot()+
  geom_point(aes(x=cpod1.BPH, y=cpod2.BPH, fill=location.id),
             size=2, shape=21, colour="black",
             position=position_jitter(width = 0.3,height = 0.3)) +
  paletteer::scale_fill_paletteer_d("rcartocolor::Vivid") +
  facet_wrap(~location.id,ncol=4)+
  theme_classic() +
  labs(x="CPOD",y="CPOD2",
       title = "BPH - HP",
       fill="Location") +
  theme(
    plot.title = element_blank(),
    legend.position = "none")

bphplot2

#Save combined plot:
bphplots= ggdraw() +
  draw_plot(bphplot1, x =0.25, y = 0.6, width = 0.55, height =0.4) +
  draw_plot(bphplot2, x = 0, y = 0, width = 1, height = 0.60) +
  draw_plot_label( c("A", "B"),c(0,0),c(1,0.6)) 

bphplots

ggsave(bphplots, file=here::here("Plots","CPOD-CPOD","CPOD_CPOD_BPH_Priority1-2_HP.png"), 
       width = 20, height = 25, units = "cm")


##1c. Correlation and pair plots:  -----
cor.test(poddayh$cpod1.DPH,poddayh$cpod2.DPH, method="spearman") #r=0.93
cor.test(poddayh$cpod1.BPH,poddayh$cpod2.BPH, method="spearman") #r=0.85


png(here::here("Plots","CPOD-CPOD","CPOD-CPOD_Spearman_DPH-BPH_HP.png"), 
    width = 600, height = 300)
par(mfrow = c(1, 2))
plot(poddayh$cpod1.DPH,poddayh$cpod2.DPH, 
     pch = 19, col = "#b06c92",
     xlab = "CPOD1 DPHd", ylab = "CPOD2 DPHd",
     ylim = c(0, 24),
     xlim = c(0, 24))
abline(lm(poddayh$cpod2.DPH ~ poddayh$cpod1.DPH), col = "black", lwd = 3)
text(paste("Correlation:", round(cor(poddayh$cpod1.DPH,poddayh$cpod2.DPH,method = "spearman"), 2)),
     x = 18, y = 2)

plot(poddayh$cpod1.BPH,poddayh$cpod2.BPH, 
     pch = 19, col = "#b06c92",
     xlab = "CPOD1 BPHd", ylab = "CPOD2 BPHd",
     ylim = c(0, 24),
     xlim = c(0, 24))
abline(lm(poddayh$cpod2.BPH ~ poddayh$cpod1.BPH), col = "black", lwd = 3)
text(paste("Correlation:", round(cor(poddayh$cpod1.BPH,poddayh$cpod2.BPH,method = "spearman"), 2)),
     x = 18, y = 2)
dev.off()




pairs.dph <- ggpairs(poddayh[, c("cpod1.DPH","cpod2.DPH")],
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

ggsave(pairs.dph, file=here::here("Plots","CPOD-CPOD","CPOD_CPOD_DPH-Correlation_HP.png"), 
       width = 20, height = 20, units = "cm")

pairs.bph <- ggpairs(poddayh[, c("cpod1.BPH","cpod2.BPH")],
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

ggsave(pairs.bph, file=here::here("Plots","CPOD-CPOD","CPOD_CPOD_BPH-Correlation_Priority1-2_HP.png"), 
       width = 20, height = 20, units = "cm")


#2. PerHOUR datasets:      ------
podhourh= read.csv(here::here("Data","CPOD-CPOD_HP-PPM-BPM_Priority1-2_perHOUR_wide.csv"),header = T) %>% 
  mutate(
    across(c("location.id","cpod1.depl","cpod2.depl"),~as.factor(.)),
    datetimehour=as.POSIXct(datetimehour,tz="GMT"),
    hour=format(datetimehour,"%H"),
    cpod12.dif=cpod1.por.pres-cpod2.por.pres,
    cpod12.dif2=case_when(cpod12.dif==0 ~ "CPOD1=CPOD2",
                          cpod12.dif==-1 ~ "CPOD2 only",
                          cpod12.dif==1 ~ "CPOD1 only"),
    cpod12.dif2=factor(cpod12.dif2,
                       levels = c("CPOD1 only","CPOD2 only","CPOD1=CPOD2"),
                       ordered = T),
    cpod12.difb=cpod1.buzz.pres-cpod2.buzz.pres,
    cpod12.difb2=case_when(cpod12.difb==0 ~ "CPOD1=CPOD2",
                           cpod12.difb==-1 ~ "CPOD2 only",
                           cpod12.difb==1 ~ "CPOD1 only"),
    cpod12.difb2=factor(cpod12.difb2,
                        levels = c("CPOD1 only","CPOD2 only","CPOD1=CPOD2"),
                        ordered = T))

##2a. Differences - percentages:  ----
hplot1= podhourh %>% 
  ggplot()+
  geom_bar(aes(x=location.id, 
               y = (..count..)/sum(..count..),
               fill=cpod12.dif2),
           colour="black",
           position = "fill") +
  scale_y_continuous(labels=scales::percent) +
  paletteer::scale_fill_paletteer_d("rcartocolor::Vivid") +
  #facet_wrap(~location.id,ncol=5)+
  theme_classic() +
  labs(x="Location",y="Porpoise presence",
       title = "CPOD-CPOD: presence per hour - HP",
       fill="") +
  theme(
    axis.text.x = element_text(size=12),
    plot.title = element_text(face = "bold",size=16),
    legend.position = "none")

hplot1


hplot2= podhourh %>% 
  ggplot()+
  geom_bar(aes(x=location.id, 
               y = (..count..)/sum(..count..),
               fill= cpod12.difb2),
           colour="black",
           position = "fill") +
  scale_y_continuous(labels=scales::percent) +
  paletteer::scale_fill_paletteer_d("rcartocolor::Vivid") +
  #facet_wrap(~location.id,ncol=5)+
  theme_classic() +
  labs(x="Location",y="Porpoise buzz presence",
       title = "CPOD-CPOD: buzz presence per hour - HP",
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

ggsave(hplots, file=here::here("Plots","CPOD-CPOD","CPOD-CPOD_PerHour_Percentages_Priority1-2_HP.png"), 
       width = 20, height = 20, units = "cm")



##2b. Correlation and Pair plots: ----
cor.test(podhourh$cpod1.por.pres,podhourh$cpod2.por.pres, method="spearman") #r=0.83
cor.test(podhourh$cpod1.buzz.pres,podhourh$cpod2.buzz.pres, method="spearman") #r=0.69



pairs.pres <- ggpairs(podhourh[, c("cpod1.por.pres","cpod2.por.pres")],
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

ggsave(pairs.pres, file=here::here("Plots","CPOD-CPOD","CPOD-CPOD_HourlyPresence-Correlation_HP.png"), 
       width = 20, height = 20, units = "cm")


pairs.buzzpres <- ggpairs(podhourh[, c("cpod1.buzz.pres","cpod2.buzz.pres")],
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

ggsave(pairs.buzzpres, file=here::here("Plots","CPOD-CPOD","CPOD-CPOD_HourlyBuzzPresence-Correlation_Priority1-2_HP.png"), 
       width = 20, height = 20, units = "cm")



##2c. GAM:  ----
podhourh.long= read.csv(here::here("Data","CPOD-CPOD_HP-PPM-BPM_Priority1-2_perHOUR.csv"),header = T) %>% 
  mutate(
    across(c("location.id","cpod.depl","cpod.priority"),~as.factor(.)),
    datetimehour=as.POSIXct(datetimehour,tz="GMT"),
    hour=as.numeric(format(datetimehour,"%H")),
    por.pres=as.factor(cpod.por.pres))


library(mgcv)
gam1=gam(por.pres ~ s(hour,k=7, bs="cc",by=cpod.priority) + cpod.priority, 
         data=podhourh.long, family=binomial(link = "probit"), method = "REML")

### Plot gam - occurrence:   ----
hour = seq(0,23.5,by=0.5)
cpod.priority=as.factor(c("1","2"))
predData=as.data.frame(expand.grid(hour,cpod.priority)) %>% 
  setNames(c("hour","cpod.priority"))

predgam1=jtools::make_predictions(gam1, new_data = predData, outcome.scale="response")
podhourh.long2=podhourh.long %>% 
  mutate(
    por.pres=as.numeric(as.character(por.pres)),
    por.pres=case_when(cpod.priority=="1"  ~ por.pres,
                       cpod.priority=="2" ~ por.pres-0.03  ) )

plot.gam1=ggplot() +
  geom_point(data=podhourh.long2,aes(x=hour,y=por.pres,shape=cpod.priority,colour=cpod.priority),
             size=1,position = position_jitter(width=0.4,height = 0)) +
  geom_ribbon(data=predgam1,aes(x=hour,ymin=ymin,ymax=ymax,fill=cpod.priority),
              alpha=0.2) +
  geom_line(data=predgam1,aes(x=hour,y=por.pres,colour=cpod.priority,linetype=cpod.priority),
            size=1) +
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
podhourh.longbuzz= read.csv(here::here("Data","CPOD-CPOD_HP-PPM-BPM_Priority1-2_perHOUR.csv"),header = T) %>% 
  mutate(
    across(c("location.id","cpod.depl","cpod.priority"),~as.factor(.)),
    datetimehour=as.POSIXct(datetimehour,tz="GMT"),
    hour=as.numeric(format(datetimehour,"%H")),
    por.buzz.pres=as.factor(cpod.buzz.pres))



gam2=gam(por.buzz.pres ~ s(hour,k=7, bs="cc",by=cpod.priority) + cpod.priority, 
         data=podhourh.longbuzz, family=binomial(link = "probit"), method = "REML")

gam.check(gam2)
plot(gam2)



#Plot model: 
hour = seq(0,23.5,by=0.5)
cpod.priority=as.factor(c("1","2"))
predData=as.data.frame(expand.grid(hour,cpod.priority)) %>% 
  setNames(c("hour","cpod.priority"))
predgam2=jtools::make_predictions(gam2, new_data = predData, outcome.scale="response")

podhourh.longbuzz2=podhourh.longbuzz %>% 
  mutate(
    por.buzz.pres=as.numeric(as.character(por.buzz.pres)),
    por.buzz.pres=case_when(cpod.priority=="1"  ~ por.buzz.pres,
                            cpod.priority=="2" ~por.buzz.pres-0.03  ) )


plot.gam2=ggplot() +
  geom_ribbon(data=predgam2,aes(x=hour,ymin=ymin,ymax=ymax,fill=cpod.priority),
              alpha=0.2) +
  geom_line(data=predgam2,aes(x=hour,y=por.buzz.pres,colour=cpod.priority,linetype=cpod.priority),
            size=1) +
  geom_point(data=podhourh.longbuzz2,aes(x=hour,y=por.buzz.pres,shape=cpod.priority,colour=cpod.priority),
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

gam.plots=ggpubr::ggarrange(plot.gam1,plot.gam2, ncol=2,  common.legend = TRUE, legend="bottom")

ggsave(gam.plots, file=here::here("Plots","CPOD-CPOD","CPOD-CPOD_PerHour_GAMS_Priority1-2_HP.png"), 
       width = 20, height = 12, units = "cm")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----
#Dol                                  -----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----
# Based on 2 different mix models to extract buzzes (one each priority) 

#1. PerDAY datasets: DPH-BPH     ------
cpodpairs= read.csv(here::here("Data","CPOD-CPOD_Dol-DPH-BPH_Priority1-2_perDAY.csv"),header = T) 

podday.tablesum=cpodpairs %>% 
  group_by(location.id,depl.pair) %>% 
  summarise(
    cpod.depl1=unique(cpod.depl[pod.priority==1]),
    cpod.depl2=unique(cpod.depl[pod.priority==2]),
    tot.days=n_distinct(date))

write.csv(podday.tablesum,here::here("Data","SummaryData","CPOD_CPOD_Data summary_Dol.csv"),row.names = F ) 


##1a. DPH scatter plot:          -----
podday= read.csv(here::here("Data","CPOD-CPOD_Dol-DPH-BPH_Priority1-2_perDAY_wide.csv"),header = T) %>% 
  mutate(location.id=as.factor(location.id))

dphplot1= podday %>% 
  ggplot()+
  geom_point(aes(x=cpod1.DPH, y=cpod2.DPH, fill=location.id),
             size=2, shape=21, colour="black",
             position=position_jitter(width = 0.3,height = 0.3)) +
  paletteer::scale_fill_paletteer_d("rcartocolor::Vivid") +
  theme_classic() +
  labs(x="CPOD",y="CPOD2",
       title = "DPH - Dol",
       fill="Location") +
  theme(
    plot.title = element_text(face = "bold",size=16))

dphplot1

dphplot2= podday %>% 
  ggplot()+
  geom_point(aes(x=cpod1.DPH, y=cpod2.DPH, fill=location.id),
             size=2, shape=21, colour="black",
             position=position_jitter(width = 0.3,height = 0.3)) +
  paletteer::scale_fill_paletteer_d("rcartocolor::Vivid") +
  facet_wrap(~location.id,ncol=4)+
  theme_classic() +
  labs(x="CPOD",y="CPOD2",
       title = "DPH - Dol",
       fill="Location") +
  theme(
    plot.title = element_blank(),
    legend.position = "none")

dphplot2

#Save combined plot:
dphplots= ggdraw() +
  draw_plot(dphplot1, x =0.25, y = 0.6, width = 0.55, height =0.4) +
  draw_plot(dphplot2, x = 0, y = 0, width = 1, height = 0.60) +
  draw_plot_label( c("A", "B"),c(0,0),c(1,0.6)) 

dphplots

ggsave(dphplots, file=here::here("Plots","CPOD-CPOD","CPOD_CPOD_DPH_Dol.png"), 
       width = 20, height = 25, units = "cm")


##1b. BPH scatter plot:          -----
bphplot1= podday %>% 
  ggplot()+
  geom_point(aes(x=cpod1.BPH, y=cpod2.BPH, fill=location.id),
             size=2, shape=21, colour="black",
             position=position_jitter(width = 0.3,height = 0.3)) +
  paletteer::scale_fill_paletteer_d("rcartocolor::Vivid") +
  theme_classic() +
  labs(x="CPOD",y="CPOD2",
       title = "BPH - Dol",
       fill="Location") +
  theme(
    plot.title = element_text(face = "bold",size=16))

bphplot1

bphplot2= podday %>% 
  ggplot()+
  geom_point(aes(x=cpod1.BPH, y=cpod2.BPH, fill=location.id),
             size=2, shape=21, colour="black",
             position=position_jitter(width = 0.3,height = 0.3)) +
  paletteer::scale_fill_paletteer_d("rcartocolor::Vivid") +
  facet_wrap(~location.id,ncol=4)+
  theme_classic() +
  labs(x="CPOD",y="CPOD2",
       title = "BPH - Dol",
       fill="Location") +
  theme(
    plot.title = element_blank(),
    legend.position = "none")

bphplot2

#Save combined plot:
bphplots= ggdraw() +
  draw_plot(bphplot1, x =0.25, y = 0.6, width = 0.55, height =0.4) +
  draw_plot(bphplot2, x = 0, y = 0, width = 1, height = 0.60) +
  draw_plot_label( c("A", "B"),c(0,0),c(1,0.6)) 

bphplots

ggsave(bphplots, file=here::here("Plots","CPOD-CPOD","CPOD_CPOD_BPH_Priority1-2_Dol.png"), 
       width = 20, height = 25, units = "cm")


##1c. Correlation and pair plots:  -----
cor.test(podday$cpod1.DPH,podday$cpod2.DPH, method="spearman") #r=0.87
cor.test(podday$cpod1.BPH,podday$cpod2.BPH, method="spearman") #r=0.84


png(here::here("Plots","CPOD-CPOD","CPOD-CPOD_Spearman_DPH-BPH_Dol.png"), 
    width = 600, height = 300)
par(mfrow = c(1, 2))
plot(podday$cpod1.DPH,podday$cpod2.DPH, 
     pch = 19, col = "lightblue",
     xlab = "CPOD1 DPHd", ylab = "CPOD2 DPHd",
     ylim = c(0, 24),
     xlim = c(0, 24))
abline(lm(podday$cpod2.DPH ~ podday$cpod1.DPH), col = "black", lwd = 3)
text(paste("Correlation:", round(cor(podday$cpod1.DPH,podday$cpod2.DPH,method = "spearman"), 2)),
     x = 18, y = 2)

plot(podday$cpod1.BPH,podday$cpod2.BPH, 
     pch = 19, col = "lightblue",
     xlab = "CPOD1 BPHd", ylab = "CPOD2 BPHd",
     ylim = c(0, 24),
     xlim = c(0, 24))
abline(lm(podday$cpod2.BPH ~ podday$cpod1.BPH), col = "black", lwd = 3)
text(paste("Correlation:", round(cor(podday$cpod1.BPH,podday$cpod2.BPH,method = "spearman"), 2)),
     x = 18, y = 2)
dev.off()





pairs.dph <- ggpairs(podday[, c("cpod1.DPH","cpod2.DPH")],
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

ggsave(pairs.dph, file=here::here("Plots","CPOD-CPOD","CPOD_CPOD_DPH-Correlation_Priority1-2_Dol.png"), 
       width = 20, height = 20, units = "cm")

pairs.bph <- ggpairs(podday[, c("cpod1.BPH","cpod2.BPH")],
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

ggsave(pairs.bph, file=here::here("Plots","CPOD-CPOD","CPOD_CPOD_BPH-Correlation_Priority1-2_Dol.png"), 
       width = 20, height = 20, units = "cm")


#2. PerHOUR datasets:      ------
podhour= read.csv(here::here("Data","CPOD-CPOD_Dol-DPM-BPM_Priority1-2_perHOUR_wide.csv"),header = T) %>% 
  mutate(
    across(c("location.id","cpod1.depl","cpod2.depl"),~as.factor(.)),
    datetimehour=as.POSIXct(datetimehour,tz="GMT"),
    hour=format(datetimehour,"%H"),
    cpod12.dif=cpod1.dol.pres-cpod2.dol.pres,
    cpod12.dif2=case_when(cpod12.dif==0 ~ "CPOD1=CPOD2",
                          cpod12.dif==-1 ~ "CPOD2 only",
                          cpod12.dif==1 ~ "CPOD1 only"),
    cpod12.dif2=factor(cpod12.dif2,
                       levels = c("CPOD1 only","CPOD2 only","CPOD1=CPOD2"),
                       ordered = T),
    cpod12.difb=cpod1.buzz.pres-cpod2.buzz.pres,
    cpod12.difb2=case_when(cpod12.difb==0 ~ "CPOD1=CPOD2",
                           cpod12.difb==-1 ~ "CPOD2 only",
                           cpod12.difb==1 ~ "CPOD1 only"),
    cpod12.difb2=factor(cpod12.difb2,
                        levels = c("CPOD1 only","CPOD2 only","CPOD1=CPOD2"),
                        ordered = T))

##2a. Differences - percentages:  ----
hplot1= podhour %>% 
  ggplot()+
  geom_bar(aes(x=location.id, 
               y = (..count..)/sum(..count..),
               fill=cpod12.dif2),
           colour="black",
           position = "fill") +
  scale_y_continuous(labels=scales::percent) +
  paletteer::scale_fill_paletteer_d("rcartocolor::Vivid") +
  #facet_wrap(~location.id,ncol=5)+
  theme_classic() +
  labs(x="Location",y="Porpoise presence",
       title = "CPOD-CPOD: presence per hour - Dol",
       fill="") +
  theme(
    axis.text.x = element_text(size=12),
    plot.title = element_text(face = "bold",size=16),
    legend.position = "none")

hplot1


hplot2= podhour %>% 
  ggplot()+
  geom_bar(aes(x=location.id, 
               y = (..count..)/sum(..count..),
               fill= cpod12.difb2),
           colour="black",
           position = "fill") +
  scale_y_continuous(labels=scales::percent) +
  paletteer::scale_fill_paletteer_d("rcartocolor::Vivid") +
  #facet_wrap(~location.id,ncol=5)+
  theme_classic() +
  labs(x="Location",y="Porpoise buzz presence",
       title = "CPOD-CPOD: buzz presence per hour - Dol",
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

ggsave(hplots, file=here::here("Plots","CPOD-CPOD","CPOD-CPOD_PerHour_Percentages_Priority1-2_Dol.png"), 
       width = 20, height = 20, units = "cm")



##2b. Correlation and Pair plots: ----
cor.test(podhour$cpod1.dol.pres,podhour$cpod2.dol.pres, method="spearman") #r=0.83
cor.test(podhour$cpod1.buzz.pres,podhour$cpod2.buzz.pres, method="spearman") #r=0.81



pairs.pres <- ggpairs(podhour[, c("cpod1.dol.pres","cpod2.dol.pres")],
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

ggsave(pairs.pres, file=here::here("Plots","CPOD-CPOD","CPOD-CPOD_HourlyPresence-Correlation_Dol.png"), 
       width = 20, height = 20, units = "cm")


pairs.buzzpres <- ggpairs(podhour[, c("cpod1.buzz.pres","cpod2.buzz.pres")],
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

ggsave(pairs.buzzpres, file=here::here("Plots","CPOD-CPOD","CPOD-CPOD_HourlyBuzzPresence-Correlation_Priority1-2_Dol.png"), 
       width = 20, height = 20, units = "cm")



##2c. GAM:  ----
podhour.long= read.csv(here::here("Data","CPOD-CPOD_Dol-DPM-BPM_Priority1-2_perHOUR.csv"),header = T) %>% 
  mutate(
    across(c("location.id","cpod.depl","cpod.priority"),~as.factor(.)),
    datetimehour=as.POSIXct(datetimehour,tz="GMT"),
    hour=as.numeric(format(datetimehour,"%H")),
    dol.pres=as.factor(cpod.dol.pres))


library(mgcv)
gam1=gam(dol.pres ~ s(hour,k=4, bs="cc",by=cpod.priority) + cpod.priority, 
         data=podhour.long, family=binomial(link = "probit"), method = "REML")

### Plot gam - occurrence:   ----
hour = seq(0,23.5,by=0.5)
cpod.priority=as.factor(c("1","2"))
predData=as.data.frame(expand.grid(hour,cpod.priority)) %>% 
  setNames(c("hour","cpod.priority"))

predgam1=jtools::make_predictions(gam1, new_data = predData, outcome.scale="response")
podhour.long2=podhour.long %>% 
  mutate(
    dol.pres=as.numeric(as.character(dol.pres)),
    dol.pres=case_when(cpod.priority=="1"  ~ dol.pres,
                       cpod.priority=="2" ~ dol.pres-0.03  ) )

plot.gam1=ggplot() +
  geom_point(data=podhour.long2,aes(x=hour,y=dol.pres,shape=cpod.priority,colour=cpod.priority),
             size=1,position = position_jitter(width=0.4,height = 0)) +
  geom_ribbon(data=predgam1,aes(x=hour,ymin=ymin,ymax=ymax,fill=cpod.priority),
              alpha=0.2) +
  geom_line(data=predgam1,aes(x=hour,y=dol.pres,colour=cpod.priority,linetype=cpod.priority),
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
podhour.longbuzz= read.csv(here::here("Data","CPOD-CPOD_Dol-DPM-BPM_Priority1-2_perHOUR.csv"),header = T) %>% 
  mutate(
    across(c("location.id","cpod.depl","cpod.priority"),~as.factor(.)),
    datetimehour=as.POSIXct(datetimehour,tz="GMT"),
    hour=as.numeric(format(datetimehour,"%H")),
    por.buzz.pres=as.factor(cpod.buzz.pres))



gam2=gam(por.buzz.pres ~ s(hour,k=10, bs="cc",by=cpod.priority) + cpod.priority, 
         data=podhour.longbuzz, family=binomial(link = "probit"), method = "REML")

gam.check(gam2)
plot(gam2)



#Plot model: 
hour = seq(0,23.5,by=0.5)
cpod.priority=as.factor(c("1","2"))
predData=as.data.frame(expand.grid(hour,cpod.priority)) %>% 
  setNames(c("hour","cpod.priority"))
predgam2=jtools::make_predictions(gam2, new_data = predData, outcome.scale="response")

podhour.longbuzz2=podhour.longbuzz %>% 
  mutate(
    por.buzz.pres=as.numeric(as.character(por.buzz.pres)),
    por.buzz.pres=case_when(cpod.priority=="1"  ~ por.buzz.pres,
                            cpod.priority=="2" ~por.buzz.pres-0.03  ) )


plot.gam2=ggplot() +
  geom_ribbon(data=predgam2,aes(x=hour,ymin=ymin,ymax=ymax,fill=cpod.priority),
              alpha=0.2) +
  geom_line(data=predgam2,aes(x=hour,y=por.buzz.pres,colour=cpod.priority,linetype=cpod.priority),
            size=1) +
  geom_point(data=podhour.longbuzz2,aes(x=hour,y=por.buzz.pres,shape=cpod.priority,colour=cpod.priority),
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

gam.plots=ggpubr::ggarrange(plot.gam1,plot.gam2, ncol=2,  common.legend = TRUE, legend="bottom")

ggsave(gam.plots, file=here::here("Plots","CPOD-CPOD","CPOD-CPOD_PerHour_GAMS_Priority1-2_Dol.png"), 
       width = 20, height = 12, units = "cm")



