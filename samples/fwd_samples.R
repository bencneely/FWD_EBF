#--------------------------------------------------------------
#Ben Neely
#04/02/2023
#Analyze 2021 and 2022 FWD gill net samples
#--------------------------------------------------------------

## Clear R
cat("\014")  
rm(list=ls())

## Install and load packages
## Checks if package is installed, installs if not, activates for current session
if("FSA" %in% rownames(installed.packages()) == FALSE) {install.packages("FSA")}
library(FSA)

if("rio" %in% rownames(installed.packages()) == FALSE) {install.packages("rio")}
library(rio)

if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
library(lubridate)

if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
library(tidyverse)

## Set ggplot theme
pubtheme=theme_classic()+
  theme(panel.grid=element_blank(), 
        panel.background=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(fill="transparent"),
        axis.title=element_text(size=22,face="bold"),
        axis.text=element_text(size=18),
        legend.position="none")
options(scipen=999)

## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Desktop/FWD EBF 2023/analysis/samples")

## Read in data with import
## Data include all Freshwater Drum sampled with standard gill nets in 2021 and 2022
fish=import("fwd_fish.csv")
samp=import("fwd_samp.csv")
locs=import("locs.csv")

## Identify all impoundments where FWD were sampled and filter sample and location list
tmp=unique(fish$impoundment_code)
samp1=filter(samp,impoundment_code %in% tmp)
locs1=filter(locs,impoundment_code %in% tmp)

## Join location data to sample data
samp2=samp1%>%
  left_join(locs1,by="impoundment_code")%>%
  mutate(yr=year(sample_date))%>%
  select(id=fish_sample_id,code=impoundment_code,impd,yr,long,lat,ac,huc8)

## Clean up fish data to match sample data and add Wr
fish2=fish%>%
  expandCounts(~group_count)%>%
  mutate(wr=wrAdd(weight,length,species),
         yr=year(sample_date),
         cmgrp=lencat(length,10))%>%
  select(id=fish_sample_id,code=impoundment_code,yr,spp=species,tl=length,w=weight,wr,lcat=length_category,cmgrp)%>%
  drop_na(tl)

## Total fish sampled in 2021 and 2022
nrow(fish2)

## Total impoundments with FWD sampled in 2021 or 2022
length(unique(fish2$code))

###############################################################################
###############################################################################
## Gill net total CPE by year
cpe1=fish2%>%
  group_by(id,code,yr,spp)%>%
  summarize(tot=n())

## Left join sample data with catch per net data
cpe2=samp2%>%
  left_join(cpe1,by="id")%>%
  mutate(spp="Freshwater Drum")%>%
  select(id,code=code.x,impd,yr=yr.x,long,lat,ac,huc8,spp,tot)%>%
  replace_na(list(tot=0,pref=0))

## Calculate total CPE
cpeout=cpe2%>%
  group_by(code,impd,long,lat,ac,huc8,spp)%>%
  summarize(tot_cpe=mean(tot))%>%
  ungroup()

## Export for plotting in GIS
export(cpeout,"drumlocs.csv")

## Summary statistics for tot_cpe and pref_cpe
min(cpeout$tot_cpe)
median(cpeout$tot_cpe)
max(cpeout$tot_cpe)

###############################################################################
###############################################################################
## Length summary statistics
min(fish2$tl)
median(fish2$tl)
max(fish2$tl)

## Percentage of fish between 130 and 150 mm
(nrow(filter(fish2,tl>=130 & tl<150))/nrow(fish2))*100

## Percentage of fish that are quality and preferred length
nrow(filter(fish2,tl>=300))/nrow(fish2)*100
nrow(filter(fish2,tl>=380))/nrow(fish2)*100

## Length frequency
ggplot(fish2,aes(x=cmgrp))+
  geom_bar(width=10,fill="gray50",color="gray50")+
  scale_x_continuous(limits=c(0,620),
                     breaks=seq(0,600,50),
                     expand=c(0,0),
                     name="Total length (mm)")+
  scale_y_continuous(limits=c(0,41),
                     breaks=seq(0,40,5),
                     expand=c(0,0),
                     name="Count")+
  geom_vline(xintercept=195,linetype="dashed")+
  geom_vline(xintercept=295,linetype="dashed")+
  geom_vline(xintercept=375,linetype="dashed")+
  geom_vline(xintercept=505,linetype="dashed")+
  annotate("text",label="SS",x=2,y=40,hjust=0,vjust=1,size=8)+
  annotate("text",label="S-Q",x=197,y=40,hjust=0,vjust=1,size=8)+
  annotate("text",label="Q-P",x=297,y=40,hjust=0,vjust=1,size=8)+
  annotate("text",label="P-M",x=377,y=40,hjust=0,vjust=1,size=8)+
  annotate("text",label="M-T",x=507,y=40,hjust=0,vjust=1,size=8)+
  pubtheme

ggsave(plot=last_plot(),"LF.png",width=10,height=5,units="in",bg="white")
###############################################################################
###############################################################################
## Relative weight

## Define boxplot summary function
f=function(x) {
  r=quantile(x,probs=c(0.05,0.25,0.5,0.75,0.95))
  names(r)=c("ymin","lower","middle","upper","ymax")
  r
}

## Organize data
fish3=fish%>%
  expandCounts(~group_count)%>%
  mutate(wr=wrAdd(weight,length,species))%>%
  select(lcat=length_category,wr)%>%
  drop_na()

fish3%>%
  group_by(lcat)%>%
  summarize(med_wr=median(wr))

##Arrange PSD categories as factor for plotting
fish3$lcat=factor(fish3$lcat,levels=c("SS","S-Q","Q-P","P-M","M-T","T"))

##Plot relative weight boxplots
ggplot(fish3,aes(x=lcat,y=wr))+
  stat_summary(fun.data=f,geom="boxplot")+
  geom_jitter(size=2,alpha=0.5)+
  scale_y_continuous(limits=c(49,151),
                     breaks=seq(50,150,10),
                     expand=c(0,0))+
  labs(x="PSD length category",y="Relative weight")+
  pubtheme

ggsave(plot=last_plot(),"wr.png",width=10,height=5,units="in",bg="white")
