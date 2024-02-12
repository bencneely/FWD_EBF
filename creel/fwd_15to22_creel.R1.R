#--------------------------------------------------------------
#Ben Neely
#02/07/2024
#Examine Freshwater Drum in creel surveys from 2015 to 2019 and 2022
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
        axis.title=element_text(size=26,face="bold"),
        axis.text=element_text(size=20),
        legend.position=c(0.99,0.99),
        legend.background=element_rect(fill="transparent"),
        legend.text=element_text(size=24),
        legend.title=element_blank(),
        legend.key.width=unit(0.5,"in"),
        legend.justification=c("right","top"))
options(scipen=999)

## Set seed for replication
set.seed(207)

## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Documents/Active manuscripts/FWD EBF 2023/R1 analysis figures and submission/R1 analysis/creel_15to22.R1/")

## Read in data with import
## These data only include reservoirs with at least one observed Freshwater Drum in the creel
dat=import("fwd_15to22_creel.csv")

################################################################################
## Proportion of anglers targeting either Freshwater Drum or anything
## Clean up data to include only pertinent years
dat1=dat%>%
  mutate(yr=year(as.Date(sample_date,format="%m/%d/%Y")))%>%
  filter(yr %in% c(2015:2018,2022))%>%
  select(impd=impoundment_code,yr,pref=preferred_species)

## Raw number of respondents that identified each targeted species
dat1%>%
  filter(pref != "")%>%
  mutate(targ=case_when(pref=="***No Fish***" ~ "anything",
                        pref=="Freshwater Drum" ~ "drum",
                        TRUE ~ "other"))%>%
  group_by(targ)%>%
  summarize(n=n())%>%
  ungroup()%>%
  mutate(total=sum(n),
         prop=n/total)
################################################################################
## In reservoirs with at least one observed freshwater drum in the creel 
## from 2015 to 2018 and 2022...
## 17.10% did not have a specified angling target
## 0.000168% were targeting Freshwater Drum 
## 82.88% were targeting other species
################################################################################

################################################################################
## Number of Freshwater Drum observed in each creel
## Clean up data
dat2=dat%>%
  mutate(yr=year(as.Date(sample_date,format="%m/%d/%Y")),
         tl_cm=length*2.54)%>%
  filter(species=="Freshwater Drum")%>%
  filter(yr %in% c(2015:2018,2022))%>%
  expandCounts(~group_count)%>%
  replace_na(list(keep_release=0))%>%
  select(impd=impoundment_code,yr,spp=species,
         tl=length,tl_cm,kr=keep_release,bs=boat_or_shore,
         pref=preferred_species)

## See how many drum were caught at each location and year
xtabs(~impd+yr,subset(dat2,spp=="Freshwater Drum"))
################################################################################
## 13 surveys had at least 100 observed or angler reported Freshwater Drum
## 27 surveys had at least one observation
################################################################################

################################################################################
## Create function that assigns mm length to inch length group
set.seed=39

mm_est=function(x){
  a=x*25.4
  b=(x+0.999)*25.4
  c=round(runif(1,a,b),0)
 return(c)
}

################################################################################
## Number harvested and released and size structure from the 13 surveys
## Only keep impoundments with over 100 drum observed/reported in creel
dat3=dat2%>%
  mutate(keep=case_when(impd=="CLTR" & yr==2015 ~ 1,
                        impd=="CLTR" & yr==2017 ~ 1,
                        impd=="ELDR" & yr==2015 ~ 1,
                        impd=="ELDR" & yr==2018 ~ 1,
                        impd=="GELR" & yr==2022 ~ 1,
                        impd=="KANR" & yr==2022 ~ 1,
                        impd=="KIRR" & yr==2018 ~ 1,
                        impd=="LOVR" & yr==2015 ~ 1,
                        impd=="MARR" & yr==2022 ~ 1,
                        impd=="MILR" & yr==2018 ~ 1,
                        impd=="MILR" & yr==2022 ~ 1,
                        impd=="PERR" & yr==2016 ~ 1,
                        impd=="WEBR" & yr==2022 ~ 1))%>%
  filter(keep==1,
         tl>=4)%>%
  rowwise()%>%
  mutate(tl_mm=mm_est(tl))%>%
  ungroup()%>%
  mutate(cmgrp=lencat(tl_mm,10),
         cmgrp_plot=(cmgrp+5))%>%
  select(-keep,-bs)%>%
  drop_na()

## See how many were kept and released for graph legend
xtabs(~kr,dat3)

## Look at proportion harvested by 5 cm group
prop_harv=dat3%>%
  mutate(k=case_when(kr==1 ~ 1,
                     TRUE ~ 0),
         r=case_when(kr==0 ~ 1,
                     TRUE ~ 0),
         cm5=lencat(tl_mm,50),
         cm5_plot=(cm5+25))%>%
  group_by(cm5_plot)%>%
  summarize(k=sum(k),
            r=sum(r))%>%
  ungroup()%>%
  mutate(harv=(k/(k+r))*1140,
         simp_harv=harv/1140)

## KS test to compare size of harvested (k) and released (r) fish
k=subset(dat3,kr==1)$cmgrp
r=subset(dat3,kr==0)$cmgrp
ks.test(k,r)

##Quick visualization to verify KS test
ggplot(dat3,aes(color=factor(kr),x=cmgrp))+
  stat_ecdf(geom="line")

## Total length frequency
## Note that these are just the fish observed or reported by anglers from the 13 surveys
ggplot()+
  geom_bar(dat3,mapping=aes(fill=factor(kr),color=factor(kr),x=cmgrp_plot),position="stack",width=10)+
  geom_line(subset(prop_harv,(k+r)>=25),mapping=aes(x=cm5_plot,y=harv),linewidth=1.5)+
  scale_fill_manual(values=c("gray","black"),
                    labels=c("Release: N = 4,124","Harvest: N = 838"))+
  scale_color_manual(values=c("gray","black"),
                    labels=c("Release: N = 4,124","Harvest: N = 838"))+
  scale_x_continuous(limits=c(70,930),
                     breaks=seq(0,900,50),
                     expand=c(0,0),
                     name="Total length (mm)")+
  scale_y_continuous(limits=c(0,405),
                     breaks=seq(0,400,50),
                     expand=c(0,0),
                     name="Count",
                     sec.axis=sec_axis(trans=~./1140,
                                       breaks=seq(0,1,0.05),
                                       labels=scales::percent,
                                       name="Percent harvested"))+
  annotate("text",x=620,y=330,label="D = 0.152",size=8,hjust=0,vjust=1)+
  annotate("text",x=620,y=308,label="italic(P) < 0.001",size=8,hjust=0,vjust=1,parse=T)+
  pubtheme
ggsave(plot=last_plot(),"fwd_15to22_catch.R1.png",bg="white",width=12,height=6)
