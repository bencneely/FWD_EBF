#--------------------------------------------------------------
#Ben Neely
#02/12/2024
#Create Freshwater Drum YPR and SPR plots for GELR and TCRR
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

if("patchwork" %in% rownames(installed.packages()) == FALSE) {install.packages("patchwork")}
library(patchwork)

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
        axis.title=element_text(size=20,face="bold"),
        axis.text=element_text(size=14),
        legend.key.width=unit(2,"cm"),
        legend.title=element_blank(),
        legend.text=element_text(size=16),
        legend.background=element_rect(fill="transparent"))
options(scipen=999)

## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Documents/Active manuscripts/FWD EBF 2023/R1 analysis figures and submission/R1 analysis/pop models.R1/")

## Read in data with import
dat=import("pop mod plot data.R1.csv")

###############################################################################
###############################################################################
## YPR plots
## GELR
gelr_ypr=ggplot(subset(dat,impd=="Glen Elder"))+
  geom_line(aes(x=u,y=ypr,linetype=factor(mll)),linewidth=1.5)+
  scale_linetype_manual(values=c("solid","dotted","longdash"),
                        labels=c("203 mm MLL","356 mm MLL","457 mm MLL"))+
  scale_y_continuous(breaks=seq(0,0.4,0.05),
                     name="YPR (kg)")+
  scale_x_continuous(breaks=seq(0,0.5,0.05),
                     labels=scales::percent,
                     name="")+
  annotate("text",label="Glen Elder",x=0.01,y=0.415,hjust=0,vjust=1,size=8)+
  coord_cartesian(xlim=c(0,0.51),
                  ylim=c(0,0.42),
                  expand=F)+
  pubtheme+
  theme(legend.position=c(0.999,0.001),
        legend.justification=c(1,0))

## TCRR
tcrr_ypr=ggplot(subset(dat,impd=="Tuttle Creek"))+
  geom_line(aes(x=u,y=ypr,linetype=factor(mll)),linewidth=1.5)+
  scale_linetype_manual(values=c("solid","dotted","longdash"),
                        labels=c("203 mm MLL","356 mm MLL","457 mm MLL"))+
  scale_y_continuous(breaks=seq(0,0.4,0.05),
                     name="")+
  scale_x_continuous(breaks=seq(0,0.5,0.05),
                     labels=scales::percent,
                     name="")+
  annotate("text",label="Tuttle Creek",x=0.01,y=0.415,hjust=0,vjust=1,size=8)+
  coord_cartesian(xlim=c(0,0.51),
                  ylim=c(0,0.42),
                  expand=F)+
  pubtheme+
  theme(legend.position=c(0.999,0.001),
        legend.justification=c(1,0))

###############################################################################
###############################################################################
## SPR plots
## GELR
gelr_spr=ggplot(subset(dat,impd=="Glen Elder"))+
  geom_hline(aes(yintercept=0.4),color="gray",linewidth=1.1)+
  geom_line(aes(x=u,y=spr,linetype=factor(mll)),linewidth=1.5)+
  scale_linetype_manual(values=c("solid","dotted","longdash"),
                        labels=c("203 mm MLL","356 mm MLL","457 mm MLL"))+
  scale_y_continuous(breaks=seq(0,0.9,0.1),
                     labels=scales::percent,
                     name="SPR")+
  scale_x_continuous(breaks=seq(0,0.5,0.05),
                     labels=scales::percent,
                     name="Angling exploitation")+
  annotate("text",label="Glen Elder",x=0.01,y=0.01,hjust=0,vjust=0,size=8)+
  coord_cartesian(xlim=c(0,0.51),
                  ylim=c(0,0.905),
                  expand=F)+
  pubtheme+
  theme(legend.position=c(0.999,0.999),
        legend.justification=c(1,1))

## TCRR
tcrr_spr=ggplot(subset(dat,impd=="Tuttle Creek"))+
  geom_hline(aes(yintercept=0.4),color="gray",linewidth=1.1)+
  geom_line(aes(x=u,y=spr,linetype=factor(mll)),linewidth=1.5)+
  scale_linetype_manual(values=c("solid","dotted","longdash"),
                        labels=c("203 mm MLL","356 mm MLL","457 mm MLL"))+
  scale_y_continuous(breaks=seq(0,0.9,0.1),
                     labels=scales::percent,
                     name="")+
  scale_x_continuous(breaks=seq(0,0.5,0.05),
                     labels=scales::percent,
                     name="Angling exploitation")+
  annotate("text",label="Tuttle Creek",x=0.01,y=0.01,hjust=0,vjust=0,size=8)+
  coord_cartesian(xlim=c(0,0.51),
                  ylim=c(0,0.905),
                  expand=F)+
  pubtheme+
  theme(legend.position=c(0.999,0.999),
        legend.justification=c(1,1))

###############################################################################
###############################################################################
## Combine plots
out=(gelr_ypr|tcrr_ypr)/(gelr_spr|tcrr_spr)
ggsave(plot=out,"pop mods.R1.png",width=12,height=7,bg="white")