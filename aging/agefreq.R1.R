#--------------------------------------------------------------
#Ben Neely
#02/06/2024
#Plot FWD age frequency
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

if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
library(tidyverse)

## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Documents/Active manuscripts/FWD EBF 2023/R1 analysis figures and submission/R1 analysis/aging.R1/")

## Set ggplot theme
pubtheme=theme_classic()+
  theme(panel.grid=element_blank(), 
        panel.background=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(fill="transparent"),
        axis.title=element_text(size=22,face="bold"),
        axis.text=element_text(size=20),
        legend.position="none")

## Import data
gelr_points=import("gelr_vb.R1.xlsx",which="GELR_all")
tcrr_points=import("tcrr_vb.R1.xlsx",which="TCRR_all")
  
## Clean up gelr data
 gelr=gelr_points%>%
    select(impd,tl=cmgrp,age=agecap)%>%
    group_by(age)%>%
    summarize(n=n())%>%
    ungroup()%>%
    mutate(prop=n/sum(n))%>%
    complete(age=min(age):max(age),
             fill=list(n=0,prop=0))
sum(gelr$n)

## Clean up tcrr data
tcrr=tcrr_points%>%
  select(impd,tl=cmgrp,age=agecap)%>%
  group_by(age)%>%
  summarize(n=n())%>%
  ungroup()%>%
  mutate(prop=n/sum(n))%>%
  complete(age=min(age):max(age),
           fill=list(n=0,prop=0))
sum(tcrr$n)

################################################################################
################################################################################
## Plot age frequency histograms for each population

################################################################################
## Glen Elder
gelr_afplot=ggplot(gelr)+
  geom_bar(aes(x=age,y=prop),stat="identity",width=1)+
  scale_x_continuous(limits=c(-0.5,43),
                     breaks=seq(0,40,5),
                     name="",
                     expand=c(0,0))+
  scale_y_continuous(limits=c(0,0.145),
                     breaks=seq(0,0.14,0.02),
                     name="",
                     expand=c(0,0))+
   annotate("text",label="Glen Elder",x=42,y=0.14,hjust=1,vjust=1,size=8)+
   annotate("text",label="N = 110",x=42,y=0.12,hjust=1,vjust=1,size=5)+
   pubtheme  
  
## Tuttle Creek
tcrr_afplot=ggplot(tcrr)+
  geom_bar(aes(x=age,y=prop),stat="identity",width=1)+
  scale_x_continuous(limits=c(-0.5,43),
                     breaks=seq(0,40,5),
                     name="Estimated age",
                     expand=c(0,0))+
  scale_y_continuous(limits=c(0,0.265),
                     breaks=seq(0,0.25,0.05),
                     name="Catch proportion",
                     expand=c(0,0))+
  annotate("text",label="Tuttle Creek",x=42,y=0.26,hjust=1,vjust=1,size=8)+
  annotate("text",label="N = 167",x=42,y=0.225,hjust=1,vjust=1,size=5)+
  pubtheme+
  theme(axis.title.y=element_text(hjust=-3.7))

## Combine plots
out=gelr_afplot/tcrr_afplot

## Export plot
ggsave(plot=out,"af.R1.png",bg="white",width=10,height=6)
