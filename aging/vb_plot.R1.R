#--------------------------------------------------------------
#Ben Neely
#01/26/2024
#Plot FWD growth models
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

if("cowplot" %in% rownames(installed.packages()) == FALSE) {install.packages("cowplot")}
library(cowplot)

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
        axis.title=element_text(size=26,face="bold"),
        axis.text=element_text(size=24),
        legend.position=c(0.15,0.01),
        legend.background=element_rect(fill="transparent"),
        legend.text=element_text(size=30),
        legend.title=element_blank(),
        legend.key.width=unit(1,"in"),
        legend.key.height=unit(0.7,"in"),
        legend.justification=c("left","bottom"))

## Import data
tcrr_points=import("tcrr_vb.R1.xlsx",which="TCRR_all")%>%
  select(impd,tl=cmgrp,age=agecap)

tcrr_preds=import("tcrr_vb.R1.xlsx",which="TCRR_preds")

gelr_points=import("gelr_vb.R1.xlsx",which="GELR_all")%>%
  select(impd,tl=cmgrp,age=agecap)

gelr_preds=import("gelr_vb.R1.xlsx",which="GELR_preds")

## Combine data for plotting
points=bind_rows(tcrr_points,gelr_points)
preds=bind_rows(tcrr_preds,gelr_preds)

## Labels for vB equations
gelr_lab=paste0("TL==",576,"~group('(',1-e^{-",0.215,"~(age","+",1.037,")},')')")
tcrr_lab=paste0("TL==",508,"~group('(',1-e^{-",0.285,"~(age","+",0.355,")},')')")

## Estimate age at preferred and memorable lengths
subset(preds,lci95>379 & lci95<383)
subset(preds,uci95>379 & uci95<383)

subset(preds,lci95>496 & lci95<510)
subset(preds,uci95>508 & uci95<510)

################################################################################
################################################################################
## Plot von Bertalanffy growth model for GELR and TCRR drum
ggplot()+
  geom_point(points,mapping=aes(x=age,y=tl,fill=impd,color=impd,shape=impd),size=6,alpha=0.4)+
  geom_ribbon(preds,mapping=aes(x=age,ymin=lci95,ymax=uci95,fill=impd),alpha=0.6)+
  geom_line(preds,mapping=aes(x=age,y=tl,color=impd),linewidth=2)+
  scale_fill_manual(values=c("black","gray50"),
                    label=c("Glen Elder:","Tuttle Creek:"))+
  scale_color_manual(values=c("black","gray50"),
                     label=c("Glen Elder:","Tuttle Creek:"))+
  scale_shape_manual(values=c(21,24),
                     label=c("Glen Elder:","Tuttle Creek:"))+
  scale_x_continuous(limits=c(-0.6,43),
                     breaks=seq(0,40,5),
                     expand=c(0,0),
                     name="Estimated age")+
  scale_y_continuous(limits=c(0,750),
                     breaks=seq(0,700,100),
                     expand=c(0,0),
                     name="Total length (mm)")+
  annotate("text",label=gelr_lab,x=19.0,y=216,parse=T,hjust=0,vjust=1,size=10)+
  annotate("text",label=tcrr_lab,x=20.2,y=111,parse=T,hjust=0,vjust=1,size=10)+
  pubtheme

save_plot(plot=last_plot(),"vb.R1.png",bg="white",base_width=12,base_height=6)