#--------------------------------------------------------------
#Ben Neely
#02/06/2024
#Estimate natural mortality of FWD populations
#--------------------------------------------------------------

## Clear R
cat("\014")  
rm(list=ls())

###############################################################################
################################################################################
## Natural mortality using Pauly nls-T estimator
## Then et al 2015 - 
## Evaluating the predictive performance of empirical estimators of natural
## mortality rate using information on over 200 fish species
## ICES Journal of Marine Science 72:82-92

## Pauly nls-T estimator

## Set up parameters
gelr_tmax=42
gelr_k=0.215
gelr_linf=576

tcrr_tmax=28
tcrr_k=0.285
tcrr_linf=508

## GELR natural mortality
(gelr_paulynlst=4.118*(gelr_k^0.73)*gelr_linf^-0.333)

## TCRR natural mortality estimated two ways
(tcrr_paulynlst=4.118*(tcrr_k^0.73)*tcrr_linf^-0.333)
