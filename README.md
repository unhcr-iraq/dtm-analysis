Displacement Tracking Matrix Analysis
============

Analysis of Internally displaced Persons in Iraq - starting from Dec 2013 

Dataset from http://iomiraq.net/dtm-page

The code is organised as follows:

analysis.R source all the scripts below.


## Reorganising the dataset -- parse the data, add aggreated column, prepare some ranking
source("~/unhcr_r_project/displacement/recode.R")

## Generating a series of graph on asylum, orgin, data and accomodation
source("~/unhcr_r_project/displacement/plot.R")

## Aggreagting data at level1 & level 2 and joing it with population information as well as refugee information
## Result is govnames and disnames
source("~/unhcr_r_project/displacement/aggregated.R")

## prepare some classification of sites based on population level -- data is aggregated per sites -- not by report
source("~/unhcr_r_project/displacement/classification.R")

## Choroplets maps based on aggregated information at level1 and level2
source("~/unhcr_r_project/displacement/mapchoro.R")

## Point based maps looking at accomodation and priority needs
source("~/unhcr_r_project/displacement/map.R")

## Relocation simulation based on camp absorption capacity
source("~/unhcr_r_project/displacement/simulation.R")

## Looking at histogram of reported location based on population size and accomodation
source("~/unhcr_r_project/displacement/histo.R")
   
