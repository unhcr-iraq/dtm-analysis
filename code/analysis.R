########################################################## Displacement Tracking Matrix analysis #################

## Edouard Legoupil -Information Management Officer- legoupil..at..unhcr.org

## Loading all
source("code/packages.R")

### Dataset from http://iomiraq.net/dtm-page See also http://www.humanitarianresponse.info/applications/data/datasets/locations/iraq
rm(master)

# master <- read.csv('data/final_18082014_dtm_master.csv', header=F) master <-
# read.csv('data/DTM_Dataset_-_01092014_final_0.csv', header=F) master_14092014 <-
# read.csv('data/DTM_Dataset_14092014.csv', header=F)
# read.csv("data/DTM_Dataset_28092014.csv", header = F)
master <- read.csv("data/DTM_Dataset_26102014.csv", header = F)

# master_or <- read.csv('data/DTM_Dataset_28092014.csv', header=F) label <- read.csv('data/label.csv')
# names(master_or) <- label[,3] rm(label) rm(master_or) master_or$Master.Families2 <- as.numeric(master_or$Master.Families) test <- master$Master.Families -
# (master$total/6)

## Reorganising the dataset -- parse the data, add aggreated column, prepare some ranking
source("code/recode.R")

## Generating a series of graph on asylum, orgin, data and accomodation
source("code/plot.R")

## Aggregating data at level1 & level 2 and joining it with population information as well as refugee information Result is govnames and disnames
source("code/aggregated.R")


## Reapportion to the 1.8 millions figures and generate graph for the one pager Result is govnames and disnames
source("code/caseload.R")

## prepare some classification of sites based on population level -- data is aggregated per sites -- not by report Result is govnames and disnames
source("code/classification.R")

## Relocation simulation based on camp absorption capacity
source("code/simulation.R")

## Prioritisation plan of locations to be assessed based on IOM baseline
source("code/baseline.R")

## Choroplets maps based on aggregated information at level1 and level2
source("code/mapchoro.R")

## Point based maps looking at accomodation and priority needs
source("code/map.R")

## Looking at histograme of reported location based on population size and accomodation
source("code/histo.R")

## Winterisation analysis based on bioclim dataset http://www.worldclim.org/
source("code/climate.R") 
