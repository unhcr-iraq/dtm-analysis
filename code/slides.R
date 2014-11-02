library(devtools)

# install_github('slidify', 'ramnathv') install_github('slidifyLibraries', 'ramnathv')

library(slidify)

# Creating now the folder where the presentation will be developped
author("presentation")

## Now create the slides from the mark down file
slidify("index.Rmd") 
