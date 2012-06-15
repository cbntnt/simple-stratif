
## Preparation:
## 1. Add "Rcmd" to the windows path;
## http://cran.r-project.org/bin/windows/base/rw-FAQ.html#Rcmd-is-not-found-in-my-PATH_0021
## 2. First install dependent packages:
## see "DESCRIPTION" file in the pkg dir

install.packages(c("utils", "stratification", "sp", "raster", "maptools"))

## Preferred option - install from source code directly and make html files:
system("Rcmd INSTALL --html D:/simple-stratif/optim-stratif/pkg")

## Prepare a compiled package for sharing:
system("Rcmd build D:/simple-stratif/optim-stratif/pkg")
library(soilcarbon)

#Instructions for others - save attachment to working directory, load R session in directory, run following command.... 
 

## Install package from a zipped source code:
system("Rcmd INSTALL soilcarbon_0.1-0.tar.gz")
library(soilcarbon)


# end of script;