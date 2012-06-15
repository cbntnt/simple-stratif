
## Preparation:
## 1. Add "Rcmd" to the windows path;
## http://cran.r-project.org/bin/windows/base/rw-FAQ.html#Rcmd-is-not-found-in-my-PATH_0021
## 2. Before you can install package from the source code, you first need to install all dependent packages:
## List of dependent packages are available in the "DESCRIPTION" file in the pkg dir;

install.packages(c("utils", "stratification", "sp", "raster", "maptools"))

## INSTALLING PACKAGE FROM SOURCE CODE:
## 1. Preferred option - install from source code directly and make html files:
system("Rcmd INSTALL --html D:/simple-stratif/optim-stratif/pkg")

## 2. Prepare a compiled package for sharing:
system("Rcmd build D:/simple-stratif/optim-stratif/pkg")
library(soilcarbon)

## Installing package from tar.gz file containing the source code: 
# Download the package:
download.file("http://code.google.com/p/simple-stratif/source/browse/optim-stratif/soilcarbon_0.1-0.tar.gz", "soilcarbon_0.1-0.tar.gz")
# Install package from a zipped source code:
system("Rcmd INSTALL soilcarbon_0.1-0.tar.gz")
library(soilcarbon)

## * the package versions and absolute paths need to be adjusted manually of course

# end of script;