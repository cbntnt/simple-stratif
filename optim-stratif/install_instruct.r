
## Preparation:
## 1. Add "Rcmd" to the windows path;
## http://cran.r-project.org/bin/windows/base/rw-FAQ.html#Rcmd-is-not-found-in-my-PATH_0021
## 2. Before you can install package from the source code, you first need to install all dependent packages:
## List of dependent packages are available in the "DESCRIPTION" file in the pkg dir;

   
## INSTALLING PACKAGE FROM SOURCE CODE - NATIVE INSTALL:
## 1. Preferred option - install from source code directly and make html files:
install.packages(c("utils", "stratification", "sp", "raster", "maptools"))
system("Rcmd INSTALL --html D:/simple-stratif/optim-stratif/pkg")
library(soilcarbon)

## 2. Prepare a compiled package for sharing (.tar.gz):
system("Rcmd build D:/simple-stratif/optim-stratif/pkg")
                                    #commit zip file

## Installing package from tar.gz file containing the source code - any install: 
# Download the package:
download.file("http://code.google.com/p/simple-stratif/source/browse/optim-stratif/soilcarbon_0.1-0.tar.gz", "soilcarbon_0.1-0.tar.gz")
# Install package from a zipped source code:
install.packages(c("utils", "stratification", "sp", "raster", "maptools"))
system("Rcmd INSTALL soilcarbon_0.1-0.tar.gz")
library(soilcarbon)

## * the package versions and absolute paths need to be adjusted manually of course

# end of script;