
## Preparation:
## 1. Add "Rcmd" to the windows path;
## http://cran.r-project.org/bin/windows/base/rw-FAQ.html#Rcmd-is-not-found-in-my-PATH_0021
## 2. Before you can install package from the source code, you first need to install all dependent packages:
## List of dependent packages are available in the "DESCRIPTION" file in the pkg dir;

   
## INSTALLING PACKAGE FROM SOURCE CODE - NATIVE INSTALL or MAKE:

# if issues with proxy use below prior to selecting mirror 
setInternet2() 

## 1. Preferred option - install from source code directly and make html files:
install.packages(c("utils", "stratification", "sp", "raster", "maptools"))
system("Rcmd INSTALL --html D:/simple-stratif/optim-stratif/pkg")
library(soilcarbon)

## 2. Prepare a compiled package for sharing (.tar.gz):
system("Rcmd build D:/simple-stratif/optim-stratif/pkg")
# commit zip file

## Installing package from tar.gz file containing the source code - any install: 
# Download the package:
download.file("http://code.google.com/p/simple-stratif/source/browse/optim-stratif/soilcarbon_0.2-1.tar.gz", "soilcarbon_0.2-1.tar.gz")
# Install package from a zipped source code:
install.packages(c("utils", "stratification", "sp", "raster", "maptools"))
system("Rcmd INSTALL soilcarbon_0.2-1.tar.gz")
library(soilcarbon)

## * the package versions and absolute paths need to be adjusted manually of course

# Load some data sets not available without the package:
download.file("http://simple-stratif.googlecode.com/svn/optim-stratif/pkg/data/springf.rda", "springf.rda")
load("springf.rda")


         ############################################################################################################

#update GSIF
install.packages(c("RCurl", "XML", "rgdal", "raster", "sp", "aqp", "mda", "gstat", "plotKML", "dismo", "rJava"))
download.file("http://gsif.r-forge.r-project.org/GSIF_0.2-2.tar.gz", "GSIF_0.2-2.tar.gz")
system("R CMD INSTALL GSIF_0.2-2.tar.gz")

#update plotKML
download.file("http://plotkml.r-forge.r-project.org/plotKML_0.2-4.tar.gz", "plotKML_0.2-4.tar.gz")
system("R CMD INSTALL plotKML_0.2-4.tar.gz")

                            download.file("http://cran.r-project.org/src/contrib/XML_3.9-4.tar.gz", "XML_3.9-4.tar.gz")
[5:25:44 PM] Tomislav Hengl: system("Rcmd ...


# end of script;