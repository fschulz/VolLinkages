# Clear memory
rm(list = ls())

 #Set System from German to English
Sys.setlocale('LC_ALL','C') 

# install and load libraries
libraries = c("car",
              "abind",
              "quantmod",
              "zoo", 
              "Epi",
              "tseries",
              "urca",
              "vars",
              "tsDyn",
              "stats", 
              "fGarch",
              "np",
              "corpcor",
              "FinTS",
              "rugarch", 
              "rmgarch",
              "Rcpp",
              "truncnorm",
              "Kendall",
              "sm",
              "ccgarch")

lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})

lapply(libraries,require,quietly=TRUE,character.only=TRUE)

#install archived package uroot
pkgfile = "uroot_1.4.tar.gz"

download.file(url      = "http://cran.r-project.org/src/contrib/Archive/uroot/uroot_1.4.tar.gz",
              destfile = pkgfile)

install.packages(pkgs  = pkgfile,
                 type  = "source",
                 repos = NULL)

require("uroot")
unlink(pkgfile)

#set options and seed
options(digits=3)
set.seed(20)

#read data
Data      <- read.delim("Data.txt")
Data$Date <- as.Date(Data$Date)
lp        <- read.delim("Data_log.txt")

#source dependent files
source("VolLinkagesVECM.R")
source("VolLinkagesVARPlot.R")