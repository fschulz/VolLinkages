# Clear memory
rm(list = ls())
# Set working directory
#setwd("C:...")

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

source("VolLinkagesData.R")
source("VolLinkagesVECM.R")

par(mfrow = c(1, 3))

plot(u.final[, 1],
     type = "h",
     ylab = "",
     xlab = "", 
     main = "Crude Oil")

plot(u.final[, 2],
     type = "h",
     ylab = "",
     xlab = "", 
     main = "Rapeseed")

plot(u.final[, 3],
     type = "h",
     ylab = "",
     xlab = "", 
     main = "Biodiesel")
