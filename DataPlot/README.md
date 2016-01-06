[<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/banner.png" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **VolLinkages** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml
------------------------------------------------------------------------------
Paper: Volatility linkages between energy and agricultural commodity prices
------------------------------------------------------------------------------
Quantlet: -
------------------------------------------------------------------------------
Description: Code used to make the plots and tables presented in the paper
------------------------------------------------------------------------------
Usage: -
------------------------------------------------------------------------------
Inputs: Data.R, FGLS.R, RiskManagement.R
------------------------------------------------------------------------------
Output: -
------------------------------------------------------------------------------
Keywords: Energy, Agriculture, Commodities
------------------------------------------------------------------------------
See also: -
------------------------------------------------------------------------------
Author: Brenda Lòpez Cabrera, Franziska Schulz
------------------------------------------------------------------------------
```

```R
# Clear memory
rm(list = ls())
# Set working directory
setwd("C:\\Users\\Franziska Schulz\\Code")
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
"uroot",
"sm",
"ccgarch")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
install.packages(x)
})
getwd()
source("Data.R")
source("Plot1.R")
source("Table2.R")
source("Table3.R")
source("Table4.R")
source("VECM.R") # Equation 17 + 18 /
source("Plot3.R")
source("Table5.R") /
source("Plot4&5.R")
source("FGLS.R") # Table 7 + 8
source("Table6.R")
source("Plot2.R")
source("Plot6.R")
source("RF_Plot7.R")
source("Plot8.R") /
source("Plot9.R")
source("Table9.R")
source("Table10.R")
# Application to Hedging:
source("RiskManagement.R")
```
