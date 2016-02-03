source("VolLinkagesBootstrap.R")
## Plots

par(mfrow = c(3, 3))
## Variances
plot(Data$Date[1:465],
     Sigma_t[, 1, 1],
     type     = "l", 
     lwd      = 2.2,
     main     = "Crude Oil",
     cex.axis = 1.5, 
     xlab     = "",
     ylab     = "",
     cex.main = 2,
     ylim     = c(0, 0.0095))

lines(Data$Date[1:465],
      quantMatrix0.05[1, 1, ], 
      lwd = 1.5,
      lty = 2)

lines(Data$Date[1:465],
      quantMatrix0.95[1, 1, ], 
      lwd = 1.5,
      lty = 2)


plot(Data$Date[1:465],
     Sigma_t[, 2, 2],
     type     = "l", 
     lwd      = 2.2,
     main     = "Rapeseed",
     cex.axis = 1.5, 
     xlab     = "",
     ylab     = "",
     cex.main = 2,
     ylim     = c(0, 0.0019))


lines(Data$Date[1:465],
      quantMatrix0.05[2, 2, ], 
      lwd = 1.5,
      lty = 2)

lines(Data$Date[1:465],
      quantMatrix0.95[2, 2, ], 
      lwd = 1.5,
      lty = 2)


plot(Data$Date[1:465],
     Sigma_t[, 3, 3],
     type = "l", 
     lwd = 2.2,
     main = "Biodiesel",
     cex.axis = 1.5, 
     xlab = "",
     ylab = "",
     cex.main = 2,
     ylim = c(0, 7e-04))

lines(Data$Date[1:465],
      quantMatrix0.05[3, 3, ], 
      lwd = 1.5,
      lty = 2)

lines(Data$Date[1:465],
      quantMatrix0.95[3, 3, ], 
      lwd = 1.5,
      lty = 2)

options(scipen = 5)

## correlations
plot(Data$Date[1:465],
     Sigma_t[,1, 2]/(sqrt(Sigma_t[, 1,1 ] * Sigma_t[, 2, 2])),
     type     = "l",
     lwd      = 2.2, 
     main     = "Crude Oil-Rapeseed",
     cex.axis = 1.5, 
     xlab     = "",
     ylab     = "",
     cex.main = 2,
     ylim     = c(-0.4, 0.7))

lines(Data$Date[1:465],
      quantMatrix0.05[1, 2, ], 
      lwd = 1.5,
      lty = 2)

lines(Data$Date[1:465],
      quantMatrix0.95[1, 2, ], 
      lwd = 1.5,
      lty = 2)

abline(h = 0, lty = 2)

plot(Data$Date[1:465],
     Sigma_t[, 1, 3]/(sqrt(Sigma_t[ , 1, 1] * Sigma_t[ , 3, 3])),
     type     = "l",
     lwd      = 2.2, 
     main     = "Crude Oil-Biodiesel",
     cex.axis = 1.5, 
     xlab     = "",
     ylab     = "",
     cex.main = 2,
     ylim     = c(-0.4, 0.55))

lines(Data$Date[1:465],
      quantMatrix0.05[1, 3, ], 
      lwd = 1.5,
      lty = 2)

lines(Data$Date[1:465],
      quantMatrix0.95[1, 3, ], 
      lwd = 1.5,
      lty = 2)

abline(h = 0, lty = 2)

plot(Data$Date[1:465],
     Sigma_t[ , 2, 3]/sqrt(Sigma_t[ , 2, 2] * Sigma_t[, 3, 3]), 
     type     = "l", 
     lwd      = 2.2, 
     main     = "Rapeseed-Biodiesel", 
     cex.axis = 1.5, 
     xlab     = "", 
     ylab     = "", 
     cex.main = 2, 
     ylim     = c(-0.3, 0.4))

lines(Data$Date[1:465], 
      quantMatrix0.05[2, 3, ],
      lwd = 1.5, 
      lty = 2)

lines(Data$Date[1:465], 
      quantMatrix0.95[2, 3, ], 
      lwd = 1.5, 
      lty = 2)

abline(h = 0, lty = 2)
