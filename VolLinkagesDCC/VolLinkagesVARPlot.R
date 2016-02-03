par(mfrow = c(2, 3))
# plot vaiances
plot(Data$Date[3:467],
     H_t_DCC[, 1, 1],
     type     = "l", 
     cex.axis = 1.4,
     main     = "Crude Oil",
     cex.lab  = 2, 
     lwd      = 3,
     xlab     = "",
     ylab     = "",
     cex.main = 2)

plot(Data$Date[3:467],
     H_t_DCC[, 2, 2],
     type     = "l", 
     cex.axis = 1.4,
     main     = "Rapeseed",
     cex.lab  = 2, 
     lwd      = 3,
     xlab     = "",
     ylab     = "",
     cex.main = 2)

plot(Data$Date[3:467],
     H_t_DCC[, 3, 3],
     type     = "l", 
     cex.axis = 1.4,
     main     = "Biodiesel",
     cex.lab  = 2, 
     lwd      = 3,
     xlab     = "",
     ylab     = "",
     cex.main = 2)

options(scipen = 5)

## plot correlations
plot(Data$Date[3:467],
     H_t_DCC[, 1, 2]/sqrt(H_t_DCC[, 1, 1] * H_t_DCC[, 2, 2]),
     type     = "l",
     cex.axis = 1.4, 
     main     = "Crude Oil - Rapeseed",
     cex.lab  = 2,
     lwd      = 3, 
     xlab     = "",
     ylab     = "",
     cex.main = 2)

abline(h = 0, lty = 2)

plot(Data$Date[3:467],
     H_t_DCC[, 1, 3]/sqrt(H_t_DCC[, 1, 1] * H_t_DCC[, 3, 3]),
     type = "l",
     cex.axis = 1.4, 
     main = "Crude Oil - Biodiesel",
     cex.lab = 1.2, 
     lwd = 3,
     xlab = "",
     ylab = "",
     cex.main = 2)

abline(h = 0, lty = 2)

plot(Data$Date[3:467],
     H_t_DCC[, 2, 3]/sqrt(H_t_DCC[, 2, 2] * H_t_DCC[, 3, 3]),
     type = "l",
     cex.axis = 1.4, 
     main = "Rapeseed - Biodiesel",
     cex.lab = 1.2, 
     lwd = 3,
     xlab = "",
     ylab = "",
     cex.main = 2)

abline(h = 0, lty = 2)
