# Plot 1
par(mfrow = c(1, 2))
plot(Data$Date,
     Data$Crude.Oil,
     cex.axis = 1.2,
     cex.lab  = 1.2, 
     type     = "l",
     lwd      = 3,
     xlab     = "Date",
     ylab     = "Price", 
     ylim     = c(0, 1500))

lines(Data$Date,
      Data$Biodiesel,
      type = "l",
      lwd  = 3, 
      lty  = 2)

lines(Data$Date,
      Data$Rapeseed,
      type = "l",
      lwd  = 4, 
      lty  = 3)

legend("topleft",
       c("Crude Oil", "Biodiesel", "Rapeseed"), 
       y.intersp = 0.5,
       cex       = 1.2,
       lty       = c(1, 2, 3), 
       lwd       = 3,
       bty       = "n")

plot(Data$Date,
     lp$Crude.Oil,
     cex.axis = 1.2,
     cex.lab  = 1.2, 
     type     = "l",
     lwd      = 3,
     xlab     = "Date",
     ylab     = "Price")

lines(Data$Date,
      lp$Biodiesel,
      type = "l",
      lwd  = 3, 
      lty  = 2)

lines(Data$Date,
      lp$Rapeseed,
      type = "l",
      lwd  = 4, 
      lty  = 3)

legend("topleft",
       c("Crude Oil", "Biodiesel", "Rapeseed"), 
       y.intersp = 0.5,
       cex       = 1.2,
       lty       = c(1, 2, 3), 
       lwd       = 3,
       bty       = "n")
