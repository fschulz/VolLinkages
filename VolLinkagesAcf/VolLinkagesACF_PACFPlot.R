## Figure 4 & 5 Crude oil

# Plot ACF & PACF
Acf(resid[, 1],
    main     = "",
    cex.lab  = 1.7,
    cex.axis = 1.7, 
    cex.main = 2,
    lwd      = 2,
    xlab     = "",
    ylab     = "")

title(main = "Crude Oil", cex.main = 2)


pacf(resid[, 1],
     main     = "",
     cex.lab  = 1.7,
     cex.axis = 1.7, 
     lwd      = 2,
     xlab     = "",
     ylab     = "")

title(main = "Crude Oil", cex.main = 2)


Acf(resid[, 1]^2,
    main     = "",
    cex.lab  = 1.7,
    cex.axis = 1.7, 
    cex.main = 2,
    lwd      = 2,
    xlab     = "",
    ylab     = "")

title(main = "Crude Oil", cex.main = 2)


pacf(resid[, 1]^2,
     main     = "",
     cex.lab  = 1.7,
     cex.axis = 1.7, 
     lwd      = 2,
     xlab     = "",
     ylab     = "")

title(main = "Crude Oil", cex.main = 2)

# Rapeseed

# Plot ACF & PACF
Acf(resid[, 2],
    main     = "",
    cex.lab  = 1.7,
    cex.axis = 1.7,
    lwd      = 2, 
    xlab     = "",
    ylab     = "")

title(main = "Rapeseed", cex.main = 2)


pacf(resid[, 2],
     main     = "",
     cex.lab  = 1.7,
     cex.axis = 1.7,
     lwd      = 2, 
     xlab     = "",
     ylab     = "")

title(main = "Rapeseed", cex.main = 2)


Acf(resid[, 2]^2,
    main = "",
    cex.lab = 1.7,
    cex.axis = 1.7, 
    lwd = 2,
    xlab = "",
    ylab = "")

title(main = "Rapeseed", cex.main = 2)


pacf(resid[, 2]^2,
     main     = "",
     cex.lab  = 1.7,
     cex.axis = 1.7, 
     lwd      = 2,
     xlab     = "",
     ylab     = "")

title(main = "Rapeseed", cex.main = 2)

# Biodiesel

# Plot ACF & PACF
Acf(resid[, 3]^2,
    main     = "",
    cex.lab  = 1.4,
    cex.axis = 1.7, 
    lwd      = 2,
    xlab     = "",
    ylab     = "")

title(main = "Biodiesel", cex.main = 2)


pacf(resid[, 3]^2,
     main     = "",
     cex.lab  = 1.7,
     cex.axis = 1.7, 
     lwd      = 2,
     xlab     = "",
     ylab     = "")

title(main = "Biodiesel", cex.main = 2)


Acf(resid[, 3],
     main = "",
    cex.lab  = 1.4,
    cex.axis = 1.7,
    lwd      = 2, 
    xlab     = "",
    ylab     = "")

title(main = "Biodiesel", cex.main = 2)


pacf(resid[, 3],
     main = "",
     cex.lab = 1.4,
     cex.axis = 1.7,
     lwd = 2, 
     xlab = "",
     ylab = "")

title(main = "Biodiesel", cex.main = 2)

