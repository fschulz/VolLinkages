## Unit Root tests (Table 3)

## Dickey-Fuller
print("DF")
# Crude Oil
ur1 = ur.df(lp$Crude.Oil, type = "trend", lags = 9)  # no time trend, but unit root
summary(ur1)
plot(ur1)
ur1 = ur.df(lp$Crude.Oil, type = "drift", lags = 9)  #no drift
summary(ur1)
ur1 = ur.df(lp$Crude.Oil, type = "none", lags = 9)  #model without trend or drift
print(summary(ur1))
plot(ur1)

# Biodiesel
ur2 = ur.df(lp$Biodiesel, type = "trend", lags = 9)  # no time trend, but unit root
summary(ur2)
ur2 = ur.df(lp$Biodiesel, type = "drift", lags = 9)  #no drift
summary(ur2)
ur2 = ur.df(lp$Biodiesel, type = "none", lags = 9)  #model without trend or drift
print(summary(ur2))
plot(ur2)

# Rapeseed
ur3 = ur.df(lp$Rapeseed, type = "trend", lags = 8)  # no time trend, but unit root
summary(ur3)
ur3 = ur.df(lp$Rapeseed, type = "drift", lags = 8)  #no drift
summary(ur3)
ur3 = ur.df(lp$Rapeseed, type = "none", lags = 8)  #model without trend or drift
print(summary(ur3))
plot(ur3)

## KPSS Test
print("KPSS")
kpss1 = ur.kpss(lp$Crude.Oil, lags = "long", type = "mu")
print(summary(kpss1))
kpss2 = ur.kpss(lp$Biodiesel, lags = "long", type = "mu")
print(summary(kpss2))
kpss3 = ur.kpss(lp$Rapeseed, lags = "long", type = "mu")
print(summary(kpss3))

## PP Test
print("PP")
pp1 = ur.pp(lp$Crude.Oil, model = "constant", type = "Z-tau")
print(summary(pp1))
pp2 = ur.pp(lp$Biodiesel, model = "constant", type = "Z-tau")
print(summary(pp2))
pp3 = ur.pp(lp$Rapeseed, model = "constant", type = "Z-tau")
print(summary(pp3))
