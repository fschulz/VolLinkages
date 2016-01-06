## Biodiesel
Data = read.delim("Biodiesel.txt")
Data$Date = as.Date(Data$Date)

# Rapeseed
dates = seq(as.Date("2013/01/31"), as.Date("2003/01/31"), by = "-3 months")

for (i in seq(4, length(dates), 4)) {
    dates[i] = dates[i] - 1
}

x = read.table("Rapeseed.txt",
               skip = 1,
               sep = "\t", 
               stringsAsFactors = FALSE,
               header = TRUE,
               colClasses = rep(c("Date", "numeric"), 31))

mat = data.frame(date             = seq(as.Date("2012/10/26"), as.Date("2002/11/01"), by = "-1 week"),
                 F1               = NA, 
                 F2               = NA,
                 T1               = NA,
                 T2               = NA,
                 spot             = NA,
                 stringsAsFactors = FALSE)

diffmat = outer(dates, mat$date, "-")
diffmat[diffmat < 0] = Inf
index = apply(diffmat, 2, function(x) {
    which.min(x)
})

mat$T1 = dates[index]
mat$T2 = dates[index - 1]

for (j in 1:nrow(mat)) {
    temp = x[, 2 * index[j] - 1] %in% mat$date[j]
    if (sum(temp) != 0) {
        mat$F1[j] = x[x[, 2 * index[j] - 1] %in% mat$date[j], 2 * index[j]]
    }
}

for (j in 1:nrow(mat)) {
    temp = x[, 2 * (index[j] - 1) - 1] %in% mat$date[j]
    if (sum(temp) != 0) {
        mat$F2[j] = x[x[, 2 * (index[j] - 1) - 1] %in% mat$date[j], 2 * (index[j] - 1)]
    }
}

mat$spot = mat$F1/exp(as.numeric((mat$T1 - mat$date)/7) * log(mat$F2/mat$F1) * 1/(as.numeric((mat$T2 - mat$T1)/7)))
mat = mat[nrow(mat):1, ]

## Crude Oil
spot.co = read.table("CrudeOil.txt",
                     sep              = "\t",
                     stringsAsFactors = FALSE,
                     header           = TRUE)

spot.co = spot.co[nrow(spot.co):1, ]
spot.co$Date = as.Date(spot.co$Date)

Data.new = data.frame(Date      = spot.co$Date[1:467],
                      Crude.Oil = spot.co$Price[1:467],
                      Rapeseed  = mat$spot[30:496],
                      Biodiesel = Data$Biodiesel[45:511])
Data = Data.new

Data$Date = as.Date(Data$Date)

Data$Biodiesel = na.spline(Data$Biodiesel, na.rm = TRUE)  # interpolate missing values using cubic splines
Data$Rapeseed = na.spline(Data$Rapeseed, na.rm = TRUE)  # interpolate missing values using cubic splines

# convert to m³
Data$Rapeseed = Data$Rapeseed/0.6
Data$Crude.Oil = Data$Crude.Oil * 6.28981

# logarithmic Prices
lp = log(Data[, 2:4])

## Rapeseed
week     = 1:52
temp     = as.matrix(lp$Rapeseed)
temp     = rbind(temp, matrix(NaN, nrow = 1, ncol = 1))
YN1      = matrix(data = temp, nrow = 52, byrow = FALSE)
Y1       = rowMeans(YN1, na.rm = TRUE)
hx1      = median(abs(week - median(week)))/0.6745 * (4/(3 * 52))^0.2
hy1      = median(abs(Y1 - median(Y1)))/0.6745 * (4/(3 * 52))^0.2
h1       = sqrt(hy1 * hx1)
np       = npreg(Y1 ~ week, regtype = "ll", bws = h1, ckertype = "gaussian")
season1  = fitted(np)
season11 = rep(season1, 9)
resid11  = (temp - season11)
Rapeseed = resid11[1:467]

## Crude Oil
week      = 1:52
temp      = as.matrix(lp$Crude.Oil)
temp      = rbind(temp, matrix(NaN, nrow = 1, ncol = 1))
YN1       = matrix(data = temp, nrow = 52, byrow = FALSE)
Y1        = rowMeans(YN1, na.rm = TRUE)
hx1       = median(abs(week - median(week)))/0.6745 * (4/3/52)^0.2
hy1       = median(abs(Y1 - median(Y1)))/0.6745 * (4/3/52)^0.2
h1        = sqrt(hy1 * hx1)
h1        = npregbw(Y1 ~ week, regtype = "ll", method = "cv.ls")
np        = npreg(Y1 ~ week, regtype = "ll", bws = h1, ckertype = "gaussian")
season1   = fitted(np)
season11  = rep(season1, 9)
resid11   = (temp - season11)[1:467]
Crude.Oil = resid11

## Biodiesel

week = 1:52
temp = as.matrix(lp$Biodiesel)
temp = rbind(temp, matrix(NaN, nrow = 1, ncol = 1))
YN1  = matrix(data = temp, nrow = 52, byrow = FALSE)
Y1   = rowMeans(YN1, na.rm = TRUE)
hx1  = median(abs(week - median(week)))/0.6745 * (4/3/52)^0.2
hy1  = median(abs(Y1 - median(Y1)))/0.6745 * (4/3/52)^0.2
h1   = sqrt(hy1 * hx1)
h1   = npregbw(Y1 ~ week, regtype = "ll", method = "cv.ls")
np   = npreg(Y1 ~ week, regtype = "ll", bws = h1, ckertype = "gaussian")

season1   = fitted(np)
season11  = rep(season1, 9)
resid11   = (temp - season11)[1:467]
Biodiesel = resid11

Data_deseas           = data.frame(Crude.Oil, Rapeseed, Biodiesel)
rownames(Data_deseas) = Data$Date

lp           = Data_deseas
rownames(lp) = Data$Date
