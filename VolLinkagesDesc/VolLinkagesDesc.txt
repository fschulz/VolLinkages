# Table 2

print("Mean")
print(colMeans(Data[, -1]))
print("St.D.")
print(sqrt(var(Data[, -1])))
print("Corr.")
print(cor(Data[, -1]))

r = diff(as.matrix(lp))[-1, ]  # differenced deseasonalized logs
print("Skewness")
print(c(skewness(r[, 1]),
        skewness(r[, 2]),
        skewness(r[, 3])))

print("Kurtosis")
print(c(kurtosis(r[, 1]),
        kurtosis(r[, 2]),
        kurtosis(r[, 3])))

print("Box Ljung (residuals)")
print(c(Box.test(r[, 1], lag = 20, type = "Ljung")$p.value, 
        Box.test(r[, 2], lag = 20, type = "Ljung")$p.value, 
        Box.test(r[, 3], lag = 20, type = "Ljung")$p.value))

print("Box-Ljung (squared residuals)")
print(c(Box.test(r[, 1]^2, lag = 20, type = "Ljung")$p.value, 
        Box.test(r[, 2]^2, lag = 20, type = "Ljung")$p.value, 
        Box.test(r[, 3]^2, lag = 20, type = "Ljung")$p.value))

print("ARCH")
print(c(ArchTest(r[, 1], lags = 5)$p.value,
        ArchTest(r[, 2], lags = 5)$p.value,
        ArchTest(r[, 3], lags = 5)$p.value))

print("Shapiro-Wilk")
print(c(shapiro.test(r[, 1])$p.value,
        shapiro.test(r[, 2])$p.value,
        shapiro.test(r[, 3])$p.value))

