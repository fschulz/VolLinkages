source("VolLinkagesCrossValidation.R")

Epanechnikov = function(u) {
    return(3/4 * (1 - u^2) * (abs(u) < 1))
}
Gaussian = function(u) {
    dnorm(u)
}
Quartic = function(u) {
    return(15/16 * (1 - u^2)^2 * (abs(u) < 1))
}
K = Gaussian

data = lp

diff = function(x) {
    diff = tail(x, -1) - head(x, -1)
    diff = rbind(rep(NA, ncol(x)), diff)
    return(diff)
}

diff.data = diff(data)
lag = cbind(Lag(data[, 1]), Lag(data[, 2]), Lag(data[, 3]))
colnames(lag) = c("L1", "L2", "L3")

# select lag order
vecm_ols = VAR(diff.data[-1, ],
               p      = 1,
               type   = "none",
               exogen = lag[-1, ])  # constant is insignificant!
u.est = residuals(vecm_ols)

# long run covariance
T    = nrow(u.est)
grid = (1:T)/T


## Minimizing CV criterion
#opt = optimize(CV,
#               maximum  = FALSE,
#               interval = c(0, 0.5),
#               tol      = .Machine$double.eps^0.5)
#hopt = opt$minimum
#h=hopt

h     = 0.054  # optimal for Gaussian h = 0.054, Epanechnikov h = 0.14, Quartic = 0.161
resid = u.est
sigma = array(, dim = c(3, 3, T))
for (i in 1:T) {
    tao         = grid[i]
    denominator = array(0, dim = c(3, 3))
    nominator   = 0
    for (j in 1:T) {
        denominator = denominator + K((j/T - tao)/h) * resid[j, ] %*% t(resid[j, ])
        nominator   = nominator + K((j/T - tao)/h)
    }
    sigma[, , i] = denominator/nominator  # Estimated Covariance matrix
}


eps = matrix(0, T, 3)
for (i in 1:T) {
    eigen    = eigen(sigma[, , i])
    eps[i, ] = eigen$vectors %*% (diag(eigen$values^(-1/2))) %*% t(eigen$vectors) %*% resid[i, ]
}

## EGARCH GED
spec = ugarchspec(variance.model     = list(model      = "eGARCH",
                                            garchOrder = c(1, 1)),
                  mean.model         = list(armaOrder    = c(0, 0),
                                            include.mean = FALSE),
                  distribution.model = "ged")

# Multivariate DCC-Garch

dcc.garch.spec = dccspec(uspec        = multispec(replicate(3, spec)),
                         dccOrder     = c(1, 1),
                         distribution = "mvnorm")
dcc.fit = dccfit(dcc.garch.spec, eps)

Sigma.dcc  = aperm(rcov(dcc.fit), perm = c(3, 1, 2))
Sigma.dcc  = abind(array(NA, c(2, 3, 3)), Sigma.dcc, along = 1)  # add two matrices with NA to match dimensions
sigma.uc   = aperm(sigma, perm = c(3, 1, 2))  # change order
sigma.uc   = abind(array(NA, c(2, 3, 3)), sigma.uc, along = 1)
sqrt.sigma = sigma.uc

Sigma = array(NA, dim = c(dim(Sigma.dcc)[1], 3, 3))  # conditional variance variance 
for (i in 3:dim(Sigma)[1]) {
    eigen             = eigen(sigma.uc[i, , ])
    sqrt.sigma[i, , ] = eigen$vectors %*% (diag(eigen$values^(1/2))) %*% t(eigen$vectors)
    Sigma[i, , ]      = sqrt.sigma[i, , ] %*% Sigma.dcc[i, , ] %*% t(sqrt.sigma[i, , ])
}

sqrt.sigma = array(NA, dim = c(dim(Sigma)[1], 3, 3))
y.trans    = matrix(NA, 3 * dim(Sigma)[1], 1)
lag.trans  = matrix(NA, 3 * dim(Sigma)[1], 9)
diff.trans = matrix(NA, 3 * dim(Sigma)[1], 9)

for (i in 3:dim(Sigma)[1]) {
    eigen                             = eigen(Sigma[i, , ])
    sqrt.sigma[i, , ]                 = eigen$vectors %*% (diag(eigen$values^(-1/2))) %*% t(eigen$vectors)
    y.trans[(3 * i - 2):(3 * i), ]    = sqrt.sigma[i, , ] %*% t(diff.data[i, ])
    lag.trans[(3 * i - 2):(3 * i), ]  = kronecker(t(lag[i, ]), sqrt.sigma[i, , ])
    diff.trans[(3 * i - 2):(3 * i), ] = kronecker(t(t(diff.data[i - 1, ])), sqrt.sigma[i, , ])  # for p=1 
}

lm = lm(y.trans ~ lag.trans[, 1] + lag.trans[, 2] + lag.trans[, 3] 
                                 + lag.trans[, 4] + lag.trans[, 5]
                                 + lag.trans[, 6] + lag.trans[, 7]
                                 + lag.trans[, 8] + lag.trans[, 9]
                                 + diff.trans[, 1] + diff.trans[, 2]
                                 + diff.trans[, 3] + diff.trans[, 4]
                                 + diff.trans[, 5] + diff.trans[, 6]
                                 + diff.trans[, 7] + diff.trans[, 8]
                                 + diff.trans[, 9] + 0)

sparse.lm = lm(y.trans ~ lag.trans[, 2] + lag.trans[, 3] + lag.trans[, 6]
                                        + lag.trans[, 7] + lag.trans[, 9]
                                        + diff.trans[, 3] + diff.trans[, 5]
                                        + diff.trans[, 6] + 0)
summary(sparse.lm)
alpha.hat    = matrix(sparse.lm$coeff[1:2])
alpha.hat    = c(0, alpha.hat)
se.alpha.hat = matrix(sparse.lm$coeff[1:2])
gamma.hat    = matrix(c(0, 0, sparse.lm$coeff[6], 0, sparse.lm$coeff[7:8], 0, 0, 0), ncol = 3)

sqrt.sigma      = array(NA, dim = c(dim(Sigma)[1], 3, 3))
y.trans         = matrix(NA, dim(Sigma)[1], 3)
lag.trans       = array(NA, dim = c(dim(Sigma)[1], 3, 9))
lag.trans.alpha = array(NA, dim = c(dim(Sigma)[1], 3, 3))
diff.trans      = array(NA, dim = c(dim(Sigma)[1], 3, 9))
t.diff.trans    = array(NA, dim = c(dim(Sigma)[1], 9, 3))
comb.trans      = array(NA, dim = c(dim(Sigma)[1], 9, 3))

for (i in 3:dim(Sigma)[1]) {
    eigen                  = eigen(Sigma[i, , ])
    sqrt.sigma[i, , ]      = eigen$vectors %*% (diag(eigen$values^(-1/2))) %*% t(eigen$vectors)
    y.trans[i, ]           = sqrt.sigma[i, , ] %*% t(diff.data[i, ])
    lag.trans[i, , ]       = kronecker(t(lag[i, ]), sqrt.sigma[i, , ])
    lag.trans.alpha[i, , ] = kronecker(t(lag[i, ]), sqrt.sigma[i, , ] %*% alpha.hat)
    diff.trans[i, , ]      = kronecker(t(t(diff.data[i - 1, ])), sqrt.sigma[i, , ])  # for p=1 
    t.diff.trans[i, , ]    = kronecker(t((diff.data[i - 1, ])), sqrt.sigma[i, , ])  # for p=1 
    comb.trans[i, , ]      = kronecker(t((diff.data[i - 1, ])) %*% t(lag[i, ]), sqrt.sigma[i, , ] %*% alpha.hat)
}

sum = 0
for (i in 3:dim(Sigma)[1]) {
    temp = t((diff.data[i - 1, ])) %*% t(t(diff.data[i - 1, ]))
    sum  = sum + kronecker(temp, inv(Sigma[i, , ]))
}

G0    = matrix(0, 3, dim(Sigma)[1])
G1    = array(0, c(3, 3, dim(Sigma)[1]))
temp1 = 0
temp2 = 0
for (i in 3:dim(Sigma)[1]) {
    G0[, i]   = y.trans[i, ] - diff.trans[i, , ] %*% (inv(sum)) %*% t.diff.trans[i, , ] %*% t(diff.data[i, ])
    G1[, , i] = lag.trans.alpha[i, , ] - diff.trans[i, , ] %*% (inv(sum)) %*% comb.trans[i, , ]
    
    temp1 = temp1 + t(G1[, c(2, 3), i]) %*% G1[, c(2, 3), i]
    temp2 = temp2 + t(G1[, c(2, 3), i]) %*% (G0[, i] - G1[, 1, i])
}

beta.hat = c(1, inv(temp1) %*% temp2)

cov.beta.hat = inv(temp1)
sqrt(cov.beta.hat[1, 1])  # se beta1.hat
sqrt(cov.beta.hat[2, 2])  # se beta2.hat

# extract residuals
u.final     = t(t(diff.data) - alpha.hat %*% t(beta.hat) %*% t(lag) - gamma.hat %*% t(rbind(NA, diff.data[-467, ])))
sigma.final = array(NA, dim = c(T, 3, 3))

resid = u.final[-c(1, 2), ]

for (i in 1:T) {
    tao         = grid[i]
    denominator = array(0, dim = c(3, 3))
    nominator   = 0
    for (j in 1:T) {
        denominator = denominator + K((j/T - tao)/h) * u.final[j + 2, ] %*% t(u.final[j + 2, ])
        nominator   = nominator + K((j/T - tao)/h)
    }
    sigma.final[i, , ] = denominator/nominator
}

eps.final = matrix(0, T, 3)
for (i in 1:T) {
    eigen          = eigen(sigma.final[i, , ])
    eps.final[i, ] = eigen$vectors %*% (diag(eigen$values^(-1/2))) %*% t(eigen$vectors) %*% u.final[i + 2, ]
}

dcc.fit.final = dccfit(dcc.garch.spec, eps.final)
Sigma.final   = aperm(rcov(dcc.fit.final), perm = c(3, 1, 2))

Sigma.final.uc   = array(NA, dim = c(T, 3, 3))
sqrt.sigma.final = array(NA, dim = c(T, 3, 3))
# 
for (i in 1:dim(Sigma.final)[1]) {
    eigen                   = eigen(sigma.final[i, , ])
    sqrt.sigma.final[i, , ] = eigen$vectors %*% (diag(eigen$values^(1/2))) %*% t(eigen$vectors)
    Sigma.final.uc[i, , ]   = sqrt.sigma.final[i, , ] %*% Sigma.final[i, , ] %*% t(sqrt.sigma.final[i, , ])
}

res = matrix(0, dim(Sigma.final)[1], 3)
for (i in 1:dim(Sigma.final)[1]) {
    eigen    = eigen(Sigma.final[i, , ])
    res[i, ] = t(eigen$vectors %*% (diag(eigen$values^(-1/2))) %*% t(eigen$vectors) %*% eps.final[i, ])
}

res.MVM = res
# Conditional, unconditional and short run
# covariance matrix

Sigma_t = sigma.final
G_t     = Sigma.final
H_t     = Sigma.final.uc
