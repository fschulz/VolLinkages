` Recursive Estimates

diff = function(x) {
    diff = tail(x, -1) - head(x, -1)
    diff = rbind(rep(NA, ncol(x)), diff)
    return(diff)
}

# rolling window size=200
m           = 150
beta        = array(NA, dim = c(nrow(lp) - m, 3))
beta.cov    = array(NA, dim = c(nrow(lp) - m, 2, 2))
variance.uc = matrix(NA, 3, nrow(lp) - m)
for (k in 261:(nrow(lp) - m)) {
    
    data          = lp[1:(k + m), ]
    
    # Step 1
    
    diff.data     = diff(data)
    lag           = cbind(Lag(data[, 1]), Lag(data[, 2]), Lag(data[, 3]))
    colnames(lag) = c("L1", "L2", "L3")
    
    # select lag order
    var           = VAR(lp, type = "cons", lag.max = 20, ic = "HQ")
    vecm_ols      = VAR(diff.data[-1, ], p = 1, type = "none", exogen = lag[-1, ])  # constant is insignificant!
    u.est         = residuals(vecm_ols)  # 2 shorter than u
    
    ## EGARCH mvnorm
    spec          = ugarchspec(variance.model     = list(model      = "eGARCH",
                                                         garchOrder = c(1, 1)),
                               mean.model         = list(armaOrder    = c(0, 0),
                                                         include.mean = FALSE),
                               distribution.model = "norm")
    
    # Multivariate DCC-Garch
    
    dcc.garch.spec = dccspec(uspec        = multispec(replicate(3, spec)),
                             dccOrder     = c(1, 1),
                             distribution = "mvnorm")
    dcc.fit        = dccfit(dcc.garch.spec, u.est)
    
    Sigma          = aperm(rcov(dcc.fit), perm = c(3, 1, 2))
    Sigma          = abind(array(NA, c(2, 3, 3)), Sigma, along = 1)
    
    # STEP 2
    
    sqrt.sigma     = array(0, dim = c(dim(Sigma)[1], 3, 3))
    y.trans        = matrix(0, 3 * dim(Sigma)[1], 1)
    lag.trans      = matrix(0, 3 * dim(Sigma)[1], 9)
    diff.trans     = matrix(0, 3 * dim(Sigma)[1], 9)
    
    for (i in 3:dim(Sigma)[1]) {
        eigen                             = eigen(Sigma[i, , ])
        sqrt.sigma[i, , ]                 = eigen$vectors %*% (diag(eigen$values^(-1/2))) %*% t(eigen$vectors)
        y.trans[(3 * i - 2):(3 * i), ]    = sqrt.sigma[i, , ] %*% t(diff.data[i, ])
        lag.trans[(3 * i - 2):(3 * i), ]  = kronecker(t(lag[i, ]), sqrt.sigma[i, , ])
        diff.trans[(3 * i - 2):(3 * i), ] = kronecker(t(t(diff.data[i - 1, ])), sqrt.sigma[i, , ])  # for p=1 
    }

    lm = lm(y.trans[-c(1:3)] ~ lag.trans[-c(1:3), 1] + lag.trans[-c(1:3), 2] + lag.trans[-c(1:3), 3] 
                                                     + lag.trans[-c(1:3), 4] + lag.trans[-c(1:3), 5]
                                                     + lag.trans[-c(1:3), 6] + lag.trans[-c(1:3), 7]
                                                     + lag.trans[-c(1:3), 8] + lag.trans[-c(1:3), 9]
                                                     + diff.trans[-c(1:3), 1] + diff.trans[-c(1:3), 2]
                                                     + diff.trans[-c(1:3), 3] + diff.trans[-c(1:3), 4]
                                                     + diff.trans[-c(1:3), 5] + diff.trans[-c(1:3), 6]
                                                     + diff.trans[-c(1:3), 7] + diff.trans[-c(1:3), 8]
                                                     + diff.trans[-c(1:3), 9] + 0)
    
    sparse.lm = step(lm,
                     direction = "backward", 
                     trace     = 0)
    
    alpha.hat    = matrix(lm$coeff[2:3])
    alpha.hat    = c(0, alpha.hat)
    se.alpha.hat = coef(summary(lm))[1:3, 2]
    
    gamma.hat    = matrix(c(0, 0, sparse.lm$coeff[6], 0, sparse.lm$coeff[7:8], 0, 0, sparse.lm$coeff[9]),
                          ncol = 3)
    ### 
    
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
    
    beta.hat        = c(1, inv(temp1) %*% temp2)
    
    cov.beta.hat    = inv(temp1)
    
    beta[k, ]       = beta.hat
    beta.cov[k, , ] = cov.beta.hat
    u.final         = t(t(diff.data) - alpha.hat %*% t(beta.hat) %*% t(lag) - gamma.hat %*% t(rbind(NA, diff.data[-(m + k), ])))
    dcc.fit.final   = dccfit(dcc.garch.spec, u.final[-c(1, 
        2), ])
    
    # normal distribution
    coef.dcc  = coef(dcc.fit.final)
    omega.hat = matrix(coef.dcc[c(1, 5, 9)])
    theta.hat = matrix(coef.dcc[c(3, 7, 11)])
    delta.hat = matrix(coef.dcc[c(4, 8, 12)])
    
    for (i in 1:3) {
        variance.uc[i, k] = exp(omega.hat[i]/(1 - theta.hat[i]) * exp(0.5 * delta.hat[i]^2 * inv(1 - theta.hat[i]^2)))
    }
    variance.uc
    print(k)
}


# plot
par(mar = c(5.2, 6.7, 2, 2.5),
    mgp = c(4, 2, 0))

plot(beta[, 2],
     ylim     = c(-0.3, 0.9),
     type     = "l", 
     lwd      = 3,
     ylab     = expression(beta[2]),
     cex.lab  = 2, 
     cex.axis = 2,
     xlab     = "",
     xaxt     = "n")

lines(beta[, 2] + 2 * sqrt(beta.cov[, 1, 1]),
      lwd = 3, 
      lty = 3)

lines(beta[, 2] - 2 * sqrt(beta.cov[, 1, 1]),
      lwd = 3, 
      lty = 3)

abline(h = 0, lwd = 2, lty = 2)
axis(1,
     at       = c(1, 52, 104, 156, 208, 260, 312),
     labels   = seq(2006, 2012, by = 1),
     cex.axis = 2)

# plot
par(mar = c(5.2, 6.7, 2, 2.5),
    mgp = c(4, 2, 0))

plot(beta[, 3],
     ylim     = c(-3, -1.5),
     type     = "l", lwd = 3, 
     ylab     = expression(beta[3]),
     cex.lab  = 2,
     cex.axis = 2, 
     xlab     = "",
     xaxt     = "n")

lines(beta[, 3] + 2 * sqrt(beta.cov[, 2, 2]),
      lwd = 3, 
      lty = 3)

lines(beta[, 3] - 2 * sqrt(beta.cov[, 2, 2]),
      lwd = 3, 
      lty = 3)

abline(h = 0, lwd = 2, lty = 2)
axis(1,
     at = c(1, 52, 104, 156, 208, 260, 312),
     labels = seq(2006, 2012, by = 1),
     cex.axis = 2)
