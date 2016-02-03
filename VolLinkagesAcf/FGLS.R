# Johansen test: 1 cointegration relation VAR lag
# analysis: VECM oder=1

data = lp

diff = function(x) {
    diff = tail(x, -1) - head(x, -1)
    diff = rbind(rep(NA, ncol(x)), diff)
    return(diff)
}

diff.data     = diff(data)
lag           = cbind(Lag(data[, 1]), Lag(data[, 2]), Lag(data[, 3]))
colnames(lag) = c("L1", "L2", "L3")

# select lag order
var      = VAR(lp, type = "cons", lag.max = 20, ic = "HQ")
summary(var)  # lag order 2 --> VECM with lag order 1
vecm_ols = VAR(diff.data[-1, ],
               p      = 1,
               type   = "none", 
               exogen = lag[-1, ])  # constant is insignificant!

u.est    = residuals(vecm_ols)
arch.test(vecm_ols, multivariate.only = FALSE)  # null of no ARCH in multivariate test rejected

specList = c(ugarchspec(variance.model = list(model              = "eGARCH", 
                                              garchOrder         = c(1, 1)),
                                              mean.model         = list(armaOrder = c(0, 0), include.mean = FALSE),
                                              distribution.model = "norm"), 
             ugarchspec(variance.model = list(model              = "eGARCH",
                                              garchOrder         = c(1, 1)),
                                              mean.model         = list(armaOrder = c(0, 0), include.mean = FALSE),
                                              distribution.model = "ged"))
for (i in 1:length(specList)) {
    spec           = specList[i]
   
    # Multivariate DCC-Garch
    
    dcc.garch.spec = dccspec(uspec        = multispec(replicate(3, spec)),
                             dccOrder     = c(1, 1),
                             distribution = "mvnorm")

    dcc.fit        = dccfit(dcc.garch.spec, u.est)
    infocriteria(dcc.fit)
    likelihood(dcc.fit)
    
    Sigma          = aperm(rcov(dcc.fit),
                           perm = c(3, 1, 2))
    Sigma          = abind(array(NA, c(2, 3, 3)),
                           Sigma,
                           along = 1)
    
    # STEP 2
    
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
    
    lm         = lm(y.trans[-c(1:2)] ~ lag.trans[-c(1:2), -1] + diff.trans[-c(1:2), -c(1, 4, 7)] + 0)  # alpha1 restricted to 0, since insignificant, and crude oil exogeneity imposed
    lm         = lm(y.trans ~ lag.trans[, 1] + lag.trans[, 2] + lag.trans[, 3] + lag.trans[, 4] + lag.trans[, 5] + lag.trans[, 6] + lag.trans[, 7] + lag.trans[, 8] + lag.trans[, 9] + diff.trans[, 1] + diff.trans[, 2] + diff.trans[, 3] + diff.trans[, 4] + diff.trans[, 5] + diff.trans[, 6] + diff.trans[, 7] + diff.trans[, 8] + diff.trans[, 9] + 0)  #+diff.trans[,7]
    
    sparse.lm  = step(lm, direction = "both", trace = 0)
    summary(sparse.lm)
    reduced.lm = lm(y.trans ~ lag.trans[, 2] + lag.trans[, 3] + lag.trans[, 6] + lag.trans[, 8] + lag.trans[, 9] + diff.trans[, 3] + diff.trans[, 5] + diff.trans[, 6] + 0)  #+diff.trans[,7]
    
    alpha.hat  = matrix(reduced.lm$coeff[1:2])
    alpha.hat  = c(0, alpha.hat)
    gamma.hat  = matrix(c(0, 0, reduced.lm$coeff[6], 0, reduced.lm$coeff[7:8], 0, 0, 0), ncol = 3)  #sparse.lm$coeff[9]
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
        
        temp1     = temp1 + t(G1[, c(2, 3), i]) %*% G1[, c(2, 3), i]
        temp2     = temp2 + t(G1[, c(2, 3), i]) %*% (G0[, i] - G1[, 1, i])
    }
    
    beta.hat      = c(1, inv(temp1) %*% temp2)
    
    cov.beta.hat  = inv(temp1)
    sqrt(cov.beta.hat[1, 1])  # se beta1.hat
    sqrt(cov.beta.hat[2, 2])  # se beta2.hat
    
    # extract residuals
    u.final       = t(t(diff.data) - alpha.hat %*% t(beta.hat) %*% t(lag) - gamma.hat %*% t(rbind(NA, diff.data[-nrow(diff.data), ])))
    dcc.fit.final = dccfit(dcc.garch.spec, u.final[-c(1, 2), ])
    Sigma.final   = aperm(rcov(dcc.fit.final), perm = c(3, 1, 2))
    
    res           = matrix(0, dim(Sigma.final)[1], 3)
    
    for (i in 1:dim(Sigma.final)[1]) {
        eigen    = eigen(Sigma.final[i, , ])
        res[i, ] = t(eigen$vectors %*% (diag(eigen$values^(-1/2))) %*% t(eigen$vectors) %*% u.final[i + 2, ])
    }
    print(dcc.fit.final)
}


resid = u.final[-c(1,2),]

# Conditional Covariance
H_t_DCC = Sigma.final
