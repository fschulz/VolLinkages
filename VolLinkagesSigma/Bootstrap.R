# ----------------------------------------------------------------
# Bootsprapping
# ----------------------------------------------------------------

sigma.est = function(data = resid, d, h = hopt) {
    sigma.e = array(, dim = c(3, 3, T))
    for (i in 1:T) {
        tao         = grid[i]
        denominator = array(0, dim = c(3, 3))
        nominator   = 0
        for (j in d) {
            denominator = denominator + K((j/T -  tao)/h) * data[j, ] %*% t(data[j, ])
            nominator   = nominator + K((j/T - tao)/h)
        }
        sigma.e[, , i] = denominator/nominator  # Estimated Covariance matrix
    }
    corr.e         = sigma.e
    corr.e[1, 2, ] = sigma.e[1, 2, ]/(sqrt(sigma.e[1, 1, ] * sigma.e[2, 2, ]))
    corr.e[1, 3, ] = sigma.e[1, 3, ]/(sqrt(sigma.e[1, 1, ] * sigma.e[3, 3, ]))
    corr.e[2, 3, ] = sigma.e[2, 3, ]/(sqrt(sigma.e[2, 2, ] * sigma.e[3, 3, ]))
    corr.e[2, 1, ] = corr.e[1, 2, ]
    corr.e[3, 1, ] = corr.e[1, 3, ]
    corr.e[3, 2, ] = corr.e[2, 3, ]
    return(corr.e)
}

bootstrap = boot(resid, sigma.est, R = 150)

quantiles       = apply(bootstrap$t, 2, quantile, probs = c(0.1, 0.9), na.rm = T)
quantMatrix0.05 = array(quantiles[1, ], dim = c(3, 3, T))
quantMatrix0.95 = array(quantiles[2, ], dim = c(3, 3, T))