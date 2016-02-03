
# Grid with time points
T    = nrow(resid)
grid = (1:T)/T

## Leave-one-out estimator

loo.estimator = function(i, h) {
    # observation i, time point i
    loo.sigma   = array(0, dim = c(3, 3))
    tao         = grid[i]
    denominator = array(0, dim = c(3, 3))
    nominator   = 0
    for (j in (1:T)[-i]) {
        denominator = denominator + K((j/T - tao)/h) * resid[j, ] %*% t(resid[j, ])
        nominator   = nominator + K((j/T - tao)/h)
    }
    loo.sigma = denominator/nominator  # Estimated Covariance matrix
    return(loo.sigma)
}

## Likelihood Cross Validation Criterion

CV = function(h) {
    cv.l = 0
    for (l in 1:T) {
        cv.l = cv.l + t(resid[l, ]) %*% inv(loo.estimator(l, h)) %*% resid[l, ] + log(det(loo.estimator(l, h)))
    }
    cv = 1/T * cv.l
    return(cv)
}
