source("VolLinkagesFGLS.R")
# Equation 17
print("beta")
print(beta.hat)
print("S.E. beta")
print(c(sqrt(cov.beta.hat[1, 1]), sqrt(cov.beta.hat[2, 2])))

# Equation 18
print("alpha")
print(alpha.hat)
print("gamma")
print(gamma.hat)
