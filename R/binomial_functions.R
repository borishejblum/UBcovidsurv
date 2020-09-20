dprior_beta <- function(theta, alpha = 1, beta = 1, log=FALSE){
  # alpha = 1 beta = 1 is uniform prior
  dbeta(x = theta, shape1 = alpha, shape2 = beta, log = log)
}
# 
# #binomial likelihood
# likelihood_binomial <- function(nsuccess, ntrials, theta){
#   return(theta^nsuccess*(1-theta)^(ntrials-nsuccess))
# }

dposterior_betabinomial <- function(theta, alpha = 1, beta = 1, nsuccess, ntrials, log=FALSE){
  dbeta(x = theta, shape1 = alpha + nsuccess, shape2 = beta + (ntrials-nsuccess), log = log)
}

rposterior_betabinomial <- function(n, alpha = 1, beta = 1, nsuccess, ntrials){
  rbeta(n, shape1 = alpha + nsuccess, shape2 = beta + (ntrials-nsuccess))
}


posterior_mean_betabinomial <- function(alpha = 1, beta =1, nsuccess, ntrials){
  shape1 <- alpha + nsuccess
  shape2 <- beta + (ntrials - nsuccess)
  return(shape1/(shape1 + shape2))
}

MAP_betabinomial <- function(alpha = 1, beta =1, nsuccess, ntrials){
  shape1 <- alpha + nsuccess
  shape2 <- beta + (ntrials - nsuccess)
  return((shape1 - 1)/(shape1 + shape2 - 2))
}