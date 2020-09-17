dprior <- function(theta, alpha = 1, beta = 1){
  # alpha = 1 beta = 1 is uniform prior
  dbeta(x = theta, shape1 = alpha, shape2 = beta)
}

likelihood <- function(nsuccess, ntrials, theta){
  return(theta^nsuccess*(1-theta)^(ntrials-nsuccess))
}

dposterior <- function(theta, alpha = 1, beta = 1, nsuccess, ntrials){
  dbeta(x = theta, shape1 = alpha + nsuccess, shape2 = beta + (ntrials-nsuccess))
}

rposterior <- function(n, alpha = 1, beta = 1, nsuccess, ntrials){
  rbeta(n, shape1 = alpha + nsuccess, shape2 = beta + (ntrials-nsuccess))
}
