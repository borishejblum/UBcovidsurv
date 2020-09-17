#' @importFrom HDInterval hdi
cred_int <- function(nsims = 100000, credibility_mass = 0.8, alpha = 1, beta = 1, nsuccess, ntrials){
  HDInterval::hdi(rposterior(n = nsims, alpha = alpha, beta = beta, nsuccess = nsuccess, ntrials = ntrials), 
        credMass = credibility_mass)
}
