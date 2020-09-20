#' @importFrom HDInterval hdi
cred_int <- function(nsims = 100000, credibility_mass = 0.8, rposterior){
  HDInterval::hdi(rposterior(n = nsims), credMass = credibility_mass)
}