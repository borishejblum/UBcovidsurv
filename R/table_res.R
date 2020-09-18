table_res <- function(alpha = 1, beta = 1, 
                      nsuccess, ntrials,
                      credibility_mass = 0.8,
                      nsims = 100000, 
                      ref_incid){
  
  shape1 <- alpha + nsuccess
  shape2 <- beta + (ntrials-nsuccess)
  
  posterior_mean <- shape1/(shape1 + shape2)
  
  MAP <- (shape1 - 1)/(shape1 + shape2 - 2)
  
  posterior_median <- median(rposterior(n = nsims, alpha = alpha, beta = beta, nsuccess = nsuccess, ntrials = ntrials))

  mypost <- function(x){
    dposterior(theta = x, alpha = alpha, beta = beta, nsuccess = nsuccess, ntrials = ntrials)
  }
  BF_mean <- bayes_factor(mypost, ref_inc = ref_incid, est_incid = posterior_mean)
  BF_MAP <- bayes_factor(mypost, ref_inc = ref_incid, est_incid = MAP)
  BF_median <- bayes_factor(mypost, ref_inc = ref_incid, est_incid = posterior_median)
  
  CI <- cred_int(nsims = nsims, credibility_mass = credibility_mass, 
                 alpha = alpha, beta = beta, nsuccess = nsuccess, ntrials = ntrials)
  
  precision <- 2
  res <- rbind.data.frame(
    cbind.data.frame("Indicateur" = paste0("Intervalle de crédibilité à ", credibility_mass*100, "%"), 
                     "Valeur pour 100 000"= paste0("[", round(CI["lower"]*100000, precision),
                                    " ; ", round(CI["upper"]*100000, precision),
                                    "]"),
                     "Facteur de Bayes" = ""),
    cbind.data.frame("Indicateur" = "Maximum a posteriori", 
                     "Valeur pour 100 000" = as.character(round(MAP*100000, precision)),
                     "Facteur de Bayes" = bayes_factor_interp(BF_MAP)),
    cbind.data.frame("Indicateur" = "Médiane a posteriori", 
                     "Valeur pour 100 000" = as.character(round(posterior_median*100000, precision)),
                     "Facteur de Bayes" = bayes_factor_interp(BF_median)),
    cbind.data.frame("Indicateur" = "Moyenne a posteriori", 
                     "Valeur pour 100 000" = as.character(round(posterior_mean*100000, precision)),
                     "Facteur de Bayes" = bayes_factor_interp(BF_mean))
  )
  
  return(res)
}