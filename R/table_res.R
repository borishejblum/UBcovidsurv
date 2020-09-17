table_res <- function(alpha = 1, beta = 1, 
                      nsuccess, ntrials,
                      credibility_mass = 0.8,
                      nsims = 100000){
  
  shape1 <- alpha + nsuccess
  shape2 <- beta + (ntrials-nsuccess)
  
  posterior_mean <- shape1/(shape1 + shape2)
  
  MAP <- (shape1 - 1)/(shape1 + shape2 - 2)
  
  posterior_median <- median(rposterior(n = nsims, alpha = alpha, beta = beta, nsuccess = nsuccess, ntrials = ntrials))
  
  CI <- cred_int(nsims = nsims, credibility_mass = credibility_mass, 
                 alpha = alpha, beta = beta, nsuccess = nsuccess, ntrials = ntrials)
  
  precision <- 2
  res <- rbind.data.frame(
    cbind.data.frame(Indicateur = paste0("Intervalle de crédibilité à ", credibility_mass*100, "%"), 
                     "Valeur pour 100 000"= paste0("[", round(CI["lower"]*100000, precision),
                                    " ; ", round(CI["upper"]*100000, precision),
                                    "]")),
    cbind.data.frame(Indicateur = "Maximum a posteriori", "Valeur pour 100 000" = as.character(round(MAP*100000, precision))),
    cbind.data.frame(Indicateur = "Médiane a posteriori", "Valeur pour 100 000" = as.character(round(posterior_median*100000, precision))),
    cbind.data.frame(Indicateur = "Moyenne a posteriori", "Valeur pour 100 000" = as.character(round(posterior_mean*100000, precision)))
  )
  
  return(res)
}