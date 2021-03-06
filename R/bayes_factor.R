bayes_factor <- function(dposterior, ref_inc, est_incid){
  BF <- dposterior(est_incid)/dposterior(ref_inc)
  return(BF)
}

bayes_factor_interp <- function(BF, precision = 2){
  interp <- NULL
  
  if(BF < 1){
    interp <- paste0(round(BF, precision), " (inférieur à la référence)")
  }else if(BF < 10^0.5){
    interp <- paste0(round(BF, precision), " (différence anecdotique)")
  }else if(BF < 10){
    interp <- paste0(round(BF, precision), " (augmentation substancielle)")
  }else if(BF < 10^1.5){
    interp <- paste0(round(BF, precision), " (forte augmentation)")
  }else if(BF < 10^2){
    interp <- paste0(round(BF, precision), " (très forte augmentation)")
  }else if(BF > 10^2){
    interp <- "> 100 (augmentation décisive)"
  }
  
  return(interp)
}