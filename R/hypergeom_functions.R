dprior_betabinom <- function(theta, alpha = 1, beta = 1, N, log=FALSE){
  # alpha = 1 beta = 1 is uniform prior
  K <- floor(theta*N)
  res <- lchoose(N, K) + lbeta(alpha + K, beta + N - K) - lbeta(alpha, beta)
  if(!log){
    res <- exp(res)
  }
  return(res)
}

dposterior_hypergeom <- function(theta, alpha = 1, beta = 1, N, nsuccess, ntrials, log=FALSE){
  
  if(ntrials > N){stop("More tests than the total population")}
  
  K <- floor(theta*N)
  
  res <- lchoose(N - ntrials, K - nsuccess) + lbeta(alpha + K, beta + N - K) - 
    lbeta(alpha + nsuccess, beta + ntrials - nsuccess)
  
  res[is.nan(res)] <- -Inf
  
  if(!log){
    res <- exp(res)
  }
  
  return(res)
}

rposterior_hypergeom <- function(n, alpha = 1, beta = 1, N, nsuccess, ntrials){
  p_post <- rbeta(n = n, alpha + nsuccess, beta + ntrials - nsuccess)
  return(rbinom(n = n, size = rep(N-ntrials, n), prob = p_post)/(N-ntrials))
}

posterior_mean_hypergeom <- function(alpha = 1, beta =1, N, nsuccess, ntrials){
  (N - ntrials)*(alpha + nsuccess)/((alpha + nsuccess)*(beta + ntrials - nsuccess))/N
}

MAP_hypergeom <- function(alpha = 1, beta =1, N, nsuccess, ntrials){
  temp <- rposterior_hypergeom(100000, alpha = 1, beta = 1, N = N, nsuccess = nsuccess, ntrials = ntrials)
  return(temp[which.max(dposterior_hypergeom(temp, alpha = 1, beta = 1, N = N, 
                                             nsuccess = nsuccess, ntrials = ntrials, log=TRUE))])
}