poisson <- list(distribution = "poisson", lambda = 2.0, obs = 20)
normal <- list(distribution = "normal", mu = 1.2, sigma2 = 1.0, obs = 25)
bernoulli <- list(distribution = "bernoulli", p = 0.3, obs = 30)

f_distribution <- function(lista){
  if(lista$distribution == "poisson"){ 
    return(rpois(lista$obs, lista$lambda)) 
  } 
  else if(lista$distribution == "normal"){ 
    return(rnorm(lista$obs, lista$mu, sqrt(lista$sigma2)))
  } 
  else if(list$distribution == "bernoulli"){
    return(rbinom(lista$obs, 1, lista$p))
  } 
  else{
    stop()
  }
}


