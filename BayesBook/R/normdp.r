normdp = function(x, sigma.x = NULL, mu = NULL, mu.prior = NULL, n.mu = 50, ret = FALSE){

  ## x - the vector of observations
  ## sigma.x - the population standard deviation
  ## mu - vector of possible values of the population mean
  ## mu.prior - the associated prior probability mass
  ## n.mu - if mu is NULL then a uniform prior with n.mu points is used
  ## ret - if true then the likelihood and posterior are returned as a
  ## list

  if(n.mu<3)
    stop("Number of prior values of theta must be greater than 2")

  if(is.null(mu)){
    mu = seq(min(x)-sigma.x,max(x)+sigma.x,length = n.mu)
    mu.prior = rep(1/n.mu,n.mu)
  }

  mx = mean(x)

  if(is.null(sigma.x)){
    sigma.x = sd(x-mx)
    cat(paste("Standard deviation of the residuals :"
              ,signif(sigma.x,4),"\n",sep=""))
  }else{
    if(sigma.x>0){
      cat(paste("Known standard deviation :",signif(sigma.x,4),"\n",sep=""))
    }else{
      stop("The standard deviation must be greater than zero")
    }
  }

  if(any(mu.prior<0) | any(mu.prior>1))
    stop("Prior probabilities must be between 0 and 1 inclusive")

  if(round(sum(mu.prior),7)!=1){
    warning("The prior probabilities did not sum to 1, therefore the prior has been normalized")
    mu.prior = mu.prior/sum(mu.prior)
  }

  n.mu = length(mu)
  nx = length(x)
  snx = sigma.x^2/nx
  likelihood = exp(-0.5*(mx-mu)^2/snx)
  posterior = likelihood*mu.prior/sum(likelihood*mu.prior)

  plot(mu,posterior,ylim=c(0,1.1*max(posterior,mu.prior)),pch=20, col="red",
       xlab=expression(mu),ylab=expression(Probabilty(mu)))
  points(mu,mu.prior,pch=20,col="blue")

  left = min(mu)+diff(range(mu))*0.05
  legend(left,max(posterior,mu.prior),pch=20,col=c("red","blue"),legend=c("Posterior","Prior"))

  if(ret){
      cat("The argument ret is deprecated.\n")
      cat("The results are now always returned invisibly\n")
  }

  invisible(list(mu=mu,mu.prior=mu.prior,likelihood=likelihood,posterior=posterior))

}
