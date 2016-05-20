normnp = function(x, m.x = 0 , s.x = 1, sigma.x = NULL, n.mu = 100, ret = FALSE){

  ## x - the vector of observations
  ## m.x - the mean of the normal prior
  ## s.x - the standard deviation of the normal prior
  ## sigma.x - the population standard deviation
  ## ret - if true then the prior, likelihood, posterior, mean, variance, and
  ## quantiles are returned as a list

  mean.x = mean(x)
  n.x = length(x)

  if(is.null(sigma.x)){
    sigma.x = sd(x-mean.x)
    cat(paste("Standard deviation of the residuals :",signif(sigma.x,4),"\n",sep=""))
  }else{
    if(sigma.x>0){
      cat(paste("Known standard deviation :",signif(sigma.x,4),"\n",sep=""))
    }else{
      stop("Standard deviation sigma.x must be greate than zero")
    }
  }

  if(n.mu<100)
    {
      warning("Number of prior values of mu must be greater than 100")
      n.mu = 100
    }

  if(s.x<=0){
    lb = mean.x-3.5*sigma.x/sqrt(n.x)
    ub = mean.x+3.5*sigma.x/sqrt(n.x)
    prior.precision = 0
    m.x = 0
    mu = seq(lb,ub,length=n.mu)
    mu.prior = rep(1/(ub-lb),n.mu)
  }else{
    lb = m.x-3.5*s.x
    ub = m.x+3.5*s.x
    mu = seq(lb,ub,length=n.mu)
    mu.prior = dnorm(mu,m.x,s.x)
    prior.precision = 1/s.x^2
  }


  likelihood = exp(-n.x/(2*sigma.x^2)*(mean.x-mu)^2)

  post.precision = prior.precision+(n.x/sigma.x^2)
  post.sd = sqrt(1/post.precision)
  post.mean = (prior.precision/post.precision*m.x)+((n.x/sigma.x^2)/post.precision*mean.x)

  cat(paste("Posterior mean           : ",round(post.mean,7),"\n",sep=""))
  cat(paste("Posterior std. deviation : ",round(post.sd,7),"\n",sep=""))

  posterior = dnorm(mu,post.mean,post.sd)

  plot(mu,posterior,ylim=c(0,1.1*max(posterior,mu.prior)),type="l",
       lty=1,col="blue",
       xlab=expression(mu),ylab=expression(Probabilty(mu)),
       main="Shape of prior and posterior")
  lines(mu,mu.prior,lty=2,col="red")

  left = min(mu)+diff(range(mu))*0.05
  legend(left,max(posterior,mu.prior),lty=1:2,col=c("blue","red"),legend=c("Posterior","Prior"))

  probs = c(0.005,0.01,0.025,0.05,0.5,0.95,0.975,0.99,0.995)
  qtls = qnorm(probs,post.mean,post.sd)
  names(qtls) = probs

  cat("\nProb.\tQuantile \n")
  cat("------\t---------\n")
  for(i in 1:length(probs))
    cat(paste(round(probs[i],3),"\t",round(qtls[i],7),"\n",sep=""))

  if(ret){
      cat("The argument ret is deprecated.\n")
      cat("The results are now always returned invisibly\n")
  }
  invisible(list(mu=mu,prior=mu.prior,likelihood=likelihood,posterior=posterior,mean=post.mean,sd=post.sd,quantiles=qtls))

}
