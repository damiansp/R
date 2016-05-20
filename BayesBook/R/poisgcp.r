poisgcp<-function(y, density = "normal", params=c(0,1), n.mu = 100
                  ,mu = NULL, mu.prior = NULL,
                  print.sum.stat=FALSE, alpha=0.05, ret=FALSE){
  n<-length(y)
  y.sum = sum(y)
  y.bar = mean(y)

  if(is.null(y))
    stop("Error: y has no data")

  if(sum(y<0)>0)
    stop("Error: data contains negative values")

  if(n.mu<100)
    stop("Error: there must be at least 100 points in the prior")

  if(density=="user"){
    if(is.null(mu) || is.null(mu.prior))
      stop("Error: a vector of possibilities (mu) and associated densities must be specified for a user prior")

    if(length(mu)!=length(mu.prior))
      stop("Error: There must be an equal number of values in mu and mu prior")

  }else if(density=="normal"){
    if(length(params)!=2)
      stop("Error: A mean and a std. deviation must be specified for a normal prior")
    mx = params[1]
    sx = params[2]

    if(sx<=0)
      stop("Error: the std. deviation of a normal prior must be greater than zero")

    lb = mx-3.5*sx
    ub = mx+3.5*sx

    if(lb<0)
      { cat("The normal prior has negative values.")
        cat("Whilst this is true for all normal distributions, you can sneak\n")
        cat("around it by using a large positive mean relative to the \n")
        cat("std. deviation.\n")
        stop("Error")
      }

    mu = seq(lb,ub,length=n.mu)
    mu.prior = dnorm(mu,mx,sx)
  }else if(density=="gamma"){
    if(length(params)!=2)
      stop("Error: there must be two parameters, a0 and b0 for a gamma prior")
    if(sum(params<0)>0)
      stop("Error: the parameters of a gamma prior must be positive")

    a0 = params[1]
    b0 = params[2]
    gamma.bds = qgamma(c(0.005,0.995),a0,b0)
    mu = seq(gamma.bds[1],gamma.bds[2],length=n.mu)
    mu.prior = dgamma(mu,a0,b0)
  }else{
    stop(paste("Error: unrecognized density: ",density,". The options are normal, gamma or user."))
  }

  if(sum(mu<0)>0)
    stop("Error: mu cannot contain negative values")

  cat("Summary statistics for data\n")
  cat("---------------------------\n")
  cat(paste("Number of observations:\t", n,"\n"))
  cat(paste("Sum of observations:\t", y.sum,"\n"))


  log.lik = y.sum*log(mu)-n*mu
  likelihood = exp(log.lik)
  fx.joint = approxfun(mu,mu.prior*likelihood)
  normalizing.constant = integrate(fx.joint,min(mu),max(mu))$value
  posterior = likelihood*mu.prior/normalizing.constant

  if(print.sum.stat){
    fx.posterior = approxfun(mu,posterior)
    x.fx = approxfun(mu,posterior*mu)
    posterior.mean = integrate(x.fx,min(mu),max(mu))$value
    xmusq.fx = approxfun(mu,(mu-posterior.mean)^2*posterior)
    posterior.var = integrate(xmusq.fx,min(mu),max(mu))$value
    cat("\nPosterior distribution summary statistics\n")
    cat("-----------------------------------------\n")
    cat(paste("Post. mean:\t", round(posterior.mean,3), "\n"))
    cat(paste("Post. var.:\t", round(posterior.var,4), "\n"))

    mu.int = seq(min(mu),max(mu),length=256)
    f.mu = fx.posterior(mu.int)
    suppressMessages({cdf = sintegral(mu.int,f.mu)$cdf;
                      fx.posterior.invcdf = approxfun(cdf$y,cdf$x)})
    lb = fx.posterior.invcdf(alpha/2)
    ub = fx.posterior.invcdf(1-alpha/2)
    cat(paste(round(100*(1-alpha)),"% cred. int.: ["
              , round(lb,3), ",", round(ub,3),"]\n\n"))
  }

  y.max = max(mu.prior,posterior)
  plot(mu,mu.prior,ylim=c(0,1.1*y.max),xlab=expression(mu)
       ,ylab="Density",
       ,main="Shape of continuous prior and posterior for Poisson mean"
       ,type="l",lty=2,col="red")
  lines(mu,posterior,lty=3,col="blue")
  legend(mu[1],y.max,lty=2:3,col=c("red","blue"),legend=c("Prior","Posterior"))

  if(ret){
      cat("This option is deprecated,\n")
      cat("The results of the function are always returned invisibly\\n")
  }
  invisible(list(mu=mu, mu.prior=mu.prior,likelihood=likelihood,posterior=posterior))
}



