normgcp = function(x, sigma.x = NULL, density="uniform" , params = NULL, n.mu = 50, mu = NULL, mu.prior = NULL, ret = FALSE){

  ## x - the vector of observations
  ## sigma.x - the population standard deviation
  ## density - distributional form of the prior density
  ## can be one of : normal, unform, or user
  ## by default a continuous uniform prior is used
  ## mu - vector of possible values of the population mean
  ## mu.prior - the associated prior probability mass
  ## ret - if true then the likelihood and posterior are returned as a
  ## list

  mean.x = mean(x)

  if(n.mu<3)
    stop("Number of prior values of mu must be greater than 2")

  if(is.null(sigma.x)){
    sigma.x = sd(x-mean.x)
    cat(paste("Standard deviation of the residuals :",signif(sigma.x,4),"\n",sep=""))
  }else{
    cat(paste("Known standard deviation :",signif(sigma.x,4),"\n",sep=""))
  }

  if(density=="normal" || density=="norm" || density=="n"){
    if(is.null(params)|length(params)<1)
      stop("You must supply a mean for a normal prior")
    mx = params[1]

    if(length(params)==2)  ## user has supplied sd as well
      s.x = params[2]
    else
      s.x = sigma.x

    mu = seq(mx-3.5*s.x,mx+3.5*s.x,length = n.mu)
    mu.prior = dnorm(mu,mx,s.x)
  } else if(density=="uniform" || density=="unif"){
    if(is.null(params)){
      ## set params to mean+/-3.5sd by default
      params = c(mean.x-3.5*sigma.x,mean.x+3.5*sigma.x)
    }
    if(length(params)<2)
      stop("You must supply a minimum and a maximum to use a uniform prior")
    minx = params[1]
    maxx = params[2]
    if(maxx<=minx)
      stop("The maximum must be greater than the minimum for a uniform prior")
    mu = seq(minx,maxx,length = n.mu)
    mu.prior = dunif(mu,minx,maxx)
  }else{
    ## user specified prior
    if(is.null(mu)|is.null(mu.prior))
      stop("If you wish to use a non-uniform continuous prior then you must supply a mean vector, mu, and an associated density vector, mu.prior")
  }

  if(any(mu.prior<0) | any(mu.prior>1))
    stop("Prior probabilities must be between 0 and 1 inclusive")

  crude.int = sum(diff(mu)*mu.prior[-1])
  if(round(crude.int,3)!=1){
    warning("The prior probabilities did not sum to 1, therefore the prior has been normalized")
    mu.prior = mu.prior/crude.int
    print(crude.int)
  }

  n.mu = length(mu)
  mx = mean(x)
  nx = length(x)
  snx = sigma.x^2/nx
  likelihood = exp(-0.5*(mx-mu)^2/snx)

  ## Numerically integrate the denominator
  ## First calculate the height of the function to be integrated

  f.x.mu = likelihood*mu.prior

  ## Now get a linear approximation so that we don't have to worry about
  ## the number of points specified by the user

  ap = approx(mu,f.x.mu,n=513)
  integral = sum(ap$y[2*(1:256)-1]+4*ap$y[2*(1:256)]+ap$y[2*(1:256)+1])
  integral = (ap$x[2]-ap$x[1])*integral/3

  posterior = likelihood*mu.prior/integral

  plot(mu,posterior,ylim=c(0,1.1*max(posterior,mu.prior)),type="l",
       lty=1,col="blue",
       xlab=expression(mu),ylab=expression(Probabilty(mu)))
  lines(mu,mu.prior,lty=2,col="red")

  left = min(mu)+diff(range(mu))*0.05
  legend(left,max(posterior,mu.prior),lty=1:2,col=c("blue","red"),legend=c("Posterior","Prior"))

  if(ret){
      cat("The argument ret is deprecated.\n")
      cat("The results are now always returned invisibly\n")
  }
  invisible(list(likelihood=likelihood,posterior=posterior,mu=mu,mu.prior=mu.prior))

}
