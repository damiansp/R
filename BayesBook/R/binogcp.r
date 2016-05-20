binogcp = function(x,n,density="uniform", params = c(0,1), n.pi = 1000, pi = NULL, pi.prior = NULL, ret = FALSE){

  ## n - the number of trials in the binomial
  ## x - the number of observed successes
  ## density - may be one of "exp","normal","uniform" or "user"
  ## params - if the density is not "user" then a vector of parameters
  ## must be supplied.
  ##	exp:		rate
  ##	normal: 	mean,sd
  ##	uniform: 	min,max
  ## n.pi - the number of points to divide the [0,1] interval into

  ## pi and pi.prior are only specified if density == "user"
  ## pi - the probability of success
  ## pi.prior - the associated prior probability mass
  ## ret - if true then the likelihood and posterior are returned as a
  ## list

  if(x>n)
    stop("The number of observed successes (x) must be smaller than the number of trials")
  if(n.pi<100)
    stop("Number of prior values of pi must be greater than 100")

  if(is.null(pi)||is.null(pi.prior))
    pi = seq(0+1/n.pi,1-1/n.pi,length=n.pi)
  else{
    if(length(pi)!=length(pi.prior))
      stop("pi and pi.prior must have same length")

    if(sum(pi<0|pi>1)>0)                ## check that probabilities lie on [0,1]
      stop("Values of pi must be between 0 and 1 inclusive")
  }

  if(density=="beta"){
    if(length(params)<2){
      warning("Beta prior requires two shape parameters. Default value Beta(1,1) = Uniform is being used")
      a = 1
      b = 1
    }else{
      if(params[1]<=0|params[2]<0)
        stop("Beta prior shape parameters must be greater than zero")
      a = params[1]
      b = params[2]
    }
    pi.prior = dbeta(pi,a,b)
  }else	if(density=="exp"){
    if(params[1]<=0){
      stop("Parameter for exponential density must be greater than zero")
    }else{
      rate = params[1]
      pi.prior = dexp(pi,rate)
    }
  }else if(density=="normal"){
    if(length(params)<2)
      stop("Normal prior requires a mean and std. deviation")
    else{
      mx = params[1]
      sx = params[2]
      if(sx<=0)
        stop("Std. deviation for normal prior must be greater than zero")
      pi.prior = dnorm(pi,mx,sx)
    }
  }else if(density=="uniform"){
    if(length(params)<2)
      stop("Uniform prior requires a minimum and a maximum")
    else{
      minx = params[1]
      maxx = params[2]

      if(maxx<=minx)
        stop("Maximum must be greater than minimum for a uniform prior")
      pi.prior = dunif(pi,minx,maxx)
    }
  }else if (density!="user"){
    stop(paste("Unrecognized density :",density))
  }

  likelihood = (pi^x)*((1-pi)^(n-x))

  ## Numerically integrate the denominator
  ## First calculate the height of the function to be integrated

  f.x.pi = likelihood*pi.prior

  ## Now get a linear approximation so that we don't have to worry about
  ## the number of points specified by the user

  ap = approx(pi,f.x.pi,n=513)
  integral = sum(ap$y[2*(1:256)-1]+4*ap$y[2*(1:256)]+ap$y[2*(1:256)+1])
  integral = (ap$x[2]-ap$x[1])*integral/3

  posterior = likelihood*pi.prior/integral

  plot(pi,posterior,ylim=c(0,1.1*max(posterior,pi.prior)),lty=1,type="l",col="blue",
       xlab=expression(pi),ylab="Density")
  lines(pi,pi.prior,lty=2,col="red")

  left = min(pi)+diff(range(pi))*0.05
  legend(left,max(posterior,pi.prior),lty=1:2,col=c("blue","red"),legend=c("Posterior","Prior"))
  if(ret){
      cat("The argument ret is deprecated.\n")
      cat("The results are now always returned invisibly\n")
  }
  invisible(list(likelihood=likelihood,posterior=posterior,pi=pi,pi.prior=pi.prior))

}
