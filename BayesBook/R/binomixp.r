binomixp = function(x,n,alpha0=c(1,1),alpha1=c(1,1),p=0.5,ret=FALSE){

  if(n<x)
    stop("Error: n must be greater than or equal to x")

  if(length(alpha0)!=2 || length(alpha1)!=2)
    stop("Error: the parameters for the beta priors, alpha0 and alpha1, must have two elements each")

  if(sum(alpha0<0)>0 || sum(alpha1<0)>0)
    stop("Error: the parameters for the beta priors, alpha0 and alpha1, must be greater than zero")

  if(p<=0 || p>=1)
    stop("Error: the mixing proportion p must be in the interval (0,1) exclusive")


  i = 1:x
  log0 = sum(log(alpha0[1]+i-1)-log(alpha0[1]+alpha0[2]+i-1)+log(n-i+1)-log(i))
  log1 = sum(log(alpha1[1]+i-1)-log(alpha1[1]+alpha1[2]+i-1)+log(n-i+1)-log(i))
  i = (x+1):n
  log0 = log0+sum(log(alpha0[2]+i-x-1)-log(alpha0[1]+alpha0[2]+i-1))
  log1 = log1+sum(log(alpha1[2]+i-x-1)-log(alpha1[1]+alpha1[2]+i-1))

  f0 = exp(log0)
  f1 = exp(log1)

  cat("Prior probability of the data under component 0\n")
  cat("----------------------------\n")
  cat(paste("Log prob.:\t",signif(log0,3),"\nProbability:\t ",signif(f0,5),"\n\n"))

  cat("Prior probability of the data under component 1\n")
  cat("----------------------------\n")
  cat(paste("Log prob.:\t",signif(log1,3),"\nProbability:\t ",signif(f1,5),"\n\n"))


  q0 = p
  q1 = 1-q0
  qp0 = q0*f0/(q0*f0+q1*f1)
  qp1 = 1-qp0

  cat(paste("Post. mixing proportion for component 0:\t",signif(qp0,3),"\n"))
  cat(paste("Post. mixing proportion for component 1:\t",signif(qp1,3),"\n"))

  pi = seq(0.001,0.999,by=0.001)

  prior.0 = dbeta(pi,alpha0[1],alpha0[2])
  prior.1 = dbeta(pi,alpha1[1],alpha1[2])
  prior = q0*prior.0+q1*prior.1

  alpha0.post = alpha0+c(x,n-x)
  alpha1.post = alpha1+c(x,n-x)

  posterior.0 = dbeta(pi,alpha0.post[1],alpha0.post[2])
  posterior.1 = dbeta(pi,alpha1.post[1],alpha1.post[2])
  posterior = qp0*posterior.0+qp1*posterior.1

  loglik = x*log(pi)+(n-x)*log(1-pi)
  loglik = loglik-max(loglik)
  likelihood = exp(loglik)

  normalizing.factor = sum(likelihood)/length(likelihood)
  likelihood = likelihood/normalizing.factor

  o.par = par(mfrow=c(2,2))

  ##plot the priors and the mixture prior

  y.max = max(prior.0,prior.1,prior)

  plot(pi,prior.0,ylim=c(0,y.max*1.1),
       xlab=expression(pi),ylab="Density"
       ,main="Mixture prior and it's components"
       ,type="l",lty=2,col="black")
  lines(pi,prior.1,lty=3,col="red")
  lines(pi,prior,lty=1,col="green")
  legend(0,y.max,legend=c(expression(prior[0]),expression(prior[1])
                   ,expression(prior[mix])),lty=c(2,3,1),col=c("black","red","green"))

  ##plot the posteriors and the mixture posterior

  y.max = max(posterior.0,posterior.1,posterior)

  plot(pi,posterior.0,ylim=c(0,y.max*1.1),
       xlab=expression(pi),ylab="Density"
       ,main="Mixture posterior and it's components"
       ,type="l",lty=2,col="black")
  lines(pi,posterior.1,lty=3,col="red")
  lines(pi,posterior,lty=1,col="green")
  legend(0,y.max,legend=c(expression(posterior[0]),expression(posterior[1])
                   ,expression(posterior[mix])),lty=c(2,3,1),col=c("black","red","green"))

  ##plot the mixture posterior likelihood and mixture posterior

  y.max = max(prior,posterior,likelihood)

  plot(pi,prior,ylim=c(0,y.max*1.1),
       xlab=expression(pi),ylab="Density"
       ,main="Mixture prior, likelihood and mixture posterior"
       ,type="l",lty=2,col="black")
  lines(pi,likelihood,lty=3,col="red")
  lines(pi,posterior,lty=1,col="green")
  legend(0,y.max,legend=c(expression(prior[mix]),expression(likelihood)
                   ,expression(posterior[mix])),lty=c(2,3,1),col=c("black","red","green"))

  par(o.par)

  if(ret){
      cat("The argument ret is deprecated.\n")
      cat("The results are now always returned invisibly\n")
  }
  invisible(list(pi=pi,prior=prior,likelihood=likelihood,posterior=posterior))
}





