normmixp = function(x,sigma.x,prior0,prior1,p=0.5,n.mu=100,ret=FALSE){

  if(length(x)==0)
    stop("Error: x must contain at least one observation")

  if(sigma.x<=0)
    stop("Error: sigma.x must be greater than zero")

  if(length(prior0)!=2 || length(prior1)!=2)
    stop("Error: there must be 2 parameters for each prior, 2 means and 2 standard deviations")

  prior.means = c(prior0[1],prior1[1])
  prior.sds = c(prior0[2],prior1[2])

  if(sum(prior.sds<=0)>0)
    stop("Error: the prior standard deviations should be greater than zero")

  if(p<=0 || p>=1)
    stop("Error: p should be between 0 and 1 exclusive")

  n = length(x)
  x.bar = mean(x)
  q0 = p
  q1 = 1-p

  post.prec0 = (1/prior.sds[1]^2) + (n/sigma.x^2)
  post.var0 = 1/post.prec0
  post.sd0 = sqrt(post.var0)
  post.mean0 = (prior.means[1]/(prior.sds[1]^2*post.prec0))+(n*x.bar/(sigma.x^2*post.prec0))


  post.prec1 = (1/prior.sds[2]^2) + (n/sigma.x^2)
  post.var1 = 1/post.prec1
  post.sd1 = sqrt(post.var1)
  post.mean1 = (prior.means[2]/(prior.sds[2]^2*post.prec1))+(n*x.bar/(sigma.x^2*post.prec1))

  cat("Posterior summary statistics of component 0\n")
  cat("-------------------------------------------\n")
  cat(paste("Mean:\t\t",signif(post.mean0,3),"\n"))
  cat(paste("Std. Dev.:\t",signif(post.sd0,4),"\n"))
  cat(paste("Variance:\t",signif(post.var0,4),"\n"))
  cat(paste("Precision:\t",signif(post.prec0,4),"\n\n"))

  cat("Posterior summary statistics of component 1\n")
  cat("-------------------------------------------\n")
  cat(paste("Mean:\t\t",signif(post.mean1,3),"\n"))
  cat(paste("Std. Dev.:\t",signif(post.sd1,4),"\n"))
  cat(paste("Variance:\t",signif(post.var1,4),"\n"))
  cat(paste("Precision:\t",signif(post.prec1,4),"\n\n"))


  sd.x = sqrt(sigma.x^2/n + prior.sds[1]^2)
  f0 = dnorm(x.bar,prior.means[1],sd.x)

  cat("Predictive density of the sample mean under component 0\n")
  cat("------------------------------------------------------\n")
  cat(paste("Sample mean:\t",signif(x.bar,3),"\n"))
  cat(paste("Pred. mean:\t",signif(prior.means[1],3),"\n"))
  cat(paste("Pred. SD:\t",signif(sd.x,4),"\n"))
  cat(paste("Density:\t",signif(f0,4),"\n\n"))


  sd.x = sqrt(sigma.x^2/n + prior.sds[2]^2)
  f1 = dnorm(x.bar,prior.means[2],sd.x)

  cat("Predictive density of the sample mean under component 1\n")
  cat("------------------------------------------------------\n")
  cat(paste("Sample mean:\t",signif(x.bar,3),"\n"))
  cat(paste("Pred. mean:\t",signif(prior.means[2],3),"\n"))
  cat(paste("Pred. SD:\t",signif(sd.x,4),"\n"))
  cat(paste("Density:\t",signif(f1,4),"\n\n"))

  qp0 = q0*f0/(q0*f0+q1*f1)
  qp1 = 1-qp0

  cat(paste("Post. mixing proportion for component 0:\t",signif(qp0,3),"\n"))
  cat(paste("Post. mixing proportion for component 1:\t",signif(qp1,3),"\n"))

  k1 = qnorm(0.0001,prior0[1],prior0[2])
  k2 = qnorm(0.9999,prior0[1],prior0[2])
  step.size = (k2-k1)/1000
  mu = seq(k1,k2,by=step.size)

  prior.0 = dnorm(mu,prior.means[1],prior.sds[1])
  prior.1 = dnorm(mu,prior.means[2],prior.sds[2])
  prior = q0*prior.0+q1*prior.1

  posterior.0 = dnorm(mu,post.mean0,post.sd0)
  posterior.1 = dnorm(mu,post.mean1,post.sd1)
  posterior = qp0*posterior.0+qp1*posterior.1

  loglik = -(mu-x.bar)^2/(2*sigma.x^2/n)
  loglik = loglik-max(loglik)
  likelihood = exp(loglik)

  normalizing.factor = sum(likelihood)*step.size
  likelihood = likelihood/normalizing.factor

  f.mu = approxfun(mu,likelihood)
  cat(paste("\nIntegral of likelihood over mu: ",round(integrate(f.mu,k1,k2)$value,5),"\n"))

  o.par = par(mfrow=c(2,2))

  ##plot the priors and the mixture prior

  y.max = max(prior.0,prior.1,prior)

  plot(mu,prior.0,ylim=c(0,y.max*1.1),
       xlab=expression(mu),ylab="Density"
       ,main="Mixture prior and it's components"
       ,type="l",lty=2,col="black")
  lines(mu,prior.1,lty=3,col="red")
  lines(mu,prior,lty=1,col="blue")
  legend(k1,y.max,legend=c(expression(prior[0]),expression(prior[1])
                    ,expression(prior[mix])),lty=c(2,3,1),col=c("black","red","blue"))

  ##plot the posteriors and the mixture posterior

  y.max = max(posterior.0,posterior.1,posterior)

  plot(mu,posterior.0,ylim=c(0,y.max*1.1),
       xlab=expression(mu),ylab="Density"
       ,main="Mixture posterior and it's components"
       ,type="l",lty=2,col="black")
  lines(mu,posterior.1,lty=3,col="red")
  lines(mu,posterior,lty=1,col="blue")
  legend(k1,y.max,legend=c(expression(posterior[0]),expression(posterior[1])
                    ,expression(posterior[mix])),lty=c(2,3,1),col=c("black","red","blue"))
  ##plot the mixture posterior likelihood and mixture posterior

  y.max = max(prior,posterior,likelihood)

  plot(mu,prior,ylim=c(0,y.max*1.1),
       xlab=expression(mu),ylab="Density"
       ,main="Mixture prior, likelihood and mixture posterior"
       ,type="l",lty=2,col="black")
  lines(mu,likelihood,lty=3,col="red")
  lines(mu,posterior,lty=1,col="blue")
  legend(k1,y.max,legend=c(expression(prior[mix]),expression(likelihood)
                    ,expression(posterior[mix])),lty=c(2,3,1),col=c("black","red","blue"))

  par(o.par)

  if(ret){
      cat("The argument ret is deprecated.\n")
      cat("The results are now always returned invisibly\n")
  }
  invisible(list(mu=mu,prior=prior,likelihood=likelihood,posterior=posterior))
}





