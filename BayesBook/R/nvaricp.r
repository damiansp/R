nvaricp = function(y,mu,S0,kappa,cred.int=FALSE, alpha = 0.05, ret=FALSE){
  n = length(y)
  SST = sum((y-mu)^2)

  if(kappa>0){
    S1 = S0+SST
    kappa1 = kappa+n

    k1 = qchisq(0.01,kappa)
    k2 = S0/k1
    k3 = sqrt(k2)


    sigma = seq(0,k3,length=1002)[-1]
    k4 = diff(sigma)[1]
    sigma.sq = sigma^2
    log.prior =  -((kappa-1)/2+1)*log(sigma.sq)-S0/(2*sigma.sq)
    prior = exp(log.prior)

    log.like =  -(n/2)*log(sigma.sq)-SST/(2*sigma.sq)
    likelihood =  exp(log.like)


    kint = ((2*sum(prior))-prior[1]-prior[1001])*k4/2*0.99
    prior = prior/kint

    posterior = prior*likelihood
    kint = ((2*sum(posterior))-posterior[1]-posterior[1001])*k4/2
    posterior = posterior/kint

    y.max = max(c(prior,posterior))
    k1 = qchisq(0.01,kappa1)
    k2 = S1/k1
    k3 = sqrt(k2)

    plot(sigma, prior, type="l", col="blue",ylim=c(0,1.1*y.max),
         xlim=c(0,k3),
         main=expression(
             paste("Shape of Inverse ", chi^2," and posterior for ",
                   sigma, sep="")),
         xlab=expression(sigma),
         ylab="Density")
    lines(sigma, posterior, lty=2, col="red")
  }else if(kappa==0){                   ## Jeffrey's prior
    S = 0
    S1 = S+SST
    kappa1 = kappa+n

    k1 = qchisq(0.001,kappa1)
    k2 = S1/k1
    k3 = sqrt(k2)
    k4 = k3/1000

    sigma = seq(0,k3,length=1002)[-1]
    sigma.sq = sigma^2
    likelihood = NULL

    log.posterior =  -((kappa1-1)/2+1)*log(sigma.sq)-S1/(2*sigma.sq)
    posterior = exp(log.posterior)
    kint = ((2*sum(posterior))-posterior[1]-posterior[1001])*k4/(2*.999)
    posterior = posterior/kint

    log.prior =  -((kappa-1)/2+1)*log(sigma.sq)-S0/(2*sigma.sq)
    prior = exp(log.prior)
    kint = ((2*sum(prior))-prior[1]-prior[1001])*k4/2
    prior = prior/kint

    k1 = qchisq(0.01,kappa1)
    k2 = S1/k1
    k3 = sqrt(k2)
    k4 = 1.2*max(posterior)

    plot(sigma,prior,type="l",col="blue", ylim=c(0,k4),
         main=expression(paste("Shape of prior and posterior for ", sigma,
             sep="")),
         xlab=expression(sigma),ylab="Density")
    lines(sigma,posterior,lty=2,col="red")
  }else if(kappa<0){
    S0 = 0
    S1 = S0+SST
    kappa1 = kappa+n

    k1 = qchisq(0.001,kappa1)
    k2 = S1/k1
    k3 = sqrt(k2)
    k4 = k3/1000

    sigma = seq(0,k3,length=1002)[-1]
    sigma.sq = sigma^2

    log.posterior =  -((kappa1-1)/2+1)*log(sigma.sq)-S1/(2*sigma.sq)
    posterior = exp(log.posterior)
    kint = ((2*sum(posterior))-posterior[1]-posterior[1001])*k4/(2*.999)
    posterior = posterior/kint

    log.prior =  -((kappa-1)/2+1)*log(sigma.sq)-S0/(2*sigma.sq)
    prior = exp(log.prior)
    kint = ((2*sum(prior))-prior[1]-prior[1001])*k4/2
    prior = prior/kint

    likelihood = NULL

    k1 = qchisq(0.01,kappa1)
    k2 = S1/k1
    k3 = sqrt(k2)
    k4 = 1.2*max(posterior)

    plot(sigma,prior,type="l",col="blue", xlim=c(0,k3),ylim=c(0,k4),
         main=expression(paste("Shape of prior and posterior for ", sigma,
             sep="")),
         xlab=expression(sigma),ylab="Density")
    lines(sigma,posterior,lty=2,col="red")
    legend(sigma[1],0.9*k4,lty=1:2,col=c("blue","red")
           ,legend=c("Prior","Posterior"))
  }

  cat(paste("S1: ",signif(S1,4)," kappa1 :", signif(kappa1,3),"\n",sep=""))

  if(cred.int){
    if(kappa1<2)
      cat("Unable to calculate credible interval for sigma if kappa1<=2\n")
    else{
      sigmahat.post.mean = sqrt(S1/(kappa1-2))
      cat(paste("Estimate of sigma using posterior mean: ",
                signif(sigmahat.post.mean,4),"\n",sep=""))
    }

    q50 = qchisq(0.5,kappa1)
    sigmahat.post.median = sqrt(S1/q50)
    cat(paste("Estimate of sigma using posterior median: ",
              signif(sigmahat.post.median,4),"\n",sep=""))

    cdf = sintegral(sigma,posterior)$cdf
    Finv = approxfun(cdf$y,cdf$x)
    lb = Finv(alpha/2)
    ub = Finv(1-alpha/2)
    cat(paste(round(100*(1-alpha)),"% credible interval for sigma: [",
              signif(lb,4),", ", signif(ub,4),"]\n",sep=""))
    abline(v=c(lb,ub),col="blue",lty=3)

  }


  if(ret){
      cat("The argument ret is deprecated.\n")
      cat("The results are now always returned invisibly\n")
  }
  invisible(list(sigma=sigma,prior=prior,likelihood=likelihood,posterior=posterior,S1=S1,kappa1=kappa1))
}

