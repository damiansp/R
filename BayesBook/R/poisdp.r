poisdp = function(y.obs, mu, mu.prior, ret=FALSE){
  if(length(y.obs)==0 || is.null(y.obs))
    stop("Error: y.obs must contain at least one value")

  if(sum(y.obs<0)>0)
    stop("Error: y.obs cannot contain negative values")

  if(length(mu)!=length(mu.prior))
    stop("Error: the lengths of mu and mu.prior are unequal.\nThere must be a corresponding probability for each value of mu")

  if(sum(mu<=0)>0)
    stop("Error: the values of the rate paramter mu, must be greater than zero")

  if(sum(mu.prior<0)>0 || sum(mu.prior>1)>0)
    stop("Error: prior probabilities must be between zero and one")

  if(sum(mu.prior)!=1){
    warning("Warning: the prior does not sum to 1. The prior has been rescaled")
    mu.prior = mu.prior/sum(mu.prior)
  }

  n = length(y.obs)
  if(n==1){
    k = y.obs

    m = length(mu)
    cat("Prior\n")
    cat("-----\n")
    prior.matrix = cbind(mu,mu.prior)
    colnames(prior.matrix) = c("mu","Pr(mu)")
    print(prior.matrix)
    k1 = 0.9995
    k2 = mu[m]
    cat(paste("\nk1:\t",k1,"\nk2:\t",k2,"\n\n"))

    n = qpois(k1,k2)
    y1 = 0:n

    k1 = mu
    k1[k1==0] = 1e-9

    f.cond = matrix(0,nrow=m,ncol=n+1)
    for(i in 1:m)
      f.cond[i,] = dpois(y1,k1[i])

    rownames(f.cond) = mu
    colnames(f.cond) = y1

    cat("Conditional probability of y1 given mu\n")
    cat("-------------------------------------\n")
    print(f.cond)
    cat("\n\n")

    matrix.prior = diag(mu.prior)
    f.joint = matrix.prior%*%f.cond

    cat("Joint probability of y1 and mu\n")
    cat("-------------------------------\n")
    print(f.joint)
    cat("\n\n")

    f.marg = apply(f.joint,2,sum)

    cat("Marginal probability of y1\n")
    cat("-------------------------\n")
    print(f.marg)
    cat("\n\n")

    ## extract the column of f.cond corresponding to y.obs
    likelihood = f.cond[,y.obs+1]
    posterior = likelihood*mu.prior
    posterior = posterior/sum(posterior)

    results = cbind(mu,mu.prior,likelihood,posterior)
    colnames(results) = c("Mu","Prior","Likelihood","Posterior")
    print(results)
  }else{
    m = length(mu)

    likelihood = rep(0,m)
    for(i in 1:m)
      likelihood[i] = exp(sum(dpois(y.obs,max(mu[i],1e-9),log=TRUE)))

    posterior = likelihood*mu.prior
    posterior = posterior/sum(posterior)

    results = cbind(mu,mu.prior,likelihood,posterior)
    colnames(results) = c("Mu","Prior","Likelihood","Posterior")
    print(results)
  }

  plot.data = rbind(mu.prior,posterior)
  if(length(mu.prior)<=10){
    colnames(plot.data) = mu
    y.max = max(mu.prior,posterior)
    midpoints = barplot(plot.data,beside=TRUE,col=c("red","blue")
                       ,xlab=expression(mu)
                       ,ylab=expression(paste("Pr(",mu,"|","y)"))
                       ,ylim=c(0,y.max*1.1)
                       ,main=expression(
                           paste("Prior and posterior probability for ", mu
                                 ," given the data y")))
    legend(midpoints[1,1],y.max,legend=c("Prior","Posterior")
           ,fill=c("red","blue"))
    box()
  }else{
    y.max = max(mu.prior,posterior)
    plot(mu,mu.prior,type="l",lty=2,col="red"
         ,xlab=expression(mu)
         ,ylab=expression(paste("Pr(",mu,"|","y)"))
         ,ylim=c(0,y.max*1.1)
         ,main=expression(paste("Prior and posterior probability for ", mu
             ," given the data y")))
    lines(mu,posterior,lty=1,col="blue")
    legend(mu[2],y.max,lty=c(2,1),col=c("red","blue"),
           legend=c("Prior","Posterior"))

  }
  if(ret){
      cat("The argument ret is deprecated.\n")
      cat("The results are now always returned invisibly\n")
  }
  invisible(list(mu=mu,mu.prior=mu.prior,likelihood=likelihood,posterior=posterior))
}



