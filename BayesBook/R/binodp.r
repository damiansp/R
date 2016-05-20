binodp<-function(x,n,pi = NULL, pi.prior = NULL, n.pi = 10, ret = FALSE){

  ## n - the number of trials in the binomial
  ## x - the number of observed successes
  ## pi - the probability of success
  ## pi.prior - the associated prior probability mass
  ## ret - if true then the likelihood and posterior are returned as a
  ## list

  if(x>n)
    stop("The number of observed successes (x) must be smaller than the number of trials (n)")
  if(n.pi<3)
    stop("Number of prior values of pi must be greater than 2")

  if(is.null(pi) | is.null(pi.prior)){
    pi<-seq(0,1, length = n.pi)
    pi.prior<-rep(1/n.pi,n.pi)
  }

  if(any(pi<0) | any(pi > 1))   ## check that probabilities lie on [0,1]
    stop("Values of pi must be between 0 and 1 inclusive")

  if(any(pi.prior<0)|any(pi.prior>1))
    stop("Prior probabilities must be between 0 and 1 inclusive")

  if(round(sum(pi.prior),7)!=1){
    warning("The prior probabilities did not sum to 1, therefore the prior has been normalized")
    pi.prior<-pi.prior/sum(pi.prior)
  }

  n.pi<-length(pi)

  likelihood<-dbinom(x,n,pi)
  posterior<-likelihood*pi.prior/sum(likelihood*pi.prior)

  plot(pi,posterior,ylim=c(0,1.1*max(posterior,pi.prior)),pch=20
       ,col="blue",
       xlab=expression(pi),ylab=expression(Probabilty(pi)))
  points(pi,pi.prior,pch=20,col="red")

  legend(max(c(0.05,min(pi))),max(posterior,pi.prior),pch=20,legend=c("Posterior","Prior"),col=c("blue","red"))

  ## calculate the Conditional distribution

  f.cond<-matrix(0,nrow=n.pi,ncol=n+1)
  rownames(f.cond)<-as.character(round(pi,3))
  colnames(f.cond)<-as.character(0:n)

  for(i in 1:n.pi)
    f.cond[i,]<-dbinom(0:n,n,pi[i])

  cat("Conditional distribution of x given pi and  n:\n\n")
  print(round(f.cond,4))

  ## caculate the joint distribution of pi and x given n

  f.joint<-diag(pi.prior)%*%f.cond
  cat("\nJoint distribution:\n\n")
  print(round(f.joint,4))

  ## calculate the marginal distribtion

  f.marg<-matrix(1,nrow=1,ncol=n.pi)%*%f.joint
  cat("\nMarginal distribution of x:\n\n")
  print(round(f.marg,4))
  cat("\n\n")

  ## finally display the prior, likelihood, and posterior

  results<-cbind(pi.prior,likelihood,posterior)
  rownames(results)<-as.character(round(pi,3))
  colnames(results)<-c("Prior","Likelihood","Posterior")

  print(results)

  if(ret){
      cat("The argument ret is deprecated.\n")
      cat("The results are now always returned invisibly\n")
  }
  invisible(list(pi=pi,pi.prior=pi.prior,likelihood=likelihood,
                posterior=posterior,
                f.cond=f.cond,f.joint=f.joint,f.marg=f.marg))
}
