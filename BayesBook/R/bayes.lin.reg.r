bayes.lin.reg = function(y,x, slope.prior = "flat"
                        ,intcpt.prior = "flat",
                        mb0 = 0, sb0 = 0, ma0 = 0, sa0 = 0,
                        sigma=NULL, alpha = 0.05, plot.data = FALSE,
                        pred.x = NULL, ret = FALSE){

  if(sum(is.na(y))>0 || sum(is.na(x))>0)
    stop("Error: x and y may not contain missing values")

  if(length(y)!=length(x))
    stop("Error: x and y are unequal lengths")

  if(!is.null(sigma) && sigma<=0){
    stop("Error: the std. deviation of the resisuals, sigma, must be greater than or equal to zero")
  }

  if(sum(slope.prior==c("normal","norm","n","flat","f"))==0)
    stop("The slope prior must be one of \"normal\" or \"flat\"")
  else if(sum(slope.prior==c("normal","norm","n"))==1)
    slope.prior = "normal"
  else if(sum(slope.prior==c("flat","f"))==1)
    slope.prior = "flat"

  if(sum(intcpt.prior==c("normal","norm","n","flat","f"))==0)
    stop("The interpcet prior must be one of \"normal\" or \"flat\"")
  else if(sum(intcpt.prior==c("normal","norm","n"))==1)
    intcpt.prior = "normal"
  else if(sum(intcpt.prior==c("flat","f"))==1)
    intcpt.prior = "flat"

  if(slope.prior=="normal" && sb0<=0)
    stop("Error: the prior std. devation sb0 must be greater than zero")

  if(intcpt.prior=="normal" && sa0<=0)
    stop("Error: the prior std. devation sa0 must be greater than zero")

  if(alpha<=0 || alpha>0.5)
    stop("Error: alpha must be in the range (0, 0.5]")

  if(length(y)<=2)
    stop("Error: you really should have more than 2 points for a regression!")

  n = length(y)
  x.bar = mean(x)
  y.bar = mean(y)
  x2.bar = mean(x^2)
  xy.bar = mean(x*y)
  y2.bar = mean(y^2)

  b.ls = (xy.bar-x.bar*y.bar)/(x2.bar-x.bar^2)
  fitted = y.bar + b.ls*(x-x.bar)
  residuals = y-fitted

  A0 = y.bar-b.ls*x.bar
  Ax.bar = y.bar

  sigma.known = TRUE
  if(is.null(sigma)){
    sigma.known = FALSE
    sigma = sqrt(sum((y-(Ax.bar+b.ls*(x-x.bar)))^2)/(n-2))
    cat(paste("Standard deviation of residuals: ",signif(sigma,3),"\n"))
  }else{
    cat(paste("Known standard deviation: ",signif(sigma,3),"\n"))
  }

  SSx = n*(x2.bar-x.bar^2)
  lb = 0
  ub = 0
  prior.b = rep(0,1001)
  beta = prior.b
  likelihood.b = prior.b
  posterior.b = prior.b

  if(slope.prior=="flat"){
    prior.prec.b = 0
    ## should be zero by default but make sure it is.
    mb0 = 0
    prec.ls = SSx/sigma^2
    sd.ls = sqrt(1/prec.ls)
    post.prec.b = prior.prec.b+prec.ls
    post.var.b = 1/post.prec.b
    post.sd.b = sqrt(post.var.b)
    post.mean.b = prior.prec.b/post.prec.b*mb0+prec.ls/post.prec.b*b.ls

    lb = post.mean.b-4*post.sd.b
    ub = post.mean.b+4*post.sd.b
    beta = seq(lb,ub,length=1001)
    prior.b = rep(1,1001)
    norm.const = (2*sum(prior.b)-prior.b[1]-prior.b[1001]*((ub-lb)/1000))/2
    prior.b = prior.b/norm.const
    likelihood.b = dnorm(beta,b.ls,sd.ls)
    posterior.b = dnorm(beta,post.mean.b,post.sd.b)
  }else{
    prior.prec.b = 1/sb0^2
    prec.ls = SSx/sigma^2
    sd.ls = sqrt(1/prec.ls)
    post.prec.b = prior.prec.b+prec.ls
    post.var.b = 1/post.prec.b
    post.sd.b = sqrt(post.var.b)
    post.mean.b = prior.prec.b/post.prec.b*mb0+prec.ls/post.prec.b*b.ls

    lb = post.mean.b-3*post.sd.b
    ub = post.mean.b+3*post.sd.b
    beta = seq(lb,ub,length=1001)
    prior.b = dnorm(beta,mb0,sb0)
    likelihood.b = dnorm(beta,b.ls,sd.ls)
    posterior.b = dnorm(beta,post.mean.b,post.sd.b)
  }

  old.par = par(mfrow=c(2,2))

  y.max = max(c(prior.b,likelihood.b,posterior.b))
  plot(beta,prior.b,type="l",col="black",lty=1,
       ylim=c(0,1.1*y.max),xlab=expression(beta),
       ylab="",
       main=expression(paste("Prior, likelihood and posterior for ",beta,
           sep="")),
       sub="(slope)")
  lines(beta,likelihood.b,lty=2,col="red")
  lines(beta,posterior.b,lty=3,col="blue")
  legend(beta[2],y.max,lty=1:3,col=c("black","red","blue"),
         legend=c("Prior","Likelihood","Posterior"),bty="n")

  alpha.xbar = rep(0,1001)
  prior.a = alpha.xbar
  likelihood.a = alpha.xbar
  posterior.a = alpha.xbar

  if(intcpt.prior=="flat"){
    prior.prec.a = 0
    ##should be zero by default but make sure
    ma0 = 0
    prec.ls = n/(sigma^2)
    sd.ls = sqrt(1/prec.ls)
    post.prec.a = prior.prec.a+prec.ls
    post.var.a = 1/post.prec.a
    post.sd.a = 1/sqrt(post.prec.a)
    post.mean.a = prior.prec.a/post.prec.a*ma0+prec.ls/post.prec.a*y.bar

    lb = post.mean.a-4*post.sd.a
    ub = post.mean.a+4*post.sd.a
    alpha.xbar = seq(lb,ub,length=1001)
    prior.a = rep(1,1001)
    norm.const = (2*sum(prior.a)-prior.a[1]-prior.a[1001]*((ub-lb)/1000))/2
    prior.a = prior.a/norm.const
    likelihood.a = dnorm(alpha.xbar,y.bar,sd.ls)
    posterior.a = dnorm(alpha.xbar,post.mean.a,post.sd.a)
  }else{
    prior.prec.a = 1/sa0^2
    prec.ls = n/(sigma^2)
    sd.ls = sqrt(1/prec.ls)
    post.prec.a = prior.prec.a+prec.ls
    post.var.a = 1/post.prec.a
    post.sd.a = sqrt(post.var.a)
    post.mean.a = prior.prec.a/post.prec.a*ma0+prec.ls/post.prec.a*y.bar

    lb = post.mean.a-3*post.sd.a
    ub = post.mean.a+3*post.sd.a
    alpha.xbar = seq(lb,ub,length=1001)
    prior.a = dnorm(alpha.xbar,ma0,sa0)
    likelihood.a = dnorm(alpha.xbar,y.bar,sd.ls)
    posterior.a = dnorm(alpha.xbar,post.mean.a,post.sd.a)
  }

  cat("\t\tPosterior Mean\tPosterior Std. Deviation\n")
  cat("\t\t--------------\t------------------------\n")
  cat(paste("Intercept:\t",signif(post.mean.a,4),"\t\t",
            signif(post.sd.a,5),"\n",sep=""))
  cat(paste("Slope:\t\t",signif(post.mean.b,4),"\t\t",
            signif(post.sd.b,5),"\n\n",sep=""))

  y.max = max(c(prior.a,likelihood.a,posterior.a))
  plot(alpha.xbar,prior.a,type="l",col="black",lty=1,
       ylim=c(0,1.1*y.max),xlab=expression(alpha),
       ylab="",
       main=expression(paste("Prior, likelihood and posterior for ",alpha[bar(x)],
           sep="")),
       sub="(intercept)")
  lines(alpha.xbar,likelihood.a,lty=2,col="red")
  lines(alpha.xbar,posterior.a,lty=3,col="blue")
  legend(alpha.xbar[2],y.max,lty=1:3,col=c("black","red","blue"),
         legend=c("Prior","Likelihood","Posterior"),bty="n")

  if(sigma.known){
    s.e = sqrt(x2.bar-x.bar^2)
    x.lwr = x.bar-3*s.e
    x.upr = x.bar+3*s.e
    x.values = seq(x.lwr,x.upr,length=1001)
    pred.y = post.mean.b*(x.values-x.bar)+post.mean.a

    se.pred = sqrt(post.var.a+(x.values-x.bar)^2*post.var.b+sigma^2)
    t.crit = qt(1-alpha*.5,n-2)
    pred.lb = pred.y-t.crit*se.pred
    pred.ub = pred.y+t.crit*se.pred
  }else{
    s.e = sqrt(x2.bar-x.bar^2)
    x.lwr = x.bar-3*s.e
    x.upr = x.bar+3*s.e
    x.values = seq(x.lwr,x.upr,length=1001)
    pred.y = post.mean.b*(x.values-x.bar)+post.mean.a

    se.pred = sqrt(post.var.a+(x.values-x.bar)^2*post.var.b+sigma^2)
    z.crit = qnorm(1-alpha*0.5)
    pred.lb = pred.y-z.crit*se.pred
    pred.ub = pred.y+z.crit*se.pred
  }

  y.max = max(pred.ub)
  y.min = min(pred.lb)

  print(c(y.min,y.max))


  if(plot.data){
    plot(y~x,main=paste("Predicitions with ",round(100*(1-alpha))
               ,"% bounds",sep=""),xlab="x",ylab="y",ylim=1.1*c(y.min,y.max))
    lines(x.values,pred.y,lty=1,col="black")
  }else{
    plot(x.values,pred.y,type="l",lty=1,col="black",
         main=paste("Predicitions with ",round(100*(1-alpha)),
           "% bounds",sep=""),xlab="x",ylab="y",
         ylim=1.1*c(y.min,y.max))
  }

  lines(x.values,pred.lb,lty=2,col="red")
  lines(x.values,pred.ub,lty=3,col="blue")

  legend(x.values[1],y.max,lty=1:3,col=c("black","red","blue"),
         legend=c("Predicted value",
           paste(round(100*(1-alpha)),"% lower bound",sep=""),
           paste(round(100*(1-alpha)),"% upper bound",sep="")),
         cex=0.7,bty="n")

  pred.y = NULL
  pred.se = NULL
  if(!is.null(pred.x)){
    pred.y = post.mean.a+post.mean.b*(pred.x-x.bar)
    pred.se = sqrt(post.var.a + (pred.x - x.bar)^2 * post.var.b + sigma^2)
    predicted.values = cbind(pred.x,pred.y,pred.se)
    cat("x\tPredicted.y\tSE\n")
    cat("----\t----------\t-----\n")
    n.pred.x = length(pred.x)
    for(i in 1:n.pred.x){
      cat(paste(signif(predicted.values[i,1],4),"\t",sep=""))
      cat(paste(signif(predicted.values[i,2],4),"\t\t",sep=""))
      cat(paste(signif(predicted.values[i,3],5),"\n",sep=""))
    }
  }

  par(old.par)
  if(ret){
      cat("The argument ret is deprecated.\n")
      cat("The results are now always returned invisibly\n")
  }
  if(!is.null(pred.x)){
      invisible(list(post.coef=c(post.mean.a,post.mean.b),
                  post.coef.sd=c(post.sd.a,post.sd.b),
                  pred.x=pred.x, pred.y = pred.y, pred.se = pred.se))
  }else{
      invisible(list(post.coef=c(post.mean.a,post.mean.b),
                     post.coef.sd=c(post.sd.a,post.sd.b)))
  }

}
