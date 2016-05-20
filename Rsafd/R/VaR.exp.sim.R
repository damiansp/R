`VaR.exp.sim` <-
function(n, Q, copula, x.est, y.est, lambda1, lambda2 ) 
{
    sim.uv <- rcopula(copula, n)
    Xsim <- gpd.2q(sim.uv$x,x.est)
    Ysim <- gpd.2q(sim.uv$y,y.est)
    ww <- log(lambda1*exp(Xsim) + lambda2*exp(Ysim))
    varv <- -as.vector(quantile(ww,Q))
    esf <- vector(length = length(Q), mode = "numeric")
    for (i in 1:length(Q))  esf[i] <- -mean(ww[ww < -varv[i]])

    val <- c(n,varv,esf)
    nn <-  vector(length = length(Q)*2 + 1, mode = "character")
    nn[1] <- "Simulation size"
    nn[2:(length(Q)+1)] <- paste("VaR Q=",Q,sep = "")
    nn[(length(Q)+2):(2*length(Q)+1)] <- paste("ES Q=",Q,sep = "")
    names(val) <- nn
    val
}

