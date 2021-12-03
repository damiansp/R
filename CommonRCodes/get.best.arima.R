get.best.arima <- function(x.ts, maxord=c(1, 1, 1, 1, 1, 1)) {
  best.aic <- Inf
  n <- length(x.ts)
  for (p in 0:maxord[1]) {
    for(d in 0:maxord[2]) { 
      for(q in 0:maxord[3]) {
      	for(P in 0:maxord[4]) {
      	  for(D in 0:maxord[5]) {
      	  	for(Q in 0:maxord[6]) {
      	  	  fit <- arima(x.ts, 
      	  	               order=c(p, d, q), 
      	  	               seas=list(order=c(P, D, Q), frequency(x.ts)), 
      	  	               method='CSS')
      	  	  fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
              if (fit.aic < best.aic) {
			    best.aic <- fit.aic
			    best.fit <- fit
			    best.model <- c(p, d, q, P, D, Q)
		      }
	        }
	      }
	    }
	  }
	}
  }
  list(best.aic, best.fit, best.model)	
}