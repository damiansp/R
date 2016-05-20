`VaR.exp.portf` <-
function(Q, copula, x.est, y.est, lambda1, lambda2,
      range, subdivisions = 1000, tol.f = 5e-5)   
{     
     uniroot(fbar.exp.portf, interval=range, copula = copula,x.est = x.est, y.est = y.est,
       lambda1 = lambda1, lambda2 = lambda2, q = Q,
          subdivisions = subdivisions,tol.f=tol.f)$root
}

