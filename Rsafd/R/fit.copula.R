`fit.copula` <-
function(data,family="normal", plotPicture=T, init.est=NA, epsilon=1e-5, ...)
{
     if (!inherits(data,"empirical.copula")) {data <- empirical.copula(data)}
     families.implemented <- c("normal","frank","kimeldorf.sampson",
                      "gumbel","galambos","husler.reiss",
                     "bb1","tawn","bb2","bb3","bb5","bb6","bb7","normal.mix","bb4", "joe")

     w <- charmatch(family, families.implemented, nomatch = -1)
     if (w <= 0) {
           message <- paste("Family ",family," is not implemented in fit.copula function")
           message <- paste(message," Valid family names are ",families.implemented)
           stop(message)
     }
     if (is.na(init.est)) {
   Kt <- Kendalls.tau(empirical.copula(data))
 # Kt <- cor(data@x,data@y)
       if (Kt <= 0) {
           message <- paste("Negative dependence is detected in fit.copula function")
           message <- paste(message," Negative dependence is not implemented yet.")
           stop(message)
     }
     
    #starting point of parameters!
    Ktrd <- round(Kt, digits = 1)
    if (Ktrd == 0) {
          Ktrd <- 0.1
          message <- paste("Very small dependence is detected in fit.copula function")
          warning(message)
    }
    if (Ktrd == 1) Ktrd <- 0.9
    nline <- Ktrd *10 + 1
    x0 <- switch(w, Kend.table[nline,2],
          Kend.table[nline,3],Kend.table[nline,4],Kend.table[nline,5],Kend.table[nline,6],
          Kend.table[nline,7],  c(Kend.table[nline,9], Kend.table[nline,8]),
          c(Kend.table[nline,11],Kend.table[nline,11], Kend.table[nline,10]),
          c(Kend.table[nline,12],Kend.table[nline,13]),
          c(Kend.table[nline,14],Kend.table[nline,15]),
          c(Kend.table[nline,16],Kend.table[nline,17]),
          c(Kend.table[nline,18],Kend.table[nline,19]),
          c(Kend.table[nline,20],Kend.table[nline,21]),
          c(0.5, Kend.table[nline,2],Kend.table[nline,2]),
          c(Kend.table[nline,22],Kend.table[nline,23]),
          c(2.0) )
    }
    else{ x0 <- init.est }
     
    copula <- switch(w, normal.copula(x0), frank.copula(x0), kimeldorf.sampson.copula(x0),        gumbel.copula(x0), galambos.copula(x0), husler.reiss.copula(x0),
            bb1.copula(x0[1],x0[2]), tawn.copula(x0[1],x0[2],x0[3]),
            bb2.copula(x0[1],x0[2]),
            bb3.copula(x0[1],x0[2]),
            bb5.copula(x0[1],x0[2]),
            bb6.copula(x0[1],x0[2]),
            bb7.copula(x0[1],x0[2]),
            normal.mix.copula(x0[1],x0[2],x0[3]),
            bb4.copula(x0[1],x0[2]), joe.copula(x0[1]))
    copTemp <- copula

    neg.log.lik <- function(param, copula, data) {
               copula@parameters <- param
               val <- dcopula(copula, data@x, data@y)
               nll <- sum(log(val))
               -nll
    }
    fit <- nlminb(x0, neg.log.lik, copula=copula, data=data, lower=copula@param.lowbnd, upper=copula@param.upbnd)
    cat("Maximum Likelihood Estimation of copula:\n")
    cat("MLE terminated due to ",fit$message,"\n")
    copula@parameters <- fit$par
    names(copula@parameters) <- names(copTemp@parameters)

    if (plotPicture) {
          contour.plot(data)
          contour.plot(copula, labex = 0, col = 5, lwd = 2, lty = 4, add = T, xlab = "", ylab = "")
          title("Empirical and Fitted Level Sets")  
    }
    copula
}

