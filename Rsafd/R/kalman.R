`kalman` <-
function(FF, SigV, GG, SigW, Xhat, Omega, Y)
{
    Delta <- GG %*% Omega %*% t(GG) + SigW
    Theta <- FF %*% Omega %*% t(GG)
    X <- FF %*% Xhat + Theta %*% solve(Delta) %*% (Y - GG %*% Xhat)
    Om <- FF %*% Omega %*% t(FF) + SigV - Theta %*% solve(Delta) %*% t(
        Theta)
    Ret <- list(xpred = X, error = Om)
    Ret
}

