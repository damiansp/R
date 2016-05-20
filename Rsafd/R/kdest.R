`kdest` <-
function(X,Y,H,N=256,lims=c(range(X),range(Y)),COND=0,PLOT=T,XNAME="X",YNAME="Y")
{
    # Compute from a bivariate sample of values of (X,Y) on a regular NxN grid
    # the values of the joint density of (X,Y) if COND=0, a joint estimate of the
    # conditional density of Y given X if COND=1 or
    # a joint estimate of the conditional density of X given Y if COND=2
    # Use a 2D-Gaussian kernel and condition by division by the marginal obtained by integration

    LX <- length(X)
    if(length(Y) != LX)
        stop("Vectors must have same lengths"
            )
    GRIDX <- seq((5 * lims[1] - lims[2])/4, (5 * lims[2] - lims[1])/4, length = N)
    DELTAX <- (3 * (lims[2] - lims[1]))/(2 * 256)
    GRIDY <- seq((5 * lims[3] - lims[4])/4, (5 * lims[4] - lims[3])/4, length = N)
    DELTAY <- (3 * (lims[4] - lims[3]))/(2 * 256)
    if(missing(H))
        H <- c(bandwidth.nrd(X),bandwidth.nrd(Y))
    H <- H/4
    DX <- outer(GRIDX, X, "-")/H[1]
    DY <- outer(GRIDY, Y, "-")/H[2]
    Z <- matrix(dnorm(DX), N, LX) %*% t(matrix(dnorm(DY), N, LX))/(LX * H[1] * H[2])
    cat("Bandwidths values used: ", H,"\n")
    MARGX <- outer(DELTAX * DELTAY * apply(Z,1,sum),rep(1,N),"*")
    if(COND == 1)
        Z[MARGX > 1e-005] <- Z[MARGX>1e-005]/MARGX[MARGX>1e-005]
    MARGY <- outer(rep(1, N), DELTAX*DELTAY*apply(Z, 2, sum),"*")
    if(COND == 2)
        Z[MARGY > 1e-005] <- Z[MARGY >1e-005]/MARGY[MARGY >1e-005]
    if(PLOT == T) {
        persp3d(GRIDX, GRIDY, Z, aspect=c(1, 1, 0.5), col = "lightblue",xlab = XNAME, ylab = YNAME, zlab = "Density")
    }
    list(deltax=DELTAX,deltay=DELTAY,gridx=GRIDX,gridy=GRIDY,z=Z)
}

