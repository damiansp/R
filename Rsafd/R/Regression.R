#########################
# Regression Analysis

"lm.diag" <- 
function(LM)
{
    # LM should be an lm object
    # Use the ls.diag function, see its help file
    TST <- lsfit(model.matrix(LM), response(LM), intercept = F)
    DIAG <- ls.diag(TST)
    # std.dev =residual standard deviation (for each column of the response if it was a matrix). This is the square root of the quantity: the residual sum of squares divided by the degrees of freedom. The degrees of freedom are the number of non-missing observations minus the number of parameters.
    # hat =vector containing the diagonal of the hat matrix (see the hat function).
    # std.res =vector or matrix containing the standardized residuals. This uses hat and std.dev to standardize the residuals.
    # stud.res =vector or matrix containing the studentized residuals. This uses hat and a different estimate of the standard deviation.
    # cooks =vector or matrix containing Cook's distance for each observation. The element in the i-th row and j-th column is the measure of the distance between the parameter estimates for the j-th regression with and without the i-th observation.
    # correlation =correlation matrix for the parameter estimates.
    # std.err = vector or matrix of the standard errors of the parameter estimates.
    list(stddev = DIAG$stdev, hat = DIAG$hat, stdres = DIAG$std.res,
        studres = DIAG$stud.res, cooks = DIAG$cooks, correlation =
        DIAG$correlation, stderr = DIAG$std.err)
}

"fns" <- 
function(x, THETA)
{
    FORWARD <- THETA[1] + (THETA[2] + THETA[3] * x) * exp( - x/THETA[
        4])
    FORWARD
}

"yns" <- 
function(x, THETA)
{
    TT <- THETA[3] * THETA[4]
    TTT <- THETA[4] * (THETA[2] + TT)
    EX <- exp( - x/THETA[4])
    YIELD <- THETA[1] + (TTT * (1 - EX))/x - TT * EX
    YIELD
}

"bns" <- 
function(COUPON, AI, LIFE, X = 100, THETA = c(0.059999999999999998, 0, 0,
    1))
{
    NbBonds <- length(COUPON)
    TT <- THETA[3] * THETA[4]
    TTT <- THETA[4] * (THETA[2] + TT)
    LL <- floor(1 + LIFE)
    PRICE <- rep(0, NbBonds)
    DURATION <- rep(0, NbBonds)
    for(I in 1:NbBonds) {
        x <- seq(to = LIFE[I], by = 1, length = LL[I])
        EX <- exp( - x/THETA[4])
        DISCOUNT <- exp(x * THETA[1] + (TTT * (1 - EX)) - TT * (EX *
            x))
        CF <- rep((COUPON[I] * X)/100, LL[I])
        CF[LL[I]] <- CF[LL[I]] + X
        PRICE[I] <- sum(CF * DISCOUNT)
        NUM <- sum(x * CF * DISCOUNT)
        DURATION[I] <- NUM/PRICE[I]
    }
    PRICE <- PRICE - AI
    list(price = PRICE, duration = DURATION)
}

"bscall" <- 
function(TAU = 0.029999999999999999, K = 1, S, R = 0.10000000000000001, SIG = 
    0.14999999999999999)
{
    # TAU is the time TO maturity in YEARS
    # K is the strike
    # S is the spot
    # R is the yearly short interest rate
    # SIG is the (annualized) volatility
    # returns the price of a European call given by Black Scholes formul
    d1 <- log(S/K) + TAU * (R + SIG^2/2)
    d1 <- d1/(SIG * sqrt(TAU))
    d2 <- d1 - SIG * sqrt(TAU)
    S * pnorm(d1) - K * exp( - R * TAU) * pnorm(d2)
}

"isig"<-
function(TAU, K, S, R, CALL, TOL = 1e-005, JMAX = 1000)
{
    # TAU is the time TO maturity in years
    # K is the strike
    # S is the spot
    # R is the yearly short interest rate
    # CALL is the price of a call
    # returns the implied by Black Scholes formula
    # As is the code requires scalars (n vector) and outputs a scalar
    SIG1 <- 0
    SIG2 <- 1.
    C1 <- bscall(TAU, K, S, R, SIG1)
    C2 <- bscall(TAU, K, S, R, SIG2)
    if((C1 - CALL) * (C2 - CALL) >= 0)
        return(-1)
    for(J in 1:JMAX) {
        MIDSIG <- (SIG2 + SIG1)/2
        TC <- bscall(TAU, K, S, R, MIDSIG)
        if(TC < CALL)
            SIG1 <- MIDSIG
        else SIG2 <- MIDSIG
        if(SIG2 - SIG1 < TOL)
            return(SIG1)
    }
}

"kreg" <- 
function(x, y, xpred = x, kernel = 4, b)
{
    ########################################################################
    # kreg
    #       ----
    # kernel regression (possibly multivariate)
    #
    # Inputs
    # ------
    # x  nxp matrix of n observations & p explanatory variables
    # y  vector of n observations of the response variable
    # xpred n'xp matrix of n' regressors for prediction
    # kernel function
    #        1 <-> uniform
    #        2 <-> cosine
    #        3 <-> triangular
    #        4 <-> Epanechnikov
    #        5 <-> quartic
    #        6 <-> triweight
    #        7 <-> Gaussian
    # b  bandwidth
    #
    # Outputs
    # -------
    # xpred  same as in input
    # ypred  vector of the predictions (for the rows of xpred)
    # b      same as in input
    #
    ###########################################################################
    if(is.vector(x)) {
        n <- length(x)
        p <- 1
    }
    else if(is.matrix(x)) {
        n <- dim(x)[1]
        p <- dim(x)[2]
    }
    if(is.vector(xpred)) {
        npred <- length(xpred)
        ppred <- 1
    }
    else if(is.matrix(xpred)) {
        npred <- dim(xpred)[1]
        ppred <- dim(xpred)[2]
    }
    if(is.vector(y)) {
        ny <- length(y)
        py <- 1
    }
    else if(is.matrix(y)) {
        ny <- dim(y)[1]
        py <- dim(y)[2]
    }
    if(ny != n) {
        stop("The number of rows of x should be the same\n  as the number of rows of y"
            )
    }
    if(py != 1) {
        stop("y should not have more than one column")
    }
    if(p != ppred) {
        stop("The number of columns of x should be the same\n  as the number of columns of xpred"
            )
    }
    ypred <- rep(0, npred)
    x <- c(x)
    dim(x) <- c(length(x), 1)
    dim(y) <- c(length(y), 1)
    xxpred <- c(xpred)
    dim(xxpred) <- c(length(xxpred), 1)
    dim(ypred) <- c(length(ypred), 1)
    z <- .C("kreg",
        as.double(x),
        as.double(y),
        as.double(xxpred),
        ypred = as.double(ypred),
        as.integer(n),
        as.integer(p),
        as.integer(npred),
        as.integer(kernel),
        as.double(b))
    ypred <- z$ypred
    dim(ypred) <- c(npred, 1)
    list(xpred = xpred, ypred = ypred, b = b)
}
