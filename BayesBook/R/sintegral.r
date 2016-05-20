sintegral = function(x, fx, n.pts = 256, ret = FALSE){
    ## numerically integrates fx over x using Simpsons rule
    ## x - a sequence of x values
    ## fx - the value of the function to be integrated at x
    ##    - or a function
    ## n.pts - the number of points to be used in the integration
    ## ret - if true returns the partial sums of the integration
    ## note this argument is deprecated and ignored

    message("Note: sintegral's behavior has changed.\nTo get the value of the integral use sintegral(x,fx)$value.\nTo get the cdf use sintegral(x,fx)$cdf")

    if(class(fx)=="function")
        fx = fx(x)

    n.x = length(x)

    if(n.x!=length(fx))
        stop("Unequal input vector lengths")

    if(n.pts<64)
        n.pts = 64

    ## use linear approximation to get equally spaced x values


    ap = approx(x,fx,n=2*n.pts+1)

    h = diff(ap$x)[1]

    integral = h*(ap$y[2*(1:n.pts)-1] + 4*ap$y[2*(1:n.pts)] +
                  ap$y[2*(1:n.pts)+1])/3


    invisible(list(value = sum(integral),
                   cdf = list(x=ap$x[2*(1:n.pts)],y=cumsum(integral))))
}
