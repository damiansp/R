`l1fit` <-
function(x,y, intercept = TRUE)
{
    warning("l1fit() in R is just a wrapper to rq(). Use that instead!")
    if(intercept) rq(y ~ x, tau = 0.5)
    else rq(y ~ x - 1, tau = 0.5)
}

