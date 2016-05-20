`fns` <-
function(x, THETA)
{
    FORWARD <- THETA[1] + (THETA[2] + THETA[3] * x) * exp( - x/THETA[
        4])
    FORWARD
}

