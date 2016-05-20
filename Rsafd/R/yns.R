`yns` <-
function(x, THETA)
{
    TT <- THETA[3] * THETA[4]
    TTT <- THETA[4] * (THETA[2] + TT)
    EX <- exp( - x/THETA[4])
    YIELD <- THETA[1] + (TTT * (1 - EX))/x - TT * EX
    YIELD
}

