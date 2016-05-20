`bscall` <-
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

