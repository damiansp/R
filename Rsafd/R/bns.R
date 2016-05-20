`bns` <-
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

