`gpdjoint.2p` <-
function(x, y, copula, x.est, y.est)
{
    h <- gpd.2p(x, x.est)
    g <- gpd.2p(y, y.est)
    val <- pcopula(copula, h, g)
    val
}

