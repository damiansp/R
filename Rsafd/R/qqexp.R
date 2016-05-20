`qqexp` <-
function(x, nq = 50)
{
    values <- seq(from = 0.0001, to = 0.99,length = nq)
    qvalues <- qexp(values)
    plot(qvalues, quantile(x, probs=values),
    xlab="Theoretical Quantiles",
    ylab = "Sample Quantiles")
    title("Exponential Q-Q Plot")
    abline(lmrob(quantile(x, probs=values)~qvalues))
}

