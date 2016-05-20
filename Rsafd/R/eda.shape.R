`eda.shape` <-
function(x)
{   oop <- par(mfrow = c(2, 2))
    on.exit(par(oop))
    hist(x, probability=TRUE, main="Histogram", xlab="")
    boxplot(x, main ="Boxplot")
    iqd <- IQR(x)
    plot(density(x, width = 2 * iqd), ylab = "", 
        type = "l", main="Density")
    qqnorm(x)
    qqline(x)
    
}

