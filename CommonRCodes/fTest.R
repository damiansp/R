#F Test for sig. differenc in variances (set at 0.05; can be altered in qf() parameters)

f.test <- function(x, y){
	Fs <- var(x) / var(y)
	if(Fs >= 1) Fs else 1/Fs
	FLow <- qf(0.025, length(x), length(y))
	FUpp <- qf(0.975, length(x), length(y))
	if(Fs >= FLow & Fs <= FUpp) return("Variances not significantly different") else return("Variances significantly different")
	}