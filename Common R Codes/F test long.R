f.test.long <- function(v1, v2, n1, n2) {
	Fs <- v1/v2
	if(Fs >= 1) Fs else 1/Fs
	FLow <- qf(0.025, n1, n2)
	FUpp <- qf(0.975, n1, n2)
	if((Fs >= FLow) & (Fs <= FUpp)) {
		return("Variances not significantly different")
		} else {
			return("Variances significantly different")
			}
	}