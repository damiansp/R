##############################################################
# Given input: Contingency table of Observed Vals,           #
# Return: Contingency table of P values for each observation #
##############################################################

allison.binom.g <- function(M) {
	GTest <- g.test(M)
	Exp <- GTest$expected
	EProp <- Exp / sum(M)
	OProp <- M / sum(M)
	Cm <- ceiling(OProp - EProp)
	BP <- pbinom(M-Cm, sum(M), Exp / sum(M))
	BP <- Cm-BP
	return(BP)
}

allison.binom.chi <- function(M) {
	CSTest <- chisq.test(M, simulate.p.value=T)
	Exp <- CSTest$expected
	EProp <- Exp / sum(M)
	OProp <- CSTest$observed / sum(M)
	Cm <- ceiling(OProp - EProp)
	BP <- pbinom(M-Cm, sum(M), Exp / sum(M))
	BP <- Cm-BP
	BP
}