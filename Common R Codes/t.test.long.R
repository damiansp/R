t.test.long <- function(m1, m2, n1, n2, v1, v2) {
	ts <- (m1-m2)/sqrt(((((n1-1)*v1)+((n2-1)*v2)) / (n1+n2-2)) * ((n1+n2)/(n1*n2))); ts
	if(ts >= 0) 2*(1-pt(ts, (n1+n2-2))) else 2*(1-pt(-ts, (n1+n2-2)))
}