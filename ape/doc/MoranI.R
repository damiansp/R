###################################################
### chunk number 1: 
###################################################
#line 22 "/private/tmp/Rtmp3myfcO/R.INSTALL11053e46/ape/inst/doc/MoranI.Rnw"
options(width=60)


###################################################
### chunk number 2: 
###################################################
#line 120 "/private/tmp/Rtmp3myfcO/R.INSTALL11053e46/ape/inst/doc/MoranI.Rnw"
body <- c(4.09434, 3.61092, 2.37024, 2.02815, -1.46968)
longevity <- c(4.74493, 3.3322, 3.3673, 2.89037, 2.30259)
names(body) <- names(longevity) <- c("Homo", "Pongo", "Macaca", "Ateles", "Galago")


###################################################
### chunk number 3: 
###################################################
#line 129 "/private/tmp/Rtmp3myfcO/R.INSTALL11053e46/ape/inst/doc/MoranI.Rnw"
library(ape)
trnwk <- "((((Homo:0.21,Pongo:0.21):0.28,Macaca:0.49):0.13,Ateles:0.62)"
trnwk[2] <- ":0.38,Galago:1.00);"
tr <- read.tree(text = trnwk)
plot(tr)
axisPhylo()


###################################################
### chunk number 4: 
###################################################
#line 141 "/private/tmp/Rtmp3myfcO/R.INSTALL11053e46/ape/inst/doc/MoranI.Rnw"
w <- 1/cophenetic(tr)
w


###################################################
### chunk number 5: 
###################################################
#line 147 "/private/tmp/Rtmp3myfcO/R.INSTALL11053e46/ape/inst/doc/MoranI.Rnw"
diag(w) <- 0


###################################################
### chunk number 6: 
###################################################
#line 152 "/private/tmp/Rtmp3myfcO/R.INSTALL11053e46/ape/inst/doc/MoranI.Rnw"
Moran.I(body, w)


###################################################
### chunk number 7: 
###################################################
#line 173 "/private/tmp/Rtmp3myfcO/R.INSTALL11053e46/ape/inst/doc/MoranI.Rnw"
Moran.I(body, w, alt = "greater")


###################################################
### chunk number 8: 
###################################################
#line 179 "/private/tmp/Rtmp3myfcO/R.INSTALL11053e46/ape/inst/doc/MoranI.Rnw"
Moran.I(longevity, w)


###################################################
### chunk number 9: 
###################################################
#line 241 "/private/tmp/Rtmp3myfcO/R.INSTALL11053e46/ape/inst/doc/MoranI.Rnw"
data(carnivora)
carnivora$log10SW <- log10(carnivora$SW)
carnivora$log10FW <- log10(carnivora$FW)


###################################################
### chunk number 10: 
###################################################
#line 248 "/private/tmp/Rtmp3myfcO/R.INSTALL11053e46/ape/inst/doc/MoranI.Rnw"
fm1.carn <- log10SW ~ Order/SuperFamily/Family/Genus
co1 <- correlogram.formula(fm1.carn, data = carnivora)
plot(co1)


###################################################
### chunk number 11: 
###################################################
#line 266 "/private/tmp/Rtmp3myfcO/R.INSTALL11053e46/ape/inst/doc/MoranI.Rnw"
fm2.carn <- log10SW + log10FW ~ Order/SuperFamily/Family/Genus
co2 <- correlogram.formula(fm2.carn, data = carnivora)
print(plot(co2))


###################################################
### chunk number 12: 
###################################################
#line 277 "/private/tmp/Rtmp3myfcO/R.INSTALL11053e46/ape/inst/doc/MoranI.Rnw"
plot(co2, FALSE)


