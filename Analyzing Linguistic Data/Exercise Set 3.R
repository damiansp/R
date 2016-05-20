##Ch 3 Exercises
data(alice)
alice <- tolower(alice)
length(alice) %% 40	#will have a remainder of 29 when divided into 40 parts
wonderland <- data.frame(word = alice[1:27240], chunk = cut(1:27240, breaks=40, labels=FALSE))
wonderland[1:5,]

wonderland$alice <- wonderland$word=="alice"
countOfAlice <- tapply(wonderland$alice, wonderland$chunk, sum)
countOfAlice.tab <- xtabs(~countOfAlice)

#(1)
wonderland$hare <- wonderland$word=="hare"
countOfHare <- tapply(wonderland$hare, wonderland$chunk, sum)
countOfHare.tab <- xtabs(~countOfHare)

wonderland$very <- wonderland$word=="very"
countOfvery <- tapply(wonderland$very, wonderland$chunk, sum)
countOfvery.tab <- xtabs(~countOfvery)



#(2)
par(mfrow=c(1,3))
truehist(countOfAlice, ylim=c(0, 0.44)); rug(countOfAlice); lines(density(countOfAlice))
truehist(countOfHare, ylim=c(0, 0.44)); rug(countOfHare); lines(density(countOfHare))
truehist(countOfvery, ylim=c(0, 0.44)); rug(countOfvery); lines(density(countOfvery))
par(mfrow=c(1,1))