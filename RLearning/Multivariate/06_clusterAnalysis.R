#=========#=========#=========#=========#=========#=========#=========#=========
setwd('~/Learning/R/RLearning/Multivariate')

library(flexclust)
library(lattice)
library(mclust)
library(MVA)
library(mvtnorm)
#demo('Ch-CA')


measure <- structure(
  list(
    V1=1:20, 
    V2=c(34L, 37L, 38L, 36L, 38L, 43L, 40L, 38L, 40L, 41L, 36L, 36L, 34L, 33L, 
         36L, 37L, 34L, 36L, 38L, 35L), 
    V3=c(30L, 32L, 30L, 33L, 29L, 32L, 33L, 30L, 30L, 32L, 24L, 25L, 24L, 22L, 
         26L, 26L, 25L, 26L, 28L, 23L), 
    V4=c(32L, 37L, 36L, 39L, 33L, 38L, 42L, 40L, 37L, 39L, 35L, 37L, 37L, 34L, 
         38L, 37L, 38L, 37L, 40L, 35L)), 
  .Names=c("V1", "V2", "V3", "V4"), 
  class="data.frame", 
  row.names=c(NA, -20L))
measure <- measure[,-1]
names(measure) <- c("chest", "waist", "hips")
measure$gender <- gl(2, 10)
levels(measure$gender) <- c("male", "female")

dat <- rbind(rmvnorm(25, mean=c(3, 3)), 
             rmvnorm(20, mean=c(10, 8)), 
             rmvnorm(10, mean=c(20, 1)))
             
dm <- dist(measure[, c("chest", "waist", "hips")])
round(dm, 2)

plot(cs <- hclust(dm, method='single'))
plot(cs <- hclust(dm, method='complete'))
plot(cs <- hclust(dm, method='average'))

body.pc <- princomp(dm, cor=T)
x.lim <- range(body.pc$scores[, 1])
plot(body.pc$scores[, 1:2], pch=16, col=cutree(cs, h=7.5))
plot(body.pc$scores[, 1:2], pch=16, col=cutree(cs, h=6), asp=1)


jet <- structure(
  list(
    V1=c(82L, 89L, 101L, 107L, 115L, 122L, 127L, 137L, 147L, 166L, 174L, 175L, 
         177L, 184L, 187L, 189L, 194L, 197L, 201L, 204L, 255L, 328L), 
    V2=c(1.468, 1.605, 2.168, 2.054, 2.467, 1.294, 2.183, 2.426, 2.607, 4.567, 
         4.588, 3.618, 5.855, 2.898, 3.88, 0.455, 8.088, 6.502, 6.081, 7.105, 
         8.548, 6.321), 
    V3=c(3.3, 3.64, 4.87, 4.72, 4.11, 3.75, 3.97, 4.65, 3.84, 4.92, 3.82, 4.32,
         4.53, 4.48, 5.39, 4.99, 4.5, 5.2, 5.65, 5.4, 4.2, 6.45), 
    V4=c(0.166, 0.154, 0.177, 0.275, 0.298, 0.15, 0, 0.117, 0.155, 0.138, 0.249,
         0.143, 0.172, 0.178, 0.101, 0.008, 0.251, 0.366, 0.106, 0.089, 0.222, 
         0.187), 
    V5=c(0.1, 0.1, 2.9, 1.1, 1, 0.9, 2.4, 1.8, 2.3, 3.2, 3.5, 2.8, 2.5, 3, 3, 
         2.64, 2.7, 2.9, 2.9, 3.2, 2.9, 2),
    V6=c(0L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 0L, 1L, 1L, 
         1L, 1L, 0L, 1L)), 
  .Names=c("V1", "V2", "V3", "V4", "V5", "V6"), 
  class="data.frame", 
  row.names = c(NA, -22L))
colnames(jet) <- c("FFD", "SPR", "RGF", "PLF", "SLF", "CAR")
rownames(jet) <- c(
  "FH-1", "FJ-1", "F-86A", "F9F-2", "F-94A", "F3D-1", "F-89A", "XF10F-1",
  "F9F-6", "F-100A", "F4D-1", "F11F-1", "F-101A", "F3H-2", "F-102A", "F-8A", 
  "F-104B", "F-105B", "YF-107A", "F-106A", "F-4B", "F-111A")
jet$CAR <- factor(jet$CAR, labels = c("no", "yes"))
X <- scale(jet[, c('SPR', 'RGF', 'PLF', 'SLF')], center=F, scale=T)
dj <- dist(X)
plot(cc <- hclust(dj), main='Jets')

pr <- prcomp(dj)$x[, 1:2]
plot(pr,
     pch=(1:2)[cutree(cc, k=2)],
     col=c('black', 'red')[jet$CAR],
     xlim=range(pr) * c(1, 1.5))
legend('topright',
       col=c(1, 1, 2, 2),
       legend=c('1:no', '2:no', '1:yes', '2:yes'),
       pch=c(1, 2, 1, 2),
       title='Cluster / CAR',
       bty='n')
       


# 4. K-Means Clustering
# 4.1 Clustering the states of the USA based on crime-rate profiles
`crime` <- structure(
  c(2, 2.2, 2, 3.6, 3.5, 4.6, 10.7, 5.2, 5.5, 5.5, 6, 8.9, 11.3, 3.1, 2.5, 1.8, 
    9.2, 1, 4, 3.1, 4.4, 4.9, 9, 31, 7.1, 5.9, 8.1, 8.6, 11.2, 11.7, 6.7, 10.4, 
    10.1, 11.2, 8.1, 12.8, 8.1, 13.5, 2.9, 3.2, 5.3, 7, 11.5, 9.3, 3.2, 12.6, 5, 
    6.6, 11.3, 8.6, 4.8, 14.8, 21.5, 21.8, 29.7, 21.4, 23.8, 30.5, 33.2, 25.1,
    38.6, 25.9, 32.4, 67.4, 20.1, 31.8, 12.5, 29.2, 11.6, 17.7, 24.6, 32.9, 
    56.9, 43.6, 52.4, 26.5, 18.9, 26.4, 41.3, 43.9, 52.7, 23.1, 47, 28.4, 25.8, 
    28.9, 40.1, 36.4, 51.6, 17.3, 20, 21.9, 42.3, 46.9, 43, 25.3, 64.9, 53.4, 
    51.1, 44.9, 72.7, 31, 28, 24, 22, 193, 119, 192, 514, 269, 152, 142, 90, 
    325, 301, 73, 102, 42, 170, 7, 16, 51, 80, 124, 304, 754, 106, 41, 88, 99, 
    214, 367, 83, 208, 112, 65, 80, 224, 107, 240, 20, 21, 22, 145, 130, 169,
    59, 287, 135, 206, 343, 88, 106, 102, 92, 103, 331, 192, 205, 431, 265, 176, 
    235, 186, 434, 424, 162, 148, 179, 370, 32, 87, 184, 252, 241, 476, 668, 
    167, 99, 354, 525, 319, 605, 222, 274, 408, 172, 278, 482, 285, 354, 118, 
    178, 243, 329, 538, 437, 180, 354, 244, 286, 521, 401, 103, 803, 755, 949, 
    1071, 1294, 1198, 1221, 1071, 735, 988, 887, 1180, 1509, 783, 1004, 956, 
    1136, 385, 554, 748, 1188, 1042, 1296, 1728, 813, 625, 1225, 1340, 1453, 
    2221, 824, 1325, 1159, 1076, 1030, 1461, 1787, 2049, 783, 1003, 817, 1792, 
    1845, 1908, 915, 1604, 1861, 1967, 1696, 1162, 1339, 2347, 2208, 2697, 2189, 
    2568, 2758, 2924, 2822, 1654, 2574, 2333, 2938, 3378, 2802, 2785, 2801, 
    2500, 2049, 1939, 2677, 3008, 3090, 2978, 4131, 2522, 1358, 2423, 2846, 
    2984, 4373, 1740, 2126, 2304, 1845, 2305, 3417, 3142, 3987, 3314, 2800, 
    3078, 4231, 3712, 4337, 4074, 3489, 4267, 4163, 3384, 3910, 3759, 164, 228, 
    181, 906, 705, 447, 637, 776, 354, 376, 328, 628, 800, 254, 288, 158,
    439, 120, 99, 168, 258, 272, 545, 975, 219, 169, 208, 277, 430, 598, 193, 
    544, 267, 150, 195, 442, 649, 714, 215, 181, 169, 486, 343, 419, 223, 478, 
    315, 402, 762, 604, 328), 
  .Dim=c(51L, 7L), 
  .Dimnames=list(
    c("ME", "NH", "VT", "MA", "RI", "CT", "NY", "NJ", "PA", "OH", "IN", "IL", 
      "MI", "WI", "MN", "IA", "MO", "ND", "SD", "NE", "KS", "DE", "MD", "DC", 
      "VA", "WV", "NC", "SC", "GA", "FL", "KY", "TN", "AL", "MS", "AR", "LA", 
      "OK", "TX", "MT", "ID", "WY", "CO", "NM", "AZ", "UT", "NV", "WA", "OR", 
      "CA", "AK", "HI"), 
    c("Murder", "Rape", "Robbery", "Assault", "Burglary", "Theft", "Vehicle")))
crime <- as.data.frame(crime)
head(crime)
plot(crime, pch = ".", cex = 1.5)

subset(crime, Murder > 15)
plot(crime, pch=c(".", "+")[(rownames(crime) == "DC") + 1], cex=1.5)

sapply(crime, var)
rge <- sapply(crime, function(x) diff(range(x)))
crime_s <- sweep(crime, 2, rge, FUN = "/")
sapply(crime_s, var)

n <- nrow(crime_s)
wss <- rep(0, 6)
wss[1] <- (n - 1) * sum(sapply(crime_s, var))
for (i in 2:6) { wss[i] <- sum(kmeans(crime_s, centers = i)$withinss) }
plot(1:6, 
     wss, 
     type="b", 
     xlab="Number of groups", 
     ylab="Within groups sum of squares")

kmeans(crime_s, centers = 2)$centers * rge
crime_pca <- prcomp(crime_s)
plot(crime_pca$x[, 1:2], type='n', asp=1) #, pch=16, col=kmeans(crime_s, centers=2)$cluster)
text(crime_pca$x[, 1:2], 
     rownames(crime_s), 
     col=kmeans(crime_s, centers=2)$cluster)
plot(crime_pca$x[, 1:2], type='n', asp=1)
text(crime_pca$x[, 1:2], 
     rownames(crime_s), 
     col=kmeans(crime_s, centers=3)$cluster)

# 4.2 Clustering Romano-British pottery
pottery.dist <- dist(
    pots <- scale(pottery[, colnames(pottery) != "kiln"], center=F))
levelplot(as.matrix(pottery.dist), xlab = "Pot Number", ylab = "Pot Number")

pottery.cluster <- kmeans(pots, centers=3)$cluster
xtabs(~pottery.cluster + kiln, data=pottery)



# 5. Model-Based Clustering
# 5.2 Maximum likelihood estimation in a finite mixture density with 
#     multivariate normal components
cnt <- c(
  "Iceland", "Norway", "Sweden", "Finland", "Denmark", "UK", "Eire",
  "Germany", "Netherlands", "Belgium", "Switzerland", "France", "Spain",
  "Portugal", "Italy", "Greece", "Yugoslavia", "Albania", "Bulgaria", "Romania",
  "Hungary", "Czechia", "Slovakia", "Poland", "CIS", "Lithuania", "Latvia",
  "Estonia")
thomson <- expand.grid(answer=factor(c("no", "yes")),
                       question=factor(paste("Q", 1:6, sep="")),
                       country=factor(cnt, levels=cnt))  
thomson$Freq <- c(
  0, 5, 0, 5, 0, 4, 0, 5, 0, 5, 0, 5, 1, 6, 1, 5, 0, 6, 0, 5, 0, 4, 1, 4, 0, 11, 
  4, 7, 0, 7, 0, 11, 5, 5, 3, 6, 0, 6, 2, 4, 0, 6, 0, 6, 1, 5, 2, 4, 1, 12, 4, 
  9, 0, 12, 3, 9, 7, 4, 6, 7, 7, 12, 2, 16, 0, 20, 1, 19, 9, 10, 0, 17, 0, 1, 1, 
  2, 0, 3, 2, 0, 2, 0, 0, 3, 0, 14, 0, 13, 0, 13, 2, 12, 11, 2, 1, 13, 0, 8, 0, 
  8, 0, 8, 1, 7, 2, 5, 1, 7, 2, 0, 0, 2, 0, 2, 1, 1, 2, 0, 0, 2, 0, 5, 0, 5, 0,
  4, 2, 2, 5, 0, 0, 4, 7, 3, 1, 7, 3, 5, 8, 2, 10, 0, 1, 7, 11, 1, 0, 12, 2, 8, 
  5, 6, 11, 0, 0, 11, 5, 1, 0, 6, 2, 4, 3, 3, 6, 0, 0, 6, 8, 7, 0, 15, 1, 13, 9, 
  6, 13, 2, 0, 15, 7, 1, 0, 8, 3, 5, 7, 1, 8, 0, 0, 7, 11, 4, 0, 15, 7, 8, 11, 
  4, 15, 0, 0, 14, 3, 2, 2, 3, 3, 2, 3, 2, 3, 3, 2, 3, 3, 0, 0, 3, 2, 1, 3, 0, 
  3, 0, 0, 3, 7, 0, 0, 6, 6, 1, 6, 1, 6, 1, 0, 7, 4, 1, 0, 5, 1, 4, 5, 0, 5, 0, 
  0, 5, 18, 2, 0, 20, 17, 3, 20, 0, 20, 0, 0, 20, 13, 0, 1, 14, 14, 0, 16, 0, 
  13, 0, 15, 0, 18, 0, 0, 19, 13, 5, 17, 2, 17, 0, 0, 19, 7, 0, 1, 6, 5, 2, 7, 
  0, 7, 0, 1, 6, 8, 0, 0, 8, 8, 0, 8, 0, 8, 0, 0, 8, 5, 0, 0, 5, 5, 0, 5, 0, 5, 
  0, 0, 5,2, 2, 0, 3, 0, 3, 3, 0, 3, 0, 0, 3)
ttab <- xtabs(Freq ~ country + answer + question, data=thomson)
thomsonprop <- prop.table(ttab, c(1,3))[,"yes",]
plot(1:(22 * 6), 
     rep(-1, 22 * 6), 
     ylim=c(-nlevels(thomson$country), -1), 
     type="n",
     axes=F, 
     xlab="", 
     ylab="")
for (q in 1:6) {   
  tmp <- ttab[,,q]
  xstart <- (q - 1) * 22 + 1
  y <- -rep(1:nrow(tmp), rowSums(tmp))
  x <- xstart + unlist(sapply(rowSums(tmp), function(i) 1:i))
  pch <- unlist(apply(tmp, 1, function(x) c(rep(19, x[2]), rep(1, x[1]))))
  points(x, y, pch = pch)
}
axis(2, 
     at=-(1:nlevels(thomson$country)), 
     labels=levels(thomson$country),
     las=2, 
     tick=F, 
     line=0)
mtext(text=paste("Question", 1:6), 3, at=22 * (0:5), adj=0)
(mc <- Mclust(thomsonprop))
plot(mc, thomsonprop, what="BIC", col="black")
cl <- mc$classification
nm <- unlist(sapply(1:3, function(i) names(cl[cl == i])))
ttab <- ttab[nm,,]
plot(1:(22 * 6), 
     rep(-1, 22 * 6), 
     ylim=c(-nlevels(thomson$country), -1), 
     type="n",
     axes=F, 
     xlab="", 
     ylab="")
for (q in 1:6) {   
  tmp <- ttab[,,q]
  xstart <- (q - 1) * 22 + 1
  y <- -rep(1:nrow(tmp), rowSums(tmp))
  x <- xstart + unlist(sapply(rowSums(tmp), function(i) 1:i))
  pch <- unlist(apply(tmp, 1, function(x) c(rep(19, x[2]), rep(1, x[1]))))
  points(x, y, pch=pch)
}
axis(2, 
     at=-(1:nlevels(thomson$country)), 
     labels=dimnames(ttab)[[1]], 
     las=2, 
     tick=F, 
     line=0)
mtext(text=paste("Question", 1:6), 3, at=22 * (0:5), adj=0)
abline(h=-cumsum(table(cl))[-3] - 0.5, col="grey")
text(-c(0.75, 0.75, 0.75), -cumsum(table(cl)) + table(cl)/2,
     label=paste("Cluster", 1:3), 
     srt=90, 
     pos=1)