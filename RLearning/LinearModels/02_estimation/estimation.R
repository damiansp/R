library(faraway)


# 8 Example
data(gala)
gala

m1 <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data=gala)
summary(m1)


# Inefficient method for computing coefs (also inaccurate if correlation is high)
x <- model.matrix(~ Area + Elevation + Nearest + Scruz + Adjacent, data=gala)
y <- gala$Species
xtxi <- solve(t(x) %*% x)  # (XTX)^-1
beta.hat <- xtxi %*% t(x) %*% y
beta.hat


# Better but not perfect:
solve(crossprod(x, x),  crossprod(x, y))  # crossprod(A, B) =  ATB

