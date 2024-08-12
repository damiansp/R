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
names(m1)
m.summary <- summary(m1)
names(m.summary)

# est sd
sqrt(deviance(m1) / df.residual(m1))
m.summary$sigma

# (XTX)^-1
xtxi <- m.summary$cov.unscaled
sqrt(diag(xtxi)) * m.summary$sigma  # SEs for coefs
m.summary$coef

1 - deviance(m1) / sum((y - mean(y))^2)  # Rsq
m.summary$r.squared


# 9. Identifiability
gala$Adiff <- gala$Area - gala$Adjacent  # linearly dependent variable
g <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent + Adiff, gala)
summary(g)  # NAs estimated
