# Imports ----
library(dagitty)
library(rethinking)

set.seed(41)


# SIM
## No interaction effect
N = 100
G <- sample(c(rep(1, N/2), rep(2, N/2))) # 50 play Game 1, 50 Game 2, and shuffle them.
P <- sample(c(rep(1, N/2), rep(2, N/2))) # 50 have personality type 1, 50 personality type 2

# making continuous "enjoyment" data
# which I model here as a linear regression with some noise.
E_cont <- 0 + 1*G+1*P+0*G*P + rnorm(N)
cut_points <- c(-Inf, 1.5, 2.5, 3.5, 4.5, Inf)
E <- findInterval(E_cont, cut_points, all.inside = TRUE)

dordlogsim4 <- data.frame(N, E, G, P)
mordlogsim4 <- ulam(
  alist(
    E ~ dordlogit(phi, cutpoints),
    phi <- bG*G + bP*P + bGP*G*P,
    c(bG, bP, bGP) ~ dnorm(0, 0.5),
    cutpoints ~ dnorm(0, 1.5)
  ), data=dordlogsim4, chains=4, cores=4
)

# cumulative log-probs
precis(mordlogsim4, depth=2)
# cumulative probabilities:
round(inv_logit(coef(mordlogsim4)), 3)

# Plot the data
simplehist(dordlogsim4$E, xlim=c(0.5,5.5), xlab="response")
# Plot posterior predictive distribution
simplehist(as.vector(d4sim), xlab="response")


# REAL
# import real data
d <- data.frame()

mordlog4 <- ulam(
  alist(
    E ~ dordlogit(phi, cutpoints),
    phi <- bG*G + bP*P + bGP*G*P,
    c(bG, bP, bGP) ~ dnorm(0, 0.5),
    cutpoints ~ dnorm(0, 1.5)
  ), data=d, chains=4, cores=4
)
