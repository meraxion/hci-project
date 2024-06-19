# Imports ----
library(dagitty)
library(rethinking)

set.seed(41)

# Causal Model ----
## Simple ----
gs <- dagitty("dag{G -> E
              G [exposure]
              E [outcome]}")
impliedConditionalIndependencies(gs)



## Personality ----
gp <- dagitty("dag{G -> E <- P
              G [exposure]
              E [outcome]}")
impliedConditionalIndependencies(gp)

# Generative Model ----

## Simple
"
E ~ dnorm(mu, sigma),
mu <- b0 + b[G],
b0 ~ dnorm(0, 1),
b[G] ~ dnorm(0, 1),
sigma ~ dexp(1)
"

## Personality
"
E ~ dnorm(mu, sigma),
mu <- b0 + b1[G] + b2[P],
b0 ~ dnorm(0,1),
b1[G] ~ dnorm(0,1),
b2[P] ~ dnorm(0,1),
sigma ~ dexp(1)
"

## Ordered Categorical Outcomes
"
R ~ Ordered-logit(theta[i], kappa)
theta[i] = 0
Kappa[k] ~ dnorm(0, 1.5)
"


# Simulation ----
## Very Simplest - intercept only

N <- 100 # 100 
E <- rnorm(N)

dsim0 <- data.frame(N, E)
msim0 <- ulam(
  alist(
    E ~ dnorm(mu, sigma),
    mu <- b0,
    b0 ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data=dsim0
)
precis(msim0)
post <- extract.samples(msim0)
dens(post$b0)

## Simple ----
## Sim 1: Equal enjoyment in both games:
N <- 100 # 100 participants
G <- sample(c(rep(1, N/2), rep(2, N/2))) # 50 play Game 1, 50 Game 2, and shuffle them.
E <- rnorm(N)

dsim1 <- data.frame(N, G, E)

msim1 <- ulam(
  alist(
    E ~ dnorm(mu, sigma),
    mu <- b0 + b[G],
    b0 ~ dnorm(0, 1),
    b[G] ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data=dsim1
)
precis(msim1, depth=2)

post <- extract.samples(msim1)
coeffs <- post$b
dens(coeffs[,1])
dens(coeffs[,2])



## Sim 2: Game 1 is much more fun than game 2:
N = 100
G <- sample(c(rep(1, N/2), rep(2, N/2))) # 50 play Game 1, 50 Game 2, and shuffle them.
Gbs <- c(1.5, 0)
E <- rnorm(N, Gbs[G])
  
dsim2 <- data.frame(N, G, E)

msim2 <- ulam(
  alist(
    E ~ dnorm(mu, sigma),
    mu <- b0 + b[G],
    b0 ~ dnorm(0, 1),
    b[G] ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data=dsim2
)
precis(msim2, depth=2)

post <- extract.samples(msim2)
coeffs <- post$b
dens(coeffs[,1])
dens(coeffs[,2])

# So, here it...kind of retrieves the effect? It finds that, correctly, Game 1 has on average 1.5 higher enjoyment
# than Game 2, but doesn't find the "right" values for the two coeffs.
# Something to think about.

## Personality ----
# No effect sim
N = 100
G <- sample(c(rep(1, N/2), rep(2, N/2))) # 50 play Game 1, 50 Game 2, and shuffle them.
Gbs <- c(0,0)
Pbs <- c(0,0)
E <- rnorm(N, Gbs[G] + Pbs[G])

dsim3 <- data.frame(N, G, E)

msim3 <- ulam(
  alist(
    E ~ dnorm(mu, sigma),
    mu <- b0 + b1[G] + b2[G],
    b0 ~ dnorm(0, 1),
    b1[G] ~ dnorm(0, 1),
    b2[G] ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data=dsim3
)
precis(msim3, depth=2)

post <- extract.samples(msim3)
coeffs <- c(post$b1, post$b2)
dens(coeffs[,1])
dens(coeffs[,2])

# Game type effect sim
N = 100
G <- sample(c(rep(1, N/2), rep(2, N/2))) # 50 play Game 1, 50 Game 2, and shuffle them.
Gbs <- c(1.5,0)
Pbs <- c(0,0)
E <- rnorm(N, Gbs[G] + Pbs[G])

dsim4 <- data.frame(N, G, E)

msim4 <- ulam(
  alist(
    E ~ dnorm(mu, sigma),
    mu <- b0 + b1[G] + b2[G],
    b0 ~ dnorm(0, 1),
    b1[G] ~ dnorm(0, 1),
    b2[G] ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data=dsim4
)
precis(msim4, depth=2)

post <- extract.samples(msim4)
coeffs <- c(post$b1, post$b2)
dens(coeffs[,1])
dens(coeffs[,2])

# Personality effect sim
N = 1000
G <- sample(c(rep(1, N/2), rep(2, N/2))) # 50 play Game 1, 50 Game 2, and shuffle them.
Gbs <- c(0,0)
P <- sample(c(rep(1, N/2), rep(2, N/2))) # 50 have personality type 1, 50 personality type 2
Pbs <- c(1.5,0)
E <- rnorm(N, Gbs[G] + Pbs[P])

dsim5 <- data.frame(N, G, E, P)

msim5 <- ulam(
  alist(
    E ~ dnorm(mu, sigma),
    mu <- b0 + b1[G] + b2[P],
    b0 ~ dnorm(0, 1),
    b1[G] ~ dnorm(0, 1),
    b2[G] ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data=dsim5
)
precis(msim5, depth=2)

post <- extract.samples(msim5)
coeffs <- c(post$b1, post$b2)
dens(coeffs[,1])
dens(coeffs[,2])

# Model with covariance


# Dual effect sim




## Ordered Logit ----
## Dead Simple
N = 100
E <- sample(c(1:7), N, replace=TRUE)

dordlogsim1 <- data.frame(N, E)

mordlogsim1 <- ulam(
  alist(
    E ~ dordlogit(0, cutpoints),
    cutpoints ~ dnorm(0, 1.5)
  ), data=dordlogsim1, chains=4, cores=4
)
# cumulative log-probs:
precis(mordlogsim1 ,depth=2)
# cumulative probabilities:
round(inv_logit(coef(mordlogsim1)), 3)

## Now with game type and personality indicators,
# Some effect of gender, some of interaction
N = 100
G <- sample(c(rep(1, N/2), rep(2, N/2))) # 50 play Game 1, 50 Game 2, and shuffle them.
P <- sample(c(rep(1, N/2), rep(2, N/2))) # 50 have personality type 1, 50 personality type 2

# making continuous "enjoyment" data
# which I model here as a linear regression with some noise.
E_cont <- 0 + 2*G+0*P+0*G*P + rnorm(N)
# defining the scales we used for our question
cut_points <- 5
# "cutting" that continuous data to fit into ordered categories:
E <- cut(E_cont, breaks = cut_points, labels = c(-2, -1, 0, 1, 2), ordered_result = TRUE)

dordlogsim2 <- data.frame(N, E, G, P)

mordlogsim2 <- ulam(
  alist(
    E ~ dordlogit(phi, cutpoints),
    phi <- bG*G + bP*P + bGP*G*P,
    c(bG, bP, bGP) ~ dnorm(0, 0.5),
    cutpoints ~ dnorm(0, 1.5)
  ), data=dordlogsim2, chains=4, cores=4
)

# cumulative log-probs
precis(mordlogsim2, depth=2)
# cumulative probabilities:
round(inv_logit(coef(mordlogsim2)), 3)

## Only interaction effect
N = 100
G <- sample(c(rep(1, N/2), rep(2, N/2))) # 50 play Game 1, 50 Game 2, and shuffle them.
P <- sample(c(rep(1, N/2), rep(2, N/2))) # 50 have personality type 1, 50 personality type 2

# making continuous "enjoyment" data
# which I model here as a linear regression with some noise.
E_cont <- 0 + 0*G+0*P+2*G*P + rnorm(N)
# defining the scales we used for our question
cut_points <- 5
# "cutting" that continuous data to fit into ordered categories:
E <- cut(E_cont, breaks = cut_points, labels = c(-2, -1, 0, 1, 2), ordered_result = TRUE)

dordlogsim3 <- data.frame(N, E, G, P)
mordlogsim3 <- stan( fit=mordlogsim2@stanfit, data=dordlogsim3, chains=4 ) 

# cumulative log-probs
precis(mordlogsim3, depth=2)
# cumulative probabilities:
round(inv_logit(coef(mordlogsim3)), 3)

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

# plot the simulated data
plot(E)

# plot posterior predictive distribution
mu4 <- link(mordlogsim4)

mu_mean4 <- apply(mu4, 2, mean)
mu_PI <- apply(mu4, 2, PI)

d4sim <- sim(mordlogsim4)
d_PI <- apply(d4sim, 2, PI)

plot(mu_mean4 ~ dordlogsim4$E, col=rangi2, ylim=range(mu_PI),
     xlab="Observed divorce", ylab="Predicted divorce")

simplehist(dordlogsim4$E, xlim=c(0.5,5.5), xlab="response")
plot(precis(mordlogsim4, depth=2))

cum_pr_k <- (cumsum(table(dordlogsim4$E)/nrow(dordlogsim4)))
plot(1:5, cum_pr_k, type="b", xlab="response", ylim=c(0,1))

plot(1:4, round(inv_logit(coef(mordlogsim4)[4:7]), 3))

simplehist(as.vector(d4sim), xlab="response")

  # Statistical Model ----
## Simple
m1 <- ulam(
  alist(
    E ~ dnorm(mu, sigma),
    mu <- b0 + b[G],
    b0 <- dnorm(0, 1),
    b[G] ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data=d
)
precis(m1, depth=2)


## Regression Personality

## Interaction




# Restart ----
# Right, so what I actually want to do is to generate two groups and just estimate the means for both of them,
# and then take the posterior predictive of both and see if the 95% HDIs overlap.

# The Generative Model now becomes
N <- 100
P_G1 <- rnorm(N)
E_G1 <- rnorm(N, mean = 0 + 0.5*P_G1)
P_G2 <- rnorm(N)
E_G2 <- rnorm(N, mean = 0 + 2*P_G2)

dsimz <- data.frame(N, E_G1, P_G1, E_G2, P_G2)
msimz <- ulam(
  alist(
    E_G1 ~ dnorm(mu1, sigma1),
    mu1 <- b0_1 + b1_1*P_G1,
    b0_1 ~ dnorm(0,1),
    b1_1 ~ dnorm(0,1),
    sigma1 ~ dexp(1),
    
    E_G2 ~ dnorm(mu2, sigma2),
    mu2 <- b0_2 + b1_2*P_G2,
    b0_2 ~ dnorm(0,1),
    b1_2 ~ dnorm(0,1),
    sigma2 ~ dexp(1)
  ), data=dsimz
)
plot(precis(msimz))

post <- extract.samples(msimz)

P_seq1 <- seq(from=min(dsimz$P_G1), to=max(dsimz$P_G1), length.out=100)
P_seq2 <- seq(from=min(dsimz$P_G2), to=max(dsimz$P_G2), length.out=100)
mu <- link(msimz, data=list(P_G1=P_seq1, P_G2=P_seq2))
mu_mean_1 <- apply(mu$mu1, 2, mean)
mu.PI_1 <- apply(mu$mu1, 2, PI)
mu_mean_2 <- apply(mu$mu2, 2, mean)
mu.PI_2 <- apply(mu$mu2, 2, PI)

plot(E_G1 ~ P_G1, data=dsimz, col="blue")
lines(P_seq1, mu_mean_1, lwd=2)
shade(mu.PI_1, P_seq1)

points(E_G2 ~ P_G2, data=dsimz, col="red")
lines(P_seq2, mu_mean_2, lwd=2)
shade(mu.PI_2, P_seq2)

# This shows the intercept for each game
dens(post$b0_1, show.HPDI = 0.95, col=rgb(0,0,1,1/4), xlim=c(0, 2.5))
dens(post$b0_2, show.HPDI = 0.95, col=rgb(1,0,0,1/4), add = TRUE)

# This shows the effect of personality on each game
dens(post$b1_1, show.HPDI = 0.95, col=rgb(0,0,1,1/4), xlim=c(0, 2.5))
dens(post$b1_2, show.HPDI = 0.95, col=rgb(1,0,0,1/4), add = TRUE)

# This does the same, but slightly prettier
frame()

x <- post$b1_1
adj <- 0.5
main <-  ""
thed <-  density(x, adjust = adj)
plot(thed, main=main, xlim=c(0, 2.5))
hpd <- HPDI(x, prob=TRUE)
shade(thed, hpd, col=col.alpha("blue",0.3))

x <- post$b1_2
thed <- density(x, adjust=adj)
lines(thed$x, thed$y)
hpd <- HPDI(x, prob=TRUE)
shade(thed, hpd, col=col.alpha("red",0.3))

